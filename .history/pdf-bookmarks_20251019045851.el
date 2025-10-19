;; -*- lexical-binding: t; -*-
;; Author: Raoul Comninos

;;; pdf-bookmarks.el --- Simple bookmark system for PDF Tools  -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple bookmark system for pdf-tools that stores bookmarks
;; based on PDF filename (not hash-based).
;;
;; Features:
;; - Create bookmarks at current page (with custom names)
;; - Access/navigate to bookmarks via completion
;; - Return to last position before bookmark jump
;; - Rename bookmarks
;; - Delete bookmarks
;; - Bookmarks stored per-file in ~/.emacs.d/pdf-bookmarks/
;;
;; Keybindings:
;;   ' b - Create bookmark at current page
;;   ' g - Access/navigate to (go to) bookmark
;;   ' l - Go back to Last position (before bookmark jump)
;;   ' r - Rename a bookmark
;;   ' d - Delete a bookmark
;;
;; To use this, add to your init.el:
;;   (require 'pdf-bookmarks)

;;; Code:

(require 'cl-lib)  ; For cl-remove-if

;;;; Variables ------------------------------------------------------------------

(defvar pdf-bookmarks-storage-directory
  (expand-file-name "pdf-bookmarks/" user-emacs-directory)
  "Directory to store PDF bookmark files.")

(defvar pdf-bookmarks-current-file-bookmarks nil
  "List of bookmarks for the current PDF file.")

(defvar pdf-bookmarks-last-position nil
  "Stores the last position before jumping to a bookmark.
This is a plist with :page and optionally :bookmark-name.")


;;;; Helper Functions -----------------------------------------------------------

(defun pdf-bookmarks-ensure-directory ()
  "Ensure the bookmarks storage directory exists."
  (unless (file-directory-p pdf-bookmarks-storage-directory)
    (make-directory pdf-bookmarks-storage-directory t)))

(defun pdf-bookmarks-get-file-path ()
  "Get the bookmark file path for the current PDF."
  (when-let ((pdf-file (buffer-file-name)))
    (pdf-bookmarks-ensure-directory)
    (expand-file-name 
     (concat (file-name-base pdf-file) ".bookmarks")
     pdf-bookmarks-storage-directory)))

(defun pdf-bookmarks-load ()
  "Load bookmarks for the current PDF file."
  (when-let ((bookmark-file (pdf-bookmarks-get-file-path)))
    (setq pdf-bookmarks-current-file-bookmarks
          (if (file-exists-p bookmark-file)
              (with-temp-buffer
                (insert-file-contents bookmark-file)
                (read (current-buffer)))
            nil))))

(defun pdf-bookmarks-save ()
  "Save bookmarks for the current PDF file."
  (when-let ((bookmark-file (pdf-bookmarks-get-file-path)))
    (with-temp-file bookmark-file
      (prin1 pdf-bookmarks-current-file-bookmarks (current-buffer)))))


;;;; Interactive Commands -------------------------------------------------------

(defun pdf-bookmarks-create ()
  "Create a bookmark at the current page."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (let* ((current-page (pdf-view-current-page))
         (bookmark-name (read-string "Bookmark name: " 
                                     (format "Page %03d" current-page)))
         (bookmark-entry (list :name bookmark-name 
                              :page current-page
                              :timestamp (current-time))))
    
    ;; Remove any existing bookmark with the same name
    (setq pdf-bookmarks-current-file-bookmarks
          (cl-remove-if (lambda (bm) (string= (plist-get bm :name) bookmark-name))
                        pdf-bookmarks-current-file-bookmarks))
    
    ;; Add the new bookmark
    (push bookmark-entry pdf-bookmarks-current-file-bookmarks)
    
    (pdf-bookmarks-save)
    ;; Clear the minibuffer immediately
    (message nil)))

(defun pdf-bookmarks-access ()
  "Access and navigate to a bookmark."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* (;; Sort bookmarks numerically by page number FIRST
           (sorted-bookmarks (cl-sort (copy-sequence pdf-bookmarks-current-file-bookmarks)
                                      #'<
                                      :key (lambda (bm) (plist-get bm :page))))
           ;; Create display strings with zero-padded page numbers for proper sorting
           (bookmark-choices (mapcar (lambda (bm)
                                      (let ((name (plist-get bm :name))
                                            (page (plist-get bm :page)))
                                        (cons (if (string-prefix-p "Page " name)
                                                  ;; If name already starts with "Page", replace with padded version
                                                  (replace-regexp-in-string "^Page [0-9]+" 
                                                                           (format "Page %03d" page)
                                                                           name)
                                                ;; Otherwise, prepend page number
                                                (format "Page %03d -- %s" page name))
                                              bm)))
                                    sorted-bookmarks))
           (choice (completing-read "Go to bookmark: " 
                                   (mapcar #'car bookmark-choices)
                                   nil t))
           (selected-bookmark (cdr (assoc choice bookmark-choices))))
      
      (when selected-bookmark
        ;; Save current position before jumping
        (let ((current-page (pdf-view-current-page)))
          (setq pdf-bookmarks-last-position 
                (list :page current-page
                      :pdf-file (buffer-file-name))))
        
        ;; Navigate to the selected bookmark
        (let ((page (plist-get selected-bookmark :page)))
          (pdf-view-goto-page page)
          ;; Clear the minibuffer/echo area completely
          (message nil))))))

(defun pdf-bookmarks-delete ()
  "Delete a bookmark."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* ((current-page (pdf-view-current-page))
           ;; Sort bookmarks numerically by page number FIRST
           (sorted-bookmarks (cl-sort (copy-sequence pdf-bookmarks-current-file-bookmarks)
                                      #'<
                                      :key (lambda (bm) (plist-get bm :page))))
           ;; Create display strings with zero-padded page numbers for proper sorting
           (bookmark-choices (mapcar (lambda (bm)
                                      (let ((name (plist-get bm :name))
                                            (page (plist-get bm :page)))
                                        (cons (if (string-prefix-p "Page " name)
                                                  ;; If name already starts with "Page", replace with padded version
                                                  (replace-regexp-in-string "^Page [0-9]+" 
                                                                           (format "Page %03d" page)
                                                                           name)
                                                ;; Otherwise, prepend page number
                                                (format "Page %03d -- %s" page name))
                                              bm)))
                                    sorted-bookmarks))
           ;; Find bookmark on current page for initial selection
           (current-page-choice (car (cl-find-if (lambda (choice)
                                                   (= (plist-get (cdr choice) :page) current-page))
                                                 bookmark-choices)))
           (choice (completing-read "Delete bookmark: " 
                                   (mapcar #'car bookmark-choices)
                                   nil t
                                   current-page-choice))  ;; Use as INITIAL-INPUT instead
           (selected-bookmark (cdr (assoc choice bookmark-choices))))
      
      (when selected-bookmark
        (setq pdf-bookmarks-current-file-bookmarks
              (cl-remove selected-bookmark pdf-bookmarks-current-file-bookmarks))
        (pdf-bookmarks-save)
        ;; Clear the minibuffer immediately
        (message nil)))))

(defun pdf-bookmarks-rename ()
  "Rename a bookmark."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* ((current-page (pdf-view-current-page))
           ;; Sort bookmarks numerically by page number FIRST
           (sorted-bookmarks (cl-sort (copy-sequence pdf-bookmarks-current-file-bookmarks)
                                      #'<
                                      :key (lambda (bm) (plist-get bm :page))))
           ;; Create display strings with zero-padded page numbers for proper sorting
           (bookmark-choices (mapcar (lambda (bm)
                                      (let ((name (plist-get bm :name))
                                            (page (plist-get bm :page)))
                                        (cons (if (string-prefix-p "Page " name)
                                                  ;; If name already starts with "Page", replace with padded version
                                                  (replace-regexp-in-string "^Page [0-9]+" 
                                                                           (format "Page %03d" page)
                                                                           name)
                                                ;; Otherwise, prepend page number
                                                (format "Page %03d -- %s" page name))
                                              bm)))
                                    sorted-bookmarks))
           ;; Find bookmark on current page for initial selection
           (current-page-choice (car (cl-find-if (lambda (choice)
                                                   (= (plist-get (cdr choice) :page) current-page))
                                                 bookmark-choices)))
           (choice (completing-read "Rename bookmark: " 
                                   (mapcar #'car bookmark-choices)
                                   nil t
                                   current-page-choice))  ;; Use as INITIAL-INPUT instead
           (selected-bookmark (cdr (assoc choice bookmark-choices))))
      
      (when selected-bookmark
        (let* ((old-name (plist-get selected-bookmark :name))
               (new-name (read-string "New bookmark name: " old-name)))
          (when (and new-name (not (string-empty-p new-name)))
            ;; Update the bookmark name
            (plist-put selected-bookmark :name new-name)
            (pdf-bookmarks-save)
            ;; Clear the minibuffer immediately
            (message nil)))))))

(defun pdf-bookmarks-back ()
  "Toggle between current position and last saved position.
On first use (before jumping to any bookmark), saves current position
and waits for you to navigate elsewhere before toggling."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (let ((current-page (pdf-view-current-page))
        (current-file (buffer-file-name)))
    
    (if (null pdf-bookmarks-last-position)
        ;; First time: save current position and inform user
        (progn
          (setq pdf-bookmarks-last-position 
                (list :page current-page
                      :pdf-file current-file))
          (message "Position saved (page %d). Navigate elsewhere, then use ' l to return." current-page))
      
      (let ((last-page (plist-get pdf-bookmarks-last-position :page))
            (last-file (plist-get pdf-bookmarks-last-position :pdf-file)))
        
        ;; Check if we're in the same PDF file
        (if (not (equal last-file current-file))
            (message "Last position was in a different PDF: %s" 
                     (file-name-nondirectory last-file))
          
          ;; Check if we're already at the saved position
          (if (= current-page last-page)
              (message "Already at saved position (page %d)" current-page)
            
            ;; Save current position before jumping
            (setq pdf-bookmarks-last-position 
                  (list :page current-page
                        :pdf-file current-file))
            
            ;; Go back to the last position
            (pdf-view-goto-page last-page)
            (message nil)))))))


;;;; Keybindings ----------------------------------------------------------------

;; Set up keybindings when pdf-view is loaded
;; Using "'" (apostrophe/quote) as prefix - typically available in most modes
;; First unbind ' to make it available as a prefix
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "'") nil)
  (define-key pdf-view-mode-map (kbd "' b") #'pdf-bookmarks-create)
  (define-key pdf-view-mode-map (kbd "' g") #'pdf-bookmarks-access)
  (define-key pdf-view-mode-map (kbd "' l") #'pdf-bookmarks-back)
  (define-key pdf-view-mode-map (kbd "' r") #'pdf-bookmarks-rename)
  (define-key pdf-view-mode-map (kbd "' d") #'pdf-bookmarks-delete))


(provide 'pdf-bookmarks)
;;; pdf-bookmarks.el ends here
