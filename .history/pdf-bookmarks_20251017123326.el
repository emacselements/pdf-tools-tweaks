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
;; - Rename bookmarks
;; - Delete bookmarks
;; - Bookmarks stored per-file in ~/.emacs.d/pdf-bookmarks/
;;
;; Keybindings:
;;   ' b - Create bookmark at current page
;;   ' g - Access/navigate to (go to) bookmark
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
                                     (format "Page %02d" current-page)))
         (bookmark-entry (list :name bookmark-name 
                              :page current-page
                              :timestamp (current-time))))
    
    ;; Remove any existing bookmark with the same name
    (setq pdf-bookmarks-current-file-bookmarks
          (cl-remove-if (lambda (bm) (string= (plist-get bm :name) bookmark-name))
                        pdf-bookmarks-current-file-bookmarks))
    
    ;; Add the new bookmark
    (push bookmark-entry pdf-bookmarks-current-file-bookmarks)
    
    ;; Sort bookmarks by page number
    (setq pdf-bookmarks-current-file-bookmarks
          (sort pdf-bookmarks-current-file-bookmarks
                (lambda (a b) (< (plist-get a :page) (plist-get b :page)))))
    
    (pdf-bookmarks-save)
    (message "Bookmark '%s' created for page %d" bookmark-name current-page)))

(defun pdf-bookmarks-access ()
  "Access and navigate to a bookmark."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* ((bookmark-choices
            (mapcar (lambda (bm)
                      (cons (format "%s (Page %d)" 
                                    (plist-get bm :name)
                                    (plist-get bm :page))
                            bm))
                    pdf-bookmarks-current-file-bookmarks))
           (choice (completing-read "Go to bookmark: " bookmark-choices nil t))
           (selected-bookmark (cdr (assoc choice bookmark-choices))))
      
      (when selected-bookmark
        (let ((page (plist-get selected-bookmark :page)))
          (pdf-view-goto-page page)
          (message "Navigated to bookmark: %s (Page %d)" 
                   (plist-get selected-bookmark :name) page))))))

(defun pdf-bookmarks-delete ()
  "Delete a bookmark."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* ((bookmark-choices
            (mapcar (lambda (bm)
                      (cons (format "%s (Page %d)" 
                                    (plist-get bm :name)
                                    (plist-get bm :page))
                            bm))
                    pdf-bookmarks-current-file-bookmarks))
           (choice (completing-read "Delete bookmark: " bookmark-choices nil t))
           (selected-bookmark (cdr (assoc choice bookmark-choices))))
      
      (when selected-bookmark
        (setq pdf-bookmarks-current-file-bookmarks
              (cl-remove selected-bookmark pdf-bookmarks-current-file-bookmarks))
        (pdf-bookmarks-save)
        (message "Deleted bookmark: %s" (plist-get selected-bookmark :name))))))

(defun pdf-bookmarks-rename ()
  "Rename a bookmark."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* ((bookmark-choices
            (mapcar (lambda (bm)
                      (cons (format "%s (Page %d)" 
                                    (plist-get bm :name)
                                    (plist-get bm :page))
                            bm))
                    pdf-bookmarks-current-file-bookmarks))
           (choice (completing-read "Rename bookmark: " bookmark-choices nil t))
           (selected-bookmark (cdr (assoc choice bookmark-choices))))
      
      (when selected-bookmark
        (let* ((old-name (plist-get selected-bookmark :name))
               (new-name (read-string "New bookmark name: " old-name)))
          (when (and new-name (not (string-empty-p new-name)))
            ;; Update the bookmark name
            (plist-put selected-bookmark :name new-name)
            (pdf-bookmarks-save)
            (message "Renamed bookmark from '%s' to '%s'" old-name new-name)))))))


;;;; Keybindings ----------------------------------------------------------------

;; Set up keybindings when pdf-view is loaded
;; Using "'" (apostrophe/quote) as prefix - typically available in most modes
;; First unbind ' to make it available as a prefix
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "'") nil)
  (define-key pdf-view-mode-map (kbd "' b") #'pdf-bookmarks-create)
  (define-key pdf-view-mode-map (kbd "' g") #'pdf-bookmarks-access)
  (define-key pdf-view-mode-map (kbd "' r") #'pdf-bookmarks-rename)
  (define-key pdf-view-mode-map (kbd "' d") #'pdf-bookmarks-delete))


(provide 'pdf-bookmarks)
;;; pdf-bookmarks.el ends here
