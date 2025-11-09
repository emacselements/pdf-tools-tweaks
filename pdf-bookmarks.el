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
;; - Migrate bookmarks when files are renamed/moved
;; - Bookmarks stored per-file in ~/.emacs.d/pdf-bookmarks/
;; - C-x 0 aborts annotation editing and deletes empty annotations
;; - C-x C-s saves annotation without closing (standard save behavior)
;;
;; Keybindings:
;;   ' b - Create bookmark at current page
;;   ' g - Access/navigate to (go to) bookmark
;;   ' l - Go back to Last position (before bookmark jump)
;;   ' r - Rename a bookmark
;;   ' d - Delete a bookmark
;;   ' m - Migrate bookmarks from a renamed file
;;
;; Annotation editing:
;;   C-x C-s - Save annotation changes (keeps window open)
;;   C-x 0   - Abort editing and close window (deletes annotation if empty)
;;   C-c C-c - Save annotation changes and close window
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

(defun pdf-bookmarks-find-similar-files (target-basename)
  "Find bookmark files similar to TARGET-BASENAME.
Returns a list of (similarity-score . filepath) pairs, sorted by similarity."
  (pdf-bookmarks-ensure-directory)
  (let ((all-bookmark-files (directory-files pdf-bookmarks-storage-directory t "\\.bookmarks$"))
        (candidates nil))
    (dolist (file all-bookmark-files)
      (let* ((basename (file-name-base file))
             (score (pdf-bookmarks-similarity-score target-basename basename)))
        (when (> score 0)
          (push (cons score file) candidates))))
    (sort candidates (lambda (a b) (> (car a) (car b))))))

(defun pdf-bookmarks-similarity-score (str1 str2)
  "Calculate similarity score between STR1 and STR2.
Returns a score between 0 and 100, with higher being more similar."
  (let* ((s1 (downcase str1))
         (s2 (downcase str2))
         (len1 (length s1))
         (len2 (length s2))
         (max-len (max len1 len2))
         (min-len (min len1 len2)))
    (if (= max-len 0)
        100
      ;; Calculate based on common substring and length difference
      (let ((common-prefix-len (cl-loop for i from 0 below min-len
                                        while (= (aref s1 i) (aref s2 i))
                                        finally return i))
            (common-suffix-len (cl-loop for i from 1 to min-len
                                        while (= (aref s1 (- len1 i))
                                                (aref s2 (- len2 i)))
                                        finally return (1- i))))
        (* 100 (/ (float (+ common-prefix-len common-suffix-len))
                  max-len))))))

(defun pdf-bookmarks-get-file-path ()
  "Get the bookmark file path for the current PDF."
  (when-let ((pdf-file (buffer-file-name)))
    (pdf-bookmarks-ensure-directory)
    (expand-file-name 
     (concat (file-name-base pdf-file) ".bookmarks")
     pdf-bookmarks-storage-directory)))

(defun pdf-bookmarks-check-migration ()
  "Check if bookmarks need migration from a renamed file.
Returns the source file path if migration candidate found, nil otherwise."
  (when-let* ((current-pdf (buffer-file-name))
              (target-bookmark-file (pdf-bookmarks-get-file-path)))
    (unless (file-exists-p target-bookmark-file)
      ;; No bookmark file exists for current PDF
      (let* ((current-basename (file-name-base current-pdf))
             (similar-files (pdf-bookmarks-find-similar-files current-basename)))
        (when (and similar-files
                   (> (caar similar-files) 30)) ; Minimum 30% similarity
          (cdar similar-files))))))

(defun pdf-bookmarks-load ()
  "Load bookmarks for the current PDF file.
If no bookmarks exist, check for migration candidates."
  (when-let ((bookmark-file (pdf-bookmarks-get-file-path)))
    (setq pdf-bookmarks-current-file-bookmarks
          (if (file-exists-p bookmark-file)
              (with-temp-buffer
                (insert-file-contents bookmark-file)
                (read (current-buffer)))
            ;; No bookmarks found - check for migration
            (when-let ((migration-source (pdf-bookmarks-check-migration)))
              (when (y-or-n-p (format "Found bookmarks from '%s'. Migrate to current file? "
                                      (file-name-nondirectory migration-source)))
                (pdf-bookmarks-migrate-from-file migration-source)
                pdf-bookmarks-current-file-bookmarks))))))

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
           ;; Find bookmark on current page for default
           (current-page-choice (car (cl-find-if (lambda (choice)
                                                   (= (plist-get (cdr choice) :page) current-page))
                                                 bookmark-choices)))
           (prompt (if current-page-choice
                      (format "Delete bookmark (default %s): " current-page-choice)
                    "Delete bookmark: "))
           (choice (completing-read prompt
                                   (mapcar #'car bookmark-choices)
                                   nil t
                                   nil nil
                                   current-page-choice))
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
           ;; Find bookmark on current page for default
           (current-page-choice (car (cl-find-if (lambda (choice)
                                                   (= (plist-get (cdr choice) :page) current-page))
                                                 bookmark-choices)))
           (prompt (if current-page-choice
                      (format "Rename bookmark (default %s): " current-page-choice)
                    "Rename bookmark: "))
           (choice (completing-read prompt
                                   (mapcar #'car bookmark-choices)
                                   nil t
                                   nil nil
                                   current-page-choice))
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

(defun pdf-bookmarks-migrate-from-file (source-file)
  "Migrate bookmarks from SOURCE-FILE to the current PDF.
Creates a backup of the source file, then deletes the original."
  (interactive
   (list (completing-read "Migrate bookmarks from: "
                          (directory-files pdf-bookmarks-storage-directory nil "\\.bookmarks$")
                          nil t)))
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))

  ;; Handle interactive call with just filename vs programmatic call with full path
  (let ((source-path (if (file-name-absolute-p source-file)
                         source-file
                       (expand-file-name source-file pdf-bookmarks-storage-directory))))

    (unless (file-exists-p source-path)
      (error "Source bookmark file does not exist: %s" source-path))

    ;; Load bookmarks from source
    (let ((migrated-bookmarks
           (with-temp-buffer
             (insert-file-contents source-path)
             (read (current-buffer)))))

      ;; Create backup of source file
      (let ((backup-file (concat source-path ".bak")))
        (copy-file source-path backup-file t))

      ;; Delete the original source file
      (delete-file source-path)

      ;; Set and save to current PDF
      (setq pdf-bookmarks-current-file-bookmarks migrated-bookmarks)
      (pdf-bookmarks-save)

      (message "Migrated %d bookmark(s) from %s (backup saved)"
               (length migrated-bookmarks)
               (file-name-nondirectory source-path)))))

(defun pdf-bookmarks-migrate ()
  "Interactively migrate bookmarks from a similar bookmark file.
Shows top candidates based on filename similarity."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))

  (let* ((current-basename (file-name-base (buffer-file-name)))
         (similar-files (pdf-bookmarks-find-similar-files current-basename)))

    (if (null similar-files)
        (message "No bookmark files found to migrate from")

      (let* ((choices (mapcar (lambda (item)
                               (let ((score (car item))
                                     (file (cdr item)))
                                 (cons (format "%s (%.0f%% similar)"
                                              (file-name-nondirectory file)
                                              score)
                                       file)))
                             (cl-subseq similar-files 0 (min 5 (length similar-files)))))
             (choice (completing-read "Migrate from: " (mapcar #'car choices) nil t))
             (selected-file (cdr (assoc choice choices))))

        (when selected-file
          (pdf-bookmarks-migrate-from-file selected-file))))))


;;;; PDF Annotation Window Keybindings -----------------------------------------

(declare-function pdf-annot-edit-contents-abort "pdf-annot")
(declare-function pdf-annot-edit-contents-commit "pdf-annot")
(declare-function pdf-annot-edit-contents-save-annotation "pdf-annot")
(declare-function pdf-annot-delete "pdf-annot")
(declare-function pdf-annot-get "pdf-annot")

(defun pdf-annot-save-annotation-contents ()
  "Save the annotation contents without closing the edit buffer.
This is the standard Emacs save behavior - save but keep editing."
  (interactive)
  (when (and (boundp 'pdf-annot-edit-contents-minor-mode)
             pdf-annot-edit-contents-minor-mode)
    (pdf-annot-edit-contents-save-annotation)))

(defun pdf-annot-delete-window-and-abort ()
  "Delete the annotation editing window and abort editing.
If the annotation's original content is empty (or only whitespace),
delete the annotation entirely. Otherwise, just abort the changes
and close the window."
  (interactive)
  (when (and (boundp 'pdf-annot-edit-contents-minor-mode)
             pdf-annot-edit-contents-minor-mode)
    (let* ((current-annotation (and (boundp 'pdf-annot-edit-contents--annotation)
                                    pdf-annot-edit-contents--annotation))
           ;; Check the ORIGINAL annotation content, not the buffer content
           (original-content (when current-annotation
                              (or (pdf-annot-get current-annotation 'contents) "")))
           (annotation-is-empty (string-empty-p (string-trim original-content))))
      ;; First abort the editing (this closes the window)
      (pdf-annot-edit-contents-abort)
      ;; If the annotation's original content was empty, delete it
      (when (and annotation-is-empty current-annotation)
        (pdf-annot-delete current-annotation)))))

;; Set up annotation editing keybindings
(with-eval-after-load 'pdf-annot
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-x 0")
    #'pdf-annot-delete-window-and-abort)
  (define-key pdf-annot-edit-contents-minor-mode-map (kbd "C-x C-s")
    #'pdf-annot-save-annotation-contents))


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
  (define-key pdf-view-mode-map (kbd "' d") #'pdf-bookmarks-delete)
  (define-key pdf-view-mode-map (kbd "' m") #'pdf-bookmarks-migrate))


(provide 'pdf-bookmarks)
;;; pdf-bookmarks.el ends here
