;; -*- lexical-binding: t; -*-
;; Author: Raoul Comninos

;;; pdf-tools-settings-fixes.el --- PDF Tools setup, fixes, and extras  -*- lexical-binding: t; -*-

;;; Commentary:
;; Well-structured configuration and patches for pdf-tools:
;; - Install & base settings
;; - Keybindings
;; - Robust annotation edit/delete (handles stale IDs)
;; - Better edit buffer UX (cursor position + dynamic height)
;; - Smart quit with save prompt for unsaved changes
;; - Save-place integration
;; - Make invisible text (3 Tr) visible
;; - Clipboard timeout tweak
;; - Export annotations (external helper)
;; - Custom annotation types ("Mark" & "Box")
;; - PDF bookmarks system for navigation
;; - Optional trailing-whitespace trim before saving annot text

;;; Code:


;;;; 1) Install & Base Settings -------------------------------------------------

(pdf-tools-install)
(pdf-loader-install)

;; Remember all PDF passwords permanently.
(setq epdfinfo-cache-passwords t)

;; Disable continuous scrolling if you prefer discrete pages.
(setopt pdf-view-continuous nil)

;; Change zoom increments (10%).
(setq pdf-view-resize-factor 1.1)
;; (setq pdf-view-resize-factor 1.05) ; Use this for 5% steps.


;;;; 2) Keybindings -------------------------------------------------------------

;; Navigation / buffer ops
(define-key pdf-view-mode-map (kbd "g") nil)
(define-key pdf-view-mode-map (kbd "g g") #'pdf-view-first-page)
(define-key pdf-view-mode-map (kbd "G")   #'pdf-view-last-page)
(define-key pdf-view-mode-map (kbd "e")   #'pdf-view-goto-page)
(define-key pdf-view-mode-map (kbd "r")   #'pdf-view-revert-buffer)

;; Annotations
;; (define-key pdf-view-mode-map (kbd "a") #'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "d")   #'pdf-annot-delete)
(define-key pdf-view-mode-map (kbd "h")   #'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "~")   #'pdf-annot-add-squiggly-markup-annotation)
(define-key pdf-view-mode-map (kbd "u")   #'pdf-annot-add-underline-markup-annotation)
(define-key pdf-view-mode-map (kbd "s")   #'pdf-annot-add-strikeout-markup-annotation)

;; PDF Bookmarks
(define-key pdf-view-mode-map (kbd "b")   #'pdf-bookmarks-create)
(define-key pdf-view-mode-map (kbd "B")   #'pdf-bookmarks-access)


;;;; 3) Robust Annotation Editing (stale ID fix + UX) ---------------------------

;; 3.1 Commit with auto-retry on "No such annotation"
(defun pdf-annot-edit-contents-commit-with-retry ()
  "Save annotation with automatic retry on 'No such annotation' error."
  (interactive)
  (condition-case err
      (pdf-annot-edit-contents-finalize t)
    (error
     (if (string-match-p "No such annotation" (error-message-string err))
         (progn
           (message "Annotation ID became stale, refreshing buffer...")
           (with-current-buffer (pdf-annot-get-buffer pdf-annot-edit-contents--annotation)
             (revert-buffer t t))
           (sit-for 0.5)
           (condition-case err2
               (progn
                 (pdf-annot-edit-contents-finalize t)
                 (message "Annotation saved after buffer refresh"))
             (error
              (message "Failed to save annotation: %s" (error-message-string err2))
              (when (y-or-n-p "Keep edit buffer open? ")
                (pdf-annot-edit-contents-finalize nil)))))
       (error "Failed to save annotation: %s" (error-message-string err))))))

;; 3.2 Delete with auto-retry on "No such annotation"
(defun pdf-annot-delete-with-retry (a)
  "Delete annotation A with automatic retry on 'No such annotation' error."
  (interactive
   (list (pdf-annot-read-annotation
          "Click on the annotation you wish to delete")))
  (condition-case err
      (progn
        (with-current-buffer (pdf-annot-get-buffer a)
          (pdf-info-delannot (pdf-annot-get-id a))
          (set-buffer-modified-p t)
          (pdf-annot-run-modified-hooks :delete a))
        (when (called-interactively-p 'any)
          (message "Annotation deleted")))
    (error
     (if (string-match-p "No such annotation" (error-message-string err))
         (progn
           (message "Annotation ID became stale, refreshing buffer...")
           (with-current-buffer (pdf-annot-get-buffer a)
             (revert-buffer t t))
           (sit-for 0.5)
           (let* ((page (pdf-annot-get a 'page))
                  (edges (pdf-annot-get a 'edges))
                  (buffer (pdf-annot-get-buffer a))
                  (found-annot nil))
             (with-current-buffer buffer
               (let ((current-annots (pdf-annot-getannots page nil buffer)))
                 (catch 'found
                   (dolist (annot current-annots)
                     (when (equal (pdf-annot-get annot 'edges) edges)
                       (setq found-annot annot)
                       (throw 'found annot)))))
               (if found-annot
                   (progn
                     (pdf-info-delannot (pdf-annot-get-id found-annot))
                     (set-buffer-modified-p t)
                     (pdf-annot-run-modified-hooks :delete found-annot)
                     (message "Annotation deleted after buffer refresh"))
                 (message "Could not find annotation to delete - it may have already been removed")))))
       (error "Failed to delete annotation: %s" (error-message-string err))))))

;; 3.3 UX: position cursor at end when starting to edit
(defun pdf-annot-edit-contents-position-cursor ()
  "Position cursor at end of annotation content when editing starts."
  (when (and (bound-and-true-p pdf-annot-edit-contents-minor-mode)
             pdf-annot-edit-contents--annotation)
    (goto-char (point-max))))

;; HERE
;; Ensure edit buffer shows point at end
(advice-add
 'pdf-annot-edit-contents-noselect :after
 (lambda (&rest _args)
   (with-current-buffer
       (get-buffer-create
        (format "*Edit Annotation %s*"
                (buffer-name (pdf-annot-get-buffer pdf-annot-edit-contents--annotation))))
     (goto-char (point-max)))))

;; ;; 3.4 UX: dynamic edit window height based on wrapped visual lines
;; (defun pdf-annot-count-visual-lines (buffer window-width)
;;   "Count visual lines in BUFFER accounting for wrapping at WINDOW-WIDTH."
;;   (with-current-buffer buffer
;;     (let ((total-lines 0))
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (not (eobp))
;;           (let* ((line-end (line-end-position))
;;                  (line-text (buffer-substring-no-properties (point) line-end))
;;                  (line-length (length line-text)))
;;             (setq total-lines (+ total-lines
;;                                  (max 1 (ceiling (/ (float line-length)
;;                                                     (float (max 1 (- window-width 2))))))))
;;             (forward-line 1))))
;;       total-lines)))

;; (defun pdf-annot-dynamic-window-height (a)
;;   "Override `pdf-annot-edit-contents' to set dynamic window height for A."
;;   (let* ((buffer (pdf-annot-edit-contents-noselect a))
;;          (pdf-window-width (window-width (selected-window)))
;;          (visual-lines (pdf-annot-count-visual-lines buffer pdf-window-width))
;;          (desired-height (max 8 (+ visual-lines 4))) ; 3 padding + 1 header
;;          (target-height (min 25 desired-height))
;;          (win (select-window
;;                (display-buffer
;;                 buffer
;;                 `((display-buffer-reuse-window
;;                    display-buffer-split-below-and-attach)
;;                   (inhibit-same-window . t)
;;                   (window-height . ,target-height))))))
;;     (with-selected-window win
;;       (goto-char (point-min))
;;       (set-window-start win (point-min) t)
;;       (goto-char (point-max)))
;;     win))

;; 3.5 Advices (override/around)
(advice-add 'pdf-annot-edit-contents-commit       :override #'pdf-annot-edit-contents-commit-with-retry)
(advice-add 'pdf-annot-delete                     :override #'pdf-annot-delete-with-retry)

;; Cleanup stray old advice name if present
(advice-remove 'pdf-annot-edit-contents-save-annotation
               #'pdf-annot-edit-contents-save-annotation-fixed)


;;;; 3.6) Smart Quit with Save Prompt for Unsaved Changes ----------------------

(defun pdf-view-quit-window-with-save-prompt (&optional kill)
  "Quit the PDF window, prompting to save unsaved changes if any exist.
With prefix argument KILL, kill the buffer instead of just burying it."
  (interactive "P")
  (let ((buffer (current-buffer)))
    (if (buffer-modified-p buffer)
        (let ((choice (read-char-choice
                       (format "PDF has unsaved changes. [s]ave, [d]iscard, or [c]ancel? ")
                       '(?s ?d ?c))))
          (cond
           ((eq choice ?s)  ; Save
            (condition-case err
                (progn
                  (save-buffer)
                  (message "Changes saved.")
                  (quit-window kill))
              (error
               (message "Failed to save changes: %s" (error-message-string err)))))
           ((eq choice ?d)  ; Discard
            (set-buffer-modified-p nil)
            (message "Changes discarded.")
            (quit-window kill))
           ((eq choice ?c)  ; Cancel
            (message "Quit cancelled."))))
      ;; No unsaved changes, quit normally
      (quit-window kill))))

;; Bind the smart quit function to Q key  
(with-eval-after-load 'pdf-view
  (define-key pdf-view-mode-map (kbd "Q") #'pdf-view-quit-window-with-save-prompt)
  ;; Also bind to lowercase q for convenience
  (define-key pdf-view-mode-map (kbd "q") #'pdf-view-quit-window-with-save-prompt))

;; Alternative approach: advice on quit-window to catch all quit attempts
(advice-add 'quit-window :around
            (lambda (orig-fun &optional kill window)
              "Advice to prompt for saving when quitting PDF buffers with unsaved changes."
              (if (and (eq major-mode 'pdf-view-mode)
                       (buffer-modified-p))
                  (let ((choice (read-char-choice
                                 "PDF has unsaved changes. [s]ave, [d]iscard, or [c]ancel? "
                                 '(?s ?d ?c))))
                    (cond
                     ((eq choice ?s)  ; Save
                      (condition-case err
                          (progn
                            (save-buffer)
                            (message "Changes saved.")
                            (funcall orig-fun kill window))
                        (error
                         (message "Failed to save changes: %s" (error-message-string err)))))
                     ((eq choice ?d)  ; Discard
                      (set-buffer-modified-p nil)
                      (message "Changes discarded.")
                      (funcall orig-fun kill window))
                     ((eq choice ?c)  ; Cancel
                      (message "Quit cancelled."))))
                ;; Not a PDF buffer with changes, or no changes - proceed normally
                (funcall orig-fun kill window))))


;;;; 4) Save Last Place in PDFs -------------------------------------------------

(require 'saveplace-pdf-view) ; external package
(save-place-mode 1)


;;;; 5) Fix Invisible Text (3 Tr -> visible black text) -------------------------

;; Requires: sudo apt-get install qpdf
(defun my-fix-pdf-selection ()
  "Make invisible text (3 Tr) visible with black fill in current PDF."
  (interactive)
  (let ((pdf-file (buffer-file-name)))
    (unless (string-equal (file-name-extension pdf-file) "pdf")
      (error "Not a PDF file"))
    (message "Converting PDF to QDF format...")
    (shell-command (format "qpdf --qdf --object-streams=disable --replace-input '%s'" pdf-file))
    (message "Fixing text rendering modes and setting black fill color...")
    (with-temp-buffer
      (insert-file-contents-literally pdf-file)
      (let ((replacements 0))
        (goto-char (point-min))
        (while (re-search-forward "3 Tr" nil t)
          (replace-match "0 g\n0 0 0 rg\n0 Tr" nil nil)
          (setq replacements (1+ replacements)))
        (message "Made %d replacements of '3 Tr' with black-filled '0 Tr'" replacements)
        (write-region (point-min) (point-max) pdf-file nil 'silent)))
    (revert-buffer t t t)
    (message "PDF transparency fix applied successfully!")))


;;;; 6) Clipboard Timeout Tweak -------------------------------------------------

(setq x-selection-timeout 10000) ; 10 seconds (default is often too short)


;;;; 7) Export Annotations (external helper file) -------------------------------

(load-file (expand-file-name "pdf-export-annotations.el"
                             (file-name-directory load-file-name)))


;;;; 8) Custom Annotation Types ("Mark" & "Box") --------------------------------

(with-eval-after-load 'pdf-annot
  (defcustom pdf-annot-mark-color "#8A2BE2" ; purple
    "Color used for the custom mark annotation."
    :type 'color :group 'pdf-annot)

  (defcustom pdf-annot-box-color "#FF6B35" ; orange
    "Color used for the custom box annotation."
    :type 'color :group 'pdf-annot)

  (defun pdf-annot-add-mark-markup-annotation (list-of-edges &optional pages)
    "Add a purple 'mark' (highlight) to LIST-OF-EDGES on PAGES."
    (interactive (list (pdf-view-active-region t)))
    (pdf-annot-add-markup-annotation
     list-of-edges 'highlight pages
     `((color . ,pdf-annot-mark-color)
       (label . "Mark"))))

  (defun pdf-annot-add-box-markup-annotation (list-of-edges &optional pages)
    "Add an orange squiggly line (dashed look) to LIST-OF-EDGES on PAGES."
    (interactive (list (pdf-view-active-region t)))
    (pdf-annot-add-markup-annotation
     list-of-edges 'squiggly pages
     `((color . ,pdf-annot-box-color)
       (label . "Dashed Line"))))

  ;; Keys for custom annotations  
  (define-key pdf-view-mode-map (kbd ",") #'pdf-annot-add-mark-markup-annotation)
  (define-key pdf-view-mode-map (kbd "a") #'pdf-annot-add-box-markup-annotation))


;;;; 9) PDF Bookmarks System ----------------------------------------------------

(require 'cl-lib)  ; For cl-remove-if

(defvar pdf-bookmarks-storage-directory
  (expand-file-name "pdf-bookmarks/" user-emacs-directory)
  "Directory to store PDF bookmark files.")

(defvar pdf-bookmarks-current-file-bookmarks nil
  "List of bookmarks for the current PDF file.")

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
  (message "DEBUG: pdf-bookmarks-load called")
  (condition-case err
      (when-let ((bookmark-file (pdf-bookmarks-get-file-path)))
        (message "DEBUG: Bookmark file path: %s" bookmark-file)
        (setq pdf-bookmarks-current-file-bookmarks
              (if (file-exists-p bookmark-file)
                  (progn
                    (message "DEBUG: File exists, reading...")
                    (with-temp-buffer
                      (insert-file-contents bookmark-file)
                      (read (current-buffer))))
                nil)))
    (error
     (message "Error in pdf-bookmarks-load: %s" (error-message-string err))
     (setq pdf-bookmarks-current-file-bookmarks nil))))

(defun pdf-bookmarks-save ()
  "Save bookmarks for the current PDF file."
  (when-let ((bookmark-file (pdf-bookmarks-get-file-path)))
    (with-temp-file bookmark-file
      (prin1 pdf-bookmarks-current-file-bookmarks (current-buffer)))))

(defun pdf-bookmarks-create ()
  "Create a bookmark at the current page."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  
  (let ((current-page (pdf-view-current-page)))
    (message "Debug: pdf-view-current-page returned: %S (type: %s)" 
             current-page (type-of current-page))
    
    ;; Try alternative methods if pdf-view-current-page fails
    (unless (and current-page (numberp current-page))
      (message "Debug: Trying alternative page detection methods...")
      (setq current-page 
            (or current-page
                (and (boundp 'pdf-view-current-page) pdf-view-current-page)
                (pdf-view-current-page)
                1))) ; fallback to page 1
    
    (message "Debug: Final current-page value: %S" current-page)
    
    (unless (and current-page (numberp current-page) (> current-page 0))
      (error "Could not get current page number. Got: %S" current-page))
    
    (let* ((bookmark-name (read-string "Bookmark name: " 
                                       (format "Page %d" current-page)))
           (bookmark-entry (list :name bookmark-name 
                                :page current-page
                                :timestamp (current-time))))
      
      (message "Debug: Creating bookmark '%s' on page %d" bookmark-name current-page)
      
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
      
      (message "Debug: Total bookmarks now: %d" (length pdf-bookmarks-current-file-bookmarks))
      (message "Debug: Bookmark file path: %s" (pdf-bookmarks-get-file-path))
      (message "Debug: Bookmark entry created: %S" bookmark-entry)
      
      (condition-case err
          (pdf-bookmarks-save)
        (error
         (message "Error during pdf-bookmarks-save: %s" (error-message-string err))
         (message "Error details: %S" err)
         (message "Stack trace:")
         (backtrace)))
      
      (message "Bookmark '%s' created for page %d" bookmark-name current-page))))

(defun pdf-bookmarks-access ()
  "Access and navigate to a bookmark."
  (interactive)
  
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  ;; Load bookmarks from file
  (let ((bookmark-file (pdf-bookmarks-get-file-path)))
    (setq pdf-bookmarks-current-file-bookmarks
          (when (and bookmark-file (file-exists-p bookmark-file))
            (condition-case nil
                (with-temp-buffer
                  (insert-file-contents bookmark-file)
                  (read (current-buffer)))
              (error nil)))))
  
  (if (null pdf-bookmarks-current-file-bookmarks)
      (message "No bookmarks found for this PDF")
    
    (let* ((bookmark-alist
            (mapcar (lambda (bm)
                      (cons (format "%s (Page %d)" 
                                    (plist-get bm :name)
                                    (plist-get bm :page))
                            bm))
                    pdf-bookmarks-current-file-bookmarks)))
      
      (let ((choice-string (completing-read 
                           "Go to bookmark: " 
                           bookmark-alist 
                           nil t)))
        
        (let ((selected-bookmark (cdr (assoc choice-string bookmark-alist))))
          (when selected-bookmark
            (let ((page (plist-get selected-bookmark :page)))
              (when (and page (numberp page))
                (pdf-view-goto-page page)
                (message "Navigated to: %s" choice-string))))))))

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
                      (format "%s (Page %d)" 
                              (plist-get bm :name)
                              (plist-get bm :page)))
                    pdf-bookmarks-current-file-bookmarks))
           (choice (completing-read "Delete bookmark: " bookmark-choices nil t))
           ;; Find the matching bookmark
           (selected-bookmark 
            (cl-find-if (lambda (bm)
                          (string= choice
                                   (format "%s (Page %d)" 
                                           (plist-get bm :name)
                                           (plist-get bm :page))))
                        pdf-bookmarks-current-file-bookmarks)))
      
      (when selected-bookmark
        (setq pdf-bookmarks-current-file-bookmarks
              (cl-remove selected-bookmark pdf-bookmarks-current-file-bookmarks))
        (pdf-bookmarks-save)
        (message "Deleted bookmark: %s" (plist-get selected-bookmark :name))))))

(defun pdf-bookmarks-list ()
  "List all bookmarks for debugging."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (pdf-bookmarks-load)
  (message "PDF file: %s" (buffer-file-name))
  (message "Bookmark file: %s" (pdf-bookmarks-get-file-path))
  (message "Bookmarks loaded: %S" pdf-bookmarks-current-file-bookmarks)
  
  (if pdf-bookmarks-current-file-bookmarks
      (dolist (bm pdf-bookmarks-current-file-bookmarks)
        (message "- %s (Page %d)" (plist-get bm :name) (plist-get bm :page)))
    (message "No bookmarks found")))

(defun pdf-bookmarks-test ()
  "Test completion with dummy data."
  (interactive)
  (let* ((test-alist '(("Test Bookmark 1 (Page 5)" . (:name "Test Bookmark 1" :page 5))
                       ("Test Bookmark 2 (Page 10)" . (:name "Test Bookmark 2" :page 10))))
         (choice (completing-read "Test completion: " test-alist nil t)))
    (message "You selected: %s" choice)
    (message "Associated data: %S" (cdr (assoc choice test-alist)))))


;;;; 10) Optional: Trim trailing whitespace on annot save -----------------------

;; Comment this advice out to disable automatic whitespace trimming.
(advice-add 'pdf-annot-edit-contents-finalize :before
            (lambda (&optional save &rest _args)
              (when save
                (save-excursion
                  (goto-char (point-max))
                  (while (and (> (point) (point-min))
                              (memq (char-before) '(?\s ?\t ?\n ?\r)))
                    (delete-char -1))))))


(provide 'pdf-tools-settings-fixes)
;;; pdf-tools-settings-fixes.el ends here
