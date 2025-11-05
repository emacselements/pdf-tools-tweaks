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
;; (define-key pdf-view-mode-map (kbd "u") #'pdf-view-first-page)
;; (define-key pdf-view-mode-map (kbd "G")   #'pdf-view-last-page)
;; (define-key pdf-view-mode-map (kbd "e")   #'pdf-view-goto-page)
(define-key pdf-view-mode-map (kbd "r")   #'pdf-view-revert-buffer)

;; Annotations
;; (define-key pdf-view-mode-map (kbd "a") #'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "d")   #'pdf-annot-delete)
(define-key pdf-view-mode-map (kbd "h")   #'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "~")   #'pdf-annot-add-squiggly-markup-annotation)
(define-key pdf-view-mode-map (kbd "u")   #'pdf-annot-add-underline-markup-annotation)
(define-key pdf-view-mode-map (kbd "s")   #'pdf-annot-add-strikeout-markup-annotation)


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

;; 3.4 UX: increase edit window height
;; Default is 0.25 (25% of frame), increased to 0.30 (30%) for longer notes
(setq pdf-annot-edit-contents-display-buffer-action
      '((display-buffer-reuse-window
         display-buffer-split-below-and-attach)
        (inhibit-same-window . t)
        (window-height . 0.30)))

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
              (let ((target-buffer (if window
                                      (window-buffer window)
                                    (current-buffer))))
                (if (and (buffer-live-p target-buffer)
                         (with-current-buffer target-buffer
                           (and (eq major-mode 'pdf-view-mode)
                                (buffer-modified-p))))
                    (with-current-buffer target-buffer
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
                          (message "Quit cancelled.")))))
                  ;; Not a PDF buffer with changes, or no changes - proceed normally
                  (funcall orig-fun kill window)))))


;;;; 4) Save Last Place in PDFs -------------------------------------------------

(require 'saveplace-pdf-view) ; external package
(save-place-mode 1)



;;;; 5) Annotation Tooltip Text Wrapping ----------------------------------------

;; Set tooltip frame parameters to enable text wrapping with max width
(setq tooltip-frame-parameters
      '((name . "tooltip")
        (internal-border-width . 2)
        (border-width . 1)
        (width . 80)))  ; Set max width in characters

;; Advice to wrap text in tooltips for pdf-annot
(defun pdf-annot-wrap-tooltip-text (text)
  "Wrap TEXT to fit within tooltip width, keeping header and content on separate lines."
  (with-temp-buffer
    (let ((inhibit-read-only t))
      (insert text)
      ;; The annotation format is: "Header\nContent"
      (goto-char (point-min))
      (when (search-forward "\n" nil t)
        (let ((header-end (1- (point)))
              (content-start-pos (point)))
          ;; Save the header with its properties and the content
          (let ((header (buffer-substring (point-min) header-end))
                (content (buffer-substring content-start-pos (point-max))))
            ;; Clear buffer and rebuild
            (erase-buffer)
            ;; Insert header with its original formatting
            (insert header)
            (insert "\n")  ; Newline after header
            ;; Insert content and wrap it
            (when (> (length content) 0)
              (let ((wrap-start (point)))
                (insert content)
                (let ((fill-column 80))
                  (fill-region wrap-start (point-max))))))))
      (buffer-string))))

(advice-add 'pdf-annot-print-annotation :filter-return
            (lambda (text)
              (pdf-annot-wrap-tooltip-text text)))


;;;; 6) Clipboard Timeout Tweak -------------------------------------------------

(setq x-selection-timeout 10000) ; 10 seconds (default is often too short)


;;;; 7) Export Annotations (external helper file) -------------------------------

(load-file (expand-file-name "pdf-export-annotations.el"
                             (file-name-directory load-file-name)))


;;;; 8) Custom Annotation Types ("Mark", "Box", & "Green") ---------------------

(with-eval-after-load 'pdf-annot
  (defcustom pdf-annot-mark-color "#8A2BE2" ; purple
    "Color used for the custom mark annotation."
    :type 'color :group 'pdf-annot)

  (defcustom pdf-annot-box-color "#FF6B35" ; orange
    "Color used for the custom box annotation."
    :type 'color :group 'pdf-annot)

  (defcustom pdf-annot-green-color "#00FF00" ; green
    "Color used for the custom green highlight annotation."
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

  (defun pdf-annot-add-green-markup-annotation (list-of-edges &optional pages)
    "Add a green highlight to LIST-OF-EDGES on PAGES."
    (interactive (list (pdf-view-active-region t)))
    (pdf-annot-add-markup-annotation
     list-of-edges 'highlight pages
     `((color . ,pdf-annot-green-color)
       (label . "Green"))))

  ;; Keys for custom annotations
  (define-key pdf-view-mode-map (kbd ",") #'pdf-annot-add-mark-markup-annotation)
  (define-key pdf-view-mode-map (kbd "a") #'pdf-annot-add-box-markup-annotation)
  (define-key pdf-view-mode-map (kbd "g") #'pdf-annot-add-green-markup-annotation))


;;;; 9) Optional: Trim trailing whitespace on annot save -----------------------

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
