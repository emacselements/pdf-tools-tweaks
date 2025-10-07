;; -*- lexical-binding: t; -*-
;; Author: Raoul Comninos
;; Install and Settings

(pdf-tools-install)
(pdf-loader-install)
(define-key pdf-view-mode-map (kbd "g") nil)
(define-key pdf-view-mode-map (kbd "g g") 'pdf-view-first-page)
(define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
(define-key pdf-view-mode-map (kbd "e") 'pdf-view-goto-page)
(define-key pdf-view-mode-map (kbd "r") 'pdf-view-revert-buffer)
;; (define-key pdf-view-mode-map (kbd "a") 'pdf-annot-add-text-annotation)
(define-key pdf-view-mode-map (kbd "d") 'pdf-annot-delete)
(define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
(define-key pdf-view-mode-map (kbd "~") 'pdf-annot-add-squiggly-markup-annotation)
(define-key pdf-view-mode-map (kbd "u") 'pdf-annot-add-underline-markup-annotation)
(define-key pdf-view-mode-map (kbd "s") 'pdf-annot-add-strikeout-markup-annotation)

; Remember all my pdf passwords permanently
(setq epdfinfo-cache-passwords t)

(setopt pdf-view-continuous nil)

;; Change the Zoom increments
(setq pdf-view-resize-factor 1.1) ;; 10% increments
;; (setq pdf-view-resize-factor 1.05) ; 5% increments

;;;;;;;;;;;;;;;; 

;; Fix for pdf-tools annotation editing bug

(defun pdf-annot-edit-contents-commit-with-retry ()
  "Save annotation with automatic retry on 'No such annotation' error.
Also trims trailing whitespace before saving."
  (interactive)
  ;; Trim trailing whitespace before attempting to save
  (save-excursion
    (goto-char (point-max))
    (while (and (> (point) (point-min))
                (memq (char-before) '(?\s ?\t ?\n ?\r)))
      (delete-char -1)))
  (condition-case err
      ;; Try the normal save first
      (pdf-annot-edit-contents-finalize t)
    (error
     (if (string-match-p "No such annotation" (error-message-string err))
         (progn
           (message "Annotation ID became stale, refreshing buffer...")
           ;; Revert the PDF buffer to refresh annotation IDs
           (with-current-buffer (pdf-annot-get-buffer pdf-annot-edit-contents--annotation)
             (revert-buffer t t))
           (sit-for 0.5) ; Brief pause for buffer refresh
           ;; Try to find the annotation again and save
           (condition-case err2
               (progn
                 (pdf-annot-edit-contents-finalize t)
                 (message "Annotation saved after buffer refresh"))
             (error
              (message "Failed to save annotation: %s" (error-message-string err2))
              (when (y-or-n-p "Keep edit buffer open? ")
                (pdf-annot-edit-contents-finalize nil)))))
       ;; For other errors, just show the message
       (error "Failed to save annotation: %s" (error-message-string err))))))

;; Fix for annotation deletion with stale IDs
(defun pdf-annot-delete-with-retry (a)
  "Delete annotation with automatic retry on 'No such annotation' error."
  (interactive
   (list (pdf-annot-read-annotation
          "Click on the annotation you wish to delete")))
  (condition-case err
      ;; Try normal deletion first
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
           ;; Revert the PDF buffer to refresh annotation IDs
           (with-current-buffer (pdf-annot-get-buffer a)
             (revert-buffer t t))
           (sit-for 0.5)
           ;; Try to find and delete the annotation by position
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
       ;; For other errors, just propagate them
       (error "Failed to delete annotation: %s" (error-message-string err))))))

;; Cursor positioning fix for annotation editing
(defun pdf-annot-edit-contents-position-cursor ()
  "Position cursor at end of annotation content when editing starts."
  (when (and (bound-and-true-p pdf-annot-edit-contents-minor-mode)
             pdf-annot-edit-contents--annotation)
    (goto-char (point-max))))

;; Add hook to position cursor at end when edit buffer is set up
(advice-add 'pdf-annot-edit-contents-noselect :after 
            (lambda (&rest _args)
              (with-current-buffer (get-buffer-create 
                                   (format "*Edit Annotation %s*" 
                                          (buffer-name (pdf-annot-get-buffer pdf-annot-edit-contents--annotation))))
                (goto-char (point-max)))))

;; Override both functions
(advice-add 'pdf-annot-edit-contents-commit :override 
            #'pdf-annot-edit-contents-commit-with-retry)

;; Also override the finalize function which is what C-c C-c actually calls
(advice-add 'pdf-annot-edit-contents-finalize :around 
            (lambda (orig-fun &optional save &rest args)
              (when save
                ;; Trim trailing whitespace before saving
                (save-excursion
                  (goto-char (point-max))
                  (while (and (> (point) (point-min))
                              (memq (char-before) '(?\s ?\t ?\n ?\r)))
                    (delete-char -1))))
              (apply orig-fun save args)))

(advice-add 'pdf-annot-delete :override 
            #'pdf-annot-delete-with-retry)

;; Remove any previous advice
(advice-remove 'pdf-annot-edit-contents-save-annotation 
               #'pdf-annot-edit-contents-save-annotation-fixed)

;; (message "PDF-tools annotation fix loaded. Will auto-retry on 'No such annotation' errors for both editing and deletion. Cursor will be positioned at end of text when editing.")

;;;;;;;;;;;;;;;; 

;; (add-hook 'pdf-view-mode-hook (lambda () (pdf-view-midnight-minor-mode)))
;; (setq pdf-view-midnight-colors '("#d0d0d0" . "#222222"))
;; ;; (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))

;;;;;;;;;;;;;;;; 

;; Save the last place in my PDF

(require 'saveplace-pdf-view) ; requires saveplace-pdf-view
(save-place-mode 1)

;;;;;;;;;;;;;;;; 

;; Fix PDF text selection visibility:
;; Some PDFs use the '3 Tr' rendering mode (invisible text) to hide
;; text, often for accessibility or anti-copying purposes. This script:
;;
;; 1. Converts the PDF to QDF format using `qpdf` so it's editable.
;; 2. Replaces '3 Tr' with '0 Tr' (normal fill text mode).
;; 3. Prepends '0 g' and '0 0 0 rg' to ensure black stroke and fill color,
;;    avoiding faint or invisible text caused by inherited color states.
;; 4. Reloads the buffer so changes take effect in Emacs.
;;
;; Result: hidden or copy-resistant text becomes visible and selectable
;; in Emacs' PDF viewer, with readable black fill color.

;; sudo apt-get install qpdf

(defun my-fix-pdf-selection ()
  "Fix PDF so invisible text (3 Tr) becomes visible with black fill."
  (interactive)
  (let ((pdf-file (buffer-file-name)))
    (unless (string-equal (file-name-extension pdf-file) "pdf")
      (error "Not a PDF file"))

    ;; Step 1: Convert to QDF format
    (message "Converting PDF to QDF format...")
    (shell-command (format "qpdf --qdf --object-streams=disable --replace-input '%s'" pdf-file))

    ;; Step 2: Fix rendering mode and force black color
    (message "Fixing text rendering modes and setting black fill color...")
    (with-temp-buffer
      (insert-file-contents-literally pdf-file)
      (let ((replacements 0))
        (goto-char (point-min))
        (while (re-search-forward "3 Tr" nil t)
          (replace-match "0 g\n0 0 0 rg\n0 Tr" nil nil) ; Add black stroking and nonstroking fill
          (setq replacements (1+ replacements)))
        (message "Made %d replacements of '3 Tr' with black-filled '0 Tr'" replacements)
        (write-region (point-min) (point-max) pdf-file nil 'silent)))

    ;; Step 3: Reload PDF
    (revert-buffer t t t)
    (message "PDF transparency fix applied successfully!")))

;;;;;;;;;;;;;;;; 

;; clipboard timeout error fix

(setq x-selection-timeout 10000)  ; 10 seconds instead of default

;;;;;;;;;;;;;;;; 

;; Export annotations functionality
(load-file (expand-file-name "pdf-export-annotations.el" 
                             (file-name-directory load-file-name)))


;; Add a new annotation type for marking text

;; --- Custom "Mark" annotation type for pdf-tools -------------------------------
(with-eval-after-load 'pdf-annot
  (defcustom pdf-annot-mark-color "#8A2BE2" ; purple
    "Color used for the custom mark annotation."
    :type 'color
    :group 'pdf-annot)

  (defcustom pdf-annot-box-color "#FF6B35" ; orange
    "Color used for the custom box annotation."
    :type 'color
    :group 'pdf-annot)

  (defun pdf-annot-add-mark-markup-annotation (list-of-edges &optional pages)
    "Add a purple 'mark' markup annotation to selected text.
This is similar to highlight but uses a different color to distinguish marked text."
    (interactive
     (list (pdf-view-active-region t)))
    (pdf-annot-add-markup-annotation list-of-edges 'highlight pages
                                   `((color . ,pdf-annot-mark-color)
                                     (label . "Mark"))))

  (defun pdf-annot-add-box-markup-annotation (list-of-edges &optional pages)
    "Add an orange dashed line annotation under selected text.
Uses squiggly underline with orange color to create a dashed pattern."
    (interactive
     (list (pdf-view-active-region t)))
    (pdf-annot-add-markup-annotation list-of-edges 'squiggly pages
                                   `((color . ,pdf-annot-box-color)
                                     (label . "Dashed Line"))))

  ;; Add keybindings for annotations
  (define-key pdf-view-mode-map (kbd "m") 'pdf-annot-add-mark-markup-annotation)
  (define-key pdf-view-mode-map (kbd "b") 'pdf-annot-add-box-markup-annotation))



(provide 'pdf-tools-settings-fixes)

