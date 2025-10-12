;; -*- lexical-binding: t; -*-
;; Author: Raoul Comninos
;; Export PDF annotations to org-mode format

(defun org-pdf-export-annotations-to-file (&optional output-file)
  "Export all annotations from current PDF to an org-mode file."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (let* ((pdf-file (buffer-file-name))
         (default-output (concat (file-name-sans-extension pdf-file) "-annotations.org"))
         (output (or output-file 
                     (read-file-name "Save annotations to: " nil default-output nil 
                                     (file-name-nondirectory default-output))))
         (all-annotations (pdf-annot-getannots))
         ;; Filter to only include 'text' type annotations
         (annotations (cl-remove-if-not
                       (lambda (annot) (eq (pdf-annot-get annot 'type) 'text))
                       all-annotations))
         (pdf-title (file-name-sans-extension (file-name-nondirectory pdf-file))))
    
    (with-temp-file output
      ;; Org-mode header
      (insert (format "#+TITLE: Annotations from %s\n" pdf-title))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "#+FILETAGS: :pdf:annotations:\n"))
      (insert (format "#+STARTUP: showeverything\n\n"))
      
      ;; Link back to original PDF
      (insert (format "* Source Document\n"))
      (insert (format "[[file:%s][%s]]\n\n" pdf-file (file-name-nondirectory pdf-file)))
      
      (if (null annotations)
          (insert "* No Annotations\nNo annotations found in this PDF.\n")
        
        ;; Group annotations by page
        (let ((pages-hash (make-hash-table :test 'equal)))
          ;; Collect annotations by page
          (dolist (annot annotations)
            (let ((page (pdf-annot-get annot 'page)))
              (push annot (gethash page pages-hash))))
          
          ;; Sort pages and export
          (let ((sorted-pages (sort (hash-table-keys pages-hash) '<)))
            (insert (format "* Annotations (%d total)\n\n" (length annotations)))
            (dolist (page sorted-pages)
              (let ((page-annots (reverse (gethash page pages-hash))))
                (insert (format "** Page %d\n\n" page))
                
                (dolist (annot page-annots)
                  (let* ((type (pdf-annot-get annot 'type))
                         (contents (pdf-annot-get annot 'contents))
                         (subject (pdf-annot-get annot 'subject))
                         (markup-edges (pdf-annot-get annot 'markup-edges))
                         (selected-text "")
                         (type-icon (cond
                                    ((eq type 'highlight) "🟡")
                                    ((eq type 'note) "📝")
                                    ((eq type 'text) "💭")
                                    ((eq type 'underline) "📑")
                                    ((eq type 'strikeout) "❌")
                                    ((eq type 'squiggly) "〰️")
                                    (t "📌"))))
                    
                    ;; Try to get selected text for markup annotations
                    (when markup-edges
                      (condition-case nil
                          (setq selected-text (pdf-info-gettext page markup-edges))
                        (error (setq selected-text "[Could not extract text]"))))
                    
                    ;; Use a simple format without deep nesting
                    (insert (format "*%s %s*" type-icon (capitalize (symbol-name type))))
                    (when subject
                      (insert (format " /%s/" subject)))
                    (insert "\n")
                    
                    ;; Selected/highlighted text 
                    (when (and selected-text (not (string-empty-p selected-text)))
                      (insert (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" (string-trim selected-text))))
                    
                    ;; Note content
                    (when (and contents (not (string-empty-p contents)))
                      (insert (format "\n%s\n" contents)))
                    
                    (insert "\n---\n\n")))))))))
    
    (message "Annotations exported to: %s" output)
    (when (y-or-n-p "Open exported file? ")
      (find-file output))))

(defun pdf-export-annotations-by-type (annotation-type &optional output-file)
  "Export specific type of annotations (highlight, note, etc.) to org-mode file."
  (interactive (list (intern (completing-read "Annotation type: " 
                                              '("highlight" "note" "text" "underline" "strikeout" "squiggly")))))
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))
  
  (let* ((pdf-file (buffer-file-name))
         (default-output (format "%s-%s-annotations.org" 
                                 (file-name-sans-extension pdf-file) 
                                 annotation-type))
         (output (or output-file 
                     (read-file-name (format "Save %s annotations to: " annotation-type) 
                                     nil default-output nil 
                                     (file-name-nondirectory default-output))))
         (all-annotations (pdf-annot-getannots))
         (filtered-annotations (cl-remove-if-not 
                                (lambda (annot) (eq (pdf-annot-get annot 'type) annotation-type))
                                all-annotations))
         (pdf-title (file-name-sans-extension (file-name-nondirectory pdf-file)))
         (type-icon (cond
                     ((eq annotation-type 'highlight) "🟡")
                     ((eq annotation-type 'note) "📝")
                     ((eq annotation-type 'text) "💭")
                     ((eq annotation-type 'underline) "📑")
                     ((eq annotation-type 'strikeout) "❌")
                     ((eq annotation-type 'squiggly) "〰️")
                     (t "📌"))))
    
    (with-temp-file output
      ;; Org-mode header
      (insert (format "#+TITLE: %s %s Annotations from %s\n" 
                      type-icon (capitalize (symbol-name annotation-type)) pdf-title))
      (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "#+FILETAGS: :pdf:annotations:%s:\n" annotation-type))
      (insert (format "#+STARTUP: showeverything\n\n"))
      
      ;; Link back to original PDF
      (insert (format "* Source Document\n"))
      (insert (format "[[file:%s][%s]]\n\n" pdf-file (file-name-nondirectory pdf-file)))
      
      (if (null filtered-annotations)
          (insert (format "* No %s Annotations\nNo %s annotations found in this PDF.\n" 
                          (capitalize (symbol-name annotation-type))
                          annotation-type))
        
        ;; Group by page
        (let ((pages-hash (make-hash-table :test 'equal)))
          (dolist (annot filtered-annotations)
            (let ((page (pdf-annot-get annot 'page)))
              (push annot (gethash page pages-hash))))
          
          (let ((sorted-pages (sort (hash-table-keys pages-hash) '<)))
            (insert (format "* %s Annotations (%d total)\n\n" 
                            (capitalize (symbol-name annotation-type)) 
                            (length filtered-annotations)))
            
            (dolist (page sorted-pages)
              (let ((page-annots (reverse (gethash page pages-hash))))
                (insert (format "** Page %d\n\n" page))
                
                (dolist (annot page-annots)
                  (let ((contents (pdf-annot-get annot 'contents))
                        (subject (pdf-annot-get annot 'subject))
                        (markup-edges (pdf-annot-get annot 'markup-edges))
                        (selected-text ""))
                    
                    ;; Get selected text for markup annotations
                    (when markup-edges
                      (condition-case nil
                          (setq selected-text (pdf-info-gettext page markup-edges))
                        (error (setq selected-text "[Could not extract text]"))))
                    
                    ;; Simple format without deep nesting
                    (insert (format "*%s Entry*" type-icon))
                    (when subject
                      (insert (format " /%s/" subject)))
                    (insert "\n")
                    
                    ;; Selected text
                    (when (and selected-text (not (string-empty-p selected-text)))
                      (insert (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" (string-trim selected-text))))
                    
                    ;; Note content
                    (when (and contents (not (string-empty-p contents)))
                      (insert (format "\n%s\n" contents)))
                    
                    (insert "\n---\n\n")))))))))
    
    (message "%s annotations exported to: %s" (capitalize (symbol-name annotation-type)) output)
    (when (y-or-n-p "Open exported file? ")
      (find-file output))))

(defun markdown-pdf-export-annotations-to-file (&optional output-file)
  "Export all annotations from current PDF to a Markdown file."
  (interactive)
  (unless (eq major-mode 'pdf-view-mode)
    (error "Not in a PDF buffer"))

  (let* ((pdf-file (buffer-file-name))
         (default-output (concat (file-name-sans-extension pdf-file) "-annotations.md"))
         (output (or output-file
                     (read-file-name "Save annotations to: " nil default-output nil
                                     (file-name-nondirectory default-output))))
         (all-annotations (pdf-annot-getannots))
         ;; Filter to only include 'text' type annotations
         (annotations (cl-remove-if-not
                       (lambda (annot) (eq (pdf-annot-get annot 'type) 'text))
                       all-annotations))
         (pdf-title (file-name-sans-extension (file-name-nondirectory pdf-file))))

    (with-temp-file output
      ;; Markdown header
      (insert (format "# Annotations from %s\n\n" pdf-title))
      (insert (format "**Date:** %s  \n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "**Tags:** pdf, annotations\n\n"))

      ;; Link back to original PDF
      (insert (format "## Source Document\n\n"))
      (insert (format "[%s](%s)\n\n" (file-name-nondirectory pdf-file) pdf-file))

      (if (null annotations)
          (insert "## No Annotations\n\nNo annotations found in this PDF.\n")

        ;; Group annotations by page
        (let ((pages-hash (make-hash-table :test 'equal)))
          ;; Collect annotations by page
          (dolist (annot annotations)
            (let ((page (pdf-annot-get annot 'page)))
              (push annot (gethash page pages-hash))))

          ;; Sort pages and export
          (let ((sorted-pages (sort (hash-table-keys pages-hash) '<)))
            (insert (format "## Annotations (%d total)\n\n" (length annotations)))
            (dolist (page sorted-pages)
              (let ((page-annots (reverse (gethash page pages-hash))))
                (insert (format "### Page %d\n\n" page))

                (dolist (annot page-annots)
                  (let* ((type (pdf-annot-get annot 'type))
                         (contents (pdf-annot-get annot 'contents))
                         (subject (pdf-annot-get annot 'subject))
                         (markup-edges (pdf-annot-get annot 'markup-edges))
                         (selected-text "")
                         (type-icon (cond
                                    ((eq type 'highlight) "🟡")
                                    ((eq type 'note) "📝")
                                    ((eq type 'text) "💭")
                                    ((eq type 'underline) "📑")
                                    ((eq type 'strikeout) "❌")
                                    ((eq type 'squiggly) "〰️")
                                    (t "📌"))))

                    ;; Try to get selected text for markup annotations
                    (when markup-edges
                      (condition-case nil
                          (setq selected-text (pdf-info-gettext page markup-edges))
                        (error (setq selected-text "[Could not extract text]"))))

                    ;; Use markdown format
                    (insert (format "**%s %s**" type-icon (capitalize (symbol-name type))))
                    (when subject
                      (insert (format " *%s*" subject)))
                    (insert "\n\n")

                    ;; Selected/highlighted text
                    (when (and selected-text (not (string-empty-p selected-text)))
                      (insert (format "> %s\n\n" (replace-regexp-in-string "\n" "\n> " (string-trim selected-text)))))

                    ;; Note content
                    (when (and contents (not (string-empty-p contents)))
                      (insert (format "%s\n\n" contents)))

                    (insert "---\n\n")))))))))

    (message "Annotations exported to: %s" output)
    (when (y-or-n-p "Open exported file? ")
      (find-file output))))

;; Convenient keybinding
(define-key pdf-view-mode-map (kbd "C-c C-e") 'org-pdf-export-annotations-to-file)
(define-key pdf-view-mode-map (kbd "C-c C-t") 'pdf-export-annotations-by-type)
(define-key pdf-view-mode-map (kbd "C-c C-m") 'markdown-pdf-export-annotations-to-file)

(provide 'pdf-export-annotations)