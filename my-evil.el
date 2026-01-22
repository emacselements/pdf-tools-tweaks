;; -*- lexical-binding: t; -*-

;; EVIL MODE SETTINGS
;; Download and install Evil Mode
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil Mode
(setq evil-default-state 'normal)
(setq evil-want-C-u-scroll t)
(setq evil-want-Y-yank-to-eol t)
(require 'evil)
(add-hook 'pdf-view-mode-hook 'evil-mode)

;; Setting a leader key in evil-mode (,)
(evil-set-leader 'normal (kbd ","))
(evil-define-key 'normal 'global (kbd "<leader>s") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>q") 'kill-this-buffer)

;; Evil surround package
(unless (package-installed-p 'evil-surround)
  (package-refresh-contents)
  (package-install 'evil-surround))
(require 'evil-surround)
(global-evil-surround-mode 1)

(setq evil-shift-width 4) ;; number of columns by which line shifts using > and <.
(setq evil-want-C-u-delete t) ;; This works in conjunction with C-w which deletes last word.
(setq evil-want-Y-yank-to-eol t) ;; Yanks line, excluding the newline character.

(with-eval-after-load 'evil-surround
  (add-to-list 'evil-surround-pairs-alist '(?\* . ("**" . "**")))
  (add-to-list 'evil-surround-pairs-alist '(?~ . ("~~" . "~~")))
  (add-to-list 'evil-surround-pairs-alist '(?\" . ("\"" . "\""))))

;; (define-key evil-normal-state-map (kbd "RET") 'evil-next-visual-line)

(define-key evil-normal-state-map (kbd "RET") 
  (lambda () 
    (interactive) 
    (evil-next-visual-line 1)
    (back-to-indentation)))

(setq x-select-enable-clipboard t)

(provide 'my-evil)
