;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package yasnippet
  :ensure t
  :config
;;(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
 (yas-global-mode 1)
)

(use-package markdown-mode
:ensure t
:mode ("README\\.md\\'" . gfm-mode)
:init (setq markdown-command "multimarkdown")
:bind (:map markdown-mode-map
       ("C-c C-e" . markdown-do)))

(use-package lsp-bridge
 :load-path "~/.config/emacs/site-lisp/lsp-bridge"
 :hook (js-jsx-mode . (lambda ()
                    (require 'lsp-bridge)
                    (lsp-bridge-enable)))
 :config
;;    (setq acm-quick-access-use-number-select t)
    (setq lsp-bridge-python-command "~/.config/emacs/site-lisp/myemacs/bin/python3")
    ;;(setq lsp-bridge-enable-auto-format-code t);;自动格式化
    (setq lsp-bridge-enable-completion-in-string t)
    (setq lsp-bridge-enable-search-words  t)
    ;;(setq lsp-bridge-enable-org-babel t)
    (setq lsp-bridge-enable-hover-diagnostic t)
    (setq acm-enable-copilot t)
    (global-lsp-bridge-mode)
;; (define-key acm-mode-map (kbd "M-1") (lambda () (interactive) (insert "1")))
;; (define-key acm-mode-map (kbd "M-2") (lambda () (interactive) (insert "2")))
;; (define-key acm-mode-map (kbd "M-3") (lambda () (interactive) (insert "3")))
;; (define-key acm-mode-map (kbd "M-4") (lambda () (interactive) (insert "4")))
;; (define-key acm-mode-map (kbd "M-5") (lambda () (interactive) (insert "5")))
;; (define-key acm-mode-map (kbd "M-6") (lambda () (interactive) (insert "6")))
;; (define-key acm-mode-map (kbd "M-7") (lambda () (interactive) (insert "7")))
;; (define-key acm-mode-map (kbd "M-8") (lambda () (interactive) (insert "8")))
;; (define-key acm-mode-map (kbd "M-9") (lambda () (interactive) (insert "9")))
;; (define-key acm-mode-map (kbd "M-0") (lambda () (interactive) (insert "0")))
   (define-key acm-mode-map (kbd "<tab>") 'nil)
)

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
