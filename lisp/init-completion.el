;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
)

(use-package yasnippet
  :ensure t
  :config
  ;;(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode 1)
)

(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

(use-package lsp-bridge
  :load-path "./site-lisp/lsp-bridge"
  :config

  ;;(setq acm-backend-copilot-network-proxy '(:host "127.0.0.1" :port 20171))

  ;; (setq lsp-bridge-enable-log t)
  ;;    (setq acm-quick-access-use-number-select t)
  (setq lsp-bridge-python-command "/home/apple115/.emacs.d/site-lisp/my-emacs-python/bin/python3.11")
  (setq acm-enable-copilot t)
  (setq acm-enable-citre t)
  (setq acm-enable-capf t)
  (setq acm-candidate-match-function 'orderless-flex)
  ;;(setq lsp-bridge-enable-auto-format-code t);;自动格式化
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words  t)
  (setq lsp-bridge-multi-lang-server-extension-list
        '(
          (("jsx"). "typescript_tailwindcss")
          (("html"). "html_emmet")
          (("tsx"). "typescript_tailwindcss_emmet")
          ))
  ;;(setq lsp-bridge-enable-org-babel t)
  (setq  lsp-bridge--get-language-id-func t)
  (setq lsp-bridge-enable-hover-diagnostic t)
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
  (define-key acm-mode-map
              (kbd "<tab>")
              'yas-expand)
)
(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
