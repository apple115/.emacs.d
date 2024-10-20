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
  ;;(setq yas-snippet-dies '("~/.config/emacs/snippets"))
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
  ;; (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-python-command "/home/apple115/.emacs.d/site-lisp/my-emacs-python/bin/python3.11")
  (setq acm-enable-copilot t)
  (setq acm-enable-citre t)
  (setq acm-enable-capf t)
  (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-enable-auto-format-code t);;自动格式化
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words  t)
  (setq lsp-bridge-find-def-fallback-function 'citre-jump)
  (setq lsp-bridge-find-ref-fallback-function 'citre-jump-to-reference)
  (setq lsp-bridge-multi-lang-server-extension-list
        '(
          (("jsx"). "typescript_tailwindcss")
          (("html"). "html_emmet")
          (("tsx"). "typescript_tailwindcss_emmet")
          ))
  ;; (setq lsp-bridge-enable-org-babel t) ;;error 与denote冲突
  (setq lsp-bridge--get-language-id-func t)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (define-key acm-mode-map
              (kbd "<tab>")
              'yas-expand)
  (global-lsp-bridge-mode)
)



(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
