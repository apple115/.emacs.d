;;; init-lsp-bridge.el --- Lsp tools settings -*- lexical-binding: t -*-
;;; Commentary:
;;; lsp-bridge is a language server client for Emacs, which provides
;;; Code:
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
  :after (markdown-mode yasnippet)
  :init (yas-global-mode 1)
  :elpaca '(lsp-bridge
              :host github
              :repo "manateelazycat/lsp-bridge"
              :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources" "icons")
              :build (:not elpaca--byte-compile))
  :config
  ;; (setq lsp-bridge-enable-log t)
  (setq lsp-bridge-python-command  "/Users/apple115/.emacs.d/site-lisp/lsp-bridge/.venv/bin/python3.13")
  (setq acm-enable-copilot t)
  (setq acm-enable-citre t)
  (setq acm-candidate-match-function 'orderless-flex)
  ;; (setq lsp-bridge-enable-auto-format-code t);;自动格式化
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words  t)
  (setq lsp-bridge-find-def-fallback-function 'citre-jump)
  (setq lsp-bridge-find-ref-fallback-function 'citre-jump-to-reference)
  (setq lsp-bridge-multi-lang-server-extension-list
        '(
          ;; (("jsx"). "typescript_tailwindcss")
          ;; (("html"). "html_emmet")
          ;; (("tsx"). "typescript_tailwindcss_emmet")
          ))
  ;; (setq lsp-bridge-enable-org-babel t) ;;error 与denote冲突
  (setq lsp-bridge--get-language-id-func t)
  (setq lsp-bridge-enable-hover-diagnostic t)
  (define-key acm-mode-map   (kbd "<tab>") 'yas-expand)
  (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-n") #'acm-select-next)
  (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-p") #'acm-select-prev)
  (evil-collection-define-key 'normal 'lsp-bridge-mode-map
    "K"   'lsp-bridge-popup-documentation
    "gd"  'lsp-bridge-find-def
    "gr" 'lsp-bridge-find-references
    )
  (global-lsp-bridge-mode)
  )

(provide 'init-lsp-bridge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
