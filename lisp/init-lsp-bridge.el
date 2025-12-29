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
  :load-path "site-lisp/lsp-bridge"
  :config
  ;;(setq lsp-bridge-log-level 'debug)
  (setq lsp-bridge-python-command  "~/.emacs.d/site-lisp/lsp-bridge/.venv/Scripts/python.exe")
  ;; (setq acm-enable-copilot t)
  (setq acm-enable-citre t)
  (setq acm-candidate-match-function 'orderless-flex)
  (setq acm-enable-icon 'nil)
  ;; (setq lsp-bridge-enable-auto-format-code t);;自动格式化
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words  t)
  (setq lsp-bridge-multi-lang-server-extension-list
        '(
          ;; (("jsx") . "typescript_tailwindcss")
          ;; (("html") . "html_emmet")
          (("vue") . "volar_vtsls")
          (("tsx") . "tsx_tailwindcss")))
  ;; (setq lsp-bridge-enable-org-babel t) ;;error 与denote冲突
  ;; 检测项目根目录：支持多种项目类型
  (setq lsp-bridge-get-project-path-by-filepath
    (lambda (filepath)
      (let ((root nil))
        ;; 按优先级检测项目根目录
        (or
         ;; Go 项目
         (setq root (locate-dominating-file filepath "go.mod"))
         ;; Node.js/Vue/React 项目
         (setq root (locate-dominating-file filepath "package.json"))
         ;; Python 项目
         (setq root (locate-dominating-file filepath "setup.py"))
         (setq root (locate-dominating-file filepath "pyproject.toml"))
         ;; Rust 项目
         (setq root (locate-dominating-file filepath "Cargo.toml"))
         ;; Git 仓库
         (setq root (locate-dominating-file filepath ".git"))
         ;; .dir-locals.el
         (setq root (locate-dominating-file filepath ".dir-locals.el")))
        (when root
          (expand-file-name root)))))
  (setq lsp-bridge-get-language-id
        (lambda (project-path file-path server-name extension-name)
            (cond

             ;; vtsls: same as Neovim tsserver_filetypes
           ((string-equal server-name "vtsls")
            (cond
             ((string-equal extension-name "ts") "typescript")
             ((string-equal extension-name "tsx") "typescriptreact")
             ((string-equal extension-name "js") "javascript")
             ((string-equal extension-name "jsx") "javascriptreact")
             ((string-equal extension-name "vue") "vue")
             (t extension-name)))

              ;; volar: always "vue" for .vue files
           ((string-equal server-name "volar")
            (if (string-equal extension-name "vue") "vue" extension-name))

            ((string-equal server-name "typescript")
            (cond
            ((string-equal extension-name "ts")
                "typescript")
            ((string-equal extension-name "tsx")
                "typescriptreact")
            (t extension-name)))

            ((string-equal server-name "tailwindcss")
            (cond
            ((string-equal extension-name "ts")
                "typescript")
            ((string-equal extension-name "tsx")
                "typescriptreact")
            ((string-equal extension-name "jsx")
                "javascriptreact")
            ((string-equal extension-name "js")
                "javascript")
            ((string-equal extension-name "svelte")
                "svelte")
            ((string-equal extension-name "vue")
                "vue")
            (t extension-name)))
            (t extension-name))))
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
