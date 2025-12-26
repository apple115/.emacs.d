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
  ;;:after (markdown-mode yasnippet)
  ;;:init (yas-global-mode 1)
  :config
  ;;(setq lsp-bridge-log-level 'debug)
  (setq lsp-bridge-python-command  "/Users/apple115/.emacs.d/site-lisp/lsp-bridge/.venv/bin/python3.13")
  (setq acm-enable-copilot nil)
  (setq acm-enable-citre t)
  (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-complete-manually t)  ; 手动触发补全
  ;; (setq lsp-bridge-enable-auto-format-code t);;自动格式化
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words  t)
  (setq lsp-bridge-find-def-fallback-function 'citre-jump)
  (setq lsp-bridge-find-ref-fallback-function 'citre-jump-to-reference)
  (setq lsp-bridge-multi-lang-server-extension-list
        '(
          ;; (("jsx"). "typescript_tailwindcss")
          ;; (("html"). "html_emmet")
          (("tsx"). "tsx_tailwindcss")
          (("vue"). "volar3_vtsls")
          ;; (("vue"). "volar_emmet")
          ))
  ;; (setq lsp-bridge-enable-org-babel t) ;;error 与denote冲突
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

           ((string-equal server-name "typescript")
            (cond
             ((string-equal extension-name "ts") "typescript")
             ((string-equal extension-name "tsx") "typescriptreact")
             (t extension-name)))

           ((string-equal server-name "tailwindcss")
            (cond
             ((string-equal extension-name "ts") "typescript")
             ((string-equal extension-name "tsx") "typescriptreact")
             ((string-equal extension-name "jsx") "javascriptreact")
             ((string-equal extension-name "js") "javascript")
             ((string-equal extension-name "svelte") "svelte")
             ((string-equal extension-name "vue") "vue")
             (t extension-name)))

           (t extension-name))))

  (setq lsp-bridge-enable-hover-diagnostic t)
  (evil-make-overriding-map acm-mode-map 'insert)
  (define-key acm-mode-map (kbd "C-n") #'acm-select-next)
  (define-key acm-mode-map (kbd "C-p") #'acm-select-prev)

  (defun my-smart-tab ()
    (interactive)
    (let ((char-before (char-before)))
      (if (or (bolp)                           ; 如果在行首
              (eq char-before ?\s)             ; 如果前一个是空格
              (eq char-before ?\t)             ; 如果前一个是制表符
              (eq char-before ?\n))            ; 如果前一个是换行
          (insert "\t")                        ; 缩进
        (lsp-bridge-popup-complete-menu))))    ; 否则补全

  (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "<tab>") #'my-smart-tab)
  (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-i") #'my-smart-tab)

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
