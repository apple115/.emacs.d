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
  (setq lsp-bridge-python-command (expand-file-name "~/.emacs.d/site-lisp/.venv/bin/python"))
  ;;remote edit
  (setq lsp-bridge-remote-python-command "~/.")
  (setq lsp-bridge-remote-python-file "")

  (setq acm-enable-copilot nil)
  (setq acm-enable-citre t)
  (setq acm-backend-lsp-candidate-min-length  1)
  (setq acm-enable-yas nil)
  ;; (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words  t)
  (setq lsp-bridge-find-def-fallback-function 'citre-jump)
  (setq lsp-bridge-find-ref-fallback-function 'citre-jump-to-reference)
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
  (setq lsp-bridge-multi-lang-server-extension-list
        '(
          ;; (("jsx"). "typescript_tailwindcss")
          ;; (("html"). "html_emmet")
          (("tsx"). "tsx_tailwindcss")
          (("vue"). "volar_vtsls")
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
  ;; (setq lsp-bridge-enable-auto-format-code t);;自动格式化

  (setq lsp-bridge-complete-manually nil)  ; 手动触发补全
  ;; (evil-make-overriding-map acm-mode-map 'insert)
  ;; (define-key acm-mode-map (kbd "C-n") #'acm-select-next)
  ;; (define-key acm-mode-map (kbd "C-p") #'acm-select-prev)

  ;; (defun my-smart-tab ()
  ;;   (interactive)
  ;;   (let ((char-before (char-before)))
  ;;     (if (or (bolp)                           ; 如果在行首
  ;;             (eq char-before ?\s)             ; 如果前一个是空格
  ;;             (eq char-before ?\t)             ; 如果前一个是制表符
  ;;             (eq char-before ?\n))            ; 如果前一个是换行
  ;;         (insert "\t")                        ; 缩进
  ;;       (lsp-bridge-popup-complete-menu))))    ; 否则补全

  ;; (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "<tab>") #'my-smart-tab)
  ;; (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-i") #'my-smart-tab)

  (define-key acm-mode-map   (kbd "<tab>") 'yas-expand)

  (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-n") #'acm-select-next)
  (evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-p") #'acm-select-prev)

  (evil-collection-define-key 'normal 'lsp-bridge-mode-map
    "K"   'lsp-bridge-popup-documentation
    "gd"  'lsp-bridge-find-def
    "gr" 'lsp-bridge-find-references
    )

  ;; 设置lsp-bridge-ref-mode 使其符合evil 用户的操作
  (with-eval-after-load 'lsp-bridge-ref
    (evil-set-initial-state 'lsp-bridge-ref-mode 'motion)

    (evil-define-key 'motion lsp-bridge-ref-mode-map
      ;; --- 基础移动 ---
      (kbd "j") 'evil-next-line
      (kbd "k") 'evil-previous-line

      ;; --- 核心跳转 (修正 SPC 冲突) ---
      (kbd "C-n") 'lsp-bridge-ref-jump-next-keyword
      (kbd "C-p") 'lsp-bridge-ref-jump-prev-keyword

      ;; 方案 A: 使用 RET 直接进入文件，使用 Tab 预览
      (kbd "RET") 'lsp-bridge-ref-open-file          ; 直接跳转到文件并关闭/切出 ref 窗口
      (kbd "TAB") 'lsp-bridge-ref-open-file-and-stay ; 预览：开文件但光标留在 ref 列表

      ;; 方案 B: 如果你习惯用大写字母跳转
      (kbd "L")   'lsp-bridge-ref-open-file          ; 就像 L 是向右冲进文件里
      (kbd "M-n") 'lsp-bridge-ref-jump-next-file
      (kbd "M-p") 'lsp-bridge-ref-jump-prev-file

      ;; --- 过滤与管理 ---
      (kbd "d")   'lsp-bridge-ref-remove-line-from-results
      (kbd "f")   'lsp-bridge-ref-filter-match-results

      ;; --- 编辑与退出 ---
      (kbd "i")   'lsp-bridge-ref-switch-to-edit-mode
      (kbd "q")   'lsp-bridge-ref-quit
      (kbd "C-c C-c") 'lsp-bridge-ref-replace-all-matches))

  (with-eval-after-load 'lsp-bridge-ref
    ;; 确保进入编辑模式时，默认处于 Normal 状态而非 Emacs 状态
    (add-to-list 'evil-normal-state-modes 'lsp-bridge-ref-edit-mode)

    ;; 针对编辑模式的专用键位
    (evil-define-key 'normal lsp-bridge-ref-edit-mode-map
      ;; 保存修改并应用到所有文件 (类似于 Vim 的 :wq)
      (kbd "C-c C-c") 'lsp-bridge-ref-replace-all-matches
      (kbd "C-x C-s") 'lsp-bridge-ref-replace-all-matches
      (kbd "ZZ")      'lsp-bridge-ref-replace-all-matches

      ;; 放弃修改并退出编辑 (类似于 Vim 的 :q!)
      (kbd "C-c C-k") 'lsp-bridge-ref-quit
      (kbd "ZQ")      'lsp-bridge-ref-quit))

  (global-lsp-bridge-mode)
  )

(provide 'init-lsp-bridge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp-bridge.el ends here
