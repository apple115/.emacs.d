;;; init-programming.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-lsp-bridge.el, init-prog.el, init-compile.el, init-dape.el, init-realgud.el
;;; Code:

;; ---- merged from init-lsp-bridge.el ----

(use-package markdown-mode
  :ensure t
  :mode
  ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

(when (eq my-lsp-provider 'lsp-bridge)
  (use-package lsp-bridge
    :load-path "site-lisp/lsp-bridge"
  :config
  ;;(setq lsp-bridge-log-level 'debug)
  ;; uv 管理的虚拟环境 (uv sync 自动生成)
  (setq lsp-bridge-python-command
        (expand-file-name
         (if +is-win-p
             "site-lisp/lsp-bridge/.venv/Scripts/python.exe"
           "site-lisp/lsp-bridge/.venv/bin/python")
         user-emacs-directory))
  ;;remote edit
  ;; (setq lsp-bridge-remote-python-command "~/.")
  ;; (setq lsp-bridge-remote-python-file "")

  (if +is-win-p
      (setq acm-enable-icon nil))

  (setq acm-enable-copilot nil)
  (setq acm-enable-yas nil)
  (setq acm-enable-citre nil)
  ;; (setq acm-candidate-match-function 'orderless-flex)
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-enable-search-words t)
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

  ;; (setq lsp-bridge-enable-hover-diagnostic t)
  ;; (setq lsp-bridge-enable-auto-format-code t);;自动格式化

  (setq lsp-bridge-complete-manually nil)  ; 手动触发补全
  ;; (setq lsp-bridge-enable-completion-in-minibuffer t)
  ;; (evil-make-overriding-map acm-mode-map 'insert)
  ;; (define-key acm-mode-map (kbd "C-n") #'acm-select-next)
  ;; (define-key acm-mode-map (kbd "C-p") #'acm-select-prev)

  (+leader-keys
    :keymaps 'lsp-bridge-mode-map
    "l"  '(:ignore t :which-key "LSP")
    "ld" 'lsp-bridge-diagnostic-list
    "la" 'lsp-bridge-code-action
    "lr" 'lsp-bridge-rename
    "lf" 'lsp-bridge-code-format)

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

  ;; 不全局启用，按需通过命令或右键菜单开启
  ;; (global-lsp-bridge-mode)

  (defun +lsp-bridge-toggle ()
    "Toggle `lsp-bridge-mode' in current buffer."
    (interactive)
    (if (bound-and-true-p lsp-bridge-mode)
        (lsp-bridge-mode -1)
      (lsp-bridge-mode 1)))

  ;; 在右键上下文菜单中加入 LSP Bridge 开关
  (defun +context-menu-lsp-bridge (menu click)
    "Add LSP Bridge toggle entry to context MENU."
    (define-key menu [lsp-bridge-separator]
      '(menu-item "--single-line"))
    (define-key menu [lsp-bridge-toggle]
      `(menu-item
        ,(format "LSP Bridge (%s)"
                 (if (bound-and-true-p lsp-bridge-mode) "on" "off"))
        +lsp-bridge-toggle
        :button (:toggle . (bound-and-true-p lsp-bridge-mode))))
    menu)

  (add-hook 'context-menu-functions #'+context-menu-lsp-bridge)
  ))

(when (eq my-lsp-provider 'lsp-proxy)
  (let ((proxy-dir (expand-file-name "site-lisp/lsp-proxy" user-emacs-directory)))
    (if (file-directory-p proxy-dir)
        (use-package lsp-proxy
          :load-path "site-lisp/lsp-proxy"
          :config
          ;; 显式指定二进制路径，缺失时才自动下载
          (setq lsp-proxy-server-path
                (expand-file-name (if +is-win-p "emacs-lsp-proxy.exe" "emacs-lsp-proxy")
                                  (expand-file-name "lsp-proxy" user-emacs-directory)))
          (unless (file-exists-p lsp-proxy-server-path)
            (lsp-proxy-install-server))

          ;; 性能调优：按需求注释掉某一行
          (setq lsp-proxy--send-changes-idle-time 0.2)   ; 批量发送改动，减少服务器压力
          (setq lsp-proxy-idle-delay 0.3)                ; 默认 0.5，延迟触发
          (setq lsp-proxy-max-completion-item 15)        ; 减少补全候选数量
          (setq lsp-proxy-diagnostics-max-push-count 30) ; 限制单次推送诊断数
          (setq lsp-proxy-enable-symbol-highlighting nil); 关闭光标处符号高亮
          (setq lsp-proxy-enable-hover-eldoc nil)        ; 关闭自动 hover，手动 K 查看
          (setq lsp-proxy-inline-completion-enable-predicates nil) ; 关闭 inline 补全
          (setq lsp-proxy-xref-optimization-strategy 'lazy)        ; 大文件 xref 最省资源
          (setq lsp-proxy-large-file-threshold (* 5 1024 1024))    ; 5MB 即走大文件逻辑
          (setq lsp-proxy-log-buffer-max nil)            ; 关闭日志 buffer

          ;; 在你常用的 mode 里自动启动
          (dolist (hook '(go-ts-mode-hook
                          rust-ts-mode-hook
                          python-mode-hook
                          python-ts-mode-hook
                          typescript-ts-mode-hook
                          tsx-ts-mode-hook
                          js-ts-mode-hook
                          web-mode-hook))
            (add-hook hook #'lsp-proxy-mode))

          ;; evil 键位，尽量保持和 lsp-bridge 一致
          (with-eval-after-load 'evil
            (evil-define-key 'normal lsp-proxy-mode-map
              (kbd "K")  'lsp-proxy-describe-thing-at-point
              (kbd "gd") 'lsp-proxy-find-definition
              (kbd "gr") 'lsp-proxy-find-references
              (kbd "gD") 'lsp-proxy-find-declaration
              (kbd "gi") 'lsp-proxy-find-implementations)

            (+leader-keys
              :keymaps 'lsp-proxy-mode-map
              "l"  '(:ignore t :which-key "LSP")
              "lr" 'lsp-proxy-rename
              "la" 'lsp-proxy-execute-code-action
              "lf" 'lsp-proxy-format-buffer
              "ld" 'lsp-proxy-show-project-diagnostics)))
      (message "lsp-proxy 未克隆，请先运行 git submodule add https://github.com/jadestrong/lsp-proxy.git site-lisp/lsp-proxy"))))

;; ---- merged from init-prog.el ----
(use-package devdocs
  :ensure t
  :bind
  (:map prog-mode-map
        ("<f1>" . +devdocs-search))
  :config
  (defun +devdocs-lookup()
    "devdocs look at symbol at point"
    (interactive)
    (devdocs-lookup nil (thing-at-point 'symbol t)))
  (defun +devdocs-search()
    "devdocs look at symbol at point"
    (interactive)
    (devdocs-search (thing-at-point 'symbol t)))
)

;; ---- merged from init-compile.el ----
(setq-default compilation-scroll-output t)

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package compile
  :ensure nil
  :config
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-test "^\\s-*\\(.*\\.go\\):\\([0-9]+\\):" 1 2 nil 2))
  (add-to-list 'compilation-error-regexp-alist 'go-test)
  (add-to-list 'compilation-error-regexp-alist
               '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
                 1 2 (4) (5)))
  )

(use-package quickrun
  :ensure t
  )

;; ---- merged from init-dape.el ----
(use-package dape
  :ensure t
  ;; :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; Save breakpoints on quit
  ;; ((kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;;  (after-init . dape-breakpoint-load))

  ;; :init
  ;; To use window configuration like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)

  ;; :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  ;; (setq dape-cwd-fn 'projectile-project-root)
  )

;; ---- merged from init-realgud.el ----
(use-package realgud
 :ensure t)

(provide 'init-programming)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-programming.el ends here
