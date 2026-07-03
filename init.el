;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; ==================== 包管理配置 ====================
;; 配置 package.el
(require 'package)

;; 设置包源
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; 初始化包管理器
(package-initialize)

;; 确保 use-package 已安装
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 use-package
(eval-when-compile
  (require 'use-package))

;;(setq use-package-always-ensure t)  ;; 自动安装包
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics t)  ;; 统计加载时间

;; 设置垃圾回收参数
;;(setq gc-cons-threshold most-positive-fixnum)
;;(setq gc-cons-percentage 1)
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 80000000) ;; original value * 100
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; 禁止展示菜单栏、工具栏和纵向滚动条
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 禁止自动缩放窗口先
(setq frame-inhibit-implied-resize t)

;; 禁止启动屏幕和文件对话框（UI 元素已在 frame-alist 中设置）
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; 将lisp目录放到加载路径的前面以加快启动速度
(let
    ((dir
      (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path
               (file-name-as-directory dir)))

;; 设置警告级别
(setq warning-minimum-level :error)

;; 加载各模块化配置
;; 不要在`*message*'缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-base)
  (require 'init-custom)
  (require 'init-edit)
  (require 'init-tools)
  (require 'init-keyboard)
  (require 'init-ui)
  (require 'init-completion)
  (require 'init-lsp-bridge)  ;; 已切换到 corfu + eglot
  (require 'init-langs)
  (require 'init-write)
  (require 'init-editer)
  (require 'init-org-agenda)
  (require 'init-org-capture)
  (require 'init-consult)
  (require 'init-func)
  (require 'init-dape)
  (require 'init-compile)
  (require 'init-docker)
  (require 'init-windows-manager)
  (require 'init-web-developer)
  (require 'init-python)
  (require 'init-english)
  (require 'init-sql)
  (require 'init-project)
  (require 'init-chinese)
  (require 'init-ai)
  (require 'init-emacs)
  (require 'init-git)
  (require 'init-go)
  (require 'init-prog)
  (require 'init-rust)
  (require 'init-my-blog)
  (require 'init-lsp-bridge)
  ;; (require 'init-my-theme)
  (require 'init-my-dark-theme)
  (require 'init-read)
  ;; (require 'init-org-ui)
  ;;(require 'init-eaf)
  ;; (require 'init-projectile)
  )
 (server-start)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window add-node-modules-path auto-yasnippet buffer-terminator cal-china-x
                citre claude-code-ide colorful-mode consult-dir consult-notes
                consult-todo dape denote devdocs dired-rsync dired-sidebar
                diredfl docker dumb-jump dwim-shell-command embark-consult
                emmet-mode engine-mode evil-collection evil-ghostel
                evil-indent-plus evil-matchit evil-nerd-commenter evil-surround
                evil-textobj-tree-sitter exec-path-from-shell fish-mode flycheck
                format-all general go-dlv go-fill-struct go-gen-test go-impl
                go-tag gotest gptel graphviz-dot-mode haskell-mode i18n-quick
                ibuffer-project jinx ligature link-hint magit marginalia
                markdown-mode nerd-icons-dired nerd-icons-ibuffer nov ob-go
                ob-restclient orderless org-auto-tangle org-download org-roam
                ox-gfm ox-reveal pdf-tools pinyinlib plantuml-mode popper
                python-mode quickrun rainbow-delimiters rime rust-mode scss-mode
                shackle sis sudo-edit tabspaces vertico virtualenvwrapper vlf
                web-mode wgrep)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#282828" :foreground "#ebdbb2"))))
 '(completions-common-part ((t (:foreground "#d3869b" :weight normal))))
 '(completions-first-difference ((t (:foreground "#ebdbb2" :weight normal))))
 '(consult-highlight-line ((t (:background "#3c3836" :extend t))))
 '(consult-preview-match ((t (:foreground "#83a598" :background nil))))
 '(dired-directory ((t (:background nil :foreground "#fe8019" :weight bold))))
 '(dired-flagged ((t (:foreground "#fb4934" :weight bold))))
 '(dired-header ((t (:background nil :inherit default))))
 '(dired-ignored ((t (:foreground "#a89984"))))
 '(dired-perm-write ((t (:background nil :foreground "#fb4934"))))
 '(dired-set-id ((t (:background nil :foreground "#fabd2f" :underline t))))
 '(dired-special ((t (:background nil :foreground "#d3869b"))))
 '(dired-symlink ((t (:background nil :foreground "#d3869b"))))
 '(error ((t (:foreground "#fb4934"))))
 '(font-lock-builtin-face ((t (:foreground "#ebdbb2"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#83a598"))))
 '(font-lock-comment-face ((t (:foreground "#83a598"))))
 '(font-lock-constant-face ((t (:foreground "#d3869b"))))
 '(font-lock-function-name-face ((t (:foreground "#fe8019"))))
 '(font-lock-keyword-face ((t (:foreground "#ebdbb2"))))
 '(font-lock-number-face ((t (:foreground "#d3869b"))))
 '(font-lock-operator-face ((t (:foreground "#ebdbb2"))))
 '(font-lock-string-face ((t (:foreground "#b8bb26"))))
 '(font-lock-type-face ((t (:foreground "#ebdbb2"))))
 '(font-lock-variable-name-face ((t (:foreground "#fe8019"))))
 '(fringe ((t (:background "#282828"))))
 '(ghostel-color-black ((t (:foreground "#282828"))))
 '(ghostel-color-blue ((t (:foreground "#458588"))))
 '(ghostel-color-bright-black ((t (:foreground "#928374"))))
 '(ghostel-color-bright-blue ((t (:foreground "#83a598"))))
 '(ghostel-color-bright-cyan ((t (:foreground "#8ec07c"))))
 '(ghostel-color-bright-green ((t (:foreground "#b8bb26"))))
 '(ghostel-color-bright-magenta ((t (:foreground "#d3869b"))))
 '(ghostel-color-bright-red ((t (:foreground "#fb4934"))))
 '(ghostel-color-bright-white ((t (:foreground "#ebdbb2"))))
 '(ghostel-color-bright-yellow ((t (:foreground "#fabd2f"))))
 '(ghostel-color-cyan ((t (:foreground "#689d6a"))))
 '(ghostel-color-green ((t (:foreground "#98971a"))))
 '(ghostel-color-magenta ((t (:foreground "#b16286"))))
 '(ghostel-color-red ((t (:foreground "#cc241d"))))
 '(ghostel-color-white ((t (:foreground "#a89984"))))
 '(ghostel-color-yellow ((t (:foreground "#d79921"))))
 '(header-line ((t (:background "#32302f" :foreground "#ebdbb2" :box nil :underline nil :inherit nil))))
 '(hl-line ((t (:background "#3c3836" :extend t))))
 '(isearch ((t (:background "#fabd2f" :foreground "#282828"))))
 '(lazy-highlight ((t (:background "#32302f" :foreground "#ebdbb2"))))
 '(line-number ((t (:foreground "#a89984" :background "#282828"))))
 '(line-number-current-line ((t (:foreground "#ebdbb2" :background "#32302f"))))
 '(minibuffer-prompt ((t (:foreground "#d3869b"))))
 '(mode-line ((t (:background "#1d2021" :foreground "#ebdbb2" :box nil))))
 '(mode-line-inactive ((t (:background "#32302f" :foreground "#a89984" :box nil))))
 '(org-block ((t (:background "#1d2021" :foreground "#ebdbb2"))))
 '(org-code ((t (:foreground "#b8bb26"))))
 '(org-comment ((t (:foreground "#83a598"))))
 '(org-done ((t (:foreground "#b8bb26"))))
 '(org-level-1 ((t (:foreground "#ebdbb2"))))
 '(org-level-2 ((t (:foreground "#ebdbb2"))))
 '(org-level-3 ((t (:foreground "#ebdbb2"))))
 '(org-link ((t (:foreground "#d3869b" :underline t))))
 '(org-todo ((t (:foreground "#fb4934"))))
 '(org-verbatim ((t (:foreground "#d3869b"))))
 '(region ((t (:background "#32302f"))))
 '(show-paren-match ((t (:background "#32302f"))))
 '(show-paren-mismatch ((t (:background "#fb4934" :foreground "#282828"))))
 '(vertico-current ((t (:background "#32302f" :extend t))))
 '(warning ((t (:foreground "#fabd2f")))))
