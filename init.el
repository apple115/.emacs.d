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

;; 禁止菜单栏、工具栏、滚动条模式，禁止启动屏幕和文件对话框
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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
  ;; (require 'init-chinese)
  (require 'init-ai)
  (require 'init-emacs)
  (require 'init-git)
  (require 'init-go)
  (require 'init-prog)
  (require 'init-rust)
  (require 'init-lsp-bridge)  ;; 已切换到 corfu + eglot
  (require 'init-my-theme)
  ;; (require 'init-org-ui)
  ;;(require 'init-eaf)
  ;; (require 'init-projectile)
  )
   ;; (server-start)

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window add-node-modules-path agent-shell auto-yasnippet buffer-terminator
                cape citre colorful-mode compile-angel consult-dir consult-notes
                consult-todo corfu dape denote devdocs diff-hl diredfl docker
                doom-modeline doom-themes dumb-jump dwim-shell-command
                eglot-booster eldoc-box embark-consult emmet-mode engine-mode
                evil-collection evil-indent-plus evil-matchit
                evil-nerd-commenter evil-surround evil-textobj-tree-sitter
                exec-path-from-shell fish-mode flycheck format-all general
                go-dlv go-fill-struct go-gen-test go-impl go-tag gotest
                graphviz-dot-mode haskell-mode highlight-parentheses
                ibuffer-project jinx ligature link-hint magit marginalia
                markdown-mode mini-modeline nerd-icons-corfu nerd-icons-dired
                nerd-icons-ibuffer ob-go ob-restclient orderless org-auto-tangle
                org-roam ox-gfm ox-reveal pinyinlib plantuml-mode popper
                python-mode quickrun rainbow-delimiters rust-mode scss-mode
                shackle smart-mode-line sudo-edit tabspaces tramp-hlo
                treesit-auto vertico virtualenvwrapper vlf vterm-toggle web-mode))
 '(package-vc-selected-packages
   '((lsp-proxy :url "https://github.com/jadestrong/lsp-proxy"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#fbf1c7" :foreground "#3c3836"))))
 '(completions-common-part ((t (:foreground "#8f3f71" :weight normal))))
 '(completions-first-difference ((t (:foreground "#3c3836" :weight normal))))
 '(consult-highlight-line ((t (:background "#f7efd0" :extend t))))
 '(consult-preview-match ((t (:foreground "#458588" :background nil))))
 '(error ((t (:foreground "#cc241d"))))
 '(font-lock-builtin-face ((t (:foreground "#3c3836"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#458588"))))
 '(font-lock-comment-face ((t (:foreground "#458588"))))
 '(font-lock-constant-face ((t (:foreground "#8f3f71"))))
 '(font-lock-function-name-face ((t (:foreground "#af3a03"))))
 '(font-lock-keyword-face ((t (:foreground "#3c3836"))))
 '(font-lock-number-face ((t (:foreground "#8f3f71"))))
 '(font-lock-operator-face ((t (:foreground "#3c3836"))))
 '(font-lock-string-face ((t (:foreground "#79740e"))))
 '(font-lock-type-face ((t (:foreground "#3c3836"))))
 '(font-lock-variable-name-face ((t (:foreground "#af3a03"))))
 '(fringe ((t (:background "#fbf1c7"))))
 '(header-line ((t (:background "#f2e5bc" :foreground "#3c3836" :box nil :underline nil :inherit nil))))
 '(hl-line ((t (:background "#f7efd0" :extend t))))
 '(isearch ((t (:background "#d79921" :foreground "#fbf1c7"))))
 '(lazy-highlight ((t (:background "#f2e5bc" :foreground "#3c3836"))))
 '(line-number ((t (:foreground "#928374" :background "#fbf1c7"))))
 '(line-number-current-line ((t (:foreground "#3c3836" :background "#f2e5bc"))))
 '(minibuffer-prompt ((t (:foreground "#8f3f71"))))
 '(mode-line ((t (:background "#f9f5d7" :foreground "#3c3836" :box nil))))
 '(mode-line-inactive ((t (:background "#f2e5bc" :foreground "#928374" :box nil))))
 '(org-block ((t (:background "#f9f5d7" :foreground "#3c3836"))))
 '(org-code ((t (:foreground "#79740e"))))
 '(org-comment ((t (:foreground "#458588"))))
 '(org-done ((t (:foreground "#79740e"))))
 '(org-level-1 ((t (:foreground "#3c3836"))))
 '(org-level-2 ((t (:foreground "#3c3836"))))
 '(org-level-3 ((t (:foreground "#3c3836"))))
 '(org-link ((t (:foreground "#8f3f71" :underline t))))
 '(org-todo ((t (:foreground "#cc241d"))))
 '(org-verbatim ((t (:foreground "#8f3f71"))))
 '(region ((t (:background "#f2e5bc"))))
 '(show-paren-match ((t (:background "#f2e5bc"))))
 '(show-paren-mismatch ((t (:background "#cc241d" :foreground "#fbf1c7"))))
 '(vertico-current ((t (:background "#f2e5bc" :extend t))))
 '(vterm-color-black ((t (:foreground "#282828" :background "#282828"))))
 '(vterm-color-blue ((t (:foreground "#076678" :background "#076678"))))
 '(vterm-color-bright-black ((t (:foreground "#a89984" :background "#a89984"))))
 '(vterm-color-bright-blue ((t (:foreground "#458588" :background "#458588"))))
 '(vterm-color-bright-cyan ((t (:foreground "#689d6a" :background "#689d6a"))))
 '(vterm-color-bright-green ((t (:foreground "#98971a" :background "#98971a"))))
 '(vterm-color-bright-magenta ((t (:foreground "#8f3f71" :background "#8f3f71"))))
 '(vterm-color-bright-red ((t (:foreground "#cc241d" :background "#cc241d"))))
 '(vterm-color-bright-white ((t (:foreground "#1d2021" :background "#1d2021"))))
 '(vterm-color-bright-yellow ((t (:foreground "#d79921" :background "#d79921"))))
 '(vterm-color-cyan ((t (:foreground "#427b58" :background "#427b58"))))
 '(vterm-color-green ((t (:foreground "#79740e" :background "#79740e"))))
 '(vterm-color-magenta ((t (:foreground "#b16286" :background "#b16286"))))
 '(vterm-color-red ((t (:foreground "#9d0006" :background "#9d0006"))))
 '(vterm-color-white ((t (:foreground "#d5c4a1" :background "#d5c4a1"))))
 '(vterm-color-yellow ((t (:foreground "#b57614" :background "#b57614"))))
 '(warning ((t (:foreground "#d79921")))))
