;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(require 'package)

;; 设置垃圾回收参数
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
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

;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 5))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
;; 将lisp目录放到加载路径的前面以加快启动速度
(let
    ((dir
      (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path
               (file-name-as-directory dir)))
;; 加载各模块化配置
;; 不要在`*message*'缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-edit)
  (require 'init-tools)
  (require 'init-ui)
  (require 'init-completion)
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
  (require 'init-anki)
  (require 'init-sql)
  (require 'init-project)
  (require 'init-keyboard)
  (require 'init-chinese)
  (require 'init-ai)
  (require 'init-emacs)
  (require 'init-git)
  (require 'init-go)
  ;; (require 'init-org-ui)
  (require 'init-lsp-bridge)
  ;;(require 'init-eaf)
  ;; (require 'init-projectile)
  )
(when
    (memq window-system
          '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))
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
   '(add-node-modules-path atomic-chrome auto-yasnippet cal-china-x citre
                           clojure-ts-mode colorful-mode consult-notes
                           consult-todo dape denote diredfl dirvish docker
                           doom-modeline dumb-jump dwim-shell-command eglot
                           ejc-sql embark-consult emmet-mode engine-mode
                           evil-collection evil-matchit evil-nerd-commenter
                           evil-surround evil-textobj-tree-sitter
                           exec-path-from-shell flymake-flycheck format-all
                           general gptel graphviz-dot-mode gruvbox-theme
                           haskell-mode ibuffer-project jinx ligature link-hint
                           magit marginalia markdown-mode nerd-icons-dired
                           nerd-icons-ibuffer ob-go ob-restclient olivetti
                           orderless org-appear org-auto-tangle org-contrib
                           org-modern org-modern-indent org-roam ox-gfm
                           ox-reveal pinyinlib plantuml-mode popper python-mode
                           pyvenv quickrun rainbow-delimiters scss-mode shackle
                           sis sudo-edit tabspaces tree-sitter treesit-auto
                           ultra-scroll vertico virtualenvwrapper vlf
                           vterm-toggle web-mode))
 '(package-vc-selected-packages
   '((ultra-scroll :url "https://github.com/jdtsmith/ultra-scroll"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
