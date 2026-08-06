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
(add-hook 'window-size-change-functions 'frame-hide-title-bar-when-maximized)
;; (add-to-list 'default-frame-alist '(undecorated . t))
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
  ;; 基础
  (require 'init-base)
  (require 'init-custom)
  (require 'init-emacs)
  ;; 编辑 / 界面 / 补全
  (require 'init-editor)
  (require 'init-tools)
  (require 'init-keyboard)
  (require 'init-theme)
  (require 'init-ui)
  (require 'init-completion)

  (require 'init-chinese)
  ;; 编程 / 项目 / 笔记
  (require 'init-programming)
  (require 'init-langs)
  (require 'init-org)
  (require 'init-project)
  ;; 工具 / 其他
  (require 'init-func)
  (require 'init-windows)
  (require 'init-ai)
  (require 'init-git)
  (require 'init-my-blog)
  (require 'init-reading)
  )

(unless (server-running-p)
  (server-start))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((tramp-rpc :url "https://github.com/ArthurHeymans/emacs-tramp-rpc" :lisp-dir
                "lisp")
     (i18n-quick :url "https://github.com/apple115/i18n-quick.el")
     (evil-ghostel :url "https://github.com/dakra/ghostel" :lisp-dir
                   "extensions/evil-ghostel")
     (ghostel :url "https://github.com/dakra/ghostel" :lisp-dir "lisp"))))
