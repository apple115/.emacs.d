;;; init-org-set.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: org-set

;;; Code:
(use-package org
:ensure nil
:mode ("\\.org\\'" . org-mode)
:hook ((org-mode . visual-line-mode))
:custom-face
;; 设置Org mode标题以及每级标题行的大小
;; (org-document-title ((t (:height 1.75 :weight bold))))
;; (org-level-1 ((t (:height 1.2 :weight bold))))
;; (org-level-2 ((t (:height 1.15 :weight bold))))
;; (org-level-3 ((t (:height 1.1 :weight bold))))
;; (org-level-4 ((t (:height 1.05 :weight bold))))
;; (org-level-5 ((t (:height 1.0 :weight bold))))
;; (org-level-6 ((t (:height 1.0 :weight bold))))
;; (org-level-7 ((t (:height 1.0 :weight bold))))
;; (org-level-8 ((t (:height 1.0 :weight bold))))
;; (org-level-9 ((t (:height 1.0 :weight bold))))
;; 设置代码块用上下边线包裹
;;(org-block-begin-line ((t (:underline t :background unspecified))))
:config
(require 'cl)
;; 为*设置蓝色 *内容*
(setq org-emphasis-alist
      (cons '("*" '(:emphasis t :foreground "blue"))
            (delete* "*" org-emphasis-alist :key 'car :test 'equal)))

(setq org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                            ))

(setq org-todo-keyword-faces
      '(("PROG".(:foreground "yellow" :weight bold))))

;; ---- org代码块相关的设置
(setq org-src-fontify-natively 1);代码块语法高亮
(setq org-src-tab-acts-natively 1);开启代码块语法缩进
(setq org-edit-src-content-indentation 0);代码块初始缩进范围
(add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))

(setq org-directory "~/Nutstore Files/Nutstore/org/")

;; 设置标题行之间总是有空格；列表之间根据情况自动加空格
(setq org-blank-before-new-entry '((heading . t)
                                    (plain-list-item . auto)
                                    ))


:custom
;; 标题行美化
(org-fontify-whole-heading-line t)
;; 设置标题行折叠符号
(org-ellipsis " ▾")
;; 在活动区域内的所有标题栏执行某些命令
(org-loop-over-headlines-in-active-region t)
;; TODO标签美化
(org-fontify-todo-headline t)
;; DONE标签美化
(org-fontify-done-headline t)
;; 引用块美化
(org-fontify-quote-and-verse-blocks t)
;; 隐藏宏标记
(org-hide-macro-markers t)
;; 隐藏强调标签
(org-hide-emphasis-markers t)
;; 高亮latex语法
(org-highlight-latex-and-related '(native script entities))
;; 以UTF-8显示
(org-pretty-entities t)
;; 是否隐藏标题栏的前置星号，这里我们通过org-modern来隐藏
;; (org-hide-leading-stars t)
;; 当启用缩进模式时自动隐藏前置星号
(org-indent-mode-turns-on-hiding-stars t)
;; 自动启用缩进
(org-startup-indented nil)
;; 根据标题栏自动缩进文本
(org-adapt-indentation nil)
;; 自动显示图片
(org-startup-with-inline-images t)
;; 默认以Overview的模式展示标题行
(org-startup-folded 'overview)
 ;; 允许字母列表
(org-list-allow-alphabetical t)
;; 列表的下一级设置
(org-list-demote-modify-bullet '(
                                ("-"  . "+")
                                ("+"  . "1.")
                                ("1." . "a.")
                                ))
;; 编辑时检查是否在折叠的不可见区域
(org-fold-catch-invisible-edits 'smart)
;; 在当前位置插入新标题行还是在当前标题行后插入，这里设置为当前位置
(org-insert-heading-respect-content nil)
;; 设置图片的最大宽度，如果有imagemagick支持将会改变图片实际宽度
;; 四种设置方法：(1080), 1080, t, nil
(org-image-actual-width nil)
;; imenu的最大深度，默认为2
(org-imenu-depth 4)
;; 回车要不要触发链接，这里设置不触发
(org-return-follows-link nil)
;; 上标^下标_是否需要特殊字符包裹，这里设置需要用大括号包裹
(org-use-sub-superscripts '{})
;; 复制粘贴标题行的时候删除id
(org-clone-delete-id t)
;; 粘贴时调整标题行的级别
(org-yank-adjusted-subtrees t)

;; 使用专家模式选择标题栏状态
(org-use-fast-todo-selection 'expert)

;; 归档设置
(org-archive-location "%s_archive::datetree/")
)

;; 自动展开
(use-package org-contrib
  :ensure t)

;; 自动tangle
(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode)
)

;; agenda
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (evil-set-initial-state 'org-agenda-mode 'normal)))

;; capture
(add-hook 'org-capture-mode-hook
          (lambda ()
            (evil-set-initial-state 'org-capture-mode 'normal)))

(evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "/") #'org-agenda-filter)
(evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "tab") #'org-agenda-goto)
(evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "q") #'org-agenda-quit)
(evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "j") #'org-agenda-next-line)
(evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "k") #'org-agenda-previous-line)
(evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "gr") #'org-agenda-redo-all)

(provide 'init-org-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-set.el ends here
