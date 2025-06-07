;;; init-write.el --- Write settings -*- lexical-binding: t -*-
;;; Commentary:
;;; write

;;; Code:
(use-package org
:ensure nil
:mode ("\\.org\\'" . org-mode)
:hook ((org-mode . visual-line-mode))
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

(setq org-directory "~/Documents/org/")

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

;;添加org-babel支持
(add-to-list 'org-src-lang-modes '("go" . go-ts))
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

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-directory (expand-file-name "~/Documents/org/denote"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; org is default, set others such as text, markdown-yaml, markdown-toml
  (setq denote-file-type nil)
  (setq denote-prompts '(title keywords))

  ;; We allow multi-word keywords by default.  The author's personal
  ;; preference is for single-word keywords for a more rigid workflow.
  (setq denote-allow-multi-word-keywords nil)
  (setq denote-date-format nil)

  ;; If you use Markdown or plain text files (Org renders links as buttons
  ;; right away)
  ;; (add-hook 'find-file-hook #'denote-link-buttonize-buffer)
  (setq denote-dired-rename-expert nil)

  ;; OR if only want it in `denote-dired-directories':
  (add-hook 'dired-mode-hook #'denote-dired-mode-in-directories)
  )

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :init
  ;; enable plantuml babel support
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((plantuml . t))))
  :config
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-executable-path "plantuml")
  (setq plantuml-executable-path "plantuml")
  (setq plantuml-default-exec-mode 'executable)
  ;; set default babel header arguments
  (setq org-babel-default-header-args:plantuml
        '((:exports . "results")
          (:results . "file")
          ))
)

(use-package org-excalidraw
  :load-path "./site-lisp/org-excalidraw"
  :config
  (setq org-excalidraw-directory "~/Nutstore Files/Nutstore/org/picture")
)

(use-package ox
  :ensure nil
  :custom
  (org-export-with-toc t)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-drawers nil)
  (org-export-with-priority t)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers t)
  (org-export-with-sub-superscripts '{})
  ;; `org-export-use-babel' set to nil will cause all source block header arguments to be ignored This means that code blocks with the argument :exports none or :exports results will end up in the export.
  ;; See:
  ;; https://stackoverflow.com/questions/29952543/how-do-i-prevent-org-mode-from-executing-all-of-the-babel-source-blocks
  (org-export-use-babel t)
  (org-export-headline-levels 9)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  (org-export-default-language "zh-CN") ; 默认是en
  ;; (org-ascii-text-width 72)
  )
;; export extra

(use-package ox-gfm
  :ensure t
  :after ox)

(require 'ob-python)
(require 'ob-C)
(use-package ob-go
:ensure t
)

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-inside-latex t)
)

(use-package ox-reveal
    :ensure t
    :config
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
    (reveal-mode 1)
)

(use-package hexo
 :load-path "./site-lisp/hexo.el"
 :config
 (defun hexo-my-blog ()
    (interactive)
    (hexo "~/blog/"))
(evil-collection-define-key 'normal 'hexo-mode-map (kbd "RET") #'hexo-command-open-file)
(evil-collection-define-key 'normal 'hexo-mode-map (kbd "q") #'quit-window)
(evil-collection-define-key 'normal 'hexo-mode-map (kbd "D") #'hexo-command-delete-file)
)

;; (setq ispell-program-name "aspell")
;; ;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; ;; @see https://github.com/redguardtoo/emacs.d/issues/796
;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

;; ;; (setq ispell-program-name "hunspell")
;; ;; ;; reset the hunspell so it STOPS querying locale!
;; ;; ;; "en_US" is the key to lookup in `ispell-local-dictionary-alist`
;; ;; (setq ispell-local-dictionary "en_US")
;; ;; ;; two dictionaries "en_US" and "zh_CN" are used. Feel free to remove "zh_CN"
;; ;; ;; If `ispell-local-dictionary-alist' is nil, `ispell-local-dictionary' is passed
;; ;; ;; to hunpsell cli program as dictionary.
;; ;; (setq ispell-local-dictionary-alist
;; ;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US" "zh_CN") nil utf-8)))
;; ;; ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
;; ;; ;; If it's nil, Emacs tries to automatically set up the dictionaries.
;; ;; (when (boundp 'ispell-hunspell-dictionary-alist)
;; ;;       (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

;; (use-package  wucuo
;;   :ensure t
;;   :custom
;;   (wucuo-flyspell-start-mode "fast")
;;   ;; How many seconds wucuo waits before running spell-check.
;;   (wucuo-update-interval 2)
;;   :config
;;   (add-hook 'prog-mode-hook #'wucuo-start)
;; (add-hook 'text-mode-hook #'wucuo-start)
;; (setq wucuo-spell-check-buffer-predicate
;;       (lambda ()
;;         (not (memq major-mode
;;                    '(dired-mode
;;                      vterm-mode
;;                      log-edit-mode
;;                      compilation-mode
;;                      help-mode
;;                      profiler-report-mode
;;                      speedbar-mode
;;                      gud-mode
;;                      calc-mode
;;                      Info-mode
;;                      )))))
;; )

(use-package olivetti
  :hook ((markdown-mode . olivetti-mode)
         (org-mode .  olivetti-mode)
         (nov-mode . olivetti-mode))

  :ensure t
  :custom
  (olivetti-set-width 240)
)

(use-package jinx
   :ensure t
   :config
   (global-jinx-mode 1)
(let ((st jinx--base-syntax-table))
  (modify-syntax-entry '(#x4E00 . #x9FFF) "_" st)   ; CJK Unified Ideographs
  (modify-syntax-entry '(#x3400 . #x4DBF) "_" st)   ; CJK Unified Ideographs Extension A
  (modify-syntax-entry '(#x20000 . #x2A6DF) "_" st) ; CJK Unified Ideographs Extension B
  (modify-syntax-entry '(#x2A700 . #x2B73F) "_" st) ; CJK Unified Ideographs Extension C
  (modify-syntax-entry '(#x2B740 . #x2B81F) "_" st) ; CJK Unified Ideographs Extension D
  (modify-syntax-entry '(#x2B820 . #x2CEAF) "_" st) ; CJK Unified Ideographs Extension E
  (modify-syntax-entry '(#x2CEB0 . #x2EBEF) "_" st) ; CJK Unified Ideographs Extension F
  (modify-syntax-entry '(#x30000 . #x3134F) "_" st) ; CJK Unified Ideographs Extension G
  (modify-syntax-entry '(#x31350 . #x323AF) "_" st) ; CJK Unified Ideographs Extension H
  (modify-syntax-entry '(#x2EBF0 . #x2EE5F) "_" st) ; CJK Unified Ideographs Extension I
  )
)

(use-package graphviz-dot-mode
  :ensure t
  :init
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((dot . t))))
  :config
  (setq graphviz-dot-indent-width 4)
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'init-write)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-write.el ends here
