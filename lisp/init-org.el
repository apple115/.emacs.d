;;; init-org.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-write.el, init-org-agenda.el, init-org-capture.el, init-org-ui.el
;;; Code:

;; ---- merged from init-write.el ----
;;;

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; 为*设置蓝色 *内容*
  (setq org-emphasis-alist
        (cons '("*" '(:emphasis t :foreground "blue"))
              (cl-delete "*" org-emphasis-alist :key 'car :test 'equal)))

  (setq org-todo-keywords '((sequence "TODO(t)" "PROG(p)" "WAIT(w)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                            ))

  (setq org-todo-keyword-faces
        '(("PROG".(:foreground "yellow" :weight bold))))

  ;; ---- org代码块相关的设置
  (setq org-src-fontify-natively 1);代码块语法高亮
  (setq org-src-tab-acts-natively 1);开启代码块语法缩进
  (setq org-edit-src-content-indentation 0);代码块初始缩进范围
  (add-hook 'org-mode-hook (lambda () (org-display-inline-images t)))

  (setq org-directory my-org-directory)

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

  (org-link-set-parameters
   "db"
   :follow (lambda (path)
             (let* ((db-type (org-entry-get nil "DB_TYPE") t)
                    (db-host (org-entry-get nil "DB_HOST") t)
                    (db-post (org-entry-get nil "DB_PORT") t)
                    (db-user (org-entry-get nil "DB_USER") t)
                    (db-pass (org-entry-get nil "DB_PASS") t)
                    (db-index (org-entry-get nil "DB_INDEX") t)
                    (db-name (org-entry-get nil "DB_NAME") t) ;;自定义命名
                    (proto (org-entry-get nil "PROTO_FILE" t))
                    (buf-name (format "*ghostel-%s" db_name))
                    (cmd (cond
                          ;;MYSQL 逻辑
                          ((string= db_type "mysql")
                           (format "mycli -u %s %s -h %s"
                                   db-user (if db-pass(concat "-p" db-pass) dbhost))
                           )
                          ;;Postgres 逻辑
                          ((string= db-type "pg")
                           (format "pgcli -U %s -h %s" db-user dbhost))

                          ((string= db-type "redis")
                           (format "iredis -h %s -p %s %s %s"
                                   (or db-host "127.0.0.1")
                                   (or db-port "6379")
                                   (if db-pass (concat "-a " db-pass) "")
                                   (if db-index (concat "-n " db-index) "")))
                          (t (error "不支持数据库类型:%s" db-type))))
                    )
               (if db-host
                   (progn my/open-ghostel-database buf-name cmd)
                 ))))

  )
;; 自动展开
;; (use-package org-contrib
;;   :ensure t)

;; 自动tangle
(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode)
  )

;; agenda
;; (add-hook 'org-agenda-mode-hook
;;           (lambda ()
;;             (evil-set-initial-state 'org-agenda-mode 'normal)))

;; ;; capture
;; (add-hook 'org-capture-mode-hook
;;           (lambda ()
;;             (evil-set-initial-state 'org-capture-mode 'normal)))

(with-eval-after-load 'evil-collection-mode
  (evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "/") #'org-agenda-filter)
  (evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "tab") #'org-agenda-goto)
  (evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "q") #'org-agenda-quit)
  (evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "j") #'org-agenda-next-line)
  (evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "k") #'org-agenda-previous-line)
  (evil-collection-define-key 'normal 'org-agenda-mode-map (kbd "gr") #'org-agenda-redo-all)
  )

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode-in-directories)
  :config
  (setq denote-directory (expand-file-name "denote" my-org-directory))
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
  :after org
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
  ;; enable plantuml babel support
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((plantuml . t))))
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

(use-package ob-python
  :config
  (setq org-babel-python-command "~/.venv/org/bin/python")
)

(require 'ob-C)
(use-package ob-go
  :ensure t
  )


;; (use-package org-appear
;;   :ensure t
;;   :hook (org-mode . org-appear-mode)
;;   :config
;;   (setq org-appear-autolinks t)
;;   (setq org-appear-autosubmarkers t)
;;   (setq org-appear-autoentities t)
;;   (setq org-appear-autokeywords t)
;;   (setq org-appear-inside-latex t)
;;   )

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
  ;; (evil-collection-define-key 'normal 'hexo-mode-map (kbd "RET") #'hexo-command-open-file)
  ;; (evil-collection-define-key 'normal 'hexo-mode-map (kbd "q") #'quit-window)
  ;; (evil-collection-define-key 'normal 'hexo-mode-map (kbd "D") #'hexo-command-delete-file)
  )

(use-package jinx
  :ensure t
  :config
  ;; (global-jinx-mode 1)
  )

(with-eval-after-load 'jinx-mode
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
    ))

(use-package graphviz-dot-mode
  :ensure t
  :after org
  :config
  (setq graphviz-dot-indent-width 4)
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
  ;; enable graphviz babel support
  (org-babel-do-load-languages 'org-babel-load-languages
                               (append org-babel-load-languages
                                       '((dot . t))))
  )

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (expand-file-name "roam" my-org-directory)))
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

(use-package org-download
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

;; Drag-and-drop to `dired`

;; ---- merged from init-org-agenda.el ----
(use-package calendar
    :ensure nil
    :hook (calendar-today-visible . calendar-mark-today)
    :custom
    ;; 是否显示中国节日，我们使用 `cal-chinese-x' 插件
    (calendar-chinese-all-holidays-flag nil)
    ;; 是否显示节日
    (calendar-mark-holidays-flag t)
    ;; 是否显示Emacs的日记，我们使用org的日记
    (calendar-mark-diary-entries-flag nil)
    ;; 数字方式显示时区，如 +0800，默认是字符方式如 CST
    (calendar-time-zone-style 'numeric)
    ;; 日期显示方式：year/month/day
    (calendar-date-style 'iso)
    ;; 中文天干地支设置
    (calendar-chinese-celestial-stem ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
    (calendar-chinese-terrestrial-branch ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
    ;; 设置中文月份
    (calendar-month-name-array ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])
    ;; 设置星期标题显示
    (calendar-day-name-array ["日" "一" "二" "三" "四" "五" "六"])
    ;; 周一作为一周第一天
    (calendar-week-start-day 1)
    )


(use-package org-agenda
  :ensure nil
  :hook (org-agenda-finalize . org-agenda-to-appt)
  :config
  ;; 日程模式的日期格式设置
  (setq org-agenda-format-date 'org-agenda-format-date-aligned)
  (defun org-agenda-format-date-aligned (date)
    "Format a DATE string for display in the daily/weekly agenda, or timeline.

This function makes sure that dates are aligned for easy reading."
    (require 'cal-iso)
    (let* ((dayname (aref cal-china-x-days
                          (calendar-day-of-week date)))
           (day (cadr date))
           (month (car date))
           (year (nth 2 date))
           (day-of-week (calendar-day-of-week date))
           (iso-week (org-days-to-iso-week
                      (calendar-absolute-from-gregorian date)))
           (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date)))
           (cn-month (cl-caddr cn-date))
           (cn-day (cl-cadddr cn-date))
           (cn-month-string (concat (aref cal-china-x-month-name
                                          (1- (floor cn-month)))
                                    (if (integerp cn-month)
                                        ""
                                      "（闰月）")))
           (cn-day-string (aref cal-china-x-day-name
                                (1- cn-day)))
           (extra (format " 农历%s%s%s%s"
                          (if (or (eq org-agenda-current-span 'day)
                                  (= day-of-week 1)
                                  (= cn-day 1))
                              cn-month-string
                            "")
                          (if (or (= day-of-week 1)
                                  (= cn-day 1))
                              (if (integerp cn-month) "" "[闰]")
                            "")
                          cn-day-string
                          (if (or (= day-of-week 1)
                                  (eq org-agenda-current-span 'day))
                              (format " 今年第%02d周" iso-week)
                            "")
                          ))
           )
      (format "%04d-%02d-%02d 星期%s%s%s\n" year month
              day dayname extra (concat " 第" (format-time-string "%j") "天"))))

  ;; 显示时间线
  (setq org-agenda-use-time-grid t)
  ;; 设置面包屑分隔符
  ;; (setq org-agenda-breadcrumbs-separator " ❱ ")
  ;; 设置时间线的当前时间指示串
  (setq org-agenda-current-time-string "--------------now")
  ;; 时间线范围和颗粒度设置
  (setq org-agenda-time-grid (quote ((daily today)
                                     (0600 0800 1000 1200
                                           1400 1600 1800
                                           2000 2200 2400)
                                     "......" "----------------")))
  ;; 日程视图的前缀设置
  (setq org-agenda-prefix-format '((agenda . " %i %-25:c %5t %s")
                                   (todo   . " %i %-25:c ")
                                   (tags   . " %i %-25:c ")
                                   (search . " %i %-25:c ")))
  ;; 对于计划中的任务在视图里的显示
  (setq org-agenda-scheduled-leaders
        '("计划 " "应在%02d天前开始 "))
  ;; 对于截止日期的任务在视图里的显示
  (setq org-agenda-deadline-leaders
        '("截止 " "还有%02d天到期 " "已经过期%02d天 "))
  ;; =====================
  ;; 自定义日程视图，分别显示TODO，WIP，WIAT中的任务
  ;; n键显示自定义视图，p键纯文本视图，a键默认视图
  ;; ;; =====================
  (defvar my-org-custom-daily-agenda
    `((todo "TODO"
            ((org-agenda-block-separator nil)
             (org-agenda-overriding-header "所有待办任务\n")))
      (todo "PROG"
            ((org-agenda-block-separator nil)
             (org-agenda-overriding-header "\n进行中的任务\n")))
      (todo "WAIT"
            ((org-agenda-block-separator nil)
             (org-agenda-overriding-header "\n等待中的任务\n")))
      (agenda "" ((org-agenda-block-separator nil)
                  (org-agenda-overriding-header "\n今日日程\n"))))
    "Custom agenda for use in `org-agenda-custom-commands'.")

  (setq org-agenda-custom-commands
        `(("n" "Daily agenda and top priority tasks"
           ,my-org-custom-daily-agenda)
           ))

  ;; 时间戳格式设置，会影响到 `svg-tag' 等基于正则的设置
  ;; 这里设置完后是 <2022-12-24 星期六> 或 <2022-12-24 星期六 06:53>
  (setq system-time-locale "zh_CN.UTF-8")
  (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 不同日程类别间的间隔
  (setq org-cycle-separator-lines 2)
  :custom
  ;; 设置需要被日程监控的org文件
  (org-agenda-files
   (list (expand-file-name "tasks.org" org-directory)
         (expand-file-name "diary.org" org-directory)
         (expand-file-name "habits.org" org-directory)
         (expand-file-name "projects.org" org-directory)
         ;; (expand-file-name "config.org" user-emacs-directory)
         ))
  ;; 设置org的日记文件
  ;; (org-agenda-diary-file (expand-file-name "diary.org" org-directory))
  ;; 日记插入精确时间戳
  (org-agenda-insert-diary-extract-time t)
  ;; 设置日程视图更加紧凑
  (org-agenda-compact-blocks t)
  ;; 日程视图的块分隔符
  (org-agenda-block-separator ?─)
  ;; 日视图还是周视图，通过 v-d, v-w, v-m, v-y 切换视图，默认周视图
  (org-agenda-span 'week)
  ;; q退出时删除agenda缓冲区
  (org-agenda-sticky nil)
  ;; 是否包含直接日期
  (org-agenda-include-deadlines t)
  ;; 禁止日程启动画面
  (org-agenda-inhibit-startup t)
  ;; 显示每一天，不管有没有条目
  (org-agenda-show-all-dates t)
  ;; 时间不足位时前面加0
  (org-agenda-time-leading-zero t)
  ;; 日程同时启动log mode
  (org-agenda-start-with-log-mode t)
  ;; 日程同时启动任务时间记录报告模式
  ;;(org-agenda-start-with-clockreport-mode t)
  ;; 截止的任务完成后不显示
  ;; (org-agenda-skip-deadline-if-done t)
  ;; 当计划的任务完成后不显示
  (org-agenda-skip-scheduled-if-done t)
  ;; 计划过期上限
  (org-scheduled-past-days 365)
  ;; 计划截止上限
  (org-deadline-past-days 365)
  ;; 计划中的任务不提醒截止时间
  (org-agenda-skip-deadline-prewarning-if-scheduled 1)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  ;; 设置工时记录报告格式
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :fileskip0 t :compact nil :narrow 80))
  (org-agenda-columns-add-appointments-to-effort-sum t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  ;; 标签显示的位置，第100列往前右对齐
  (org-agenda-tags-column -100)
  ;; 从星期一开始作为一周第一天
  (org-agenda-start-on-weekday 1)
  ;; 是否使用am/pm
  ;; (org-agenda-timegrid-use-ampm nil)
  ;; 搜索是不看时间
  (org-agenda-search-headline-for-time nil)
  ;; 提前3天截止日期到期告警
  (org-deadline-warning-days 3)
 )

(use-package org-habit
  :ensure nil
  :defer t
  :custom
  (org-habit-show-habits t)
  (org-habit-graph-column 70)
  (org-habit-show-all-today t)
  (org-habit-show-done-always-green t)
  (org-habit-scheduled-past-days t)
  ;; org habit show 7 days before today and 7 days after today. ! means not done. * means done.
  (org-habit-preceding-days 7)
  )

;; ---- merged from init-org-capture.el ----
(use-package org-capture
  :ensure nil
  :hook ((org-capture-mode . (lambda ()
                               (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
         (org-capture-mode . delete-other-windows))
  :custom
  (org-capture-use-agenda-date nil)
  ;; define common template
  (org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
                            "* TODO %i%?"
                            :empty-lines-after 1
                            :prepend t)
                           ("i" "Inbox" entry  (file "inbox.org")
                            ,(concat "* TODO %?\n"
                                     "/Entered on/ %U"))
                           )))


(setq org-format-latex-options
      '(:foreground default
        :background default
        :scale 1.5
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

;; ---- merged from init-org-ui.el ----
(use-package org-modern
  :ensure t
  :custom
  (org-modern-hide-stars nil)		; adds extra indentation
  (org-modern-table nil)
  (org-modern-list
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;; (use-package org-modern-indent
;;   :vc (:url "https://github.com/jdtsmith/org-modern-indent")
;;   :config ; add late to hook
;;   (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
