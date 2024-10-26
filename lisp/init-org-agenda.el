;;; init-org-agenda.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: org-agenda

;;; Code:
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


(provide 'init-org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-agenda.el ends here
