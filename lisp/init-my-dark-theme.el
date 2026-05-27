;;; init-my-dark-theme.el --- Gruvbox Dark Minimal Theme -*- lexical-binding: t -*-
;; 基于Alabaster设计理念
;; 使用自定义face设置而不是deftheme，避免主题文件路径问题

;; Gruvbox Dark 配色
(defconst my-gruvbox-dark-colors
  '((bg       . "#282828")    ; 背景
    (fg       . "#ebdbb2")    ; 前景
    (bg-hard  . "#1d2021")    ; 硬背景
    (bg-soft  . "#32302f")    ; 软背景
    (bg-more-soft . "#3c3836"); 更浅
    (string   . "#b8bb26")    ; 绿色 - 字符串
    (const    . "#d3869b")    ; 紫色 - 常量
    (comment  . "#83a598")    ; 青色 - 注释(高对比度)
    (def      . "#fe8019")    ; 橙色 - 全局定义
    (dim      . "#a89984")    ; 灰色 - UI元素
    (error    . "#fb4934")    ; 红色
    (warning  . "#fabd2f")    ; 黄色
    (search   . "#fabd2f")    ; 搜索背景
    ;; ANSI 终端颜色 (ghostel / comint)
    (ansi-black        . "#282828")
    (ansi-red          . "#cc241d")
    (ansi-green        . "#98971a")
    (ansi-yellow       . "#d79921")
    (ansi-blue         . "#458588")
    (ansi-magenta      . "#b16286")
    (ansi-cyan         . "#689d6a")
    (ansi-white        . "#a89984")
    (ansi-bright-black . "#928374")
    (ansi-bright-red   . "#fb4934")
    (ansi-bright-green . "#b8bb26")
    (ansi-bright-yellow . "#fabd2f")
    (ansi-bright-blue  . "#83a598")
    (ansi-bright-magenta . "#d3869b")
    (ansi-bright-cyan  . "#8ec07c")
    (ansi-bright-white . "#ebdbb2")
    ))

;; 应用主题设置
(defun my-gruvbox-dark-minimal-setup ()
  "设置Gruvbox Dark Minimal主题"
  (interactive)

  ;; 基础设置
  (set-background-color (cdr (assoc 'bg my-gruvbox-dark-colors)))
  (set-foreground-color (cdr (assoc 'fg my-gruvbox-dark-colors)))
  (set-cursor-color (cdr (assoc 'fg my-gruvbox-dark-colors)))

  ;; 自定义face设置 - Alabaster风格的4类高亮
  (custom-set-faces
   ;; 基础
   `(default ((t (:background ,(cdr (assoc 'bg my-gruvbox-dark-colors))
                        :foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))

   `(region ((t (:background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors))))))

   ;; Mode line
   `(mode-line ((t (:background ,(cdr (assoc 'bg-hard my-gruvbox-dark-colors))
                        :foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))
                        :box nil))))
   `(mode-line-inactive ((t (:background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors))
                              :foreground ,(cdr (assoc 'dim my-gruvbox-dark-colors))
                              :box nil))))

   ;; 1. 字符串 - 绿色，最突出
   `(font-lock-string-face ((t (:foreground ,(cdr (assoc 'string my-gruvbox-dark-colors))))))

   ;; 2. 静态常量 - 紫色
   `(font-lock-constant-face ((t (:foreground ,(cdr (assoc 'const my-gruvbox-dark-colors))))))
   `(font-lock-number-face ((t (:foreground ,(cdr (assoc 'const my-gruvbox-dark-colors))))))

   ;; 3. 注释 - 青色，高对比度
   `(font-lock-comment-face ((t (:foreground ,(cdr (assoc 'comment my-gruvbox-dark-colors))))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,(cdr (assoc 'comment my-gruvbox-dark-colors))))))

   ;; 4. 全局定义 - 橙色
   `(font-lock-function-name-face ((t (:foreground ,(cdr (assoc 'def my-gruvbox-dark-colors))))))
   `(font-lock-variable-name-face ((t (:foreground ,(cdr (assoc 'def my-gruvbox-dark-colors))))))

   ;; 不高亮标准语言关键词 - 使用默认前景色
   `(font-lock-keyword-face ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(font-lock-type-face ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(font-lock-builtin-face ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(font-lock-operator-face ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))

   ;; 错误和警告
   `(error ((t (:foreground ,(cdr (assoc 'error my-gruvbox-dark-colors))))))
   `(warning ((t (:foreground ,(cdr (assoc 'warning my-gruvbox-dark-colors))))))

   ;; 其他UI
   `(fringe ((t (:background ,(cdr (assoc 'bg my-gruvbox-dark-colors))))))
   `(line-number ((t (:foreground ,(cdr (assoc 'dim my-gruvbox-dark-colors))
                        :background ,(cdr (assoc 'bg my-gruvbox-dark-colors))))))
   `(line-number-current-line ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))
                                  :background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors))))))

   ;; 搜索
   `(isearch ((t (:background ,(cdr (assoc 'search my-gruvbox-dark-colors))
                    :foreground ,(cdr (assoc 'bg my-gruvbox-dark-colors))))))
   `(lazy-highlight ((t (:background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors))
                          :foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))

   ;; 括号匹配
   `(show-paren-match ((t (:background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors))))))
   `(show-paren-mismatch ((t (:background ,(cdr (assoc 'error my-gruvbox-dark-colors))
                               :foreground ,(cdr (assoc 'bg my-gruvbox-dark-colors))))))

   ;; Minibuffer
   `(minibuffer-prompt ((t (:foreground ,(cdr (assoc 'const my-gruvbox-dark-colors))))))

   ;; Org-mode (最小化)
   `(org-level-1 ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(org-level-2 ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(org-level-3 ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(org-link ((t (:foreground ,(cdr (assoc 'const my-gruvbox-dark-colors)) :underline t))))
   `(org-code ((t (:foreground ,(cdr (assoc 'string my-gruvbox-dark-colors))))))
   `(org-verbatim ((t (:foreground ,(cdr (assoc 'const my-gruvbox-dark-colors))))))
   `(org-comment ((t (:foreground ,(cdr (assoc 'comment my-gruvbox-dark-colors))))))
   `(org-todo ((t (:foreground ,(cdr (assoc 'error my-gruvbox-dark-colors))))))
   `(org-done ((t (:foreground ,(cdr (assoc 'string my-gruvbox-dark-colors))))))
   `(org-block ((t (:background ,(cdr (assoc 'bg-hard my-gruvbox-dark-colors))
                                :foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))))))
   `(hl-line ((t (:background ,(cdr (assoc 'bg-more-soft my-gruvbox-dark-colors)) :extend t))))
   `(header-line ((t (:background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors))
                                  :foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors))
                                  :box nil
                                  :underline nil
                                  :inherit nil
                                  ))))

   ;; consult
   `(consult-highlight-line ((t (:background ,(cdr (assoc 'bg-more-soft my-gruvbox-dark-colors)) :extend t))))
   `(consult-preview-match ((t (:foreground ,(cdr (assoc 'comment my-gruvbox-dark-colors)) :background nil))))
   `(completions-common-part ((t (:foreground ,(cdr (assoc 'const my-gruvbox-dark-colors)) :weight normal))))
   `(completions-first-difference ((t (:foreground ,(cdr (assoc 'fg my-gruvbox-dark-colors)) :weight normal))))

   ;; vertice
   `(vertico-current ((t (:background ,(cdr (assoc 'bg-soft my-gruvbox-dark-colors)) :extend t))))

   ;; dired
   `(dired-header ((t (:background nil :inherit default))))
   `(dired-perm-write ((t (:background nil :foreground ,(cdr (assoc 'error my-gruvbox-dark-colors))))))
   `(dired-directory ((t (:background nil :foreground ,(cdr (assoc 'def my-gruvbox-dark-colors)) :weight bold))))
   `(dired-symlink ((t (:background nil :foreground ,(cdr (assoc 'const my-gruvbox-dark-colors))))))
   `(dired-flagged ((t (:foreground ,(cdr (assoc 'error my-gruvbox-dark-colors)) :weight bold))))
   `(dired-ignored ((t (:foreground ,(cdr (assoc 'dim my-gruvbox-dark-colors))))))
   `(dired-set-id ((t (:background nil
                        :foreground ,(cdr (assoc 'warning my-gruvbox-dark-colors))
                        :underline t))))
   `(dired-special ((t (:background nil :foreground ,(cdr (assoc 'const my-gruvbox-dark-colors))))))

   ;; ghostel / ANSI 终端颜色
   `(ghostel-color-black        ((t (:foreground ,(cdr (assoc 'ansi-black my-gruvbox-dark-colors))))))
   `(ghostel-color-red          ((t (:foreground ,(cdr (assoc 'ansi-red my-gruvbox-dark-colors))))))
   `(ghostel-color-green        ((t (:foreground ,(cdr (assoc 'ansi-green my-gruvbox-dark-colors))))))
   `(ghostel-color-yellow       ((t (:foreground ,(cdr (assoc 'ansi-yellow my-gruvbox-dark-colors))))))
   `(ghostel-color-blue         ((t (:foreground ,(cdr (assoc 'ansi-blue my-gruvbox-dark-colors))))))
   `(ghostel-color-magenta      ((t (:foreground ,(cdr (assoc 'ansi-magenta my-gruvbox-dark-colors))))))
   `(ghostel-color-cyan         ((t (:foreground ,(cdr (assoc 'ansi-cyan my-gruvbox-dark-colors))))))
   `(ghostel-color-white        ((t (:foreground ,(cdr (assoc 'ansi-white my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-black ((t (:foreground ,(cdr (assoc 'ansi-bright-black my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-red   ((t (:foreground ,(cdr (assoc 'ansi-bright-red my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-green ((t (:foreground ,(cdr (assoc 'ansi-bright-green my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-yellow ((t (:foreground ,(cdr (assoc 'ansi-bright-yellow my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-blue  ((t (:foreground ,(cdr (assoc 'ansi-bright-blue my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-magenta ((t (:foreground ,(cdr (assoc 'ansi-bright-magenta my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-cyan  ((t (:foreground ,(cdr (assoc 'ansi-bright-cyan my-gruvbox-dark-colors))))))
   `(ghostel-color-bright-white ((t (:foreground ,(cdr (assoc 'ansi-bright-white my-gruvbox-dark-colors))))))
  ))

;; 自动启用主题
(my-gruvbox-dark-minimal-setup)

;; 提供重新加载函数
(defun my-gruvbox-dark-minimal-reload ()
  "重新加载Gruvbox Dark Minimal主题"
  (interactive)
  (my-gruvbox-dark-minimal-setup))

(provide 'init-my-dark-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-my-dark-theme.el ends here
