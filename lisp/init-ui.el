;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.7)
                            (height . 0.85)
                            (fullscreen)))


;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
;; 	doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-gruvbox-light t)
;;   (doom-themes-treemacs-config)
;;   ;;(doom-themes-vterm-config)
;;   )

(defun my-apply-font()
  (set-face-attribute 'default nil :font (font-spec :family "JetBrains Mono" :size 12 :weight 'medium))
  (set-fontset-font t '(#x2ff0 . #x9fff) (font-spec :family "LXGW WenKai" :size 12 :weight 'medium))
)

;; (defun my-apply-font()
;;   (set-face-attribute 'default nil :font (font-spec :family "Maple Mono NF CN" :size 12 :weight 'medium) :slant 'italic)
;; )


 ;; |大家|
 ;; |aabb|大家

;;像素滚动
(pixel-scroll-mode)
;; 告诉 Emacs：如果键盘信号发得太快，渲染跟不上了，就跳过中间过程，只画最后停下的地方
(setq fast-but-imprecise-scrolling t)

(my-apply-font)

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (my-apply-font)))

;;测试一下这个


;; 禁用一些GUI特性
(setq use-dialog-box nil)               ; 鼠标操作不使用对话框
(setq inhibit-default-init t)           ; 不加载 `default' 库
(setq inhibit-startup-screen t)         ; 不加载启动画面
(setq inhibit-startup-message t)        ; 不加载启动消息
(setq inhibit-startup-buffer-menu t)    ; 不显示缓冲区列表
(setq cursor-type nil)                  ; 不显示鼠标

;;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-ui.el#L208
;;; Cursor
;; The blinking cursor is distracting, but also interferes with cursor settings
;; in some minor modes that try to change it buffer-locally (like treemacs) and
;; can cause freezing for folks (esp on macOS) with customized & color cursors.
(blink-cursor-mode -1)
;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)
;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; 草稿缓冲区默认文字设置
(setq initial-scratch-message (concat ";; Happy hacking, "
                                      (capitalize user-login-name) " - Emacs ♥ you!\n\n"))

;; 设置缓冲区的文字无
(setq-default bidi-display-reordering nil)
;; 禁止使用双向括号算法
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; 设置自动折行宽度为80个字符，默认值为70
(setq-default fill-column 80)

;; 设置大文件阈值为100MB，默认10MB
(setq large-file-warning-threshold 100000000)

;; 以16进制显示字节数
(setq display-raw-bytes-as-hex t)
;; 有输入时禁止 `fontification' 相关的函数钩子，能让滚动更顺滑
(setq redisplay-skip-fontification-on-input t)

;; 禁止响铃
(setq ring-bell-function 'ignore)

;; 在光标处而非鼠标所在位置粘贴
(setq mouse-yank-at-point t)

;; 拷贝粘贴设置
(setq select-enable-primary nil)        ; 选择文字时不拷贝
(setq select-enable-clipboard t)        ; 拷贝时使用剪贴板

;; 鼠标滚动设置
(setq scroll-step 2)
(setq scroll-margin 2)
(setq hscroll-step 2)
(setq hscroll-margin 2)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq scroll-preserve-screen-position 'always)

;; 对于高的行禁止自动垂直滚动
(setq auto-window-vscroll nil)

;; 设置新分屏打开的位置的阈值
;; (setq split-width-threshold (assoc-default 'width default-frame-alist))
;; (setq split-height-threshold nil)

;; TAB键设置，在Emacs里不使用TAB键，所有的TAB默认为4个空格
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; yes或no提示设置，通过下面这个函数设置当缓冲区名字匹配到预设的字符串时自动回答yes
(setq original-y-or-n-p 'y-or-n-p)
(defalias 'original-y-or-n-p (symbol-function 'y-or-n-p))
(defun default-yes-sometimes (prompt)
  "automatically say y when buffer name match following string"
  (if (or
       (string-match "has a running process" prompt)
       (string-match "does not exist; create" prompt)
       (string-match "modified; kill anyway" prompt)
       (string-match "Delete buffer using" prompt)
       (string-match "Kill buffer of" prompt)
       ;; (string-match "still connected.  Kill it?" prompt)
       ;; (string-match "Shutdown the client's kernel" prompt)
       ;; (string-match "kill them and exit anyway" prompt)
       ;; (string-match "Revert buffer from file" prompt)
       ;; (string-match "Kill Dired buffer of" prompt)
       ;; (string-match "delete buffer using" prompt)
       ;; (string-match "Kill all pass entry" prompt)
       ;; (string-match "for all cursors" prompt)
       ;; (string-match "Do you want edit the entry" prompt)
       )
      t
    (original-y-or-n-p prompt)))
(defalias 'yes-or-no-p 'default-yes-sometimes)
(defalias 'y-or-n-p 'default-yes-sometimes)

;; 设置剪贴板历史长度300，默认为60
(setq kill-ring-max 200)

;; 在剪贴板里不存储重复内容
(setq kill-do-not-save-duplicates t)

;; 设置位置记录长度为6，默认为16
;; 可以使用 `counsel-mark-ring' or `consult-mark' (C-x j) 来访问光标位置记录
;; 使用 C-x C-SPC 执行 `pop-global-mark' 直接跳转到上一个全局位置处
;; 使用 C-u C-SPC 跳转到本地位置处
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

;; 设置 emacs-lisp 的限制
(setq max-lisp-eval-depth 10000)        ; 默认值为 800

;; 启用 `list-timers', `list-threads' 这两个命令
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)

;; 在命令行里支持鼠标
(xterm-mouse-mode 1)

;; 退出Emacs时进行确认
(setq confirm-kill-emacs 'y-or-n-p)

;;; Line numbers
;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)
;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)
;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 10000)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

;; 配置所有的编码为UTF-8，参考：
;; https://thraxys.wordpress.com/2016/01/13/utf-8-in-emacs-everywhere-forever/
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; (use-package highlight-parentheses
;;   :ensure t
;;   :hook((prog-mode . highlight-parentheses-mode)))

(setq-default auto-fill-function nil)
(setq-default visual-line-mode nil)

(defun my-header-line-vc ()
  (if (and (boundp 'vc-mode) vc-mode)
      (concat " #" (substring vc-mode 5)) ;; 直接截取分支名
    ""))
;; --- 2. 状态块颜色逻辑 (保持 Evil 用户的直观感) ---
(defun my-header-line-evil-face ()
  (cond ((evil-normal-state-p) '(:background "#d5c4a1" :foreground "#3c3836" ))
        ((evil-insert-state-p) '(:background "#b8bb26" :foreground "#fbf1c7" ))
        ((evil-visual-state-p) '(:background "#d3869b" :foreground "#fbf1c7" ))
        (t '(:background "#ebdbb2" :foreground "#3c3836"))))

(defun my-header-line-render ()
  (let* ((buffer-name (propertize (format-mode-line " %b ") 'face '(:weight bold)))
         (git-info (propertize (my-header-line-vc) 'face '(:foreground "#ecbe7b")))
         (im-info (if current-input-method
                            (propertize (concat " " current-input-method-title) 'face '(:foreground "#8ec07c"))
                            ""))
         (evil-tag (propertize (concat " " (upcase (symbol-name evil-state)) " ")
                               'face (my-header-line-evil-face))))
    (concat
     evil-tag      ; Evil 模式块
     buffer-name   ; 文件名
     git-info      ; Git 分支
     im-info
     ;; 将行列号推到最右侧
     (propertize " " 'display `(space :align-to (- right 8)))
     (format-mode-line "%l:%c"))))

;; --- 4. 设置与隐藏 ---
(setq-default header-line-format '((:eval (my-header-line-render))))
(setq-default mode-line-format nil)

;; 只有在这些动作发生时才更新，而不是每次移动光标都更新
(dolist (hook '(evil-normal-state-entry-hook
                evil-insert-state-entry-hook
                evil-visual-state-entry-hook
                after-save-hook      ;; 保存后更新
                window-configuration-change-hook)) ;; 切换窗口后更新
  (add-hook hook #'force-mode-line-update))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                                       "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                                       "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                                       "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                                       "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                                       "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                                       "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                                       "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                                       "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                                       "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                                       "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                                       ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                                       "<:<" ";;;" ))
  (global-ligature-mode t))

(setq-default cursor-in-non-selected-windows nil)

(use-package tab-bar
  :hook (window-setup . tab-bar-mode)
  :custom
  (tab-bar-tab-hints nil)
  :config
  (setq tab-bar-separator ""
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil
        tab-bar-show nil
        tab-bar-format nil
        tab-bar-close-button-show nil)

  ;; ;; 使用 super-1 super-2 ... 来切换 tab
  ;; (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
  ;; (setq tab-bar-tab-name-function
  ;;       (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
  ;;                         (count (length (window-list-1 nil 'nomini)))
  ;;                         (truncated-tab-name (if (< (length raw-tab-name)
  ;;                                                    tab-bar-tab-name-truncated-max)
  ;;                                                 raw-tab-name
  ;;                                               (truncate-string-to-width raw-tab-name
  ;;                                                                         tab-bar-tab-name-truncated-max
  ;;                                                                         nil nil tab-bar-tab-name-ellipsis))))
  ;;                    truncated-tab-name)))

  ;; ;; 给 tab 两边加上空格，更好看
  ;; (setq tab-bar-tab-name-format-function
  ;;       (lambda (tab i)
  ;;         (let ((face (funcall tab-bar-tab-face-function tab)))
  ;;           (concat
  ;;            (propertize "" 'face face)
  ;;            ;; (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t));;去除前面的1，2,3
  ;;            (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  ;; (setq tab-bar-format '(tab-bar-format-tabs))
  (tab-bar--update-tab-bar-lines)

  ;; WORKAROUND: update tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))
  )

;; (use-package ultra-scroll
;;   :vc (:url "https://github.com/jdtsmith/ultra-scroll")
;;   :hook (after-init . ultra-scroll-mode)
;;   :init
;;   (setq scroll-conservatively 101 ; important!
;;         scroll-margin 0)
;;   )

(use-package hl-line
  :ensure nil
  :hook(after-init . global-hl-line-mode)
  :init
  (defvar global-hl-line-modes
    '(prog-mode text-mode conf-mode special-mode
                org-agenda-mode dired-mode)
    "Major modes in which `hl-line-mode' should be active.")
  :config
  ;; 重定义 global-hl-line-mode，实现白名单过滤
(define-globalized-minor-mode global-hl-line-mode hl-line-mode
    (lambda ()
      (and (cond (hl-line-mode nil)
                 ((null global-hl-line-modes) nil)
                 ((eq global-hl-line-modes t))
                 ((eq (car global-hl-line-modes) 'not)
                  (not (derived-mode-p global-hl-line-modes)))
                 ((apply #'derived-mode-p global-hl-line-modes)))
           (hl-line-mode +1))))
  ;; Temporarily disable `hl-line' when selection is active, since it doesn't
  ;; serve much purpose when the selection is so much more visible.
  ;; ===== Evil Visual 时自动关闭/恢复 =====
  (defvar-local my--hl-line-was-on nil
    "Buffer-local flag to remember if hl-line was on before visual state.")

  (defun my-disable-hl-line-in-visual ()
    "Disable hl-line when entering Evil Visual state."
    (when hl-line-mode
      (setq my--hl-line-was-on t)
      (hl-line-mode -1)))

  (defun my-restore-hl-line-after-visual ()
    "Restore hl-line after exiting Evil Visual state."
    (when my--hl-line-was-on
      (setq my--hl-line-was-on nil)
      (hl-line-mode 1)))

 ;; 如果装了 evil，就挂这两个钩子
  (with-eval-after-load 'evil
    (add-hook 'evil-visual-state-entry-hook #'my-disable-hl-line-in-visual)
    (add-hook 'evil-visual-state-exit-hook  #'my-restore-hl-line-after-visual)))

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
