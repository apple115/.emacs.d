;;; init-ui.el --- UI settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (add-to-list 'default-frame-alist '(undecorated-round . t))
;; Initial frame
(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.7)
                            (height . 0.85)
                            (fullscreen)))


(use-package gruvbox-theme
 :ensure t
 :config
 (load-theme 'gruvbox-light-soft t)
)

(defun my-apply-font()
    (set-face-attribute 'default nil :font (font-spec :family "JetBrains Mono" :size 12 :weight 'medium))
    (set-fontset-font t '(#x2ff0 . #x9fff) (font-spec :family "LXGW WenKai" :size 12 :weight 'medium))
)

;; |大家|
;; |aabb|大家

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

 ;; 草稿缓冲区默认文字设置
 (setq initial-scratch-message (concat ";; Happy hacking, "
                                       (capitalize user-login-name) " - Emacs ♥ you!\n\n"))

 ;; 设置缓冲区的文字无
  (setq-default bidi-display-reordering nil)
   (setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)
 ;; 禁止使用双向括号算法
 ;; (setq bidi-inhibit-bpa t)

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


;; (defun display-line-numbers-equalize ()
;;   "Equalize The width"
;;   (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))

;; (add-hook 'find-file-hook 'display-line-numbers-equalize)

;; (setq column-number-mode t)
;; 显示行号 和 文本显示中的截断或省略
;; (global-display-line-numbers-mode t)
(global-visual-line-mode t)

;; 在这个prog-mode 和 text-mode 后添加line-number
(unless (boundp 'display-line-numbers-type)
  (defvar display-line-numbers-type nil))

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'text-mode-hook 'display-line-numbers-mode)

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

(setq-default auto-fill-function nil)
(setq-default visual-line-mode nil)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-modal t)
  (setq doom-modeline-height 25)
)

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
                              "<:<" ";;;"))
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
        tab-bar-close-button-show nil)

  ;; 使用 super-1 super-2 ... 来切换 tab
  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                truncated-tab-name)))

  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize "" 'face face)
             ;; (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t));;去除前面的1，2,3
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  (setq tab-bar-format '(tab-bar-format-tabs))

  (tab-bar--update-tab-bar-lines)

  ;; WORKAROUND: update tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))
)

(use-package ultra-scroll
:vc (:url "https://github.com/jdtsmith/ultra-scroll")
:hook (after-init . ultra-scroll-mode)
:init
 (setq scroll-conservatively 101 ; important!
        scroll-margin 0)
)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
