;;; init-theme.el --- OKLCH 自适应主题 (GUI 亮色 / TUI 跟随终端) -*- lexical-binding: t -*-
;;; Commentary:
;; 调色板以 OKLCH（感知均匀色彩空间）定义，加载时转换为 sRGB hex。
;; 色相/彩度继承原有 gruvbox 配色，观感不变，但可整体按 L/C/H 调参。
;;
;; 行为：
;; - GUI：亮色 gruvbox（保持原有外观），可用 `my-theme-selection' 强制亮/暗。
;; - TUI 真彩（24-bit）：沿用同色板 hex，但 `default' 背景不设置、透传终端
;;   背景；亮/暗按终端自动检测（Emacs OSC 11 查询 → COLORFGBG → 默认 dark）。
;; - TUI 非真彩（≤256 色，如 SSH/tmux）：改用 ANSI 槽位名，颜色跟随终端
;;   调色板，任何终端主题（gruvbox/catppuccin/solarized…）下都协调。
;;
;; 供其他模块按语义取色：`(my-theme-color 'KEY)'，真彩返回 hex，否则返回
;; ANSI 槽位名（如 header-line 的 Evil 状态块、magit status 颜色）。
;;; Code:

;; ---------- OKLCH → sRGB 转换 ----------

(defun my-theme--srgb-gamma (c)
  "对 C (0..1) 应用 sRGB 传递函数并夹取。"
  (let ((c (max 0.0 (min 1.0 c))))
    (if (<= c 0.0031308)
        (* 12.92 c)
      (- (* 1.055 (expt c (/ 1.0 2.4))) 0.055))))

(defun my-theme--oklab-to-srgb (L a b)
  "OKLab (L a b) → 线性 sRGB，取值 0..1。"
  (let* ((l_ (+ L (* 0.3963377774 a) (* 0.2158037573 b)))
         (m_ (- (- L (* 0.1055613458 a)) (* 0.0638541728 b)))
         (s_ (- (- L (* 0.0894841775 a)) (* 1.2914855480 b)))
         (l (expt l_ 3))
         (m (expt m_ 3))
         (s (expt s_ 3)))
    (list (+ (* 4.0767416621 l) (* -3.3077115913 m) (* 0.2309699292 s))
          (+ (* -1.2684380046 l) (* 2.6097574011 m) (* -0.3413193965 s))
          (+ (* -0.0041960863 l) (* -0.7034186147 m) (* 1.7076147010 s)))))

(defun my-theme--oklch-to-hex (L C H)
  "OKLCH (L C H) → \"#rrggbb\"。"
  (let* ((a (* C (cos (degrees-to-radians H))))
         (b (* C (sin (degrees-to-radians H))))
         (rgb (my-theme--oklab-to-srgb L a b)))
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (c) (round (* 255 (my-theme--srgb-gamma c)))) rgb))))

;; ---------- 调色板（OKLCH，注释为转换后的 hex） ----------

(defconst my-theme-palette-dark
  '((bg . (0.2768 0.0000 89.88))           ; #282828
    (bg-hard . (0.2408 0.0049 219.67))     ; #1d2021
    (bg-soft . (0.3109 0.0034 48.62))      ; #32302f
    (bg-more-soft . (0.3441 0.0066 48.52)) ; #3c3836
    (fg . (0.8941 0.0566 89.24))           ; #ebdbb2
    (dim . (0.6903 0.0346 76.31))          ; #a89984
    (string . (0.7652 0.1581 110.83))      ; #b8bb26
    (const . (0.7054 0.0976 2.19))         ; #d3869b
    (comment . (0.6927 0.0420 169.77))     ; #83a598
    (def . (0.7311 0.1820 51.69))          ; #fe8019
    (error . (0.6597 0.2175 30.39))        ; #fb4934
    (warning . (0.8325 0.1595 82.99))      ; #fabd2f
    (cyan . (0.7555 0.1078 137.68))        ; #8ec07c
    (search . (0.8325 0.1595 82.99))       ; #fabd2f
    (chip-normal-bg . (0.8941 0.0566 89.24))   ; #ebdbb2
    (chip-insert-bg . (0.7652 0.1581 110.83))  ; #b8bb26
    (chip-visual-bg . (0.7054 0.0976 2.19))    ; #d3869b
    (chip-fg . (0.2768 0.0000 89.88))))         ; #282828

(defconst my-theme-palette-light
  '((bg . (0.9555 0.0555 96.15))           ; #fbf1c7
    (bg-hard . (0.9655 0.0394 100.86))     ; #f9f5d7
    (bg-soft . (0.9220 0.0553 92.53))      ; #f2e5bc
    (bg-more-soft . (0.9504 0.0415 95.12)) ; #f7efd0
    (fg . (0.3441 0.0066 48.52))           ; #3c3836
    (dim . (0.6192 0.0286 67.26))          ; #928374
    (string . (0.5463 0.1124 106.46))      ; #79740e
    (const . (0.4893 0.1242 344.28))       ; #8f3f71
    (comment . (0.5756 0.0658 199.49))     ; #458588
    (def . (0.5126 0.1616 39.30))          ; #af3a03
    (error . (0.5458 0.2030 28.66))        ; #cc241d
    (warning . (0.7251 0.1429 77.71))      ; #d79921
    (cyan . (0.6450 0.0939 145.27))        ; #689d6a
    (search . (0.7251 0.1429 77.71))       ; #d79921
    (chip-normal-bg . (0.8941 0.0566 89.24))   ; #ebdbb2
    (chip-insert-bg . (0.7652 0.1581 110.83))  ; #b8bb26
    (chip-visual-bg . (0.7054 0.0976 2.19))    ; #d3869b
    (chip-fg . (0.3441 0.0066 48.52))))         ; #3c3836

;; ghostel / comint 的 ANSI 16 色（与亮暗无关，保持终端语义）
(defconst my-theme-ansi-hex
  '((black . "#282828") (red . "#cc241d") (green . "#98971a")
    (yellow . "#d79921") (blue . "#458588") (magenta . "#b16286")
    (cyan . "#689d6a") (white . "#a89984")
    (bright-black . "#928374") (bright-red . "#fb4934")
    (bright-green . "#b8bb26") (bright-yellow . "#fabd2f")
    (bright-blue . "#83a598") (bright-magenta . "#d3869b")
    (bright-cyan . "#8ec07c") (bright-white . "#ebdbb2")))

;; 非真彩终端：语义角色 → 终端调色板槽位（跟随终端主题）
(defconst my-theme-ansi-map
  '((bg . "black") (bg-hard . "black") (bg-soft . "brightblack")
    (bg-more-soft . "brightblack") (dim . "brightblack")
    (string . "green") (const . "magenta") (comment . "blue")
    (def . "yellow") (error . "red") (warning . "yellow")
    (cyan . "cyan") (search . "yellow")
    (chip-normal-bg . "brightblack") (chip-insert-bg . "green")
    (chip-visual-bg . "magenta") (chip-fg . "black")))

;; ---------- 检测与取色 ----------

(defcustom my-theme-selection 'auto
  "主题亮暗选择：`auto' 自动检测（GUI 保持亮色），或 `dark'/`light' 强制。"
  :type '(choice (const auto) (const dark) (const light))
  :group 'my)

(defvar my-theme--applied-mode nil
  "最近一次应用的模式，用于 OSC 11 异步应答后的重检。")

(defvar my-theme--hex-cache (make-hash-table :test 'equal))

(defun my-tty-24bit-p ()
  "当前终端是否支持 24-bit 真彩。"
  (or (ignore-errors (eq (tty-color-mode) -1))
      (member (getenv "COLORTERM") '("truecolor" "24bit"))))

(defun my-colorfgbg-mode ()
  "从 COLORFGBG 环境变量推断亮暗，返回 'dark/'light/nil。"
  (let ((v (getenv "COLORFGBG")))
    (when (and v (string-match ";\\([0-9]+\\)" v))
      (if (>= (string-to-number (match-string 1 v)) 8) 'light 'dark))))

(defun my-terminal-background-mode ()
  "检测终端亮暗：my-theme-selection → OSC 11 → COLORFGBG → 默认 dark。"
  (cond ((eq my-theme-selection 'dark) 'dark)
        ((eq my-theme-selection 'light) 'light)
        ((eq (terminal-parameter nil 'background-mode) 'dark) 'dark)
        ((my-colorfgbg-mode))
        (t 'dark)))

(defun my-theme-mode ()
  "当前显示环境应使用的亮暗模式。GUI 默认深色（dark）。"
  (if (display-graphic-p)
      (if (memq my-theme-selection '(dark light))
          my-theme-selection
        'dark)
    (my-terminal-background-mode)))

(defun my-theme--ansi-p ()
  "是否为非真彩 TUI（需用 ANSI 槽位色）。"
  (and (not (display-graphic-p))
       (not (my-tty-24bit-p))))

(defun my-theme--hex (key mode)
  "KEY 在 MODE 下的 hex 色（哈希缓存）。"
  (or (gethash (cons key mode) my-theme--hex-cache)
      (let* ((pal (if (eq mode 'dark)
                      my-theme-palette-dark
                    my-theme-palette-light))
             (hex (apply #'my-theme--oklch-to-hex (cdr (assq key pal)))))
        (puthash (cons key mode) hex my-theme--hex-cache)
        hex)))

(defun my-theme-color (key)
  "当前环境下 KEY 的颜色：真彩返回 hex，非真彩 TUI 返回 ANSI 槽位名。
供 init-ui（header-line）、init-git（magit status）等按语义取色。"
  (if (my-theme--ansi-p)
      (if (eq key 'fg)
          (if (eq (my-theme-mode) 'light) "black" "white")
        (or (cdr (assq key my-theme-ansi-map)) "white"))
    (my-theme--hex key (my-theme-mode))))

;; ---------- 应用 ----------

(defun my-theme--apply-hex (mode gui)
  "应用 OKLCH 色板。GUI 全 hex；TUI 真彩时 default/行号背景透传终端。"
  (let ((bg (my-theme--hex 'bg mode))
        (bg-hard (my-theme--hex 'bg-hard mode))
        (bg-soft (my-theme--hex 'bg-soft mode))
        (bg-more-soft (my-theme--hex 'bg-more-soft mode))
        (fg (my-theme--hex 'fg mode))
        (dim (my-theme--hex 'dim mode))
        (string (my-theme--hex 'string mode))
        (const (my-theme--hex 'const mode))
        (comment (my-theme--hex 'comment mode))
        (def (my-theme--hex 'def mode))
        (error (my-theme--hex 'error mode))
        (warning (my-theme--hex 'warning mode))
        (search (my-theme--hex 'search mode)))
    (when gui
      (set-background-color bg)
      (set-foreground-color fg)
      (set-cursor-color fg))
    (apply #'custom-set-faces
           (append
            (list
     ;; 基础
     `(default ((t (:background ,(and gui bg) :foreground ,fg))))
     `(region ((t (:background ,bg-soft))))
     `(mode-line ((t (:background ,bg-hard :foreground ,fg :box nil))))
     `(mode-line-inactive ((t (:background ,bg-soft :foreground ,dim :box nil))))
     ;; 语法高亮：Alabaster 风格 4 类，关键词走前景
     `(font-lock-string-face ((t (:foreground ,string))))
     `(font-lock-constant-face ((t (:foreground ,const))))
     `(font-lock-number-face ((t (:foreground ,const))))
     `(font-lock-comment-face ((t (:foreground ,comment))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
     `(font-lock-function-name-face ((t (:foreground ,def))))
     `(font-lock-variable-name-face ((t (:foreground ,def))))
     `(font-lock-keyword-face ((t (:foreground ,fg))))
     `(font-lock-type-face ((t (:foreground ,fg))))
     `(font-lock-builtin-face ((t (:foreground ,fg))))
     `(font-lock-operator-face ((t (:foreground ,fg))))
     `(error ((t (:foreground ,error))))
     `(warning ((t (:foreground ,warning))))
     ;; UI
     `(fringe ((t (:background ,(and gui bg)))))
     `(line-number ((t (:foreground ,dim :background ,(and gui bg)))))
     `(line-number-current-line ((t (:foreground ,fg :background ,(and gui bg-soft)))))
     `(isearch ((t (:background ,search :foreground ,bg))))
     `(lazy-highlight ((t (:background ,bg-soft :foreground ,fg))))
     `(show-paren-match ((t (:background ,bg-soft))))
     `(show-paren-mismatch ((t (:background ,error :foreground ,bg))))
     `(minibuffer-prompt ((t (:foreground ,const))))
     ;; Org
     `(org-level-1 ((t (:foreground ,fg))))
     `(org-level-2 ((t (:foreground ,fg))))
     `(org-level-3 ((t (:foreground ,fg))))
     `(org-link ((t (:foreground ,const :underline t))))
     `(org-code ((t (:foreground ,string))))
     `(org-verbatim ((t (:foreground ,const))))
     `(org-comment ((t (:foreground ,comment))))
     `(org-todo ((t (:foreground ,error))))
     `(org-done ((t (:foreground ,string))))
     `(org-block ((t (:background ,bg-hard :foreground ,fg))))
     `(hl-line ((t (:background ,bg-more-soft :extend t))))
     `(header-line ((t (:background ,(and gui bg-soft) :foreground ,fg
                                    :box nil :underline nil :inherit nil))))
     ;; consult / vertico
     `(consult-highlight-line ((t (:background ,bg-more-soft :extend t))))
     `(consult-preview-match ((t (:foreground ,comment :background nil))))
     `(completions-common-part ((t (:foreground ,const :weight normal))))
     `(completions-first-difference ((t (:foreground ,fg :weight normal))))
     `(vertico-current ((t (:background ,bg-soft :extend t))))
     ;; dired
     `(dired-header ((t (:background nil :inherit default))))
     `(dired-perm-write ((t (:background nil :foreground ,error))))
     `(dired-directory ((t (:background nil :foreground ,def :weight bold))))
     `(dired-symlink ((t (:background nil :foreground ,const))))
     `(dired-flagged ((t (:foreground ,error :weight bold))))
     `(dired-ignored ((t (:foreground ,dim))))
     `(dired-set-id ((t (:background nil :foreground ,warning :underline t))))
     `(dired-special ((t (:background nil :foreground ,const)))))
            ;; ghostel / comint ANSI
            (mapcar (lambda (k) `(,(intern (format "ghostel-color-%s" k))
                                  ((t (:foreground ,(cdr (assq k my-theme-ansi-hex)))))))
                    '(black red green yellow blue magenta cyan white))
            (mapcar (lambda (k) `(,(intern (format "ghostel-color-bright-%s" k))
                                  ((t (:foreground ,(cdr (assq k my-theme-ansi-hex)))))))
                    '(black red green yellow blue magenta cyan white))))))

(defun my-theme--apply-ansi (mode)
  "非真彩 TUI：用 ANSI 槽位名应用主题，颜色跟随终端调色板。"
  (let ((fg (if (eq mode 'light) "black" "white"))
        (dim "brightblack")
        (string "green") (const "magenta") (comment "blue")
        (def "yellow") (error "red") (warning "yellow")
        (search "yellow"))
    (custom-set-faces
     `(default ((t (:background nil :foreground ,fg))))
     `(region ((t (:inverse-video t))))
     `(mode-line ((t (:inverse-video t))))
     `(mode-line-inactive ((t (:inverse-video t :foreground ,dim))))
     `(font-lock-string-face ((t (:foreground ,string))))
     `(font-lock-constant-face ((t (:foreground ,const))))
     `(font-lock-number-face ((t (:foreground ,const))))
     `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground ,comment :slant italic))))
     `(font-lock-function-name-face ((t (:foreground ,def))))
     `(font-lock-variable-name-face ((t (:foreground ,def))))
     `(font-lock-keyword-face ((t (:foreground ,fg))))
     `(font-lock-type-face ((t (:foreground ,fg))))
     `(font-lock-builtin-face ((t (:foreground ,fg))))
     `(font-lock-operator-face ((t (:foreground ,fg))))
     `(error ((t (:foreground ,error))))
     `(warning ((t (:foreground ,warning))))
     `(line-number ((t (:foreground ,dim :background nil))))
     `(line-number-current-line ((t (:foreground ,fg :background nil))))
     `(isearch ((t (:background ,search :foreground "black"))))
     `(lazy-highlight ((t (:background ,dim :foreground ,fg))))
     `(show-paren-match ((t (:inverse-video t))))
     `(show-paren-mismatch ((t (:foreground ,error :inverse-video t))))
     `(minibuffer-prompt ((t (:foreground ,const))))
     `(org-level-1 ((t (:foreground ,fg))))
     `(org-level-2 ((t (:foreground ,fg))))
     `(org-level-3 ((t (:foreground ,fg))))
     `(org-link ((t (:foreground ,const :underline t))))
     `(org-code ((t (:foreground ,string))))
     `(org-verbatim ((t (:foreground ,const))))
     `(org-comment ((t (:foreground ,comment))))
     `(org-todo ((t (:foreground ,error))))
     `(org-done ((t (:foreground ,string))))
     `(org-block ((t (:foreground ,fg :background nil))))
     `(hl-line ((t (:background ,dim :extend t))))
     `(header-line ((t (:background nil :foreground ,fg))))
     `(consult-highlight-line ((t (:background ,dim :extend t))))
     `(consult-preview-match ((t (:foreground ,comment :background nil))))
     `(completions-common-part ((t (:foreground ,const :weight normal))))
     `(completions-first-difference ((t (:foreground ,fg :weight normal))))
     `(vertico-current ((t (:inverse-video t))))
     `(dired-header ((t (:background nil :inherit default))))
     `(dired-perm-write ((t (:background nil :foreground ,error))))
     `(dired-directory ((t (:background nil :foreground ,def :weight bold))))
     `(dired-symlink ((t (:background nil :foreground ,const))))
     `(dired-flagged ((t (:foreground ,error :weight bold))))
     `(dired-ignored ((t (:foreground ,dim))))
     `(dired-set-id ((t (:background nil :foreground ,warning :underline t))))
     `(dired-special ((t (:background nil :foreground ,const))))
     ;; ghostel ANSI 直通终端调色板
     `(ghostel-color-black ((t (:foreground "black"))))
     `(ghostel-color-red ((t (:foreground "red"))))
     `(ghostel-color-green ((t (:foreground "green"))))
     `(ghostel-color-yellow ((t (:foreground "yellow"))))
     `(ghostel-color-blue ((t (:foreground "blue"))))
     `(ghostel-color-magenta ((t (:foreground "magenta"))))
     `(ghostel-color-cyan ((t (:foreground "cyan"))))
     `(ghostel-color-white ((t (:foreground "white"))))
     `(ghostel-color-bright-black ((t (:foreground "brightblack"))))
     `(ghostel-color-bright-red ((t (:foreground "brightred"))))
     `(ghostel-color-bright-green ((t (:foreground "brightgreen"))))
     `(ghostel-color-bright-yellow ((t (:foreground "brightyellow"))))
     `(ghostel-color-bright-blue ((t (:foreground "brightblue"))))
     `(ghostel-color-bright-magenta ((t (:foreground "brightmagenta"))))
     `(ghostel-color-bright-cyan ((t (:foreground "brightcyan"))))
     `(ghostel-color-bright-white ((t (:foreground "brightwhite")))))))

(defun my-theme-apply ()
  "按当前显示环境应用主题（GUI / TUI 真彩 / TUI 调色板）。"
  (interactive)
  (if (display-graphic-p)
      (my-theme--apply-hex (my-theme-mode) t)
    (if (my-tty-24bit-p)
        (my-theme--apply-hex (my-theme-mode) nil)
      (my-theme--apply-ansi (my-theme-mode))))
  (setq my-theme--applied-mode (my-theme-mode)))

(defun my-theme-reload ()
  "重新应用主题。"
  (interactive)
  (my-theme-apply))

(defun my-theme-toggle ()
  "在亮色与暗色主题间切换（GUI 与 TUI 通用）。"
  (interactive)
  (setq my-theme-selection
        (if (eq (my-theme-mode) 'dark) 'light 'dark))
  (my-theme-apply)
  (message "Theme: %s" (if (eq (my-theme-mode) 'dark) "dark" "light")))

(defun my-theme-select (mode)
  "选择主题模式：`auto'（TUI 自动检测亮暗）/ `dark' / `light'。"
  (interactive
   (list (intern (completing-read "Theme mode: " '("auto" "dark" "light") nil t))))
  (setq my-theme-selection mode)
  (my-theme-apply)
  (message "Theme mode: %s" mode))

(defun my-theme--recheck-terminal ()
  "OSC 11 应答异步到达后，检测结果变化则重新应用。"
  (when (and (not (display-graphic-p))
             (not (eq (my-terminal-background-mode) my-theme--applied-mode)))
    (my-theme-apply)))

;; 应用主题（GUI 亮色 / TUI 自动检测）
(my-theme-apply)

;; TUI：等终端背景查询（OSC 11）应答后重检亮暗
(when (not (display-graphic-p))
  (run-with-idle-timer 1 nil #'my-theme--recheck-terminal))

;; 守护进程/后续新帧：按新帧类型重新应用（如 daemon 后开 GUI 帧）
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame (my-theme-apply))))

(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
