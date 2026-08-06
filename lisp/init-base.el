;;; init-base.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 杨宇辰
;;
;; Author: 杨宇辰 <apple115@apple115.local>
;; Maintainer: 杨宇辰 <apple115@apple115.local>
;; Created: August 03, 2025
;; Modified: August 03, 2025
;; Version: 0.0.1
;; Homepage: https://github.com/apple115/init-base
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;

;; 平台判断
(defconst +is-win-p (eq system-type 'windows-nt) "Running on Windows.")
(defconst +is-mac-p (eq system-type 'darwin) "Running on macOS.")
(defconst +is-linux-p (eq system-type 'gnu/linux) "Running on Linux.")
(defconst +is-wsl-p
  (and +is-linux-p
       (file-exists-p "/proc/version")
       (with-temp-buffer
         (insert-file-contents-literally "/proc/version")
         (re-search-forward "microsoft\\|WSL" nil t)))
  "Running on Windows Subsystem for Linux.")

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup))

(use-package transient
  :ensure t
  :demand t)

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

;; WSL 中设置剪贴板编码为 gbk-dos
(when +is-wsl-p
  (set-clipboard-coding-system 'gbk-dos))

;; ---- TUI (emacs -nw) 下系统剪贴板桥接 ----
(require 'cl-lib)
;; 只用 Linux 原生工具，按 PATH 可用性选择：wl-clipboard (WSLg 下直接桥接
;; Windows 剪贴板) → xclip → xsel。全部不可用时优雅降级（paste 返回 nil）。
;;
;; 为什么不用 Windows exe (powershell/clip/win32yank)：WSL 里 emacs 同步
;; call-process 调 .exe 在 interop/binfmt 异常时会卡死 Emacs（实测），
;; 且 /bin/sh 无法 exec .exe；wl-clipboard 无此风险。
;; 仅在 TUI 生效；GUI 帧下函数内 guard 自动跳过，不影响原生剪贴板。
(defconst my-tui-clipboard-readers
  '(("wl-paste" . nil)
    ("xclip" . ("-selection" "clipboard" "-o"))
    ("xsel" . ("--clipboard" "--output")))
  "读取剪贴板的命令及参数，按优先级排列。")

(defconst my-tui-clipboard-writers
  '(("wl-copy" . nil)
    ("xclip" . ("-selection" "clipboard"))
    ("xsel" . ("--clipboard" "--input")))
  "写入剪贴板的命令及参数，按优先级排列。")

(defun my-tui-clipboard--find (table)
  "在 TABLE 中找到 PATH 里第一个可用的命令，返回 (CMD . ARGS)。"
  (cl-some (lambda (entry)
             (let ((path (executable-find (car entry))))
               (when path (cons path (cdr entry)))))
           table))

(defun my-tui-clipboard-copy (text &optional _push)
  "把 TEXT 写入系统剪贴板（仅 TUI）。"
  (when (and text (not (display-graphic-p)))
    (let ((entry (my-tui-clipboard--find my-tui-clipboard-writers)))
      (when entry
        (with-temp-buffer
          (insert text)
          (apply #'call-process-region (point-min) (point-max) (car entry)
                 nil nil nil (cdr entry)))))))

(defun my-tui-clipboard-paste ()
  "从系统剪贴板读取文本（仅 TUI）；无可用后端时返回 nil。"
  (when (not (display-graphic-p))
    (let ((entry (my-tui-clipboard--find my-tui-clipboard-readers)))
      (when entry
        (with-temp-buffer
          (apply #'call-process (car entry) nil t nil (cdr entry))
          (let ((s (buffer-string)))
            (when (string-match-p "[^[:space:]]" s)
              (string-trim-right s "[\r\n]+"))))))))

(when (and +is-wsl-p (not (display-graphic-p)))
  (setq interprogram-cut-function #'my-tui-clipboard-copy)
  (setq interprogram-paste-function #'my-tui-clipboard-paste))

(provide 'init-base)
;;; init-base.el ends here
