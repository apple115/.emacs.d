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

(provide 'init-base)
;;; init-base.el ends here
