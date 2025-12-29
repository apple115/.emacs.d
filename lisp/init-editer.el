;;; init-editer.el --- editor settings -*- lexical-binding: t -*-
;;; Commentary:
;;; editor

;;; Code:
(use-package sort-tab
  :load-path "./site-lisp/my-fork-sort-tab/"
  :custom
  (sort-tab-separaor "")
  (sort-tab-name-max-length 20)
  :config
  ;; (add-hook 'tab-bar-tab-post-open-functions (lambda (tab) (sort-tab-turn-on)) t)
  (setq sort-tab-hide-function '(lambda (buf) (with-current-buffer buf (derived-mode-p 'dired-mode))))
  (setq sort-tab-show-index-number t)
)

(use-package dired
  :after evil
  :config
  ;; (setq dired-listing-switches
  ;;       "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  ;; (put 'dired-find-alternate-file 'disabled nil)
  (evil-define-key 'normal dired-mode-map
    (kbd "q") 'quit-window
    (kbd "o") 'dired-quick-access
    (kbd "c") 'dired-do-compress-to
    (kbd "h") 'dired-up-directory
    (kbd "j") 'dired-next-line
    (kbd "k") 'dired-previous-line
    (kbd "l") 'dired-find-file
    (kbd "gr") 'revert-buffer
    (kbd "gy") 'dired-show-file-type
    (kbd "m") 'dired-mark
    (kbd "u") 'dired-unmark
    (kbd "x") 'dired-do-flagged-delete
    (kbd "RET") 'dired-find-file

    (kbd "A") 'dired-do-find-regexp
    (kbd "B") 'dired-do-byte-compile
    (kbd "C") 'dired-do-copy
    (kbd "D") 'dired-do-delete
    (kbd "H") 'dired-do-hardlink
    (kbd "G") 'dired-do-chgrp
    (kbd "M") 'dired-do-chmod
    (kbd "O") 'dired-do-chown
    (kbd "R") 'dired-do-rename
    (kbd "S") 'dired-do-symlink
    (kbd "T") 'dired-do-touch
    (kbd "Y") 'dired-copy-filename-as-kill
    (kbd "Z") 'dired-do-compress
    (kbd "!") 'dired-do-shell-command
    (kbd "&") 'dired-do-async-shell-command
    (kbd "+") 'dired-create-directory
))

(use-package nerd-icons-dired
    :ensure t
    :hook(dired-mode . nerd-icons-dired-mode))

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2023-04-05 2023-06-26"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((eq system-type 'windows-nt)
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (message "%s" x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
           xfileList)
          ;; (switch-to-buffer-other-window xoutBuf)
          )
        ;; old code. calling shell. also have a bug if filename contain apostrophe
        ;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
        )
       ((eq system-type 'darwin)
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((eq system-type 'berkeley-unix)
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))


(defun my-paste-to-dired ()
"使用wl-paste 命令复制在当前文件夹中."
(interactive)
(let ((past-file-name (read-file-name "Enter file name:") ))
  (async-shell-command (format "wl-paste > %s" past-file-name) )
))

(use-package auto-save
  :load-path "./site-lisp/auto-save"
  :config
    (auto-save-enable)
    (setq auto-save-silent t)
    (setq auto-save-delete-trailing-whitespace t)
)

(use-package ibuffer
  :ensure nil
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
    (use-package nerd-icons-ibuffer
    :ensure
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

    (use-package ibuffer-project
    :ensure t
    :config
    (add-hook
    'ibuffer-hook
    (lambda ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
        (ibuffer-do-sort-by-project-file-relative))))
    )
)

(use-package nerd-icons
  :ensure t)

;;view large file
(use-package vlf
 :ensure t
 :config
 (require 'vlf-setup))

(provide 'init-editer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-editer.el ends here
