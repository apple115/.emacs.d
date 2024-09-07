;;; init-dirvish.el --- config dired mode -*- lexical-binding: t -*-
;;; Commentary: dired
;;; Code:
(use-package dired
  :ensure nil
  :config
(setq dired-hide-details-hide-symlink-targets nil)

(general-evil-define-key 'normal dired-mode-map
  "c" 'dired-do-compress-to
  "q" 'quit-window
  "h" 'dired-up-directory
  "j" 'dired-next-line
  "k" 'dired-previous-line
  "l" 'dired-find-file
  "gr" 'revert-buffer
  "gy" 'dired-show-file-type
  "m" 'dired-mark
  "u" 'dired-unmark
  "x" 'dired-do-flagged-delete
  "RET" 'dired-find-file

  "A" 'dired-do-find-regexp
  "B" 'dired-do-byte-compile
  "C" 'dired-do-copy
  "D" 'dired-do-delete
  "H" 'dired-do-hardlink
  "G" 'dired-do-chgrp
  "M" 'dired-do-chmod
  "O" 'dired-do-chown
  "R" 'dired-do-rename
  "S" 'dired-do-symlink
  "T" 'dired-do-touch
  "Y" 'dired-copy-filename-as-kill
  "Z" 'dired-do-compress
  "!" 'dired-do-shell-command
  "&" 'dired-do-async-shell-command
  "+" 'dired-create-directory
  ;; "TAB" 'dirvish-toggle-subtree
 )
)

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
"使用wl-paste 命令复制在当前文件夹中"
(interactive)
(let ((past-file-name (read-file-name "Enter file name:") ))
  (async-shell-command (format "wl-paste > %s" past-file-name) )
)
)


;; (use-package dirvish
;;   :ensure t
;;   :hook (after-init . dirvish-override-dired-mode)
;;   :config
;;   (dirvish-peek-mode)
;;   (setq dirvish-hide-details t)
;;   (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
;;   (setq dirvish-attributes
;;         '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
;; (general-evil-define-key 'normal dired-mode-map
;;   "c" 'dired-do-compress-to
;;   "q" 'quit-window
;;   "h" 'dired-up-directory
;;   "j" 'dired-next-line
;;   "k" 'dired-previous-line
;;   "l" 'dired-find-file
;;   "gr" 'revert-buffer
;;   "gy" 'dired-show-file-type
;;   "m" 'dired-mark
;;   "u" 'dired-unmark
;;   "x" 'dired-do-flagged-delete
;;   "RET" 'dired-find-file

;;   "A" 'dired-do-find-regexp
;;   "B" 'dired-do-byte-compile
;;   "C" 'dired-do-copy
;;   "D" 'dired-do-delete
;;   "H" 'dired-do-hardlink
;;   "G" 'dired-do-chgrp
;;   "M" 'dired-do-chmod
;;   "O" 'dired-do-chown
;;   "R" 'dired-do-rename
;;   "S" 'dired-do-symlink
;;   "T" 'dired-do-touch
;;   "Y" 'dired-copy-filename-as-kill
;;   "Z" 'dired-do-compress
;;   "!" 'dired-do-shell-command
;;   "&" 'dired-do-async-shell-command
;;   "+" 'dired-create-directory
;;   "TAB" 'dirvish-toggle-subtree
;;  )
;; )


(use-package all-the-icons
  :ensure t)


(provide 'init-dired)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
