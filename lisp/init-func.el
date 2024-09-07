;;; init-func.el --- func settings -*- lexical-binding: t -*-
;;; Commentary: func


(defun my-load-config ()
  "Load Emacs configuration."
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun my-open-termial-kitty ()
  "Open kitty terminal in load filepath"
  (interactive)
  (let ((directory (eshell/pwd)))
    (async-shell-command (format "kitty --directory %s" directory))
    ))

(defun open-vterm-in-other-window ()
  "Open a vterm in a new window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (multi-vterm)
  )
(defun my-vterm-apple115-switch ()
  "Create a new vterm buffer with the fixed name 'apple115'."
  (interactive)
  (let ((buffer (get-buffer "*apple115*")))
    (if (not buffer) ; 检查是否已经存在名为 'apple115' 的缓冲区
        (vterm "*apple115*")) ; 如果不存在，创建一个名为 'apple115' 的 vterm 缓冲区
    (switch-to-buffer "*apple115*"))) ; 切换到 'apple115' 缓冲区


(defun get-word-translate-to-bar()
  "translate current word to bar"
  (interactive)
  (let ((w (thing-at-point 'word)))
    (when w (with-temp-buffer  (call-process-shell-command (format "busctl --user call org.zbus.MyGreeter /org/zbus/MyGreeter org.zbus.MyGreeter1 SendWord s %s > /dev/null 2>&1" (downcase w)) nil  "*Shell Command Output*" t)))))

(defun +rename-current-file (newname)
  "Rename current visiting file to NEWNAME.
If NEWNAME is a directory, move file to it."
  (interactive
   (progn
     (unless buffer-file-name
       (user-error "No file is visiting"))
     (let ((name (read-file-name "Rename to: " nil buffer-file-name 'confirm)))
       (when (equal (file-truename name)
                    (file-truename buffer-file-name))
         (user-error "Can't rename file to itself"))
       (list name))))
  ;; NEWNAME is a directory
  (when (equal newname (file-name-as-directory newname))
    (setq newname (concat newname (file-name-nondirectory buffer-file-name))))
  (rename-file buffer-file-name newname)
  (set-visited-file-name ewname)
  (rename-buffer newname))

(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

(defun +copy-current-filename (file)
  "Copy the full path to the current FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (kill-new file)
  (message "Copying '%s' to clipboard" file))


(provide 'init-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
