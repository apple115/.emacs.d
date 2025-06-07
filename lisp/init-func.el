;;; init-func.el --- function settings -*- lexical-binding: t -*-
;;; Commentary:
;;; my personal functions


;;; Code:
(defun my-load-config ()
  "Load Emacs configuration."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun add-list-to-list (dst src)
  "Add elements of SRC to list stored in DST."
  (set dst
       (append (eval dst) src)))

(defun my-open-termial-kitty ()
  "Open kitty terminal in load file path."
  (interactive)
  (let ((directory (eshell/pwd)))
    (async-shell-command (format "kitty --directory %s" directory))
    ))

;;;###autoload
(defun +consult-fd-other-window (&optional dir initial)
  "Search for files with `fd' in DIR.
The file names must match the input regexp.  INITIAL is the
initial minibuffer input.  See `consult-grep' for details
regarding the asynchronous search and the arguments."
  (interactive "P")
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
               (default-directory dir)
               (builder (consult--fd-make-builder paths)))
  (find-file-other-window  (consult--find prompt builder initial))))

(defun my-vterm-apple115-switch ()
  "Create a new vterm buffer with the fixed name `apple115`."
  (interactive)
  (let ((buffer (get-buffer "terminal")))
    (if (not buffer) ; 检查是否已经存在名为 'apple115' 的缓冲区
        (vterm "terminal")) ; 如果不存在，创建一个名为 'apple115' 的 vterm 缓冲区
    (switch-to-buffer "terminal"))) ; 切换到 'apple115' 缓冲区

(defun +new-vtermN ()
  "Create a new vterm buffer with a name in the form of `termN`', where N is a number."
  (interactive)
  (let ((counter 1)
        (vterm-prefix "term"))
    ;; Find the next available number to use for the vterm buffer name
    (while (get-buffer (concat vterm-prefix (number-to-string counter)))
      (setq counter (1+ counter)))
    ;; Create the vterm buffer with the unique name
    (let ((vterm-name (concat vterm-prefix (number-to-string counter))))
    (vterm vterm-name)
    ;; Switch to the new buffer
    (switch-to-buffer vterm-name))))


(defun get-word-translate-to-bar()
  "Translate current word to bar."
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
  (set-visited-file-name newname)
  (rename-buffer newname))

(defun +delete-current-file (file)
  "Delete current visiting FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (when (y-or-n-p (format "Really delete '%s'? " file))
    (kill-this-buffer)
    (delete-file file)))

(defun delete-this-file()
  "Delete current file and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
    (when (yes-or-no-p (format "Really delete '%s'? " (file-name-nondirectory buffer-file-name)))
    (delete-file(buffer-file-name))
    (kill-this-buffer)))

(defun +copy-current-filename (file)
  "Copy the full path to the current FILE."
  (interactive
   (list (or buffer-file-name
             (user-error "No file is visiting"))))
  (kill-new file)
  (message "Copying '%s' to clipboard" file))


(defun +format-code-and-flycheck()
  "Format code file."
  (interactive)
  (ignore-errors
  ;; (flycheck-buffer)
  (format-all-buffer)))

(defun +hexo-new()
  "New a hexo blog."
  (interactive)
  (let ((default-directory "~/blog/"))
   (hexo-new)))


;; (use-package tailwindcss-color-mode
;;  :load-path "./site-lisp/tailwindcss-color-mode"
;; )

;; (use-package mini-bar
;;  :load-path "./site-lisp/mini-tab.el"
;; )

(provide 'init-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
