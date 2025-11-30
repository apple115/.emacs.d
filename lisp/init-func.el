;;; init-func.el --- function settings -*- lexical-binding: t -*-
;;; Commentary:
;;; my personal functions

(eval-when-compile
  (require 'init-custom))

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

;; vterm 函数 - 已替换为 eat
;; (defun my-vterm-apple115-switch ()
;;   "Create a new vterm buffer with the fixed name `apple115`."
;;   (interactive)
;;   (let ((buffer (get-buffer "terminal")))
;;     (if (not buffer)
;;         (vterm "terminal"))
;;     (switch-to-buffer "terminal")))

;; (defun +new-vtermN ()
;;   "Create a new vterm buffer with a name in the form of `termN`', where N is a number."
;;   (interactive)
;;   (let ((counter 1)
;;         (vterm-prefix "term"))
;;     (while (get-buffer (concat vterm-prefix (number-to-string counter)))
;;       (setq counter (1+ counter)))
;;     (let ((vterm-name (concat vterm-prefix (number-to-string counter))))
;;     (vterm vterm-name)
;;     (switch-to-buffer vterm-name))))

;; Eat 终端函数
(defun +new-eat ()
  "Create a new eat terminal session with numbered name."
  (interactive)
  (let ((counter 1))
    ;; 找到下一个可用的编号
    (while (get-buffer (format "*eat<%d>*" counter))
      (setq counter (1+ counter)))
    ;; 使用数字参数创建新的 eat 会话
    ;; eat 会自动创建名为 *eat<N>* 的 buffer
    (eat nil counter)))


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


;; copy https://github.com/seagle0128/.emacs.d/blob/c9ce9bc4d6bb8340dd23bfcc54a5cef299f5ef7d/lisp/init-funcs.el#L648
;; Network Proxy
(defun show-http-proxy ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" my-proxy)
    (message "No HTTP proxy")))

(defun enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,my-proxy)
          ("https" . ,my-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (show-http-proxy))

(defun disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-http-proxy))

(defun toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (disable-http-proxy)
    (enable-http-proxy)))

(defun show-socks-proxy ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun enable-socks-proxy ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string my-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" my-socks-proxy))
  (show-socks-proxy))

(defun disable-socks-proxy ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (show-socks-proxy))

(defun toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-server)
      (disable-socks-proxy)
    (enable-socks-proxy)))

(defun enable-proxy ()
  "Enbale proxy."
  (interactive)
  (enable-http-proxy)
  (enable-socks-proxy))

(defun disable-proxy ()
  "Disable proxy."
  (interactive)
  (disable-http-proxy)
  (disable-socks-proxy))

(defun toggle-proxy ()
  "Toggle proxy."
  (interactive)
  (toggle-http-proxy))

;; Enable proxy
(enable-http-proxy)
(enable-socks-proxy)

(provide 'init-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
