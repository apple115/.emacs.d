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

;; ghostel 函数
(defun my-ghostel-apple115-switch ()
  "Create a new ghostel buffer with the fixed name `apple115`."
  (interactive)
  (require 'ghostel)
  (ghostel--load-module t)
  (let ((buffer (get-buffer-create "terminal")))
    (unless (with-current-buffer buffer (derived-mode-p 'ghostel-mode))
      (ghostel--prepare-buffer buffer "terminal")
      (ghostel--init-buffer buffer "terminal"))
    (switch-to-buffer buffer)))

(defun +new-ghostelN ()
  "Create a new ghostel terminal with numbered name: term1, term2, ..."
  (interactive)
  (require 'ghostel)
  (ghostel--load-module t)
  (let ((counter 1)
        (prefix "term"))
    (while (get-buffer (concat prefix (number-to-string counter)))
      (setq counter (1+ counter)))
    (let* ((name (concat prefix (number-to-string counter)))
           (buffer (get-buffer-create name)))
      (unless (with-current-buffer buffer (derived-mode-p 'ghostel-mode))
        (ghostel--prepare-buffer buffer name)
        (ghostel--init-buffer buffer name))
      (switch-to-buffer buffer))))

;; eshell 函数（自动使用 eat，因为启用了 eat-eshell-mode）
;;(defun +new-eshell ()
;;  "Create a new eshell buffer with numbered name."
;;  (interactive)
;;  (let ((counter 1)
;;        (eshell-buffer-name "*eshell*"))
;;    ;; 查找可用的缓冲区名称
;;    (while (get-buffer (if (= counter 1)
;;                           eshell-buffer-name
;;                         (format "*eshell<%d>*" counter)))
;;      (setq counter (1+ counter)))
;;    (let ((buffer-name (if (= counter 1)
;;                            eshell-buffer-name
;;                          (format "*eshell<%d>*" counter))))
;;      ;; 创建新的 eshell 缓冲区
;;      (if (= counter 1)
;;          (eshell)
;;        (progn
;;          (eshell)
;;          (rename-buffer buffer-name)))
;;      (switch-to-buffer buffer-name))))
;;
;;(defun +eshell-toggle ()
;;  "Toggle eshell window."
;;  (interactive)
;;  (let ((buffer (get-buffer "*eshell*")))
;;    (if buffer
;;        (if (get-buffer-window buffer)
;;            (delete-window (get-buffer-window buffer))
;;          (display-buffer buffer))
;;      (eshell))))


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

;;https://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard 来源
(defun org-copy-region-as-markdown ()
  "Copy the region (in Org) to the system clipboard as Markdown."
  (interactive)
  (if (use-region-p)
      (let* ((region
	      (buffer-substring-no-properties
		      (region-beginning)
		      (region-end)))
	     (markdown
	      (org-export-string-as region 'md t '(:with-toc nil))))
	(gui-set-selection 'CLIPBOARD markdown))))

;;http://yummymelon.com/devnull/import-markdown-to-org-with-the-clipboard-in-emacs.html
(defun cc/yank-markdown-as-org ()
  "Yank Markdown text as Org.

This command will convert Markdown text in the top of the `kill-ring'
and convert it to Org using the pandoc utility."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (yank)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f markdown -t org --wrap=preserve" t t)
      (kill-region (point-min) (point-max)))
    (yank)))



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

(defun org-capture-inbox ()
     (interactive)
     (call-interactively 'org-store-link)
     (org-capture nil "i"))


(defun my/open-vterm-database (buffer-name  commond)
  "通用函数 打开一个专用的vterm 运行database"
  (let ((vterm-buffer (get-buffer buffer-name)))
    (if vterm-buffer
        (switch-to-buffer vterm-buffer)
      (progn
        (setq vterm-buffer (vterm buffer-name))
        (with-current-buffer vterm-buffer
          (vterm-send-string (concat commond "\n")))))))

(defun vterm-mysql ()
  "快速进入 MySQL (mycli)"
  (interactive)
  ;; 请在此处修改你的登录凭据，或者从环境变量中读取
  (my/open-vterm-database "*vterm-mysql*" "mycli -u root -p'你的密码' -h localhost"))

(defun vterm-pgcli ()
  "快速进入 PostgreSQL (pgcli)"
  (interactive)
  (my/open-vterm-database "*vterm-pg*" "pgcli postgres://user:password@localhost:5432/dbname"))

(defun vterm-iredis ()
  "快速进入 Redis (iredis)"
  (interactive)
  (my/open-vterm-database "*vterm-redis*" "iredis -h 127.0.0.1 -p 6379"))


(defun quick-open-fragment ()
  "快速进入 hexo 的 fragment"
  (interactive)
  (find-file "~/blog/source/fragment/index.md"))

;; Enable proxy
(enable-http-proxy)
(enable-socks-proxy)



(winner-mode +1)
(defun toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))
(global-set-key (kbd "C-x 1") #'toggle-delete-other-windows)

(provide 'init-func)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
