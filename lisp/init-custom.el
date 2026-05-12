;;; init-custom.el --- custom settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; my personal custom settings
;;; Code:

(defcustom my-org-directory (expand-file-name "~/Documents/LocalNotes/org")
  "Set org directory."
  :group 'my
  :type 'string)

(defcustom my-proxy "127.0.0.1:7897"
  "Set HTTP/HTTPS proxy."
  :group 'my
  :type 'string)

(defcustom my-socks-proxy "127.0.0.1:7897"
  "Set SOCKS proxy."
  :group 'my
  :type 'string)

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
