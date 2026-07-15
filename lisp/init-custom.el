;;; init-custom.el --- custom settings. -*- lexical-binding: t; -*-
;;; Commentary:
;;; my personal custom settings
;;; Code:

(defcustom my-org-directory (expand-file-name (if +is-win-p "~/org" "~/Documents/LocalNotes/org"))
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

(defcustom my-lsp-provider 'lsp-bridge
  "LSP backend: `lsp-bridge' or `lsp-proxy'."
  :group 'my
  :type '(choice (const lsp-bridge) (const lsp-proxy)))

;; 当前试用 lsp-proxy；想切回 lsp-bridge 注释掉下面这行即可
(setq my-lsp-provider 'lsp-proxy)

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
