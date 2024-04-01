;;; init-awesome-tray.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: awesome-tray

;;; Code:
(use-package awesome-tray
  :load-path "~/.config/emacs/site-lisp/awesome-tray"
  :config
  (setq awesome-tray-evil-show-mode t)
  (setq awesome-tray-evil-show-macro t)
  (awesome-tray-mode 1)
)

(provide 'init-awesome-tray)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tray.el ends here
