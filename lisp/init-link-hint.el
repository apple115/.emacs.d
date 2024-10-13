;;; init-link-hint.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: link-hint

;;; Code:
(use-package link-hint
  :ensure t
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
)

(provide 'init-link-hint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-link-hint.el ends here
