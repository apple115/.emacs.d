;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary: ibuffer

;;; Code:
(use-package ibuffer
  :ensure nil
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(use-package nerd-icons-ibuffer
  :ensure
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))


(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
