;;; init-dashboard.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: dashboard

;;; Code:
;; use-package with package.el:
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
    (setq dashboard-banner-logo-title "Welcome to apple115 Emacs")
)

(provide 'init-dashboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
