;;; init-auto-save.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: auto-save

;;; Code:

(use-package auto-save
  :load-path "./site-lisp/auto-save"
  :config
(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
)


(provide 'init-auto-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-auto-save.el ends here
