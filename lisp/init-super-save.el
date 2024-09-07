;;; init-super-save.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: super-save

;;; Code:

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)

;; add integration with ace-window
(add-to-list 'super-save-triggers 'ace-window)

;; save on find-file
(add-to-list 'super-save-hook-triggers 'find-file-hook)
  )


(provide 'init-super-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-super-save.el ends here
