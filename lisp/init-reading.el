;;; init-reading.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: reading

;;; Code:
(use-package nov
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
)



(provide 'init-reading)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reading.el ends here
