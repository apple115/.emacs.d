;;; init-read.el --- Write settings -*- lexical-binding: t -*-

;;; Commentary:

(use-package nov
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
)

(use-package pdf-tools
  :ensure t
)

(provide 'init-read)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-read.el ends here
