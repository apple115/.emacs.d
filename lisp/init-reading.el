;;; init-reading.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-read.el
;;; Code:

;; ---- merged from init-read.el ----
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

(provide 'init-reading)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reading.el ends here
