;;; init-js2.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: js2

;;; Code:
;;; coade
(use-package js2-mode
 :ensure t
 :config
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
)

(provide 'init-js2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-js2.el ends here
