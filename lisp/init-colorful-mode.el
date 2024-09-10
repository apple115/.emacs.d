;;; init-colorful-mode.el --- colorful-mode settings -*- lexical-binding: t -*-
;;; Commentary: colorful-mode

;;; Code:
(use-package colorful-mode
 :ensure t
 ;; :hook (prog-mode text-mode)
 :config
;; In this example add emacs color names only for yaml-mode and derived.
(add-to-list 'colorful-extra-color-keyword-functions '(yaml-mode . colorful-add-color-names))
(add-to-list 'colorful-extra-color-keyword-functions '(js-jsx-mode . colorful-add-color-names))
)

(provide 'init-colorful-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-colorful-mode.el ends here
