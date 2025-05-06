;;; init-web.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: web

;;; Code:
(use-package emmet-mode
  :ensure t
  :hook ((css-mode . emmet-mode)
         (html-mode. emmet-mode)
         )
  :init
(setq emmet-indent-after-insert nil)
(setq emmet-expand-jsx-className? t)
  :config
(add-to-list 'emmet-jsx-major-modes 'js-jsx-mode)
(add-to-list 'emmet-jsx-major-modes 'typescript-ts-mode)
(add-to-list 'emmet-jsx-major-modes 'js-ts-mode)
(add-to-list 'emmet-jsx-major-modes 'js-mode)
(add-to-list 'emmet-jsx-major-modes 'typescript-mode)
(add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
)
;; CSS
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :ensure t
  :init (setq scss-compile-at-save nil))

(use-package web-mode
  :ensure t
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package restclient
  :ensure t
)

(use-package ob-restclient
  :ensure t
)

(use-package add-node-modules-path
  :ensure t
  :hook((web-mode js-base-mode) . add-node-modules-path)
  :custom
  (add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))
)

(provide 'init-web-developer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
