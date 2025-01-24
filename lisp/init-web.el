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

(use-package restclient
  :ensure t
)

(use-package ob-restclient
  :ensure t
)

(provide 'init-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
