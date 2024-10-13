;;; init-engine-mode.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: engine-mode

;;; Code:
(use-package engine-mode
  :ensure t
  :config
    (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    )
  (engine-mode t))


(provide 'init-engine-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-engine-mode.el ends here
