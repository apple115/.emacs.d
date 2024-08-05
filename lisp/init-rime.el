;;; init-rime.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: rime

;;; Code:
;;; coade
(use-package rime
 :ensure t
 :bind ("C-\\". toggle-input-method)
 :config
 (setq rime-user-data-dir "~/.local/share/fcitx5/rime")
 (setq default-input-method "rime"
      rime-show-candidate 'popup)
)
(provide 'init-rime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
