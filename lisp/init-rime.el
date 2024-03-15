
;;; init-rime.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: rime

;;; Code:
;;; coade
(use-package rime
 :ensure t
 :config
 (setq rime-user-data-dir "~/.local/share/fcitx5/rime")
 (setq default-input-method "rime"
      rime-show-candidate 'minibuffer)
)
(provide 'init-rime)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
