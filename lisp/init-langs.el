;;; init-langs.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-ts-mode
:ensure nil
:mode "\\.rs\\'"
)

(use-package go-ts-mode
:ensure nil
:mode "\\.go\\'"
:config
(setq go-ts-mode-indent-offset 4)
)

(use-package clojure-ts-mode
:ensure t
)

(use-package cider
 :ensure t
)

(use-package haskell-mode
:ensure t
)

(use-package python-mode
:ensure t
:mode ("\\.py\\'" . python-mode)
:config
 (setq python-indent-offset 4)
)

(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'"     . sh-mode)
         ("zshrc"        . sh-mode)
         ("zshenv"       . sh-mode)
         ("/PKGBUILD\\'" . sh-mode))
  :hook (sh-mode . sh-mode-setup)
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(provide 'init-langs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-langs.el ends here
