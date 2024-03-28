;;; init-eaf.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: eaf

;;; Code:
;;; coade

(use-package eaf
  :load-path "~/.config/emacs/site-lisp/emacs-application-framework"
  :config
  (require 'eaf-browser)                ; 启用浏览器
  (require 'eaf-file-manager)
  )


(provide 'init-eaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
