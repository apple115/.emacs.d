;;; init-eaf.el --- Langs settings -*- lexical-binding: t -*-

;;; Code:

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :config
  (require 'eaf-browser)
  ;; (require 'eaf-file-manager)
  (eaf-bind-key nil "SPC" eaf-browser-keybinding)
)


(provide 'init-eaf)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-eaf.el ends here
