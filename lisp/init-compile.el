;;; init-compile.el --- compile settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(setq-default compilation-scroll-output t)

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(use-package compile
 :ensure nil
 :config
(add-to-list 'compilation-error-regexp-alist-alist
             '(go-test "^\\s-*\\(.*\\.go\\):\\([0-9]+\\):" 1 2 nil 2))
(add-to-list 'compilation-error-regexp-alist 'go-test)
(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))
)

(use-package quickrun
  :ensure t
)


(provide 'init-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-compile.el ends here
