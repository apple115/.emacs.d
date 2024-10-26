;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :config
  ;;(setq yas-snippet-dies '("~/.config/emacs/snippets"))
  (yas-global-mode 1)
)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

;;; Code:
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (vertico-mode t)
)

;;; Code:
(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
)
; Support Pinyin
(use-package pinyinlib
  :ensure t
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package citre
 :ensure t
 :init
 (require 'citre-config)
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
 :ensure t
 :init
 (marginalia-mode)
)

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
)

; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package eglot
  :ensure nil
)

(use-package eglot-booster
    :load-path "./site-lisp/eglot-booster"
	:after eglot
	:config	(eglot-booster-mode))

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
