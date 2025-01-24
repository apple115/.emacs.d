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
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :bind(:map corfu-map
             ("TAB". nil))
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode))
  :config
  (use-package nerd-icons-corfu
    :ensure t
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )
  )

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

;; (use-package hotfuzz
;;   :load-path "./site-lisp/hotfuzz"
;;   :custom
;;   (completion-styles '(hotfuzz))
;; )
;; (require 'hotfuzz-module)

(use-package marginalia
 :ensure t
 :init
 (marginalia-mode)
)

(use-package eldoc-box
  :ensure t
  :config
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
)

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; (add-hook 'completion-at-point-functions #'cape-keyword)
  ;; (add-hook 'completion-at-point-functions #'cape-dict)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;; Sanitize the `pcomplete-completions-at-point' Capf.  The Capf has undesired
;; side effects on Emacs 28 and earlier.  These advices are not needed on Emacs
;; 29 and newer.
(when (< emacs-major-version 29)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
)

; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package eldoc
;;   :init
;;   (global-eldoc-mode))

(use-package eglot
  :ensure nil
  ;; :hook (prog-mode . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider))
  :config
)

(use-package eglot-booster
    :load-path "./site-lisp/eglot-booster"
	:after eglot
	:config	(eglot-booster-mode))

(use-package ht
  :ensure t
)
(use-package lsp-copilot
  :load-path "./site-lisp/lsp-copilot"
  :config
  (setq lsp-copilot-user-languages-config (expand-file-name (concat user-emacs-directory "languages.toml")))
)

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
