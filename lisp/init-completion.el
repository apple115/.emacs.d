;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package yasnippet
  :ensure t
  :general
  (:keymaps 'override
   :states '(normal visual)
   :prefix  "SPC"
    "n y" '(yas-new-snippet :wk "new snippet")
  )
  :config
  (yas-global-mode 1)
)

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :general
  (:keymaps 'yas-minor-mode-map
   :states '(normal visual)
   :prefix  "SPC"
    "y" '(:ignore t :wk "auto-yasnippet")
    "y w"   '(aya-create)
    "y TAB" '(aya-expand)
    "y SPC" '(aya-expand-from-history)
    "y d"   '(aya-delete-from-history)
    "y c"   '(aya-clear-history)
    "y n"   '(aya-next-in-history)
    "y p"   '(aya-previous-in-history)
    "y s"   '(aya-persist-snippet)
    "y o"   '(aya-open-line)
  )
)

;; (use-package corfu
;;   :ensure t
;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-preview-current nil)
;;   (corfu-auto-delay 0.2)
;;   (corfu-popupinfo-delay '(0.4 . 0.2))
;;   :bind(:map corfu-map ("TAB". nil))
;;   :hook ((after-init . global-corfu-mode)
;;          (global-corfu-mode . corfu-popupinfo-mode))
;;   :config
;;   (use-package nerd-icons-corfu
;;     :ensure t
;;     :config
;;     (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;;     )
;;   )

;;; Code:
(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (vertico-mode t)
)

(use-package embark
  :ensure t
  :bind
  ("C-." . embark-act)         ;; pick some comfortable binding
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
  :config
 (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
 :ensure t
 :init
 (marginalia-mode)
)

;; (use-package eldoc
;;   :ensure nil
;;   )

;; (use-package eldoc-box
;;   :ensure t
;;   :hook (prog-mode . eldoc-box-hover-mode)
;;   :config
;; )

;; (use-package cape
;;   :ensure t
;;   :init
;;   (add-hook 'completion-at-point-functions #'cape-abbrev)
;;   (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;   (add-hook 'completion-at-point-functions #'cape-file)
;;   (add-hook 'completion-at-point-functions #'cape-keyword)
;;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;; ;; Sanitize the `pcomplete-completions-at-point' Capf.  The Capf has undesired
;; ;; side effects on Emacs 28 and earlier.  These advices are not needed on Emacs
;; ;; 29 and newer.
;; (when (< emacs-major-version 29)
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
;; )

; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (use-package eglot
;;   :ensure nil
;;   ;; :hook (prog-mode . eglot-ensure)
;;   :general
;;   (:keymaps 'override
;;    :states '(normal visual)
;;    :prefix  "SPC"
;;    "t l" '(eglot :wk "open eglot")
;;    "l r" '(eglot-rename :wk "rename")
;;    "l a" '(eglot-code-actions :wk "code action")
;;   )
;;   :custom
;;   (eglot-ignored-server-capabilities
;;    '(:inlayHintProvider
;;      :hoverProvider
;;      :documentHighlightProvider
;;      :documentFormattingProvider
;;      :documentRangeFormattingProvider
;;      :documentOnTypeFormattingProvider
;;      :colorProvider
;;      :foldingRangeProvider))
;;   :config
;;   (setq eglot-events-buffer-size 0)
;;   (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
;; )

;; (use-package eglot-booster
;;     :load-path "./site-lisp/eglot-booster"
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

;; (use-package ht
;;   :ensure t
;; )

;; (use-package lsp-proxy
;;   :load-path "./site-lisp/lsp-proxy"
;;   :config
;;   (setq lsp-proxy-user-languages-config (expand-file-name (concat user-emacs-directory "languages.toml")))
;;     (with-eval-after-load 'evil-collection
;;     (evil-collection-define-key 'normal 'lsp-proxy-mode-map
;;       (kbd "K")  'lsp-proxy-describe-thing-at-point
;;       ;; (kbd "gd") 'lsp-proxy-find-definition
;;       (kbd "gD") 'lsp-proxy-find-declaration
;;       (kbd "gi") 'lsp-proxy-find-implementations
;;      )))

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
