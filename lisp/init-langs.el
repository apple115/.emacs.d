;;; init-langs.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-langs.el, init-go.el, init-rust.el, init-python.el, init-web-developer.el
;;; Code:

;; ---- merged from init-langs.el ----
;;;
;;(use-package clojure-ts-mode
;;:ensure t
;;)

;;(use-package cider
;; :ensure t
;;)

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

(use-package fish-mode
  :mode (("\\.fish\\'" . fish-mode))
 :ensure t)


(use-package zig-mode
  :mode (("\\.zig\\'" . zig-mode))
 :ensure t)

;; ---- merged from init-go.el ----

(use-package go-mode
  :ensure t
  :functions (go-install-tools exec-path-from-shell-copy-envs)
  :init
  ;; Install tools
  (defconst go--tools
    '("golang.org/x/tools/gopls"
      "golang.org/x/tools/cmd/goimports"
      "honnef.co/go/tools/cmd/staticcheck"
      "github.com/go-delve/delve/cmd/dlv"
      "github.com/zmb3/gogetdoc"
      "github.com/josharian/impl"
      "github.com/cweill/gotests/..."
      "github.com/fatih/gomodifytags"
      "github.com/davidrjenni/reftools/cmd/fillstruct")
    "All necessary go tools.")

  (defun go-install-tools ()
    "Install or update go tools."
    (interactive)
    (unless (executable-find "go")
      (user-error "Unable to find `go' in `exec-path'!"))

    (message "Installing go tools...")
    (dolist (pkg go--tools)
      (set-process-sentinel
       (start-process "go-tools" "*Go Tools*" "go" "install" "-v" "-x" (concat pkg "@latest"))
       (lambda (proc _)
         (let ((status (process-exit-status proc)))
           (if (= 0 status)
               (message "Installed %s" pkg)
             (message "Failed to install %s: %d" pkg status)))))))
  :config
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-envs '("GOPATH" "GO111MODULE" "GOPROXY")))
  ;; Try to install go tools if `gopls' is not found
  (when (and (executable-find "go")
             (not (executable-find "gopls")))
    (go-install-tools))
  )

(use-package go-dlv
  :ensure t)
(use-package go-fill-struct
  :ensure t)
(use-package go-impl
  :ensure t)

(use-package go-tag
  :ensure t
  :bind (:map go-mode-map
              ("C-c t a" . go-tag-add)
              ("C-c t r" . go-tag-remove))
  :init (setq go-tag-args (list "-transform" "camelcase")))

(use-package go-gen-test
  :ensure t
  :bind (:map go-mode-map
              ("C-c t g" . go-gen-test-dwim)))

(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package gotest
  :ensure t
  :bind (:map go-mode-map
              ("C-c t f" . go-test-current-file)
              ("C-c t t" . go-test-current-test)
              ("C-c t j" . go-test-current-project)
              ("C-c t b" . go-test-current-benchmark)
              ("C-c t c" . go-test-current-coverage)
              ("C-c t x" . go-run)))

;; ---- merged from init-rust.el ----
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :ensure t
  :general
   (:keymaps 'rust-mode
   :states '(normal)
   :prefix  "SPC"
   "m" '(ignore t :wk "mode")
   "m r" '(rust-run :wk "run")
   "m k" '(rust-check :wk "check")
   "m c" '(rust-run-check :wk "run check")
   "m t" '(rust-test :wk "test current"))
)

;; ---- merged from init-python.el ----
(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  ;; note that setting `venv-location` is not necessary if you
  ;; use the default location (`~/.virtualenvs`), or if the
  ;; the environment variable `WORKON_HOME` points to the right place
  (setq venv-location "/home/apple115/.cache/pypoetry/virtualenvs/")
  )

;; ---- merged from init-web-developer.el ----
(use-package emmet-mode
  :ensure t
  :hook ((css-mode . emmet-mode)
         (html-mode. emmet-mode)
         )
  :init
(setq emmet-indent-after-insert nil)
(setq emmet-expand-jsx-className? t)
  :config
(add-to-list 'emmet-jsx-major-modes 'js-jsx-mode)
(add-to-list 'emmet-jsx-major-modes 'typescript-ts-mode)
(add-to-list 'emmet-jsx-major-modes 'js-ts-mode)
(add-to-list 'emmet-jsx-major-modes 'js-mode)
(add-to-list 'emmet-jsx-major-modes 'typescript-mode)
(add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
)
;; CSS
(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :ensure t
  :init (setq scss-compile-at-save nil))

(use-package web-mode
  :ensure t
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package restclient
  :ensure t
)

(use-package ob-restclient
  :ensure t
)

(use-package add-node-modules-path
  :ensure t
  :hook((web-mode js-base-mode tsx-ts-mode js-ts-mode js-jsx-mode typescript-ts-mode) . add-node-modules-path)
  :custom
  (add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))
)

(provide 'init-langs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-langs.el ends here
