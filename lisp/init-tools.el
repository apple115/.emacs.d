;;; init-tools.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: Useful tools to make Emacs efficient!

;;; Code:

(use-package which-key
  :ensure t
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order 'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecisewindow-fit t
        which-key-separator " → " ))

(use-package tree-sitter
  :defer 1
  :ensure t
  :config
  (global-tree-sitter-mode)
)

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package tree-sitter-langs
  :defer 1
  :ensure t
  :disabled t
  :after tree-sitter
)
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-to-list 'auto-mode-alist '("\\.ts\\'". typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'". tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'". js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'". json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'". yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'". yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'". css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'". go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'". rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'". dockerfile-ts-mode))
(setq treesit-font-lock-level 4)

(setq make-backup-files nil)                                  ; 不自动备份
(setq auto-save-default nil)                                  ; 不使用Emacs自带的自动保存


(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :config
    (setq-default
        flycheck-disabled-checkers
        (append (default-value 'flycheck-disabled-checkers)
                '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))
  ;; (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'rust-clippy 'rust-mode)
  (flycheck-add-mode 'haskell-ghc 'haskell-mode)
  (flycheck-add-mode 'go-staticcheck 'go-ts-mode)
  (setq flycheck-javascript-eslint-executable )
  (evil-define-key 'normal prog-mode-map (kbd "]d") 'flycheck-previous-error)
  (evil-define-key 'normal prog-mode-map (kbd "[d") 'flycheck-next-error)
)

(use-package flymake
  :ensure nil
  ;; :hook (prog-mode . flymake-mode)
  :config
  (evil-define-key 'normal prog-mode-map (kbd "]d") 'flymake-goto-prev-error)
  (evil-define-key 'normal prog-mode-map (kbd "[d") 'flymake-goto-next-error)
)

(use-package flymake-flycheck
 :ensure t
 :config
 (add-hook 'flymake-mode-hook 'flymake-flycheck-auto)
 (setq eldoc-documentation-function 'eldoc-documentation-compose)
 (add-hook 'flymake-mode-hook
            (lambda ()
                (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function nil t)))
)

(use-package format-all
  :ensure t
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                 ("Shell" (shfmt "-i" "4" "-ci"))
                  ("JavaScript" (prettier "-w"))
                  ("TypeScript" (prettier "-w"))
                  ("YAML" (prettier "-w"))
                  ("JSX" (prettier "-w"))
                  ("TSX" (prettier "-w"))
                  ("Haskell" (stylish-haskell))
                  ("Rust" (rustfmt "--edition" "2021"))
                  ("Python" (black))
                  ("Go" (goimports))
                  ("CMake" (cmake-format))
                  ("Dockefile" (dockfmt))
                  ("HTML" (prettier "-w"))
                  ("CSS" (prettier "-w"))
                  ("Markdown" (prettier "-w"))
                  ("YAML" (prettier "-w"))
                  ("TOML" (prettier "-w"))
                  ("Vue" (prettier "-w"))
                  ("JSON" (jq))
                  ("SQL" (sqlformat))
                  ("Ruby" (rufo))
                  ("C++" (clang-format "-style=Google"))
                  ("clojure" (zprint))
)))

(use-package link-hint
  :ensure t
  :defer t
  :config
  (setq browse-url-browser-function 'browse-url-firefox)
)

(use-package colorful-mode
 :ensure t
 ;; :hook (prog-mode text-mode)
 :config
;; In this example add emacs color names only for yaml-mode and derived.
(add-to-list 'colorful-extra-color-keyword-functions '(yaml-mode . colorful-add-color-names))
(add-to-list 'colorful-extra-color-keyword-functions '(js-jsx-mode . colorful-add-color-names))
)

(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/opt/homebrew/bin/fish")
    (use-package vterm-toggle
    :ensure t
    :bind (   :map vterm-mode-map
                ([(control return)] . vterm-toggle-insert-cd))
    :config
    (setq vterm-toggle-cd-auto-create-buffer nil)
    (defvar vterm-compile-buffer nil)
    (defun vterm-compile ()
        "Compile the program including the current buffer in `vterm'."
        (interactive)
        (setq compile-command (compilation-read-command compile-command))
        (let ((vterm-toggle-use-dedicated-buffer t)
            (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
                                                        (vterm-toggle-hide)
                                                    vterm-compile-buffer)))
        (with-current-buffer (vterm-toggle-cd)
            (setq vterm-compile-buffer (current-buffer))
            (rename-buffer "*vterm compilation*")
            (compilation-shell-minor-mode 1)
            (vterm-send-M-w)
           (vterm-send-string compile-command t)
            (vterm-send-return))))
    )
)


(use-package woman
  :ensure nil
)

(use-package atomic-chrome
  :demand t
  :vc (:url "https://github.com/KarimAziev/atomic-chrome")
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server))

(use-package dwim-shell-command
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
 )

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
