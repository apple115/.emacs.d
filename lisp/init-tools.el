;;; init-tools.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Useful tools to make Emacs efficient!

;;; Code:

(use-package which-key
  :ensure nil
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
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))


(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.19.0"))
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
        (zig . ("https://github.com/GrayJack/tree-sitter-zig")))))

(setq make-backup-files nil)                                  ; 不自动备份
(setq auto-save-default nil)                                  ; 不使用Emacs自带的自动保存

(use-package flycheck
  :ensure t
  :custom
  (flycheck-indication-mode 'nil)
  :config
    ;; (setq-default
    ;;     flycheck-disabled-checkers
    ;;     (append (default-value 'flycheck-disabled-checkers)
    ;;             '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package)))
  (setq truncate-lines nil) ; 如果单行信息很长会自动换行
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'rust-clippy 'rust-mode)
  (flycheck-add-mode 'haskell-ghc 'haskell-mode)
  (flycheck-add-mode 'go-staticcheck 'go-ts-mode)
  (evil-define-key 'normal prog-mode-map (kbd "]d") 'flycheck-previous-error)
  (evil-define-key 'normal prog-mode-map (kbd "[d") 'flycheck-next-error)
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
  ;; (setq browse-url-browser-function 'browse-url-firefox)
)

(use-package colorful-mode
 :ensure t
 ;; :hook (prog-mode text-mode)
 :config
;; In this example add emacs color names only for yaml-mode and derived.
(add-to-list 'colorful-extra-color-keyword-functions '(yaml-mode . colorful-add-color-names))
(add-to-list 'colorful-extra-color-keyword-functions '(js-jsx-mode . colorful-add-color-names))
)

;; vterm - 已替换为 eat
;; (use-package vterm
;;   :ensure t
;;   :config
;;   (setq vterm-shell "/opt/homebrew/bin/fish")
;;     (use-package vterm-toggle
;;     :ensure t
;;     :bind (:map vterm-mode-map
;;                 ([(control return)] . vterm-toggle-insert-cd))
;;     :config
;;     (setq vterm-toggle-cd-auto-create-buffer nil)
;;     (defvar vterm-compile-buffer nil)
;;     (defun vterm-compile ()
;;         "Compile the program including the current buffer in `vterm'."
;;         (interactive)
;;         (setq compile-command (compilation-read-command compile-command))
;;         (let ((vterm-toggle-use-dedicated-buffer t)
;;             (vterm-toggle--vterm-dedicated-buffer (if (vterm-toggle--get-window)
;;                                                         (vterm-toggle-hide)
;;                                                     vterm-compile-buffer)))
;;         (with-current-buffer (vterm-toggle-cd)
;;             (setq vterm-compile-buffer (current-buffer))
;;             (rename-buffer "*vterm compilation*")
;;             (compilation-shell-minor-mode 1)
;;             (vterm-send-M-w)
;;            (vterm-send-string compile-command t)
;;             (vterm-send-return))))
;;     )
;; )

;; Eat - Emulate A Terminal (更好的终端模拟器)
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)  ;; 退出时自动关闭 buffer
  :config
  ;; 在 eshell 中集成 eat
  (eat-eshell-mode)
  ;; 在 project.el 中使用 eat
  (eat-eshell-visual-command-mode))


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

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)

  ;; Set the inactivity timeout (in seconds) after which buffers are considered
  ;; inactive (default is 30 minutes):
  (buffer-terminator-inactivity-timeout (* 30 60)) ; 30 minutes

  ;; Define how frequently the cleanup process should run (default is every 10
  ;; minutes):
  (buffer-terminator-interval (* 10 60)) ; 10 minutes

  :config
  (buffer-terminator-mode 1))


(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
