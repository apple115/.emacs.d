;;; init-tools.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-tools.el, init-docker.el
;;; Code:

;; ---- merged from init-tools.el ----

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
        which-key-idle-delay 1.5
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        ;; which-key-separator " → "
        which-key-allow-regexps nil
        )
  )


(use-package treesit
  :ensure nil
  :mode (
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         )
  :config
  (add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter")
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
          (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

  (setq major-mode-remap-alist
        '(
          (python-mode . python-ts-mode)
          (js-mode . js-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (css-mode . css-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (rust-mode . rust-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (go-mode . go-ts-mode)
          (markdown-mode . markdown-ts-mode)
          )
        )
  )

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


(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel"
	:lisp-dir "lisp"
	:rev :newest)
  :init
  (setq ghostel-shell
        (cond
         ;; Windows
         (+is-win-p "pwsh")
         ;; macOS (Homebrew fish)
         (+is-mac-p
          (or (executable-find "/opt/homebrew/bin/fish")
              (executable-find "/usr/local/bin/fish")
              (executable-find "fish")))
         ;; Linux
         (t
          (or (executable-find "/usr/bin/fish")
              (executable-find "fish")))))
  ;; 禁用终端 title 自动重命名 buffer，避免路径过长
  (setq ghostel-set-title-function nil)
  :config

(use-package ghostel-compile
  :hook (after-init . ghostel-compile-global-mode))

(use-package ghostel-comint
  :hook (after-init . ghostel-comint-global-mode))
)

(use-package evil-ghostel
  :vc (:url "https://github.com/dakra/ghostel"
       :lisp-dir "extensions/evil-ghostel"
       :rev :newest)
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))


(use-package dwim-shell-command
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind
  (:map prog-mode-map
        (("C-c C-o" . dumb-jump-go-other-window)
         ("C-c C-j" . dumb-jump-go)
         ("C-c C-i" . dumb-jump-go-prompt)))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package buffer-terminator
  :ensure t
  :custom
  (buffer-terminator-verbose nil)

  ;; 每 30 分钟触发一次清理扫描。
  (buffer-terminator-interval (* 30 60))

  :config
  ;; 只保留特殊 buffer、进程 buffer 和可见 buffer，
  ;; 移除 (kill-buffer-property . inactive) 规则，普通文件 buffer 不再被清理。
  (setq buffer-terminator-rules-alist
        '((keep-buffer-property . special)
          (keep-buffer-property . process)
          (keep-buffer-property . visible)))

  ;; 只清理当前没显示在窗口里的 dired buffer。
  (defun my/cleanup-invisible-dired-buffers ()
    "Kill dired buffers that are not currently visible."
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (eq (buffer-local-value 'major-mode buf) 'dired-mode)
                 (not (get-buffer-window buf 'visible)))
        (kill-buffer buf))))

  (add-hook 'buffer-terminator-before-hook #'my/cleanup-invisible-dired-buffers)
  (buffer-terminator-mode 1))

(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-toggle dired-subtree-cycle)
  :config
  (setq dired-subtree-line-prefix " ")
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-pop-to-sidebar-on-toggle-open nil))

;; (use-package tramp-hlo
;;     :ensure t
;;     :config
;;     (tramp-hlo-setup)
;; )


(use-package rime
  :ensure t
  :config
  ;; 默认值
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "C-`" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>"))
  (setq default-input-method "rime")
  (setq rime-user-data-dir (expand-file-name "~/.local/share/fcitx5/rime"))
  (setq rime-posframe-properties
        (list :background-color "#333333"
              :foreground-color "#dcdccc"
              :internal-border-width 10))
  (setq rime-show-candidate 'posframe)
  )

;; Rimel - 仅在 WSL 中使用（其他环境使用系统输入法或不使用 emacs-rime）
;;(when (and (eq system-type 'gnu/linux)
;;          (file-exists-p "/proc/version")
;;          (with-temp-buffer
;;             (insert-file-contents-literally "/proc/version")
;;             (re-search-forward "microsoft\\|WSL" nil t)))
;; (use-package rimel
;;   :ensure t))


(use-package i18n-quick
  :vc (:url "https://github.com/apple115/i18n-quick.el" :rev "master")
  :ensure t
  :custom
  (i18n-quick-languages
   '(("zh-CN" . "src/locale/zh-CN/")
     ("en-US" . "src/locale/en-US/")))
  (i18n-quick-style 'nested)
  (i18n-quick-max-width 50)
  )

;; tramp-rpc 需要升级 Tramp 版本，暂时禁用
;; (use-package tramp-rpc
;;   :load-path "site-lisp/emacs-tramp-rpc/lisp"
;;   :config
;;   (use-package msgpack
;;     :ensure t)
;;   (setq tramp-rpc-deploy-git-build-policy 'release)
;;   (setq diff-hl-disable-on-remote t)
;;   )

(use-package appine
  ;; 核心：仅在 macOS (darwin) 系统下启用，其他系统直接跳过此配置
  :if +is-mac-p
  ;; 使用内置的包管理器从 GitHub 拉取源码
  :vc (:url "https://github.com/chaoswork/appine" :rev "master")
  :custom
  ;; 开启 org-mode 链接支持
  (appine-use-for-org-links t)

  :bind (("C-x a a" . appine)
         ("C-x a u" . appine-open-url)
         ("C-x a o" . appine-open-file))
  :config
  ;; 只有在加载后，且确实在 Org-mode 中时才激活链接跳转逻辑
  (with-eval-after-load 'org
    (when (fboundp 'appine-setup-org-links)
      (appine-setup-org-links))))

(use-package sis
  :ensure t
  :hook
  ((markdown-mode . sis-inline-mode)
   (markdown-mode . sis-context-mode)
   (markdown-ts-mode . sis-inline-mode)
   (markdown-ts-mode . sis-context-mode)
   (org-mode . sis-inline-mode)
   (org-mode . sis-context-mode)
   (org-ts-mode . sis-inline-mode)
   (org-ts-mode . sis-context-mode))
  :config
  (when +is-mac-p
    (sis-ism-lazyman-config
    "com.apple.keylayout.ABC"
    "com.apple.inputmethod.SCIM.Shuangpin")
    )
  (when +is-win-p
    (setq sis--ism 'w32)
    )
)

(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker)
)

(use-package bookmark
  :ensure nil
  :defer t
  :init
  ;; 自定义书签文件路径（如果存在）
  (let ((file "~/.emacs.bmk"))
    (when (file-exists-p file)
      (setq bookmark-default-file file)))

  :config
  (defun my-build-bookmark-candidate (bookmark)
    "Re-shape BOOKMARK for completing-read display."
    (let* ((key (cond
                 ((and (assoc 'filename bookmark)
                       (cdr (assoc 'filename bookmark)))
                  (format "%s (%s)"
                          (car bookmark)
                          (cdr (assoc 'filename bookmark))))
                 ((and (assoc 'location bookmark)
                       (cdr (assoc 'location bookmark)))
                  (format "%s (%s)"
                          (car bookmark)
                          (cdr (assoc 'location bookmark))))
                 (t
                  (car bookmark)))))
      (cons key bookmark)))

  (defun my-bookmark-set ()
    "Set and save bookmark.
If bookmark with same file name already exists, override it quietly."
    (interactive)
    (bookmark-maybe-load-default-file)
    (let* ((filename (cond
                      ((eq major-mode 'eww-mode)
                       (eww-current-url))
                      (t
                       buffer-file-name)))
           existing-bookmark)
      (when (setq existing-bookmark
                  (cl-find-if (lambda (b)
                                (let* ((f (cdr (assoc 'filename (cdr b)))))
                                  (when (and f (file-exists-p f))
                                    (setq f (file-truename f)))
                                  (string= f filename)))
                              bookmark-alist))
        (setq existing-bookmark (car existing-bookmark)))
      (bookmark-set existing-bookmark)
      (bookmark-save)
      (when existing-bookmark
        (message "Saved into existing bookmark \"%s\"" existing-bookmark))))

  (defun my-bookmark-goto ()
    "Open bookmark with completing-read."
    (interactive)
    (bookmark-maybe-load-default-file)
    (let* ((cands (delq nil (mapcar #'my-build-bookmark-candidate
                                    (and (boundp 'bookmark-alist)
                                         bookmark-alist))))
           (selected (completing-read "Bookmarks: " cands)))
      (when selected
        (bookmark-jump (cdr (assoc selected cands))))))
  )

(setq eww-retrieve-command '("readable"))

(provide 'init-tools)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tools.el ends here
