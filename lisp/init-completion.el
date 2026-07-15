;;; init-completion.el --- Merged settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Merged from: init-completion.el, init-consult.el
;;; Code:

;; ---- merged from init-completion.el ----
;;;
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

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (vertico-mode t)
  )

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package embark
  :ensure t
  :bind
   (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  )

; Support Pinyin
;; (use-package pinyinlib
;;   :ensure t
;;   :after orderless
;;   :autoload pinyinlib-build-regexp-strin
;;   :init
;;   (defun completion--regex-pinyin (str)
;;     (orderless-regexp (pinyinlib-build-regexp-string str)))
;;   (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))
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

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)  ;; 不在 echo area 使用多行
  (eldoc-idle-delay 0.5))                ;; 延迟 0.5 秒显示

;; (use-package eldoc-box
;;   :ensure t
;;   :commands (eldoc-box-help-at-point eldoc-box-quit-frame)
;;   :custom
;;   (eldoc-box-clear-with-C-g t)  ;; 按 C-g 可以关闭文档框
;;   :config
;;   ;; 可选：在 eglot 模式下自动启用 hover 模式（自动显示文档）
;;   ;; 如果觉得自动显示太干扰，可以注释掉这行，改为手动按 K 查看
;;   ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode)
;; )

;; 非 LSP 补全源：dabbrev / file / keyword / yasnippet
(use-package yasnippet-capf
  :ensure t
  :after yasnippet)

(defun +cape-setup-non-lsp ()
  "Setup non-LSP completion sources for current buffer."
  (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
  (add-hook 'completion-at-point-functions #'cape-file nil t)
  (add-hook 'completion-at-point-functions #'cape-keyword nil t)
  (add-hook 'completion-at-point-functions #'yasnippet-capf nil t))

(use-package cape
  :ensure t
  :hook ((prog-mode . +cape-setup-non-lsp)
         (text-mode . +cape-setup-non-lsp)))

;; Emacs 30 内置幽灵文本内联补全（ghost text）
(use-package completion-preview
  :ensure nil
  :hook ((prog-mode . completion-preview-mode)
         (text-mode . completion-preview-mode))
  :bind (:map completion-preview-active-mode-map
              ("C-n" . completion-preview-next-candidate)
              ("C-p" . completion-preview-previous-candidate)
              ("TAB" . completion-preview-insert))
  :custom
  (completion-preview-minimum-symbol-length 2))


;;(use-package eglot
;;  :ensure nil  ;; Emacs 内置
;;  ;; :hook (prog-mode . eglot-ensure)  ;; 如需自动启动，取消注释
;;  :general
;;  (:keymaps 'override
;;   :states '(normal visual)
;;   :prefix  "SPC"
;;   "t l" '(eglot :wk "toggle eglot")
;;   "l" '(:ignore t :wk "lsp")
;;   "l r" '(eglot-rename :wk "rename")
;;   "l a" '(eglot-code-actions :wk "code action")
;;   "l f" '(eglot-format :wk "format")
;;   "l d" '(xref-find-definitions :wk "find definition")
;;   "l D" '(xref-find-references :wk "find references")
;;   "l i" '(eglot-find-implementation :wk "find implementation"))
;;  (:keymaps 'eglot-mode-map
;;   :states 'normal
;;   "K" 'eldoc-box-help-at-point       ;; 显示浮动文档框
;;   "gd" 'xref-find-definitions
;;   "gr" 'xref-find-references
;;   "gi" 'eglot-find-implementation)
;;  :custom
;;  (eglot-autoshutdown t)  ;; 关闭缓冲区时自动停止 LSP 服务器
;;  (eglot-send-changes-idle-time 0.5)  ;; 延迟发送更改
;;  (eglot-events-buffer-size 0)  ;; 禁用事件日志缓冲区以提升性能
;;  :config
;;  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
;;  ;; 添加语言服务器配置
;;  (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))
;;  (add-to-list 'eglot-server-programs '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
;;  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio"))))

;;添加一下bin 下面的emacs-lsp-booster
;; 将 bin 目录添加到 exec-path，以便 eglot-booster 能找到 emacs-lsp-booster.exe
;; (add-to-list 'exec-path (expand-file-name "bin" user-emacs-directory))

;; (use-package eglot-booster
;;   :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

;; (use-package ht
;;   :ensure t
;; )

;; ---- merged from init-consult.el ----
(use-package consult
 :ensure t
 :general
 (:keymaps 'override
 :states '(normal visual)
 :prefix  "SPC"
  "s o" '(consult-ripgrep :wk "search word")
  "s c" '(consult-compile-error :wk "search compile error")
  "s m" '(consult-man :wk "search man")
  "s n" '(consult-notes :wk "search notes")
  "s i" '(consult-imenu :wk "find imenu")
  "s f" '(consult-fd :wk "find file")
  "s e" '(consult-flymake :wk "search diagnostic")
  "s l" '(consult-line :wk "search line in buffer")
  "s d" '(consult-dir :wk "search dir")
  "s r" '(consult-recent-file :wk "search recent file")
  "s r" '(consult-recent-file :wk "search recent file")
 )
 :config
(defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key "M-p")))

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult-source-recent-file consult-source-project-recent-file consult-source-bookmark)

    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list current-prefix-arg
                         (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (if-let* ((s (symbol-at-point)))
                               (symbol-name s)))))
      (consult-ripgrep dir initial))
    (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
    (setq consult-locate-command "mdfind -name ARG OPTS")
)

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; (setq read-file-name-function #'consult-find-file-with-preview)
;; (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
;;   (let ((default-directory (or dir default-directory))
;;         (minibuffer-completing-file-name t))
;;     (consult--read #'read-file-name-internal :state (consult--file-preview)
;;                    :prompt prompt
;;                    :initial initial
;;                    :require-match mustmatch
;;                    :predicate pred))
;;   )

(use-package consult-dir
  :ensure t
  :defer t
  :config
  (setq consult-dir-default-command #'consult-dir-dired)

  (defun consult-dir--zoxide-dirs ()
    "Return list of zoxide dirs."
    (split-string (shell-command-to-string "zoxide query -l") "\n" t))

  (defvar consult-dir--source-zoxide
    `(:name "zoxide"
      :narrow ?z
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,(lambda () (executable-find "zoxide"))
      :items ,#'consult-dir--zoxide-dirs)
    "zoxide directory source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir--source-zoxide t))

(use-package engine-mode
  :ensure t
  :config
    (defengine google
    "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    )
  (engine-mode t))

(use-package consult-notes
  :ensure t
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config
  (setq consult-notes-file-dir-sources
        `(
          ;; ("work"    ?w ,(concat org-directory "/midea/"))
          ("denote"  ?d ,(concat org-directory "/denote/"))
          ;; ("org"     ?o ,(concat org-directory "/"))
          ;; ("blog"    ?b  "/home/apple115/blog/source/_posts/")
          ;; ("books"   ?b ,(concat (getenv "HOME") "/Books/"))
          )))

(use-package consult-todo
  :ensure t
)

(use-package wgrep
  :ensure t
  :config
  ;; 设置为 t，可以在保存 wgrep buffer 时自动保存受影响的文件
  (setq wgrep-auto-save-buffer t)
  ;; 修改完后直接用习惯的 C-x C-s 保存也可以（如果你绑定了的话）
  (define-key grep-mode-map (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode))

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
