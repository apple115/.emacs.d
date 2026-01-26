;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
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

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)  ;; 不在 echo area 使用多行
  (eldoc-idle-delay 0.5))                ;; 延迟 0.5 秒显示

(use-package eldoc-box
  :ensure t
  :commands (eldoc-box-help-at-point eldoc-box-quit-frame)
  :custom
  (eldoc-box-clear-with-C-g t)  ;; 按 C-g 可以关闭文档框
  :config
  ;; 可选：在 eglot 模式下自动启用 hover 模式（自动显示文档）
  ;; 如果觉得自动显示太干扰，可以注释掉这行，改为手动按 K 查看
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode)
)

;;(use-package cape
;;  :ensure t
;;  :init
;;  ;; 添加补全源到 completion-at-point-functions
;;  ;; 注意：LSP/Eglot 补全会自动添加，这里只添加额外的补全源
;;
;;  ;; 文件路径补全（在字符串和注释中很有用）
;;  (add-hook 'completion-at-point-functions #'cape-file)
;;
;;  ;; 当前缓冲区的词语补全（类似 dabbrev）
;;  (add-hook 'completion-at-point-functions #'cape-dabbrev)
;;
;;  ;; 编程语言关键字补全
;;  (add-hook 'completion-at-point-functions #'cape-keyword)
;;
;;  :config
;;  ;; 优化 eglot 补全（避免缓存过期）
;;  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
;;
;;  ;; 让补全更安静（避免错误消息）
;;  (advice-add 'eglot-completion-at-point :around #'cape-wrap-silent)
;;
;;  ;; Emacs 28 及以下版本需要的补丁
;;  (when (< emacs-major-version 29)
;;    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
;;
;;  ;; 为特定模式添加额外的补全源
;;
;;  ;; Shell/Eshell: 添加历史补全
;;  (add-hook 'eshell-mode-hook
;;            (lambda ()
;;              (add-hook 'completion-at-point-functions #'cape-history 0 t)))
;;
;;  (add-hook 'shell-mode-hook
;;            (lambda ()
;;              (add-hook 'completion-at-point-functions #'cape-history 0 t)))
;;
;;  ;; 任何地方都可以补全 Elisp 符号（用于配置和文档）
;;  ;; 可以通过 M-x cape-elisp-symbol 手动触发
;;  )

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

;; (use-package eglot-booster
;;   :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
;;   :after eglot
;;   :config
;;   (eglot-booster-mode))

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
