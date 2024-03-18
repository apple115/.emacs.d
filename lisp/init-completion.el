;;; init-completion.el --- Completion settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package counsel
  :ensure t)
(use-package ivy
:ensure t
:init
(ivy-mode 1)
;;(counsel-mode 1)
:custom
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq enable-recursive-minibuffers t)
:bind
 (
  ("C-s" . 'swiper-isearch)          ; 绑定快捷键 C-s 为 swiper-search，替换原本的搜索功能
  ("M-x" . 'counsel-M-x)             ; 使用 counsel 替换命令输入，给予更多提示
  ("C-x C-f" . 'counsel-find-file)   ; 使用 counsel 做文件打开操作，给予更多提示
  ("M-y" . 'counsel-yank-pop)        ; 使用 counsel 做历史剪贴板粘贴，可以展示历史
  ("C-x b" . 'ivy-switch-buffer)     ; 使用 ivy 做 buffer 切换，给予更多提示
  ("C-c v" . 'ivy-push-view)         ; 记录当前 buffer 的信息
  ("C-c s" . 'ivy-switch-view)       ; 切换到记录过的 buffer 位置
 ("C-c V" . 'ivy-pop-view)          ; 移除 buffer 记录
)
)

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package yasnippet
  :ensure t
  :config
;;(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
 (yas-global-mode 1))

(use-package markdown-mode
:ensure t
:mode ("README\\.md\\'" . gfm-mode)
:init (setq markdown-command "multimarkdown")
:bind (:map markdown-mode-map
       ("C-c C-e" . markdown-do)))

(use-package lsp-bridge
 :load-path "~/.config/emacs/site-lisp/lsp-bridge"
 :hook (js-jsx-mode . (lambda ()
                    (require 'lsp-bridge)
                    (lsp-bridge-enable)))
 :config
    (setq lsp-bridge-python-command "~/.config/emacs/site-lisp/myemacs/bin/python3")
    ;;(setq lsp-bridge-enable-auto-format-code t);;自动格式化
    (setq lsp-bridge-enable-inlay-hint nil)
    (setq lsp-bridge-enable-completion-in-string t)
    (setq lsp-bridge-enable-search-words  t)
    (setq lsp-bridge-enable-org-babel t)
    (setq lsp-bridge-enable-hover-diagnostic t)
    (setq lsp-bridge-org-babel-lang-list '(emacs-lisp))
    (setq acm-quick-access-use-number-select t)
    (global-lsp-bridge-mode)
;; (define-key acm-mode-map (kbd "M-1") (lambda () (interactive) (insert "1")))
;; (define-key acm-mode-map (kbd "M-2") (lambda () (interactive) (insert "2")))
;; (define-key acm-mode-map (kbd "M-3") (lambda () (interactive) (insert "3")))
;; (define-key acm-mode-map (kbd "M-4") (lambda () (interactive) (insert "4")))
;; (define-key acm-mode-map (kbd "M-5") (lambda () (interactive) (insert "5")))
;; (define-key acm-mode-map (kbd "M-6") (lambda () (interactive) (insert "6")))
;; (define-key acm-mode-map (kbd "M-7") (lambda () (interactive) (insert "7")))
;; (define-key acm-mode-map (kbd "M-8") (lambda () (interactive) (insert "8")))
;; (define-key acm-mode-map (kbd "M-9") (lambda () (interactive) (insert "9")))
;; (define-key acm-mode-map (kbd "M-0") (lambda () (interactive) (insert "0")))

    (define-key acm-mode-map (kbd "<tab>") 'acm-hide)


)

(provide 'init-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
