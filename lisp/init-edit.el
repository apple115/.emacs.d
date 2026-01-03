;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package savehist
  :init
  (savehist-mode))

;; 可以是 async-shell-command 自动填充上一个命令
;; 中文
(advice-add #'read-shell-command
            :filter-args #'(lambda(args) (list (car args) (car shell-command-history))))

(global-so-long-mode 1)

(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-mode-line-format 'nil
        evil-symbol-word-search t)

  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  (setq evil-want-integration t) ;; necessary for evil collection
  (setq evil-visual-update-x-selection-p nil)
  (evil-mode 1)
  :config
  ;; (add-hook '(magit-mode-hook so-long-minor-mode-hook) ')
  ;; (add-hook '(magit-mode-hoo) (lambda () (evil-ex-hl-update-delay 0.25)
  ;; (setq-hook! '(magit-mode-hook so-long-minor-mode-hook)
  ;;             evil-ex-hl-update-delay 0.25)
  (evil-add-command-properties #'citre-jump :jump t)
  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (evil-define-key 'normal prog-mode-map (kbd "s") 'evil-avy-goto-char-timer)
  (global-set-key [remap evil-quit] 'kill-buffer-and-window)
  ;; 设置C-a C-e为行首行尾 insert模式下
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line)
  ;; C-f C-b
  (define-key evil-insert-state-map (kbd "C-f") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-b") 'backward-char)
  ;;C-u C-k
  (define-key evil-insert-state-map (kbd "C-u") 'evil-delete-back-to-indentation)
  (define-key evil-insert-state-map (kbd "C-k") 'delete-line)

(defun my-switch-to-english-async ()
"异步切换到英文输入法，不阻塞 UI。"
(interactive)
;; 使用 start-process 开启异步子进程，不等待返回结果
(start-process "set-im" nil "macism" "com.apple.keylayout.ABC"))

;; 在 Evil 退出插入模式时触发
(when(eq system-type 'darwin)
    (add-hook 'evil-insert-state-exit-hook #'my-switch-to-english-async)
)

;; 消除 ESC 延迟 (关键！)
(setq evil-esc-delay 0)
  )

(use-package evil-indent-plus
  :ensure t
  :after evil
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down)
  )

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

 (use-package evil-nerd-commenter
   :ensure t
   :after (evil evil-collection)
   :init
   (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
   (define-key evil-visual-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines))

(use-package evil-collection
  :ensure t
  :demand t
  :config
  (setq evil-collection-mode-list '(ibuffer calendar vterm ediff magit realgud compile docker dape vertico xref corfu mini-buffer consult woman man citre gptel cider citre nov pdf))
  (evil-collection-init))


(use-package evil-matchit
  :ensure t
  :config
  (setq global-evil-matchit-mode 1))

(use-package evil-textobj-tree-sitter
  :ensure t
  :after evil
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "call.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "call.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
  (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
  (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;; Goto start of next function
  (define-key evil-normal-state-map
              (kbd "]m")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer")))

  ;; Goto start of previous function
  (define-key evil-normal-state-map
              (kbd "[m")
              (lambda ()
                (interactive)
                (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  )

(setq x-select-request-type nil)

(use-package avy
  :ensure t)

(use-package sudo-edit
  :ensure t
  :config
  (sudo-edit-indicator-mode)
  )

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
)


;; vim keymap setting
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-;") nil)
(global-set-key (kbd "C-'") nil)

;; (use-package hippie-exp
;;   :ensure nil
;;   :config
;;   (setq-default hippie-expand-try-functions-list
;;                 '(yas-hippie-try-expand emmet-expand-line)))


;; (use-package sis
;;   :ensure t
;;   ;; :hook
;;   ;; ((text-mode  . sis-respect-start)
;;   ;;  (text-mode . sis-inline-mode))
;;   :config
;;   (sis-ism-lazyman-config
;;    "com.apple.keylayout.ABC"
;;    "com.apple.inputmethod.SCIM.Shuangpin"
;;    )
;;   ;; (sis-global-cursor-color-mode nil)
;;   (sis-global-respect-mode t)
;;   ;; (setq sis-inline-with-other t)
;; )

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
