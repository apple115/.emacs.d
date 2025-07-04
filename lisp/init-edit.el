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

(add-hook 'prog-mode-hook 'hs-minor-mode)
;; (add-hook 'after-init-hook 'auto-save-visited-mode)
(use-package hl-line
  :ensure nil
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll t) ;; allow scroll down with 'C-d'
  (setq evil-want-integration t) ;; necessary for evil collection
  (evil-mode 1)
  :config
  (evil-add-command-properties #'citre-jump :jump t)
    (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
    (evil-define-key 'normal prog-mode-map (kbd "s") 'evil-avy-goto-char-timer)
(global-set-key [remap evil-quit] 'kill-buffer-and-window)
)

(use-package evil-mc
  :after evil
  :ensure t
  :config
 (global-evil-mc-mode  1))


(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(ibuffer calendar vterm ediff magit realgud compile docker dape vertico xref corfu mini-buffer consult woman man citre gptel cider citre))
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :init
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines))

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
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer +leader-keys
    :states '(normal visual)
    :states 'nil
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "C-SPC") ;; access leader in insert mode
 )

;; vim keymap setting
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(setq set-mark-command-repeat-pop t)

(global-set-key (kbd "C-;") nil)
(global-set-key (kbd "C-'") nil)

(use-package hippie-exp
:ensure nil
:config
(setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))

(use-package sis
 :ensure t
 :hook
 (((text-mode prog-mode) . sis-context-mode)
   ((text-mode prog-mode) . sis-inline-mode))
 :config
(sis-ism-lazyman-config
   "com.apple.keylayout.ABC"
   "com.apple.inputmethod.SCIM.Shuangpin"
)
(sis-global-cursor-color-mode t)
(sis-global-respect-mode t)
(setq sis-inline-with-other t)
)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
