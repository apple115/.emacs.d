;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;;自动保存
;; (setq auto-save-visited-interval 4)
;; (setq auto-save-visited-mode t)
;; (auto-save-visited-mode +1)
(use-package savehist
  :init
  (savehist-mode))
;; A few more useful configurations...

;; 可以是async-shell-command 自动填充上一个命令
(advice-add #'read-shell-command
 :filter-args #'(lambda(args) (list (car args) (car shell-command-history))))

(add-hook 'prog-mode-hook 'hs-minor-mode)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-search-module 'evil-search)
  (evil-mode 1)
  :config
  (evil-add-command-properties #'citre-jump :jump t)
)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(ibuffer calendar vterm eshell magit realgud compile docker dape vertico atomic-chrome xref corfu))
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

(setq x-select-request-type nil)

(use-package avy
  :ensure t)

(use-package sudo-edit
  :ensure t)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package elec-pair
  :ensure nil
  :hook ((prog-mode . electric-pair-mode)
         (conf-mode . electric-pair-mode)
         )
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer +leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

 )

(global-unset-key (kbd "C-SPC"))

;; vim keymap setting
(setq mark-ring-max 6)
(setq global-mark-ring-max 6)
(setq set-mark-command-repeat-pop t)
(evil-define-key  'normal prog-mode-map (kbd "s") 'avy-goto-char-timer)
(evil-define-key  'normal text-mode-map (kbd "s") 'avy-goto-char-timer)
(evil-define-key  'visual prog-mode-map (kbd "s") 'avy-goto-char-timer)
(evil-define-key  'visual text-mode-map (kbd "s") 'avy-goto-char-timer)
(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)

(evil-define-key 'normal global-map (kbd "H") 'evil-beginning-of-line)
(evil-define-key 'normal global-map (kbd "L") 'evil-end-of-line)
(evil-define-key 'visual global-map (kbd "H") 'evil-beginning-of-line)
(evil-define-key 'visual global-map (kbd "L") 'evil-end-of-line)

;; (evil-define-key 'insert global-map (kbd "C-n") 'completion-at-point)

(evil-define-key 'normal global-map (kbd "C-.") 'popper-toggle)
(evil-define-key 'normal global-map (kbd "M-.") 'popper-cycle)
(evil-define-key 'normal global-map (kbd "m") 'consult-register-store)
(evil-define-key 'normal global-map (kbd "'") 'consult-register-load)
(evil-define-key 'insert global-map (kbd "C-.") 'popper-toggle)
(evil-define-key 'insert global-map (kbd "M-.") 'popper-cycle)
(evil-define-key 'normal global-map (kbd "K") 'eldoc-box-help-at-point)

(global-set-key (kbd "C-;") nil)
(global-set-key (kbd "C-'") nil)

(use-package atomic-chrome
  :load-path "./site-lisp/atomic-chrome"
  :config
  (atomic-chrome-start-server)
(setq atomic-chrome-buffer-open-style 'full)
)

(use-package hippie-exp
:ensure nil
:config
(setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
