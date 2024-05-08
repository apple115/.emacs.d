;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package evil
    :ensure t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode 1)
)

(use-package evil-collection
:ensure t
:after evil
:config
(setq evil-collection-mode-list '(dashboard dired ibuffer calendar vterm eshell magit realgud))
(evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :init
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
)

(setq x-select-request-type nil)

(use-package avy
 :ensure t)

(use-package sudo-edit
  :ensure t
)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (dt/leader-keys
   "." '(find-file :wk "find file")
   "g"'(:ignore t :wk "goto")
   "g c" '((lambda () (interactive) (find-file "~/.emacs.d")) :wk "Edit emacs config")
   "g s" '((lambda () (interactive) (find-file "~/.emacs.d/snippets")) :wk "Edit emacs snippet")
   "g b" '((lambda () (interactive) (find-file "~/blog")) :wk "blog")
   )

  (dt/leader-keys
    "f" '(:ignore t :wk "Find")
    "f f" '(consult-fd :wk "find file")
    "f w" '(consult-ripgrep :wk "find word")
    "f m" '(consult-man :wk "find man")
    "f n" '(consult-notes :wk "find notes")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file")
  )


  (dt/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(consult-buffer :wk "Switch buffer")
    "b k" '(kill-buffer :wk "kill buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b r" '(revert-buffer :wk "Reload buffer")
   )

  (dt/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))




(defun my-load-config ()
"Load Emacs configuration."
(interactive)
(load-file "~/.config/emacs/init.el"))

(defun my-open-termial-kitty ()
"open kitty terminal in load filepath"
(interactive)
(let ((directory (eshell/pwd)))
(async-shell-command (format "kitty --directory %s" directory))
))

   (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '(my-load-config :wk "Reload Emacs config")
)

   (dt/leader-keys
    "5" '(projectile-run-project :wk "run project")
    "6" '(projectile-test-project :wk "test project")
    "9" '(projectile-compile-project :wk "compile project")
    "=" '((lambda () (interactive) (format-all-buffer)) :wk "current buffer format")
)

   (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(my-open-termial-kitty :wk "open terminal")
   )

   (dt/leader-keys
    "o" '(:ignore t :wk "open")
    "o t" '(vterm-toggle :wk "open terminal")
    "o s" '(async-shell-command :wk "open async shell command")
    "o e" '(aweshell-dedicated-toggle :wk "aweshell")
    "o c" '((lambda () (interactive) (org-capture)) :wk "open org-capture")
    "o a" '((lambda () (interactive) (org-agenda)) :wk "open org-agenda"))

   (dt/leader-keys
    "x" '(:ignore t :wk "fix")
    "x x" '(lsp-bridge-diagnostic-list :wk "show diagnostic list")
    "x c" '(lsp-bridge-diagnostic-copy :wk "copy diagnostic list"))

   (dt/leader-keys
    "p" '(:ignore t :wk "project")
    "p p" '(projectile-switch-project :wk "project switch project")
    "p f" '(projectile-find-file :wk "project find file")
    "p d" '(projectile-dired :wk "project dired")
    "p b" '(projectile-switch-to-buffer :wk "project switch buffer"))

   (dt/leader-keys
    "d" '(:ignore t :wk "denote")
    "d n" '(denote :wk "create denote")
    "d d" '(denote-date :wk "create date note")
    "d t" '(denote-type :wk "creates a note while prompting for a file type")
    "d s" '(denote-subdirectory :wk "create note ")
    "d f" '(denote-open-or-create :wk "find denote")
    "d r" '(denote-dired-rename-file :wk "rename denote"))


)

(evil-define-key 'normal global-map (kbd "C-.") 'popper-toggle)
  (evil-define-key 'normal global-map (kbd "M-.") 'popper-cycle)

;; 定义快捷键在 rust-mode 下生效
(with-eval-after-load 'prog-mode
  (evil-define-key 'normal prog-mode-map (kbd "K") 'lsp-bridge-show-documentation)
  (evil-define-key 'normal prog-mode-map (kbd "gd") 'lsp-bridge-find-def)
  (evil-define-key 'normal prog-mode-map (kbd "gi") 'lsp-bridge-find-imp)
  (evil-define-key 'normal prog-mode-map (kbd "go") 'lsp-bridge-find-def-return)
)

(with-eval-after-load 'rust-mode
)

;; 定义快捷键在 python-mode 下生效
(with-eval-after-load 'python-mode
)


(general-define-key
 :states '(normal visual)
 :keymaps 'org-mode-map
 :prefix "SPC"
  "c" '(:ignore t :wk "mode define command")
  "c c" '(org-toggle-checkbox  :wk"toggle-checkbox")
)

(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix "SPC"
  "c" '(:ignore t :wk "mode define command")
  "c o" '(xah-open-in-external-app :wk"open the file with xopen")
  "c p" '(my-paste-to-dired  :wk "past some in the dired")
)

;; 可以继续为其他模式添加类似的代码

(evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-n") #'acm-select-next)
(evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-p") #'acm-select-prev)

;; agenda

(add-hook 'org-agenda-mode-hook
          (lambda ()
            (evil-set-initial-state 'org-agenda-mode 'normal)))
  (evil-define-key 'normal org-agenda-mode-map (kbd "q") 'org-agenda-quit)


  (evil-define-key 'normal org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
  (evil-define-key 'normal org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)


  (evil-define-key 'normal org-agenda-mode-map (kbd "<tab>") 'org-agenda-todo)
  (evil-define-key 'normal org-agenda-mode-map (kbd "gc") 'org-agenda-goto-calender)
  (evil-define-key 'normal org-agenda-mode-map (kbd "gr") 'org-agenda-redo)

  (evil-define-key 'normal org-agenda-mode-map (kbd "u") 'org-agenda-undo)

;; capture
(add-hook 'org-capture-mode-hook
          (lambda ()
            (evil-set-initial-state 'org-capture-mode 'normal)))

;; vim keymap setting
  (evil-define-key  'normal prog-mode-map (kbd "s") 'avy-goto-char-2)
  (evil-define-key  'normal text-mode-map (kbd "s") 'avy-goto-char-2)

  (evil-define-key  'insert prog-mode-map (kbd "C-y") 'yas-expand)
  (evil-define-key  'insert text-mode-map (kbd "C-y") 'yas-expand)

  (evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)

  (evil-define-key 'normal global-map (kbd "H") 'evil-beginning-of-line)
  (evil-define-key 'normal global-map (kbd "L") 'evil-end-of-line)
  (evil-define-key 'visual global-map (kbd "H") 'evil-beginning-of-line)
  (evil-define-key 'visual global-map (kbd "L") 'evil-end-of-line)

;; (message "init-base configuration: %.2fs"
;;          (float-time (time-subtract (current-time) my/init-base-start-time)))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
