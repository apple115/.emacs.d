;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;;;自动保存
;; (setq auto-save-visited-interval 4)
;; (setq auto-save-visited-mode t)
;; (auto-save-visited-mode +1)

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
;; (add-hook 'evil-insert-state-exit-hook
;;           (lambda ()
;;             (call-interactively #'save-buffer)))
;;   )

;; (add-hook 'evil-insert-state-exit-hook
;;  (lambda ()
;;         (when (and (buffer-file-name) (buffer-modified-p))
;;             (call-interactively #'save-buffer))))
(evil-add-command-properties #'citre-jump :jump t)
)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-mode-list '(ibuffer calendar vterm eshell magit realgud compile docker dape vertico atomic-chrome xref))
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

;; (use-package evil-escape
;;   :ensure t
;;   :config
;;     (evil-escape-mode 1)
;;     (setq-default evil-escape-key-sequence "jk")
;; )

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
    "SPC" '(consult-fd :wk "find file")
    "=" '(+format-code-and-flycheck :wk "flycheck and format")
    "/" '(split-window-horizontally :wk"split window horizontally")
    "-" '(split-window-vertically :wk"split window vertically")
    "." '(find-file :wk "find file")
    "," '(dired-jump :wk "open-dired")
    )

  (dt/leader-keys
    "w" '(:ignore :wk "window")
    "w 0" '(delete-window :wk "delete-window")
    "w 9" '(delete-other-windows :wk "delete-other-windows")
   )

  (dt/leader-keys
    "g"'(:ignore t :wk "goto")
    "g c" '((lambda () (interactive) (find-file "~/.emacs.d")) :wk "Edit emacs config")
    "g s" '((lambda () (interactive) (find-file "~/.emacs.d/snippets")) :wk "Edit emacs snippet")
    "g b" '((lambda () (interactive) (find-file "~/blog")) :wk "blog")
    )

  (dt/leader-keys
    "f" '(:ignore t :wk "file")
    "f f" '(consult-fd :wk "find file")
    "f /" '(+consult-fd-other-window :wk "find file on other window")
    "f R" '(+rename-current-file :wk "rename and move current file")
    "f D" '(+delete-current-file :wk "delete current file")
    "f y" '(+copy-current-filename :wk "copy current filename")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file")
    )

  (dt/leader-keys
    "s" '(:ignore t :wk "Search")
    "s o" '(consult-ripgrep :wk "search word")
    "s e" '(consult-compile-error :wk "search word")
    "s g" '(engine/search-google :wk "search google")
    "s m" '(consult-man :wk "search man")
    "s n" '(consult-notes :wk "search notes")
    "s t" '(citre-query-jump :wk "find tags")
    "s w" '(fanyi-dwim :wk "search word")
    )

  (dt/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(consult-buffer :wk "buffer-switch")
    "b ," '(switch-to-prev-buffer :wk "prev-buffer")
    "b ." '(switch-to-next-buffer :wk "next-buffer")
    "b /" '(consult-buffer-other-window :wk "Switch buffer to other window")
    "b k" '(kill-buffer :wk "kill buffer")
    "b i" '(ibuffer :wk "ibuffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    )

  (dt/leader-keys
    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region"))


  (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '(my-load-config :wk "Reload Emacs config")
    )

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t t" '(my-open-termial-kitty :wk "open terminal")
    "t o" '(get-word-translate-to-bar :wk "translate word")
    )

  (dt/leader-keys
    "o" '(:ignore t :wk "open")
    "o o" '(embark-act :wk "embark-act")
    "o e" '(compile :wk "compile")
    "o t" '(+new-vtermN :wk "open vtermN")
    "o s" '(async-shell-command :wk "open async shell command")
    "o c" '((lambda () (interactive) (org-capture)) :wk "open org-capture")
    "o a" '((lambda () (interactive) (org-agenda)) :wk "open org-agenda")
    "o b" '(hexo-my-blog  :wk "open hexo")
    "o z" '(link-hint-open-link  :wk "open link-hint")
    )

  (dt/leader-keys
    "x" '(:ignore t :wk "fix or delete")
    "x x" '(lsp-bridge-diagnostic-list :wk "show diagnostic list")
    "x c" '(lsp-bridge-diagnostic-copy :wk "copy diagnostic list")
    )

  (dt/leader-keys
    "p" '(:ignore t :wk "project")
    "p p" '(projectile-switch-project :wk "project switch project")
    "p f" '(projectile-find-file :wk "project find file")
    "p d" '(projectile-dired :wk "project dired")
    "p b" '(projectile-switch-to-buffer :wk "project switch buffer")
    "p k" '(projectile-kill-buffers :wk "kill projectile buffers")
    "p r" '(projectile-run-project :wk "run project")
    "p t" '(projectile-test-project :wk "test project")
    "p c" '(projectile-compile-project :wk "compile project")
    )

  (dt/leader-keys
    "d" '(:ignore t :wk "denote or dired")
    "d d" '(pwd :wk "pwd")
    "d j" '(dired-jump)
    "d n" '(denote :wk "create denote")
    "d t" '(denote-type :wk "creates a note while prompting for a file type")
    "d f" '(denote-open-or-create :wk "find denote")
    "d r" '(denote-dired-rename-file :wk "rename denote"))

  (dt/leader-keys
    "c" '(:ignore t :wk "compile")
    "c r"'(recompile :wk "recompile")
    "c k"'(kill-compilation :wk "kill compile")
    )

  (dt/leader-keys
    "l" '(:ignore t :wk "lsp")
    "l n"'(lsp-bridge-rename :wk "rename")
    "l a"'(lsp-bridge-code-action :wk "code action")
    )
  )

;; (general-define-key
;;  :states '(normal)
;;  :keymaps 'override
;;  :prefix "SPC"
;;  "c" '(:ignore t :wk "")
;;  "c a"'(lsp-bridge-code-action :wk "code action")
;;  "c r"'(lsp-bridge-rename :wk "lsp-bridge-rename")
;; )


(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "C-x 4 t") 'open-vterm-in-other-window)

(evil-define-key 'normal global-map (kbd "C-.") 'popper-toggle)
(evil-define-key 'normal global-map (kbd "M-.") 'popper-cycle)

(evil-define-key 'normal global-map (kbd "m") 'consult-register-store)
(evil-define-key 'normal global-map (kbd "'") 'consult-register-load)

(evil-define-key 'insert global-map (kbd "C-.") 'popper-toggle)
(evil-define-key 'insert global-map (kbd "M-.") 'popper-cycle)

;; 定义快捷键在 rust-mode 下生效


;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps 'override
;;  :prefix "SPC"
;;  "c" '(:ignore t :wk "mode define command")
;;  "c o" '(xah-open-in-external-app :wk"open the file with spine")
;;  "c p" '(my-paste-to-dired  :wk "past some in the dired")
;;  )

;; 可以继续为其他模式添加类似的代码


(evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-n") #'acm-select-next)
(evil-collection-define-key 'insert 'lsp-bridge-mode-map (kbd "C-p") #'acm-select-prev)

(evil-collection-define-key 'normal 'lsp-bridge-mode-map (kbd "K") #'lsp-bridge-show-documentation)
(evil-collection-define-key 'normal 'lsp-bridge-mode-map (kbd "gd") #'lsp-bridge-find-def)
;; (add-to-list 'evil-goto-definition-functions 'lsp-bridge-find-def)
(evil-collection-define-key 'normal 'lsp-bridge-mode-map (kbd "gi") #'lsp-bridge-find-impl)
(evil-collection-define-key 'normal 'lsp-bridge-mode-map (kbd "go") #'lsp-bridge-find-def-return)
(evil-collection-define-key 'normal 'lsp-bridge-mode-map (kbd "]d") #'lsp-bridge-diagnostic-jump-next)
(evil-collection-define-key 'normal 'lsp-bridge-mode-map (kbd "[d") #'lsp-bridge-diagnostic-jump-prev)

(evil-collection-define-key 'normal 'lsp-bridge-ref-mode-map (kbd "q") #'lsp-bridge-ref-quit)
(evil-collection-define-key 'normal 'lsp-bridge-ref-mode-map (kbd "C-n") #'lsp-bridge-ref-jump-next-keyword)
(evil-collection-define-key 'normal 'lsp-bridge-ref-mode-map (kbd "C-p") #'lsp-bridge-ref-jump-prev-keyword)

;; (add-hook 'lsp-bridge-ref-mode-hook (lambda()(add-hook 'evil-normal-state-entry-hook 'lsp-bridge-ref-switch-to-view-mode)))


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

;; (message "init-base configuration: %.2fs"
;;          (float-time (time-subtract (current-time) my/init-base-start-time)))


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
