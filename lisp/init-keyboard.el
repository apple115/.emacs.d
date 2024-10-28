;;; init-keyboard.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: keyboard

;;; Code:
(with-eval-after-load 'general
  (+leader-keys
    "SPC" '(consult-fd :wk "find file")
    "=" '(+format-code-and-flycheck :wk "flycheck and format")
    "/" '(split-window-horizontally :wk"split window horizontally")
    "-" '(split-window-vertically :wk"split window vertically")
    "." '(find-file :wk "find file")
    "," '(dired-jump :wk "open-dired")

    "1" 'sort-tab-select-visible-tab
    "2" 'sort-tab-select-visible-tab
    "3" 'sort-tab-select-visible-tab
    "4" 'sort-tab-select-visible-tab
    "5" 'sort-tab-select-visible-tab
    "6" 'sort-tab-select-visible-tab
    "7" 'sort-tab-select-visible-tab
    "8" 'sort-tab-select-visible-tab
    "9" 'sort-tab-select-visible-tab
    "0" 'sort-tab-select-visible-tab

    "w" '(:ignore :wk "window")
    "w 0" '(delete-window :wk "delete-window")
    "w 9" '(delete-other-windows :wk "delete-other-windows")

    "g"'(:ignore t :wk "goto")
    "g c" '((lambda () (interactive) (find-file "~/.emacs.d")) :wk "Edit emacs config")
    "g s" '((lambda () (interactive) (find-file "~/.emacs.d/snippets")) :wk "Edit emacs snippet")
    "g b" '((lambda () (interactive) (find-file "~/blog")) :wk "blog")

    "f" '(:ignore t :wk "file")
    "f f" '(consult-fd :wk "find file")
    "f /" '(+consult-fd-other-window :wk "find file on other window")
    "f R" '(+rename-current-file :wk "rename and move current file")
    "f D" '(+delete-current-file :wk "delete current file")
    "f y" '(+copy-current-filename :wk "copy current filename")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file")

    "s" '(:ignore t :wk "Search")
    "s o" '(consult-ripgrep :wk "search word")
    "s e" '(consult-compile-error :wk "search word")
    "s g" '(engine/search-google :wk "search google")
    "s m" '(consult-man :wk "search man")
    "s n" '(consult-notes :wk "search notes")
    "s t" '(citre-query-jump :wk "find tags")
    "s w" '(fanyi-dwim :wk "search word")

    "b" '(:ignore t :wk "buffer")
    "b b" '(consult-buffer :wk "buffer-switch")
    "b ," '(switch-to-prev-buffer :wk "prev-buffer")
    "b ." '(switch-to-next-buffer :wk "next-buffer")
    "b /" '(consult-buffer-other-window :wk "Switch buffer to other window")
    "b k" '(kill-buffer :wk "kill buffer")
    "b i" '(ibuffer :wk "ibuffer")
    "b r" '(revert-buffer :wk "Reload buffer")

    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")

    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '(my-load-config :wk "Reload Emacs config")

    "t" '(:ignore t :wk "Toggle")
    "t t" '(my-open-termial-kitty :wk "open terminal")
    "t o" '(eglot :wk "open eglot")

    "o" '(:ignore t :wk "open")
    "o o" '(embark-act :wk "embark-act")
    "o e" '(compile :wk "compile")
    "o t" '(+new-vtermN :wk "open vtermN")
    "o s" '(async-shell-command :wk "open async shell command")
    "o c" '((lambda () (interactive) (org-capture)) :wk "open org-capture")
    "o a" '((lambda () (interactive) (org-agenda)) :wk "open org-agenda")
    "o b" '(hexo-my-blog  :wk "open hexo")
    "o z" '(link-hint-open-link  :wk "open link-hint")

    "x" '(:ignore t :wk "fix or delete")
    "x x" '(lsp-bridge-diagnostic-list :wk "show diagnostic list")
    "x c" '(lsp-bridge-diagnostic-copy :wk "copy diagnostic list")

    "p" '(:ignore t :wk "project")
    "p p" '(projectile-switch-project :wk "project switch project")
    "p f" '(projectile-find-file :wk "project find file")
    "p d" '(projectile-dired :wk "project dired")
    "p b" '(projectile-switch-to-buffer :wk "project switch buffer")
    "p k" '(projectile-kill-buffers :wk "kill projectile buffers")
    "p r" '(projectile-run-project :wk "run project")
    "p t" '(projectile-test-project :wk "test project")
    "p c" '(projectile-compile-project :wk "compile project")

    "d" '(:ignore t :wk "denote or dired")
    "d d" '(pwd :wk "pwd")
    "d j" '(denote-find-link :wk"find link")
    "d n" '(denote :wk "create denote")
    "d t" '(denote-type :wk "creates a note while prompting for a file type")
    "d f" '(denote-open-or-create :wk "find denote")
    "d r" '(denote-dired-rename-file :wk "rename denote")

    "c" '(:ignore t :wk "compile")
    "c r"'(recompile :wk "recompile")
    "c k"'(kill-compilation :wk "kill compile")

    "l" '(:ignore t :wk "lsp")
    "l n"'(lsp-bridge-rename :wk "rename")
    "l a"'(lsp-bridge-code-action :wk "code action")
    ))


(with-eval-after-load 'evil-collection
;;lsp-bridge
(evil-collection-define-key 'normal 'lsp-bridge-ref-mode-map (kbd "q") #'lsp-bridge-ref-quit)
(evil-collection-define-key 'normal 'lsp-bridge-ref-mode-map (kbd "C-n") #'lsp-bridge-ref-jump-next-keyword)
(evil-collection-define-key 'normal 'lsp-bridge-ref-mode-map (kbd "C-p") #'lsp-bridge-ref-jump-prev-keyword)

(evil-collection-define-key 'insert 'lsp-bridge-mode-map
    (kbd "C-n") 'acm-select-next
    (kbd "C-p") 'acm-select-prev
)

(evil-collection-define-key 'normal 'lsp-bridge-mode-map
  (kbd "K")  'lsp-bridge-show-documentation
  (kbd "gd") 'lsp-bridge-find-def
  (kbd "gi") 'lsp-bridge-find-impl
  (kbd "go") 'lsp-bridge-find-def-return
  (kbd "]d") 'lsp-bridge-diagnostic-jump-next
  (kbd "[d") 'lsp-bridge-diagnostic-jump-prev
)
)


(provide 'init-keyboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keyboard.el ends here
