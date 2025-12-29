;;; init-keyboard.el --- global key settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; 确保 general 已加载
(require 'general)

(general-create-definer +leader-keys
  :states '(normal visual)
  :states 'nil
  :keymaps 'override
  :prefix "SPC" ;; set leader
  :global-prefix "C-SPC") ;; access leader in insert mode

(+leader-keys
    "SPC" '(consult-buffer :wk "find file")
    "=" '(+format-code-and-flycheck :wk "flycheck and format")
    "/" '(split-window-horizontally :wk"split window horizontally")
    "-" '(split-window-vertically :wk"split window vertically")
    "." '(find-file :wk "find file")
    "s ." '(sudo-edit-find-file :wk "Sudo find file")
    "," '(dired-jump :wk "open-dired")
    ";" '(popper-toggle :wk "open-pop")
    "'" '(popper-cycle :wk "cycle-pop")

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

    "g"'(:ignore t :wk "git")
    "g g" '(magit-status :wk"magit-status")

    "f" '(:ignore t :wk "file")
    "f /" '(+consult-fd-other-window :wk "find file on other window")
    "f R" '(+rename-current-file :wk "rename and move current file")
    "f D" '(+delete-current-file :wk "delete current file")
    "f y" '(+copy-current-filename :wk "copy current filename")
    "f U" '(sudo-edit :wk "Sudo edit file")

    "s" '(:ignore t :wk "Search")
    "s s" '(link-hint-open-link :wk "search link")
    "s g" '(engine/search-google :wk "search google")
    "s t" '(citre-query-jump :wk "find tags")

    "b" '(:ignore t :wk "buffer")
    "b b" '(consult-buffer :wk "buffer-switch")
    "b ," '(switch-to-prev-buffer :wk "prev-buffer")
    "b ." '(switch-to-next-buffer :wk "next-buffer")
    "b /" '(consult-buffer-other-window :wk "Switch buffer to other window")
    "b k" '(kill-current-buffer :wk "kill buffer")
    "b i" '(ibuffer :wk "ibuffer")
    "b r" '(revert-buffer :wk "Reload buffer")

    "e" '(:ignore t :wk "Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")

    "t" '(:ignore t :wk "Toggle")
    "t f" '(flychek-mode :wk "open flycheck")
    "t s" '(jinx-mode :wk "open jinx-mode")
    "t d" '(eldoc-box-hover-mode :wk "toggle eldoc-box auto display")
    "t n" '(tab-bar-new-tab :wk "one new tab")
    "t k" '(tab-close :wk "close current tab")

    "o" '(:ignore t :wk "open")
    "o o" '(embark-act :wk "embark-act")
    "o e" '(compile :wk "compile")
    "o t" '(vterm-toggle :wk "open vterm")
    "o s" '(async-shell-command :wk "open async shell command")
    "o c" '((lambda () (interactive) (org-capture)) :wk "open org-capture")
    "o a" '((lambda () (interactive) (org-agenda)) :wk "open org-agenda")
    "o b" '(hexo-my-blog  :wk "open hexo")
    "o f" '(dwim-shell-commands-macos-reveal-in-finder)

    "d" '(:ignore t :wk "dired")
    "d d" '(pwd :wk "pwd")

    "n" '(:ignore t :wk "new")
    "n n" '(denote :wk "new note")
    "n b" '(+hexo-new :wk "new blog")
    "n t" '(+new-vtermN :wk "new terminal")
    "n a t" '(my-open-termial-kitty :wk "open terminal")

    "x" '(:ignore t :wk "fix or delete"))

(provide 'init-keyboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keyboard.el ends here
