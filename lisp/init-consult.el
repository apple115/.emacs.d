;;; init-consult.el --- consult settings -*- lexical-binding: t -*-
;;; Commentary:
;;; consult

;;; Code:
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
 )
 :config
(setq consult-locate-command "mdfind -name ARG OPTS")
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
)

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

(provide 'init-consult)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-consult.el ends here
