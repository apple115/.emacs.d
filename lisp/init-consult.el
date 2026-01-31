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
  "s r" '(consult-recent-file :wk "search recent file")
 )
 :config
(defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key "M-p")))

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult-source-recent-file consult-source-project-recent-file consult-source-bookmark)

    (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
      (interactive (list current-prefix-arg
                         (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (if-let* ((s (symbol-at-point)))
                               (symbol-name s)))))
      (consult-ripgrep dir initial))
    (sanityinc/no-consult-preview sanityinc/consult-ripgrep-at-point)
    (setq consult-locate-command "mdfind -name ARG OPTS")
)

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package wgrep
  :ensure t
  :config
  ;; 设置为 t，可以在保存 wgrep buffer 时自动保存受影响的文件
  (setq wgrep-auto-save-buffer t)
  ;; 修改完后直接用习惯的 C-x C-s 保存也可以（如果你绑定了的话）
  (define-key grep-mode-map (kbd "C-c C-p") 'wgrep-change-to-wgrep-mode))

(provide 'init-consult)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-consult.el ends here
