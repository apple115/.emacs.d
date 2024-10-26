;;; init-git.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: git

;;; Code:
(use-package with-editor
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t))


(provide 'init-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
