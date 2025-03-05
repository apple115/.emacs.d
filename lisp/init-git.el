;;; init-git.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: git

;;; Code:
(use-package with-editor
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t))


(provide 'init-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
