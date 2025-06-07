;;; init-git.el --- Git settings -*- lexical-binding: t -*-
;;; Commentary:
;;; git related settings for Emacs

;;; Code:
(use-package with-editor
  :ensure t)

(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  ;; (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
    (if (fboundp 'fringe-mode) (fringe-mode '(6 . 8)))
    ;; thin fringe bitmaps
    (define-fringe-bitmap 'git-gutter-fr:added [#b111111] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [#b111111] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [#b111111] nil nil '(center repeated)))



(provide 'init-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
