;;; init-git.el --- Git settings -*- lexical-binding: t -*-
;;; Commentary:
;;; git related settings for Emacs

;;; Code:
(use-package with-editor
  :ensure t
  :demand t)

(use-package magit
  :ensure t
  :after (transient with-editor)
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-git-executable "C:\\Program Files\\Git\\bin\\git.exe")
  (setq magit-refresh-status-buffer nil)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
)

;;; 配置来源 https://github.com/seagle0128/.emacs.d/blob/58a3beb7564c89733572ae361299cf5bb91b4c4c/lisp/init-highlight.el#L230
 ;; Highlight uncommitted changes using VC
;; 过度渲染了 损害性能
;; (use-package diff-hl
;;   :ensure t
;;   :custom (diff-hl-draw-borders nil)
;;   :custom-face
;;   (diff-hl-change ((t ( :background "#83a598"))))
;;   (diff-hl-insert ((t ( :background "#b8bb26"))))
;;   (diff-hl-delete ((t ( :background "#fb4934"))))
;;   :bind (:map diff-hl-command-map
;;          ("SPC" . diff-hl-mark-hunk))
;;   :hook ((after-init . global-diff-hl-mode)
;;          (after-init . global-diff-hl-show-hunk-mouse-mode)
;;          (dired-mode . diff-hl-dired-mode))
;;   :config
;;   ;; Highlight on-the-fly
;;   (diff-hl-flydiff-mode 1)

;;   ;; Set fringe style
;;   (setq-default fringes-outside-margins t)

;;   (with-no-warnings
;;     (defun my-diff-hl-fringe-bmp-function (_type _pos)
;;       "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
;;       (define-fringe-bitmap 'my-diff-hl-bmp
;;         (vector (if (eq system-type 'gnu/linux) #b11111100 #b11100000))
;;         1 8
;;         '(center t)))
;;     (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

;;     (unless (display-graphic-p)
;;       ;; Fall back to the display margin since the fringe is unavailable in tty
;;       (diff-hl-margin-mode 1)
;;       ;; Avoid restoring `diff-hl-margin-mode'
;;       (with-eval-after-load 'desktop
;;         (add-to-list 'desktop-minor-mode-table
;;                      '(diff-hl-margin-mode nil))))

;;     ;; Integration with magit
;;     (with-eval-after-load 'magit
;;       (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
;;       (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))



(provide 'init-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
