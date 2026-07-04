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
  ;; 性能优化：减少 status 中 recent commits 数量
  (magit-log-section-commit-count 5)
  ;; commit buffer 不显示 diff
  (magit-commit-show-diff nil)
  ;; 关闭刷新 verbose 日志
  (magit-refresh-verbose nil)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-refresh-status-buffer nil)
  ;; 禁用 git pager，避免 magit 调用 git 时阻塞
  (setenv "GIT_PAGER" "cat")
  ;; 减少 git 锁竞争
  (setenv "GIT_OPTIONAL_LOCKS" "0")
  ;; Windows 需要指定 git 路径，macOS/Linux 用系统默认
  (when +is-win-p
    (setq magit-git-executable "C:\\Program Files\\Git\\bin\\git.exe"))
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)

  ;; 用轻量级 name-status 列表替换 status 里的完整 diff 渲染
  (defconst my-magit--status-alist
    '(("M" "modified" . (:foreground "#f9e2af"))
      ("A" "new file" . (:foreground "#a6e3a1"))
      ("D" "deleted"  . (:foreground "#f38ba8"))
      ("R" "renamed"  . (:foreground "#89b4fa"))
      ("C" "copied"   . (:foreground "#94e2d5"))
      ("U" "unmerged" . (:foreground "#cba6f7"))))

  (defun my-magit--wash-diff (line)
    (let* ((parts (split-string line "\t"))
           (code (car parts))
           (file (cadr parts))
           (info (cdr (assoc code my-magit--status-alist)))
           (status (if info (car info) code))
           (face (if info (cdr info) 'magit-diff-file-heading)))
      (when file
        (magit-insert-section (file file)
          (insert (propertize
                   (concat (format "%-10s" status) file "\n")
                   'font-lock-face face))))))

  (defun my-magit-insert-unstaged-files ()
    (let ((files (cl-remove-if #'string-empty-p (magit-git-lines "diff" "--name-status"))))
      (when files
        (magit-insert-section
            (unstaged 'unstaged)
          (magit-insert-heading "Unstaged changes:")
          (dolist (line files) (my-magit--wash-diff line))))))

  (defun my-magit-insert-staged-files ()
    (let ((files (cl-remove-if #'string-empty-p (magit-git-lines "diff" "--cached" "--name-status"))))
      (when files
        (magit-insert-section
            (staged 'staged)
          (magit-insert-heading "Staged changes:")
          (dolist (line files) (my-magit--wash-diff line))))))

  (setq magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-untracked-files
          my-magit-insert-unstaged-files
          my-magit-insert-staged-files
          magit-insert-stashes
          magit-insert-recent-commits)))

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

(use-package vc-msg
  :ensure t
  :bind (("C-x v m" . vc-msg-show)           ;; 当前行 commit 信息
         )   ;; 查看 parent commit
  :config
  ;; 显示后自动复制 commit hash 到 kill ring
  (setq vc-msg-copy-id-to-kill-ring t)

  ;; 如果你装了 magit，可以在弹窗里按 d 直接打开 magit-diff
  (setq vc-msg-show-commit-function 'magit-show-commit)

  ;; 自定义显示格式（可选）
  (setq vc-msg-format
        "Author: %a%nDate: %d%nSummary: %s%nHash: %H"))

(provide 'init-git)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-git.el ends here
