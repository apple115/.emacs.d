;;; init-project.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary:
;;; project management and workspace handling in Emacs.

;;; Code:
(use-package project
 :ensure nil
 :general
  (:keymaps 'override
   :states '(normal visual)
   :prefix  "SPC"
    "p" '(:ignore t :wk "project")
    "p d" '(project-dired :wk "project dired")
    "p k " '(project-forget-project :wk "project forget")
    "p c " '(project-compile :wk "project compile ")
    "p r " '(project-remember-projects-under :wk "project remember")
  )
  :config
;; (defun my/project-try-local (dir)
;; "Determine if DIR is a non-Git project."
;; (catch 'ret
;;     (let ((pr-flags '((".project")
;;                     ("go.mod" "Cargo.toml" "project.clj" "pom.xml" "package.json") ;; higher priority
;;                     ("Makefile" "README.org" "README.md"))))
;;     (dolist (current-level pr-flags)
;;         (dolist (f current-level)
;;         (when-let ((root (locate-dominating-file dir f)))
;;             (throw 'ret (cons 'local root))))))))
;; (setq project-find-functions '(my/project-try-local project-try-vc))
;; (setq project-vc-ignores'("nix/store/"  "node_modules/"  "go/pkg/"  ".direnv/" "vendor/"))
)

;; 添加启动
(use-package tabspaces
  :ensure t
  :hook (after-init-hook . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :general
  (:keymaps 'override
   :states '(normal visual)
   :prefix  "SPC"
    "p" '(:ignore t :wk "project")
    "p p" '(tabspaces-open-or-create-project-and-workspace :wk "find project")
    "p q" '(tabspaces-kill-buffers-close-workspace :wk"kill project buffer")
    "p s" '(tabspaces-save-current-project-session :wk"save project sessions")
    "p r" '(tabspaces-restore-session :wk"restore project sessions")
  )
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo nil)
  ;; (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  ;; (tabspaces-session t)
  ;; (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*"))
  :config
;; (add-hook 'after-init-hook  tabspaces-mode)
(with-eval-after-load 'consult
;; hide full buffer list (still available with "b" prefix)
(consult-customize consult--source-buffer :hidden t :default nil)

(defun consult--filter-sort-tab-buffers (buf)
  "Filter out the *sort-tab* buffer and check if buffer is local to tabspace."
  (and (not (string= (buffer-name buf) "*sort-tab*"))
       (tabspaces--local-buffer-p buf)))

;; set consult-workspace buffer list
(defvar consult--source-workspace
  (list :name     "Workspace Buffers"
        :narrow   ?w
        :history  'buffer-name-history
        :category 'buffer
        :state    #'consult--buffer-state
        :default  t
        :items    (lambda () (consult--buffer-query
                         :predicate #'consult--filter-sort-tab-buffers
                         :sort 'visibility
                         :as #'buffer-name)))

  "Set workspace buffer list for consult-buffer.")
(add-to-list 'consult-buffer-sources 'consult--source-workspace))


(provide 'init-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-project.el ends here
