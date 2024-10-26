;;; init-consult.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: consult

;;; Code:
(use-package consult
 :ensure t
 :bind
 :config
(setq read-file-name-function #'consult-find-file-with-preview)

(defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
  (interactive)
  (let ((default-directory (or dir default-directory))
        (minibuffer-completing-file-name t))
    (consult--read #'read-file-name-internal :state (consult--file-preview)
                   :prompt prompt
                   :initial initial
                   :require-match mustmatch
                   :predicate pred))
))

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
          ("denote" ?d ,(concat org-directory "/denote/"))
          ("org"     ?o ,(concat org-directory "/"))
          ("blog"    ?b  "/home/apple115/blog/source/_posts/")
          ;; ("books"   ?b ,(concat (getenv "HOME") "/Books/"))
          ))

   )

(provide 'init-consult)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-consult.el ends here
