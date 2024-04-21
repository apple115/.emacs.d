;;; init-exo-hugo.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: exo-hugo

;;; Code:
(use-package ox-hugo
  :ensure t
  :config
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
      (let* ((title (read-from-minibuffer "Post Title: ")) ; Prompt to enter the post title
             (fname (org-hugo-slug title)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title)
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ":END:"
                     "%?\n")          ; Place the cursor here finally
                   "\n")))

    (add-to-list 'org-capture-templates
                 '("h"                ; `org-capture' binding + h
                   "Hugo post"
                   entry
                   ;; It is assumed that below file is present in `org-directory'
                   ;; and that it has a "Blog Ideas" heading. It can even be a
                   ;; symlink pointing to the actual location of capture.org!
                   (file+olp "capture.org" "Notes")
                   (function org-hugo-new-subtree-post-capture-template))))
  )



(provide 'init-exo-hugo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-exo-hugo.el ends here
