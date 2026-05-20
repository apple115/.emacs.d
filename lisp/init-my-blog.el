;;; init-my-blog.el --- Blog management -*- lexical-binding: t -*-
;;; Commentary:
;;; Create and open blog posts/notes

;;; Code:

(defconst my-blog-dir "/Users/apple115/Develop/my-blog/content"
  "Blog content root directory.")

(defun my-blog-new-post (title)
  "Create a new blog post with TITLE and open it."
  (interactive "sPost title: ")
  (let* ((slug (replace-regexp-in-string " +" "-" (downcase title)))
         (filename (concat my-blog-dir "/posts/" slug ".md"))
         (date (format-time-string "%Y-%m-%d %H:%M:%S")))
    (if (file-exists-p filename)
        (find-file filename)
      (find-file filename)
      (insert (format "---\ntitle: \"%s\"\ndate: %s\ntags:\n  - \n---\n\n# %s\n" title date title))
      (goto-char (point-max)))))

(defun my-blog-new-note (title)
  "Create a new blog note with TITLE and open it."
  (interactive "sNote title: ")
  (let* ((slug (replace-regexp-in-string " +" "-" (downcase title)))
         (filename (concat my-blog-dir "/notes/" slug ".md"))
         (date (format-time-string "%Y-%m-%d")))
    (if (file-exists-p filename)
        (find-file filename)
      (find-file filename)
      (insert (format "title: %s\ndate: %s\ntags:\n  - \nexcerpt: \n\n" title date)))))

(defun my-blog-open-posts ()
  "Open posts directory in dired."
  (interactive)
  (dired (concat my-blog-dir "/posts/")))

(defun my-blog-search ()
  "Search blog content with consult-grep."
  (interactive)
  (let ((default-directory my-blog-dir))
    (consult-ripgrep my-blog-dir nil)))

(defun my-blog-open-notes ()
  "Open notes directory in dired."
  (interactive)
  (dired (concat my-blog-dir "/notes/")))

(provide 'init-my-blog)
;;; init-my-blog.el ends here
