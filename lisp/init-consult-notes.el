;;; init-consult-notes.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: consult-notes

;;; Code:
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

  ;; embark support
;;   (with-eval-after-load 'embark
;;     (defun consult-notes-open-dired (cand)
;;       "Open notes directory dired with point on file CAND."
;;       (interactive "fNote: ")
;;       ;; dired-jump is in dired-x.el but is moved to dired in Emacs 28
;;       (dired-jump nil cand))

;;     (defun consult-notes-marked (cand)
;;       "Open a notes file CAND in Marked 2.
;; Marked 2 is a mac app that renders markdown."
;;       (interactive "fNote: ")
;;       (call-process-shell-command (format "open -a \"Marked 2\" \"%s\"" (expand-file-name cand))))

;;     (defun consult-notes-grep (cand)
;;       "Run grep in directory of notes file CAND."
;;       (interactive "fNote: ")
;;       (consult-grep (file-name-directory cand)))

;;     (embark-define-keymap consult-notes-map
;;                           "Keymap for Embark notes actions."
;;                           :parent embark-file-map
;;                           ("d" consult-notes-dired)
;;                           ("g" consult-notes-grep)
;;                           ("m" consult-notes-marked))

;;     (add-to-list 'embark-keymap-alist `(,consult-notes-category . consult-notes-map))

;;     ;; make embark-export use dired for notes
;;     (setf (alist-get consult-notes-category embark-exporters-alist) #'embark-export-dired)
;;     )
   )


(provide 'init-consult-notes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-consult-notes.el ends here
