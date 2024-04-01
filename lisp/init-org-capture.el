;;; init-org-capture.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: org-capture

;;; Code:

  (use-package org-capture
    :ensure nil
    :hook ((org-capture-mode . (lambda ()
                                 (setq-local org-complete-tags-always-offer-all-agenda-tags t)))
           (org-capture-mode . delete-other-windows))
    :custom
    (org-capture-use-agenda-date nil)
    ;; define common template
    (org-capture-templates `(("t" "Tasks" entry (file+headline "tasks.org" "Reminders")
                              "* TODO %i%?"
                              :empty-lines-after 1
                              :prepend t)
                             ("n" "Notes" entry (file+headline "capture.org" "Notes")
                              "* %? %^g\n%i\n"
                              :empty-lines-after 1)
                             ;; For EWW
                             ("b" "Bookmarks" entry (file+headline "capture.org" "Bookmarks")
                              "* %:description\n\n%a%?"
                              :empty-lines 1
                              :immediate-finish t)
                             ("d" "Diary")
                             ("dt" "Today's TODO list" entry (file+olp+datetree "diary.org")
                              "* Today's TODO list [/]\n%T\n\n** TODO %?"
                              :empty-lines 1
                              :jump-to-captured t)
                             ("do" "Other stuff" entry (file+olp+datetree "diary.org")
                              "* %?\n%T\n\n%i"
                              :empty-lines 1
                              :jump-to-captured t)

                             ("l" "live")
                             ("lm" "watch movies" entry (file "20240328T131822--movies__entertainment.org")
                              "* "
                              :empty-lines 1
                              :jump-to-captured t)

                             ("lb" "read books" entry (file "20240328T131944--book__entertainment.org")
                              "* "
                              :empty-lines 1
                              :jump-to-captured t)
                             ))
    )

(provide 'init-org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-capture.el ends here
