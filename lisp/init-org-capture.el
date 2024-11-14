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
                            ))
)

(provide 'init-org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-capture.el ends here
