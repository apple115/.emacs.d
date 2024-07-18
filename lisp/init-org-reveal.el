;;; init-org-reveal.el --- Tools settings -*- lexical-binding: t -*-
;;; Commentary: org-reveal

;;; Code:
(use-package ox-reveal
    :ensure t
    :config
    (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
    (reveal-mode 1)
    )

(provide 'init-org-reveal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-reveal.el ends here
