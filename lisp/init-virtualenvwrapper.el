;;; init-virtualenvwrapper.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: virtualenvwrapper

;;; Code:
(use-package virtualenvwrapper
 :ensure t
:config
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "/home/apple115/.cache/pypoetry/virtualenvs/")
)

(provide 'init-virtualenvwrapper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-virtualenvwrapper.el ends here
