;;; init-base.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 杨宇辰
;;
;; Author: 杨宇辰 <apple115@apple115.local>
;; Maintainer: 杨宇辰 <apple115@apple115.local>
;; Created: August 03, 2025
;; Modified: August 03, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/apple115/init-base
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(use-package general
  :ensure t
  :demand t
  :config
  (general-evil-setup))

;; (use-package cl-lib
;;   :ensure t
;;   :demand t)

(elpaca-wait)

(use-package transient :ensure t)


(provide 'init-base)
;;; init-base.el ends here
