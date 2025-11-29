;;; init-base.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 杨宇辰
;;
;; Author: 杨宇辰 <apple115@apple115.local>
;; Maintainer: 杨宇辰 <apple115@apple115.local>
;; Created: August 03, 2025
;; Modified: August 03, 2025
;; Version: 0.0.1
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

(use-package transient 
  :ensure t
  :demand t)

(provide 'init-base)
;;; init-base.el ends here
