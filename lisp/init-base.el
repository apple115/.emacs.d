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
  (general-evil-setup)
  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))


(use-package transient
  :ensure t
  :demand t)

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(provide 'init-base)
;;; init-base.el ends here
