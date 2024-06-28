;;; init-doom-modeline.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: doom-modeline

;;; Code:
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
(setq doom-modeline-modal t)
(setq doom-modeline-height 25)
  )


(provide 'init-doom-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-doom-modeline.el ends here
