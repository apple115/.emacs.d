;;; init-compile.el --- compile settings -*- lexical-binding: t -*-
;;;Commentary:

;;; Code:
(setq-default compilation-scroll-output t)

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(global-set-key [f6] 'recompile)


(provide 'init-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-compile.el ends here
