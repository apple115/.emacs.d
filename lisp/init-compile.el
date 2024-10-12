;;; init-compile.el --- compile settings -*- lexical-binding: t -*-
;;;Commentary:

;;; Code:
(setq-default compilation-scroll-output t)

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


(provide 'init-compile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-compile.el ends here
