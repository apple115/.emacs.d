;;; init-docker.el --- docker settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(use-package docker
  :ensure t
  :bind
  ("C-c d" . docker)
)


(provide 'init-docker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-docker.el ends here
