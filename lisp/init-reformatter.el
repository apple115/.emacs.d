;;; init-reformatter.el --- Langs settings -*- lexical-binding: t -*-
;;; Commentary: reformatter

;;; Code:
(use-package reformatter
  :ensure t
  :config
  ;;goimports-reviser -rm-unused -set-alias -format
  (reformatter-define goimport-reviser
    :program "goimports-reviser"
    :args '("-rm-unused" "-set-alias" "-format" "-output" "stdout"))
)

(provide 'init-reformatter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reformatter.el ends here
