;;; -*- lexical-binding: t -*-

;; improved help buffer
(use-package helpful
  :custom (counsel-describe-function-function #'helpful-callable)
  :bind ([remap describe-function] . #'helpful-callable))

;; examples in the help-buffer
(use-package elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(provide 'ms-help)
