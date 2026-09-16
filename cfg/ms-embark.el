;;; -*- lexical-binding: t -*-

(use-package embark
  :bind(:map minibuffer-local-map
	     ("C-c C-c" . embark-collect)))
(use-package embark-consult)

(provide 'ms-embark)
