;;; -*- lexical-binding: t -*-


(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(provide 'ms-yaml)
