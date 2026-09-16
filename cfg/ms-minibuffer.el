;;; -*- lexical-binding: t -*-

(use-package vertico
  :init
  (vertico-mode)
  :config
  (keymap-set vertico-map "C-f" #'vertico-scroll-up)
  (keymap-set vertico-map "C-b" #'vertico-scroll-down)
  (setq vertico-cycle t))

(use-package marginalia
  :init
  (marginalia-mode))

(provide 'ms-minibuffer)
