;;; -*- lexical-binding: t -*-

(use-package ace-jump-mode
  :config
  (define-key evil-motion-state-map (kbd "SPC s") 'ace-jump-word-mode))

;; s -> type 2 chars and jump there
(use-package evil-snipe
  :config (evil-snipe-mode))

;; region -> *, # search forward backward
(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent 1))

(provide 'ms-jump)
