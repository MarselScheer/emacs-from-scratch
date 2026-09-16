;;; -*- lexical-binding: t -*-

(use-package evil
  :init
  ;; needed by evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; otherwise SPC SPC will not bring up M-x if for instance
  ;; a region is highlighted
  (define-key evil-motion-state-map " " nil)
  (evil-mode 1))

;; need to also have movement keys hjkl in some other packages like magit
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Prefer to use 'kj' to go back to normal mode
(use-package evil-escape
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj"))

(provide 'ms-evil)
