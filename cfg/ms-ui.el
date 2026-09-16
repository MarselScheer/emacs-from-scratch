;;; -*- lexical-binding: t -*-

(use-package expand-region)
(define-key evil-motion-state-map (kbd "e") 'er/expand-region)

;; show vertical line
(use-package indent-guide
  :config (indent-guide-global-mode))

;; key-bindings in minibuffer
(use-package which-key
  :config
  (which-key-mode))

;; where is the cursor
(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-duration 2))

;; Visual wrap prefix (indent wrapped lines appropriately)
(global-visual-line-mode 1)
(global-visual-wrap-prefix-mode 1)

;; Remove various UI-elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

;; Add additional UI-info
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(savehist-mode 1)

;; theme
(use-package doom-themes)
;; (load-theme 'doom-dracula t)
;; (use-package cyberpunk-theme)
;; (load-theme 'cyberpunk t)
;; (consult-theme 'deeper-blue)
(load-theme 'modus-vivendi-tinted t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:extend t :background "gray10" :foreground "yellow" :box nil :weight bold)))))

;; modeline
;; (use-package doom-modeline
;;  :init (doom-modeline-mode 0))
(use-package telephone-line)
(telephone-line-mode 1)

(provide 'ms-ui)
