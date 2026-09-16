;;; -*- lexical-binding: t -*-

(use-package lsp-mode
  ;; code also comes from https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq xref-auto-jump-to-first-definition nil) ;; necessary in older emacs for "find-definition"-functionality
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (python-mode . lsp) ;; install ty via uv
  :config
  (lsp-enable-which-key-integration t)
  (setq read-process-output-max (* 2 1024 1024))
  (setq gc-cons-threshold (* 2 800000))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.poetry\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.metaflow\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.local\\'"))
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC l d") 'lsp-describe-thing-at-point)
  (define-key evil-motion-state-map (kbd "SPC l r") 'lsp-rename)
  (define-key evil-motion-state-map (kbd "SPC l f") 'lsp-find-definition)
  (define-key evil-motion-state-map (kbd "SPC l u") 'lsp-find-references)
  (define-key evil-motion-state-map (kbd "SPC l c") 'comment-or-uncomment-region))

;; Enables "pop-up's" with additional information about
;; the current symbol or documentation for functions, etc.
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp))))  ; or lsp-deferred

(use-package flycheck)

(provide 'ms-lsp)
