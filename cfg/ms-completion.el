;;; -*- lexical-binding: t -*-

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)               ;; Disable auto completion because typing fast seems to collide with that
                                 ;; because sometimes the whole file get manipulated?!?!
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.3)
  :hook (eshell-mode . (lambda ()
			 (keymap-set corfu-map "RET" #'corfu-send)))
  :bind (:map corfu-map
	      ("ö" . corfu-insert-separator))
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package cape
  :init
  (add-hook 'prog-mode-hook (lambda () (add-hook 'completion-at-point-functions #'cape-file nil t)))
)

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(provide 'ms-completion)
