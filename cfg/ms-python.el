;;; -*- lexical-binding: t -*-

;; in order to configure the built-in python mode use
(use-package python
  :config
  (define-key python-mode-map (kbd "<backtab>") 'indent-for-tab-command)
  (define-key python-mode-map (kbd "TAB") 'completion-at-point))
;; (use-package python-mode
;;   :hook (python-mode . (lambda ()
;; 			 (define-key python-mode-map (kbd "TAB") 'completion-at-point)
;; 			 (lsp-deferred))))
  ;; :config
  ;; ;; (require 'dap-python)
  ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r i") 'py-switch-to-shell)
  ;; ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r b") 'ms/py-execute-buffer)
  ;; ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r c") 'ms/py-execute-class)
  ;; ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r r") 'ms/py-execute-region)
  ;; (setq py-split-window-on-execute nil))
  ;; ;; (setq dap-python-debugger 'debugpy)
;; (define-key python-mode-map (kbd "TAB") 'completion-at-point)

(setq python-shell-interpreter "uv")
(setq python-shell-interpreter-args "run python -i")

(use-package ruff-format)
(add-hook 'python-mode-hook 'ruff-format-on-save-mode)

(provide 'ms-python)
