;;; -*- lexical-binding: t -*-

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
	;; NOTE: This requires an evil version from 2018-03-20 or later
	(evil-get-auxiliary-keymap my-intercept-mode-map state t t)
	state))

(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC SPC") 'execute-extended-command)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b f") 'find-file)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b b") 'consult-buffer)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b k") 'kill-buffer)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b o") 'consult-buffer-other-window)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC w") 'evil-window-map)

;; close emacs
(global-set-key (kbd "C-x C-c") nil)

(provide 'ms-basic-bindings)
