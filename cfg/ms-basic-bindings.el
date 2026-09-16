;;; -*- lexical-binding: t -*-

(with-eval-after-load 'evil
(evil-define-key 'normal global-map (kbd "SPC SPC") 'execute-extended-command)
(evil-define-key 'normal global-map (kbd "SPC b f") 'find-file)
(evil-define-key 'normal global-map (kbd "SPC b b") 'consult-buffer)
(evil-define-key 'normal global-map (kbd "SPC b k") 'kill-buffer)
(evil-define-key 'normal global-map (kbd "SPC b o") 'consult-buffer-other-window)
(evil-define-key 'normal global-map (kbd "SPC w") 'evil-window-map))

;; close emacs
(global-set-key (kbd "C-x C-c") nil)

(provide 'ms-basic-bindings)
