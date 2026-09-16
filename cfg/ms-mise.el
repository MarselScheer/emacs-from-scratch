;;; -*- lexical-binding: t -*-

;; https://mise.jdx.dev/ide-integration.html#emacs
;; CLI tools installed by Mise
;; See: https://www.emacswiki.org/emacs/ExecPath
(setenv "PATH" (concat (getenv "PATH") ":/home/m/.local/share/mise/shims"))
(setq exec-path (append exec-path '("/home/m/.local/share/mise/shims")))

(provide 'ms-mise)
