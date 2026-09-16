;;; -*- lexical-binding: t -*-

(use-package openspecmacs
  :straight (openspecmacs :type git
			  :host github
			  :repo "MarselScheer/openspecmacs"
			  :branch "main"))

(with-eval-after-load 'evil
  (evil-define-key 'normal openspecmacs-mode-map
    "d" #'openspecmacs-show-change-diff))

(provide 'ms-openspecmacs)
