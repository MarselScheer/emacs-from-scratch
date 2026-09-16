;;; -*- lexical-binding: t -*-

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(provide 'ms-undo)
