;;; -*- lexical-binding: t -*-
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq delete-old-versions t)
(setq version-control t)
(setq kept-new-versions 7)

(provide 'ms-backup)
