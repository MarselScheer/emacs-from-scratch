;;; -*- lexical-binding: t -*-

(use-package denote
  :config
  (setq denote-journal-extras-title-format 'year-month-day)
  (setq denote-directory "~/syncthing/orgfiles"))

(provide 'ms-denote)
