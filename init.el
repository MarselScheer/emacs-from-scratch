;;; -*- lexical-binding: t -*-
;; instructions from the straight README
(defvar bootstrap-version)
(let ((bootstrap-file
	    (expand-file-name
	     "straight/repos/straight.el/bootstrap.el"
	     (or (bound-and-true-p straight-base-dir)
		 user-emacs-directory)))
	   (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
	 (with-current-buffer
	     (url-retrieve-synchronously
	      "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	      'silent 'inhibit-cookies)
	   (goto-char (point-max))
	   (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(add-to-list 'load-path (expand-file-name "cfg" user-emacs-directory))
(defun ms/require (feature)
  "Load FEATURE from `cfg/custom/' if it exists, else from `cfg'."
  (let* ((base (expand-file-name "cfg" user-emacs-directory))
         (custom-file (expand-file-name (format "custom/%s.el" feature) base))
         (default-file (expand-file-name (format "%s.el" feature) base)))
    (cond
     ((file-exists-p custom-file)
      (load custom-file))
     ((file-exists-p default-file)
      (load default-file))
     (t
      (error "No configuration file found for %s" feature)))))

(ms/require 'ms-basic-bindings)
(ms/require 'ms-evil)
(ms/require 'ms-org)
(ms/require 'ms-minibuffer)
(ms/require 'ms-completion)
(ms/require 'ms-search)
(ms/require 'ms-help)
(ms/require 'ms-project)
(ms/require 'ms-git)
(ms/require 'ms-lsp)
(ms/require 'ms-snippets)
(ms/require 'ms-yaml)
(ms/require 'ms-denote)
(ms/require 'ms-gptel)
(ms/require 'ms-undo)
(ms/require 'ms-embark)
(ms/require 'ms-jump)
(ms/require 'ms-eca)
(ms/require 'ms-eshell)
(ms/require 'ms-timetable)
(ms/require 'ms-openspecmacs)
(ms/require 'ms-python)
(ms/require 'ms-bookmark)
(ms/require 'ms-mise)
(ms/require 'ms-compilation)
(ms/require 'ms-ui)
(ms/require 'ms-backup)
(ms/require 'ms-experimental)

