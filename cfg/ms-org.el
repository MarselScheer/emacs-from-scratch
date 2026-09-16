;;; -*- lexical-binding: t -*-

(use-package org
  ;; :bind
  ;; (:map org-agenda-mode-map
  ;; 	("j" . org-agenda-next-line)
  ;; 	("k" . org-agenda-previous-line))
  :config
  (setq org-M-RET-may-split-line nil)
  (setq org-insert-heading-respect-content t)
  (setq org-agenda-files (directory-files "~/syncthing/orgfiles" 1 "org$"))
  (setq org-log-done "time")
  (setq org-todo-keywords '((sequence "TODO(t)" "CONT(c)" "WAIT(w)" "REMINDER(r)" "FLEETING(f)" "|" "DONE(d)" "ABORTED(a)")))
  (setq org-log-note-headings '((done . "CLOSING NOTE %t")
				(state . "State %-12s from %-12S %t")
				;; (note . "Note taken on %t")
				(note . "%d")
				(reschedule . "Rescheduled from %S on %t")
				(delschedule . "Not scheduled, was %S on %t")
				(redeadline . "New deadline from %S on %t")
				(deldeadline . "Removed deadline, was %S on %t")
				(refile . "Refiled on %t")
				(clock-out . "")))
  (setq org-capture-templates
	'(("f"                    ; ← shortcut key
	   "Fleeting note with region" ; ← description shown in the capture menu
	   entry                  ; ← type of item (plain text, entry, etc.)
	   (file+headline "~/syncthing/orgfiles/20251218T150014--fleeting-notes__general.org" "Inbox")
	   "* FLEETING %u: %? %t\n  #+begin_src\n%i\n#+end_src\n origin: file:%F")
	  ("p"                    ; ← shortcut key
	   "Fleeting note plain"        ; ← description shown in the capture menu
	   entry                  ; ← type of item (plain text, entry, etc.)
	   (file+headline "~/syncthing/orgfiles/20251218T150014--fleeting-notes__general.org" "Inbox")
	   "* FLEETING %u: %? %t\norigin: file:%F")))
  (setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "python")))))
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)))

(defun ms/save-current-and-all-org-buffers ()
  "Save the current buffer and all open Org mode buffers."
  (interactive)
  (save-buffer)
  (org-save-all-org-buffers))

(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC o a") 'org-agenda)
  (define-key evil-motion-state-map (kbd "SPC o i") 'org-indent-mode)
  (define-key evil-motion-state-map (kbd "SPC o f") 'org-cycle)
  (define-key evil-motion-state-map (kbd "SPC o c") 'org-toggle-checkbox)
  (define-key evil-motion-state-map (kbd "SPC o s") 'ms/save-current-and-all-org-buffers)
  (define-key evil-motion-state-map (kbd "SPC o h") 'org-toggle-heading)
  (define-key evil-motion-state-map (kbd "SPC n s") 'org-narrow-to-subtree)
  (define-key evil-motion-state-map (kbd "SPC n w") 'widen)
  (define-key evil-motion-state-map (kbd "SPC n t") 'org-show-todo-tree)
  (define-key evil-motion-state-map (kbd "SPC o n r") (lambda () (interactive) (org-capture nil "f")))
  (define-key evil-motion-state-map (kbd "SPC o n p") (lambda () (interactive) (org-capture nil "p"))))

(provide 'ms-org)
