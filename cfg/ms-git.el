;;; -*- lexical-binding: t -*-

(use-package magit)
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC g g") 'magit-status)
  (define-key evil-motion-state-map (kbd "SPC g b") 'magit-blame)
  (define-key evil-motion-state-map (kbd "SPC g d") 'magit-diff-dwim))

(defun ms/generate-git-commit-msg ()
  "Generate a commit message using git diff and a python script."
  (interactive)
  (let* ((diff-file "/tmp/git-diff")
	 (msg-file "/tmp/commit-msg")
	 (cmd (format "cd /home/m/.emacs.d/dspy-commit && uv run main.py %s %s" diff-file msg-file))
	 (target-buffer (current-buffer))
	 (sentinel (lambda (proc event)
		     (when (and (eq (process-status proc) 'exit)
				(zerop (process-exit-status proc))
				(file-exists-p msg-file))
		       (with-current-buffer target-buffer
			 (goto-char (point-min))
			 (insert-file-contents msg-file))
		       (let ((orig-bg (face-remap-add-relative 'default :background "DarkBlue")))
			 (run-with-timer 0.5 nil
					 (lambda (bg) (face-remap-remove-relative bg))
					 orig-bg))
		       (message "Commit message generated")))))
    ;; Generate diff file synchronously (fast)
    (shell-command (format "git diff --staged > %s" diff-file))
    ;; Run the Python script asynchronously using make-process
    (let ((proc (make-process
		 :name "gen-commit-msg"
		 :command (list "sh" "-c" cmd)
		 :noquery t
		 :sentinel sentinel)))
      ;; Set the process to run in the background without blocking
      (set-process-filter proc 'ignore))))
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC g m") 'ms/generate-git-commit-msg))

(use-package difftastic
  :defer t
  :straight (:host github :repo "pkryger/difftastic.el"))

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package git-messenger
  :config (setq git-messenger:show-detail t))


(provide 'ms-git)
