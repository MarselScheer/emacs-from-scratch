;;; -*- lexical-binding: t -*-

(defun ms/save-buffer-and-recompile ()
  "Saves the current buffer and runs project-recompile"
  (interactive)
  (progn
    (ms/save-current-and-all-org-buffers)
    (project-recompile)))
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC p s r") 'consult-ripgrep)
  (define-key evil-motion-state-map (kbd "SPC p c") 'project-compile)
  (define-key evil-motion-state-map (kbd "SPC p r") 'ms/save-buffer-and-recompile)
  (define-key evil-motion-state-map (kbd "SPC p p") 'project-switch-project)
  (define-key evil-motion-state-map (kbd "SPC p k") 'project-kill-buffers)
  (define-key evil-motion-state-map (kbd "SPC p b") 'consult-project-buffer)
  (define-key evil-motion-state-map (kbd "SPC p f") 'project-find-file)
  (define-key evil-motion-state-map (kbd "SPC p e") 'project-eshell)
  (setq project-vc-extra-root-markers '(".project.el")))

(provide 'ms-project)
