;;; -*- lexical-binding: t -*-

(use-package time-table
  :straight (time-table :type git :host github :repo "MarselScheer/time-table" :branch "time-table-buffer")
  :custom
  (time-table-work-hours 0)
  (time-table-file "~/syncthing/orgfiles/tracked-times")
  (time-table-project-names '("book" "kg" "time-table" "emacs" "backup" "end"))
  (time-table-task-names '("code" "tex" "math" "end")))
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC t t") 'time-table-track)
  (define-key evil-motion-state-map (kbd "SPC t s") 'time-table-status)
  (define-key evil-motion-state-map (kbd "SPC t S") 'time-table-summarize-projects-last-7-days)
  (define-key evil-motion-state-map (kbd "SPC t b") 'time-table-find-tracking-file)
  (define-key evil-motion-state-map (kbd "SPC t e") 'time-table-end-tracking))

(provide 'ms-timetable)
