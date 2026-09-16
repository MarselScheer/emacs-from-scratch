;;; -*- lexical-binding: t -*-

(use-package consult
  :bind (("C-s" . consult-line))) ;; search
(defun ms/consult-line-symbol-at-point ()
  "Search for a line matching the symbol found near point."
  (interactive)
  (consult-line
   (or (thing-at-point 'symbol))))
(defun ms/consult-line-last-search ()
  "Search for a line matching the symbol used during last search."
  (interactive)
  (consult-line
   (or (car consult--line-history))))
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC f p") 'ms/consult-line-symbol-at-point)
  (define-key evil-motion-state-map (kbd "SPC f l") 'ms/consult-line-last-search)
  (define-key evil-motion-state-map (kbd "SPC f s") 'consult-line))

(provide 'ms-search)
