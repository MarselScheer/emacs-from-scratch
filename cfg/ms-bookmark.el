;;; -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC m s") 'bookmark-set)
  (define-key evil-motion-state-map (kbd "SPC m j") 'bookmark-jump)
  (define-key evil-motion-state-map (kbd "SPC m J") 'bookmark-jump-other-window))

(provide 'ms-bookmark)
