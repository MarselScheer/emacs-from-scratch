;;; -*- lexical-binding: t -*-

(defun ms/eshell-history-select ()
  "Select a command from eshell history using the minibuffer."
  (interactive)
  (let* ((history-list (delete-dups (ring-elements eshell-history-ring)))
	 (command (completing-read "Eshell history: " history-list nil t)))
    (when command
      (delete-region (save-excursion (eshell-bol) (point)) (point))
      (insert command))))
(with-eval-after-load 'evil
  (evil-define-key 'insert eshell-mode-map (kbd "M-r") 'ms/eshell-history-select)
  (evil-define-key 'motion eshell-mode-map (kbd "M-r") 'ms/eshell-history-select))

(provide 'ms-eshell)
