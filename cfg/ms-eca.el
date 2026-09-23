;;; -*- lexical-binding: t -*-

(use-package eca
  :ensure t
  :config
  (setq eca-chat-use-side-window nil)
  :straight (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
;; (setq eca-extra-args '("--verbose" "--log-level" "debug"))

(defun ms/eca-rewrite-google-docstrings ()
  "Rewrite text with Google-style docstrings using eca-rewrite."
  (interactive)
  (eca-rewrite "add/update docstrings following google-style. Dont update the code itself!"))
(defun ms/eca-rewrite-by-inline-comments ()
    "Rewrite code based on inline comments using eca-rewrite."
    (interactive)
    (eca-rewrite "follow the instructions closely given by `# AI:`. In case of multiple lines marked with `# AI:` follow the instructions that are most important for the functionality of the code."))
(defun ms/eca-rewrite-add-implementation-shortcomings ()
    "Add shortcomings in the implementation to the code marked with `# AI:` using eca-rewrite"
    (interactive)
    (eca-rewrite "Add comments to the code about shortcomings in the implementation. Dont update the code itself. Focus on shortcomings that could lead to bugs or maintenance issues. Make the comments short and concise. Add them in a separate line directly above the code they refer to and start the comment with `# AI:`."))

(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC a L") 'eca-chat-expand-all-blocks)
  (define-key evil-motion-state-map (kbd "SPC a H") 'eca-chat-collapse-all-blocks)
  (define-key evil-motion-state-map (kbd "SPC a e") 'eca)
  (define-key evil-motion-state-map (kbd "SPC a r r") 'eca-restart)
  (define-key evil-motion-state-map (kbd "SPC a S") 'eca-stop)
  (define-key evil-motion-state-map (kbd "SPC a R") 'eca-chat-resume)
  (define-key evil-motion-state-map (kbd "SPC a D") 'eca-chat-delete)
  (define-key evil-motion-state-map (kbd "SPC a s") 'eca-chat-stop-prompt)
  (define-key evil-motion-state-map (kbd "SPC a a") 'eca-chat-cycle-agent)
  (define-key evil-motion-state-map (kbd "SPC a m") 'eca-chat-select-model)
  (define-key evil-motion-state-map (kbd "SPC a p") 'eca-chat-go-to-prev-user-message)
  (define-key evil-motion-state-map (kbd "SPC a n") 'eca-chat-go-to-next-user-message)
  (define-key evil-motion-state-map (kbd "SPC a k") 'eca-chat-go-to-prev-expandable-block)
  (define-key evil-motion-state-map (kbd "SPC a j") 'eca-chat-go-to-next-expandable-block)
  (define-key evil-motion-state-map (kbd "SPC a f") 'eca-chat-toggle-expandable-block)
  (define-key evil-motion-state-map (kbd "SPC a w s") #'ms/eca-rewrite-add-implementation-shortcomings)
  (define-key evil-motion-state-map (kbd "SPC a w r") 'eca-rewrite)
  (define-key evil-motion-state-map (kbd "SPC a w d") #'ms/eca-rewrite-google-docstrings)
  (define-key evil-motion-state-map (kbd "SPC a w i") #'ms/eca-rewrite-by-inline-comments))

(provide 'ms-eca)
