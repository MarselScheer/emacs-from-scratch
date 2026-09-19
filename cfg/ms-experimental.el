;;; -*- lexical-binding: t -*-

(use-package minuet
    :bind
    (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))

    ;; :init
    ;; ;; if you want to enable auto suggestion.
    ;; ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
    ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

    :config
    (setq minuet-provider 'openai-compatible)
    (setq minuet-request-timeout 2.5)
    (setq minuet-auto-suggestion-throttle-delay 1.5) ;; Increase to reduce costs and avoid rate limits
    (setq minuet-auto-suggestion-debounce-delay 0.6) ;; Increase to reduce costs and avoid rate limits

    (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key (defun fetch_key () (getenv "OPENROUTER_API_KEY")))
    (plist-put minuet-openai-compatible-options :model "qwen/qwen3-coder")


    ;; Prioritize throughput for faster completion
    (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9))


(defun ms/translate-to-german (beg end)
  "Translate the selected region to German using gptel.
BEG and END define the region to translate.
Creates a new gptel buffer, inserts the text with a translation
instruction, and sends it to the LLM."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (prompt (concat "Translate the following text to German:\n\n" text)))
    (gptel "*de*" nil prompt)
    (with-current-buffer "*de*"
      (gptel-send)
      (switch-to-buffer "*de*"))))

(defun ms/translate-to-russian (beg end)
  "Translate the selected region to Russian using gptel.
BEG and END define the region to translate.
Creates a new gptel buffer, inserts the text with a translation
instruction, and sends it to the LLM."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (prompt (concat "Translate the following text to Russian:\n\n" text)))
    (gptel "*ru*" nil prompt)
    (with-current-buffer "*ru*"
      (gptel-send)
      (switch-to-buffer "*ru*"))))

(defun ms/capture-word-at-point ()
  "Capture the word at point

Inserts the word at the end of the first line of the capture buffer
after the template opens."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (when word
      (org-capture nil "r")
	(goto-char (point-min))
	(end-of-line)
	(insert " " word)
	(goto-char (point-min))
	(open-line 1)
	(tempel-insert 'trin)
	(goto-char (point-min))
	(evil-visual-line)
	(evil-goto-line)
	(evil-end-of-line)
	(gptel-rewrite)
	)))

(use-package total-recall
  :straight (total-recall :type git :host github
			  :repo "MarselScheer/total-recall"
                          :branch "main")
  :config
  (let* ((recall-db "~/syncthing/ebook/recall.db")
         (adapter (total-recall-storage-init recall-db)))
    ;; Register the "Recall" org-capture template
    (total-recall-capture-init adapter)
    ;; Point the training session at the same database
    (setq total-recall-train-db-path recall-db)))

(provide 'ms-experimental)
