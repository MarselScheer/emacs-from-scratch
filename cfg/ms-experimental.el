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

(provide 'ms-experimental)
