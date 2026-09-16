;;; -*- lexical-binding: t -*-

(use-package gptel
  :config
  ;; (setq gptel--debug t)
  ;; (setq gptel-log-level 'debug)
  (setq gptel-default-mode #'org-mode)
  (setq gptel-org-branching-context t)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
  (setq gptel-temperature 0.0)
  (global-set-key (kbd "C-c C-<return>") 'gptel-send)
  (add-to-list 'gptel-directives '(explain . "Explain the code to a novice programmer"))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :header `(("Authorization" . ,(concat "Bearer " (getenv "OLLAMA_API_KEY"))))
    :models '(codellama:7b qwen3-coder:30b qwen3.5:4b qwen2.5-coder:7b gemma4:e4b gemma4:26b qwen3-coder:30b gemma4:cloud))
  (setq gptel-model   'deepseek/deepseek-v4-flash
	gptel-backend
	(gptel-make-openai "OpenRouter"               ;Any name you want
	  :host "openrouter.ai"
	  :endpoint "/api/v1/chat/completions"
	  :stream t
	  :key (getenv "OPENROUTER_API_KEY")
	  :models '(mistralai/mistral-small-3.2-24b-instruct
		    deepseek/deepseek-v4-pro
		    deepseek/deepseek-v4-flash
		    google/gemini-3-flash-preview
		    minimax/minimax-m2.1
		    minimax/minimax-m2.5
		    openai/gpt-oss-120b))))
(with-eval-after-load 'evil
  (define-key evil-motion-state-map (kbd "SPC a c") 'gptel)
  (define-key evil-motion-state-map (kbd "SPC a SPC") 'gptel-menu))

(provide 'ms-gptel)
