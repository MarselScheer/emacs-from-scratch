;;; -*- lexical-binding: t -*-
;; instructions from the straight README
(defvar bootstrap-version)
(let ((bootstrap-file
	    (expand-file-name
	     "straight/repos/straight.el/bootstrap.el"
	     (or (bound-and-true-p straight-base-dir)
		 user-emacs-directory)))
	   (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
	 (with-current-buffer
	     (url-retrieve-synchronously
	      "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	      'silent 'inhibit-cookies)
	   (goto-char (point-max))
	   (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package evil
  :init
  ;; needed by evil-collection
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  ;; otherwise SPC SPC will not bring up M-x if for instance
  ;; a region is highlighted
  (define-key evil-motion-state-map " " nil)
  (evil-mode 1))

;; need to also have movement keys hjkl in some other packages like magit
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Prefer to use 'kj' to go back to normal mode
(use-package evil-escape
  :config
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "kj"))

(use-package org
  ;; :bind
  ;; (:map org-agenda-mode-map
  ;; 	("j" . org-agenda-next-line)
  ;; 	("k" . org-agenda-previous-line))
  :config
  (setq org-M-RET-may-split-line nil)
  (setq org-insert-heading-respect-content t)
  (setq org-agenda-files (directory-files "~/syncthing/orgfiles" 1 "org$"))
  (setq org-log-done "time")
  (setq org-todo-keywords '((sequence "TODO(t)" "CONT(c)" "WAIT(w)" "REMINDER(r)" "FLEETING(f)" "|" "DONE(d)" "ABORTED(a)")))
  (setq org-log-note-headings '((done . "CLOSING NOTE %t")
				(state . "State %-12s from %-12S %t")
				;; (note . "Note taken on %t")
				(note . "%d")
				(reschedule . "Rescheduled from %S on %t")
				(delschedule . "Not scheduled, was %S on %t")
				(redeadline . "New deadline from %S on %t")
				(deldeadline . "Removed deadline, was %S on %t")
				(refile . "Refiled on %t")
				(clock-out . "")))
  (setq org-capture-templates
	'(("f"                    ; ← shortcut key
	   "Fleeting note with region" ; ← description shown in the capture menu
	   entry                  ; ← type of item (plain text, entry, etc.)
	   (file+headline "~/syncthing/orgfiles/20251218T150014--fleeting-notes__general.org" "Inbox")
	   "* FLEETING %u: %? %t\n  #+begin_src\n%i\n#+end_src\n origin: file:%F")
	  ("p"                    ; ← shortcut key
	   "Fleeting note plain"        ; ← description shown in the capture menu
	   entry                  ; ← type of item (plain text, entry, etc.)
	   (file+headline "~/syncthing/orgfiles/20251218T150014--fleeting-notes__general.org" "Inbox")
	   "* FLEETING %u: %? %t\norigin: file:%F")))
  (setq org-confirm-babel-evaluate
      (lambda (lang body)
        (not (string= lang "python")))))
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t))))

(define-key evil-motion-state-map (kbd "SPC o a") 'org-agenda)
(define-key evil-motion-state-map (kbd "SPC o i") 'org-indent-mode)
(define-key evil-motion-state-map (kbd "SPC o f") 'org-cycle)
(define-key evil-motion-state-map (kbd "SPC o c") 'org-toggle-checkbox)
(define-key evil-motion-state-map (kbd "SPC o s") 'org-save-all-org-buffers)
(define-key evil-motion-state-map (kbd "SPC o h") 'org-toggle-heading)
(define-key evil-motion-state-map (kbd "SPC n s") 'org-narrow-to-subtree)
(define-key evil-motion-state-map (kbd "SPC n w") 'widen)
(define-key evil-motion-state-map (kbd "SPC n t") 'org-show-todo-tree)
(define-key evil-motion-state-map (kbd "SPC o n r") (lambda () (interactive) (org-capture nil "f")))
(define-key evil-motion-state-map (kbd "SPC o n p") (lambda () (interactive) (org-capture nil "p")))
(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
     (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (keymap-set vertico-map "C-f" #'vertico-scroll-up)
  (keymap-set vertico-map "C-b" #'vertico-scroll-down)
  (setq vertico-cycle t))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto nil)               ;; Disable auto completion because typing fast seems to collide with that
                                 ;; because sometimes the whole file get manipulated?!?!
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.3)
  :hook (eshell-mode . (lambda ()
			 (keymap-set corfu-map "RET" #'corfu-send)))
  :bind (:map corfu-map
	      ("ö" . corfu-insert-separator))
  :init
  (global-corfu-mode))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

(use-package cape
  :init
  (add-hook 'prog-mode-hook (lambda () (add-hook 'completion-at-point-functions #'cape-file nil t)))
)

(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

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
(define-key evil-motion-state-map (kbd "SPC f p") 'ms/consult-line-symbol-at-point)
(define-key evil-motion-state-map (kbd "SPC f l") 'ms/consult-line-last-search)
(define-key evil-motion-state-map (kbd "SPC f s") 'consult-line)

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :custom (counsel-describe-function-function #'helpful-callable)
  :bind ([remap describe-function] . #'helpful-callable))
(use-package elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(defun ms/save-buffer-and-recompile ()
  "Saves the current buffer and runs project-recompile"
  (interactive)
  (progn
    (save-buffer)
    (project-recompile)))
(define-key evil-motion-state-map (kbd "SPC p s r") 'consult-ripgrep)
(define-key evil-motion-state-map (kbd "SPC p c") 'project-compile)
(define-key evil-motion-state-map (kbd "SPC p r") 'ms/save-buffer-and-recompile)
(define-key evil-motion-state-map (kbd "SPC p p") 'project-switch-project)
(define-key evil-motion-state-map (kbd "SPC p k") 'project-kill-buffers)
(define-key evil-motion-state-map (kbd "SPC p b") 'consult-project-buffer)
(define-key evil-motion-state-map (kbd "SPC p f") 'project-find-file)
(define-key evil-motion-state-map (kbd "SPC p e") 'project-eshell)
(setq project-vc-extra-root-markers '(".project.el"))

(use-package magit)
(define-key evil-motion-state-map (kbd "SPC g g") 'magit-status)
(define-key evil-motion-state-map (kbd "SPC g b") 'magit-blame)
(define-key evil-motion-state-map (kbd "SPC g d") 'magit-diff-dwim)

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
(define-key evil-motion-state-map (kbd "SPC g m") 'ms/generate-git-commit-msg)

(use-package diff-hl
  :config (global-diff-hl-mode))

(use-package git-messenger
  :config (setq git-messenger:show-detail t))

(use-package lsp-mode
  ;; code also comes from https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq xref-auto-jump-to-first-definition nil) ;; necessary in older emacs for "find-definition"-functionality
  (defun my/lsp-mode-setup-completion ()
	 (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	       '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (python-mode . lsp) ;; install ty via uv
  :config
  (lsp-enable-which-key-integration t)
  (setq read-process-output-max (* 2 1024 1024))
  (setq gc-cons-threshold (* 2 800000))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.venv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.poetry\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.metaflow\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.local\\'"))

(define-key evil-motion-state-map (kbd "SPC l d") 'lsp-describe-thing-at-point)
(define-key evil-motion-state-map (kbd "SPC l r") 'lsp-rename)
(define-key evil-motion-state-map (kbd "SPC l f") 'lsp-find-definition)
(define-key evil-motion-state-map (kbd "SPC l u") 'lsp-find-references)
(define-key evil-motion-state-map (kbd "SPC l c") 'comment-or-uncomment-region)

;; Enables "pop-up's" with additional information about
;; the current symbol or documentation for functions, etc.
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;; 			 (require 'lsp-pyright)
;; 			 (lsp))))  ; or lsp-deferred

(use-package flycheck)

(use-package ace-jump-mode
  :config
  (define-key evil-motion-state-map (kbd "SPC s") 'ace-jump-word-mode))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  ;; :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
  ;;        ("M-*" . tempel-insert))

  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
		(cons #'tempel-expand (cons #'cape-file
		      (remove #'tempel-expand completion-at-point-functions)))))

  ;; (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'lsp-completion-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'eshell-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-blink-duration 2))

(use-package indent-guide
  :config (indent-guide-global-mode))

(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package denote
  :config
  (setq denote-journal-extras-title-format 'year-month-day)
  (setq denote-directory "~/syncthing/orgfiles"))

(use-package goto-last-change)

(use-package gptel
  :config
  ;; (setq gptel--debug t)
  ;; (setq gptel-log-level 'debug)
  (setq gptel-default-mode #'org-mode)
  (setq gptel-org-branching-context t)
  (setq gptel-temperature 0.0)
  (global-set-key (kbd "C-c C-<return>") 'gptel-send)
  (add-to-list 'gptel-directives '(explain . "Explain the code to a novice programmer"))
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(codellama:7b))
  (setq gptel-model   'google/gemini-3-flash-preview
	gptel-backend
	(gptel-make-openai "OpenRouter"               ;Any name you want
	  :host "openrouter.ai"
	  :endpoint "/api/v1/chat/completions"
	  :stream t
	  :key (getenv "OPENROUTER_API_KEY")
	  :models '(mistralai/mistral-small-3.2-24b-instruct
		    google/gemini-3-flash-preview
		    minimax/minimax-m2.1
		    minimax/minimax-m2.5
		    openai/gpt-oss-120b))))
(define-key evil-motion-state-map (kbd "SPC a c") 'gptel)
(define-key evil-motion-state-map (kbd "SPC a m") 'gptel-menu)

(use-package ragmacs
   :ensure t
   :straight (:host github :repo "positron-solutions/ragmacs")
   :after gptel
   :defer
   :init
   (gptel-make-preset 'introspect
     :pre (lambda () (require 'ragmacs))
     :system
     "You are pair programming with the user in Emacs and on Emacs.

 Your job is to dive into Elisp code and understand the APIs and
 structure of elisp libraries and Emacs.  Use the provided tools to do
 so, but do not make duplicate tool calls for information already
 available in the chat.

 <tone>
 1. Be terse and to the point.  Speak directly.
 2. Explain your reasoning.
 3. Do NOT hedge or qualify.
 4. If you don't know, say you don't know.
 5. Do not offer unprompted advice or clarifications.
 6. Never apologize.
 7. Do NOT summarize your answers.
 </tone>

 <code_generation>
 When generating code:
 1. Always check that functions or variables you use in your code exist.
 2. Also check their calling convention and function-arity before you use them.
 3. Write code that can be tested by evaluation, and offer to evaluate
 code using the `elisp_eval` tool.
 </code_generation>

 <formatting>
 1. When referring to code symbols (variables, functions, tags etc) enclose them in markdown quotes.
    Examples: `read_file`, `getResponse(url, callback)`
    Example: `<details>...</details>`
 2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
 </formatting>"
     :tools '("introspection")))

(defun ollama-only-code-curl-to-buffer (text)
  "Send TEXT to a buffer with the name BUFFER-NAME."
  (let ((curl-command (format "curl -s -X POST http://ollama:11434/api/generate -d '{ \"model\": \"codellama:7b\",\"prompt\": \"%s. only code\", \"stream\": false }' " (replace-regexp-in-string "\n" " " text))))
    (with-current-buffer (get-buffer-create "curl-llm-out")
      (erase-buffer)
      (insert (shell-command-to-string curl-command)))))

(defun extract-json-response ()
  "Extract the 'response' field from a JSON buffer and save it to a new buffer."
  (with-current-buffer  (get-buffer "curl-llm-out")
    (let* ((json-string (buffer-string))
	   (json-data (json-read-from-string json-string))
	   (response (cdr (assoc 'response json-data))))
      (concat "\n---------------------llm-start---------------------\n" response "\n---------------------llm-end---------------------\n")
      )))

(defun llm-minibuffer (text)
  (interactive "sOllama: ")
  (ollama-only-code-curl-to-buffer text)
  (let ((response (extract-json-response)))
    (insert response))
  )

(defun llm-only-code ()
  "Returns the text selected by a region."
  (interactive)
  (let ((region (buffer-substring (region-beginning) (region-end))))
    (llm-minibuffer region)))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package expand-region)
(define-key evil-motion-state-map (kbd "e") 'er/expand-region)

(use-package embark
  :bind(:map minibuffer-local-map
	     ("C-c C-c" . embark-collect)))
(use-package embark-consult)

(use-package evil-snipe
  :config (evil-snipe-mode))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode)
  (setq evil-visualstar/persistent 1))

(use-package eca
  :ensure t
  :straight (:host github :repo "editor-code-assistant/eca-emacs" :files ("*.el")))
;; (setq eca-extra-args '("--verbose" "--log-level" "debug"))
(define-key evil-motion-state-map (kbd "SPC a e") 'eca-chat-toggle-window)
(define-key evil-motion-state-map (kbd "SPC a r") 'eca-restart)
(define-key evil-motion-state-map (kbd "SPC a s") 'eca-stop)
(defun ms/eca-rewrite-google-docstrings ()
  "Rewrite text with Google-style docstrings using eca-rewrite."
  (interactive)
  (eca-rewrite "add/update docstrings following google-style. Dont update the code itself!"))
(define-key evil-motion-state-map (kbd "SPC a w d") #'ms/eca-rewrite-google-docstrings)
  (defun ms/eca-rewrite-by-inline-comments ()
    "Rewrite code based on inline comments using eca-rewrite."
    (interactive)
    (eca-rewrite "follow the instructions closely given by `# AI:`. In case of multiple lines marked with `# AI:` follow the instructions that are most important for the functionality of the code."))
(define-key evil-motion-state-map (kbd "SPC a w i") #'ms/eca-rewrite-by-inline-comments)
  (defun ms/eca-rewrite-add-implementation-shortcomings ()
    "Add shortcomings in the implementation to the code marked with `# AI:` using eca-rewrite"
    (interactive)
    (eca-rewrite "Add comments to the code about shortcomings in the implementation. Dont update the code itself. Focus on shortcomings that could lead to bugs or maintenance issues. Make the comments short and concise. Add them in a separate line directly above the code they refer to and start the comment with `# AI:`."))
(define-key evil-motion-state-map (kbd "SPC a w s") #'ms/eca-rewrite-add-implementation-shortcomings)
(define-key evil-motion-state-map (kbd "SPC a w r") 'eca-rewrite)

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

(use-package time-table
  :straight (time-table :type git :host github :repo "MarselScheer/time-table" :branch "time-table-buffer")
  :custom
  (time-table-work-hours 0)
  (time-table-file "~/syncthing/orgfiles/tracked-times")
  (time-table-project-names '("book" "kg" "time-table" "emacs" "backup" "end"))
  (time-table-task-names '("code" "tex" "math" "end")))
(define-key evil-motion-state-map (kbd "SPC t t") 'time-table-track)
(define-key evil-motion-state-map (kbd "SPC t s") 'time-table-status)
(define-key evil-motion-state-map (kbd "SPC t S") 'time-table-summarize-projects-last-7-days)
(define-key evil-motion-state-map (kbd "SPC t b") 'time-table-find-tracking-file)
(define-key evil-motion-state-map (kbd "SPC t e") 'time-table-end-tracking)

(use-package ruff-format)
(add-hook 'python-mode-hook 'ruff-format-on-save-mode)

;; in order to configure the built-in python mode use
(use-package python
  :config
  (define-key python-mode-map (kbd "<backtab>") 'indent-for-tab-command)
  (define-key python-mode-map (kbd "TAB") 'completion-at-point))
;; (use-package python-mode
;;   :hook (python-mode . (lambda ()
;; 			 (define-key python-mode-map (kbd "TAB") 'completion-at-point)
;; 			 (lsp-deferred))))
  ;; :config
  ;; ;; (require 'dap-python)
  ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r i") 'py-switch-to-shell)
  ;; ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r b") 'ms/py-execute-buffer)
  ;; ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r c") 'ms/py-execute-class)
  ;; ;; (evil-define-key 'normal 'python-mode-map (kbd "SPC r r") 'ms/py-execute-region)
  ;; (setq py-split-window-on-execute nil))
  ;; ;; (setq dap-python-debugger 'debugpy)
;; (define-key python-mode-map (kbd "TAB") 'completion-at-point)

(setq python-shell-interpreter "uv")
(setq python-shell-interpreter-args "run python -i")

(defvar my-intercept-mode-map (make-sparse-keymap)
  "High precedence keymap.")

(define-minor-mode my-intercept-mode
  "Global minor mode for higher precedence evil keybindings."
  :global t)

(my-intercept-mode)

(dolist (state '(normal visual insert))
  (evil-make-intercept-map
	;; NOTE: This requires an evil version from 2018-03-20 or later
	(evil-get-auxiliary-keymap my-intercept-mode-map state t t)
	state))

(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC SPC") 'execute-extended-command)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b f") 'find-file)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b b") 'consult-buffer)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b k") 'kill-buffer)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC b o") 'consult-buffer-other-window)
(evil-define-key 'normal my-intercept-mode-map
  (kbd "SPC w") 'evil-window-map)

(define-key evil-motion-state-map (kbd "SPC m s") 'bookmark-set)
(define-key evil-motion-state-map (kbd "SPC m j") 'bookmark-jump)
(define-key evil-motion-state-map (kbd "SPC m J") 'bookmark-jump-other-window)

(global-visual-line-mode 1)
(global-visual-wrap-prefix-mode 1)

(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output 'first-error)
 '(warning-suppress-log-types '((native-compiler))))

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(savehist-mode 1)

(use-package fancy-compilation
  :commands (fancy-compilation-mode))
(with-eval-after-load 'compile
  (fancy-compilation-mode))

(use-package doom-themes)
(load-theme 'doom-dracula t)
;; (use-package cyberpunk-theme)
;; (load-theme 'cyberpunk t)
;; (consult-theme 'deeper-blue)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:extend t :background "gray10" :foreground "yellow" :box nil :weight bold)))))

;; (use-package doom-modeline
;;  :init (doom-modeline-mode 0))
(use-package telephone-line)
(telephone-line-mode 1)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq delete-old-versions t)
(setq version-control t)
(setq kept-new-versions 7)
