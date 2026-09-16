;;; -*- lexical-binding: t -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output 'first-error)
 '(warning-suppress-log-types '((native-compiler))))

(use-package fancy-compilation
  :commands (fancy-compilation-mode))
(with-eval-after-load 'compile
  (fancy-compilation-mode))

(provide 'ms-compilation)
