;; Don't load default library.
(setq inhibit-default-init t)

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil))

  (defconst *is-a-mac* (eq system-type 'darwin))
  (defalias 'after-load 'with-eval-after-load)

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (require 'init-site-lisp)
  (require 'init-package)
  (require 'init-exec-path)
  (require 'init-general)
  (require 'init-themes)

  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file) (load custom-file))
  (server-start))

(provide 'init)
;;; init.el ends here
