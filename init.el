;; fix annoying modification by package.el
(defun setup-package()
  (package-initialize))

(let ((gc-cons-threshold (* 256 1024 1024))
      (file-name-handler-alist nil))

  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

  (require 'init-benchmarking)
  (require 'init-bootstrap)
  (require 'init-general-settings)
  (require 'init-themes)

  (require 'init-ivy)
  (require 'init-dired)
  (require 'init-smartparens)
  (require 'init-yasnippet)
  (require 'init-company)

  (require 'init-general-modes)
  (require 'init-emacs-lisp)
  (require 'init-csharp)
  (require 'init-ruby)
  (require 'init-swift)

  (enable-log-desktop-restored-time)
  (enable-log-init-time)

  (when (file-exists-p custom-file)
    (load custom-file))

  (require 'server)
  (unless (server-running-p)
    (server-start)))
