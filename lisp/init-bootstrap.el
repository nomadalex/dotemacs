(defconst *is-a-mac* (eq system-type 'darwin))

;; Remove splash screen and message, change major mode.
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode)

;; site-lisp initialize

(eval-when-compile (require 'cl))

(add-to-list 'load-path (expand-file-name "site-lisp/downloads" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(defun ensure-download-package (name url)
  (let* ((dir (expand-file-name "site-lisp/downloads" user-emacs-directory))
         (path (expand-file-name (format "%s.el" name) dir)))
    (unless (file-exists-p path)
      (byte-compile-file
       (progn
         (message "Download %s from %s" name url)
         (unless (file-directory-p dir)
           (make-directory dir t))
         (url-copy-file url path t nil)
         path)))))

(defalias 'after-load 'with-eval-after-load)

;; package initialize

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; exec-path-from-shell initialize

(defun maybe-copy-env-from-shell (vars) t)

(use-package exec-path-from-shell
  :if (memq window-system '(ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (defalias 'maybe-copy-env-from-shell 'exec-path-from-shell-getenvs))

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available.
(setq package-check-signature (when (executable-find "gpg") 'allow-unsigned))

(provide 'init-bootstrap)