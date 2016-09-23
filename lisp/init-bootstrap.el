(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-default-init t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      custom-file (expand-file-name "custom.el" user-emacs-directory))

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

;;; package fix columns
(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

(provide 'init-bootstrap)
