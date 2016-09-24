(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-win* (eq system-type 'windows-nt))

(setq inhibit-default-init t
      inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(use-package exec-path-from-shell
  :if (or *is-a-win* (memq window-system '(ns x)))
  :ensure t
  :config
  ;; http://qiita.com/KazuSoap/items/ee7a4dec80308dbd1d41
  (when *is-a-win*
    (let ((shell-level (getenv "SHLVL")))
      (when (or (not shell-level) (string= "0" shell-level))
        (defvar msys2-root-directory "c:/msys64"
          "Root directory for MSYS2 installation")
        (setq msys2-bash-path (expand-file-name "usr/bin/bash" msys2-root-directory)
              msys2-cygpath-path (expand-file-name "usr/bin/cygpath" msys2-root-directory))

        (defun msys2-cygpath-to-win-path (pathstr)
          (with-temp-buffer
            (call-process msys2-cygpath-path nil '(t nil) nil "-amp" pathstr)
            (unless (bobp)
              (goto-char (point-min))
              (buffer-substring-no-properties (point) (line-end-position)))))

        (setq shell-file-name msys2-bash-path)
        (defvar explicit-shell-file-name)
        (setq explicit-shell-file-name shell-file-name)
        (setenv "SHELL" shell-file-name)

        (setenv "MSYSTEM" "MINGW64")

        (defun ad-exec-path-from-shell-setenv (orig-fun &rest args)
          (when (string= (car args) "PATH")
            (setf (nth 1 args) (msys2-cygpath-to-win-path (nth 1 args))))
          (apply orig-fun args))
        (advice-add 'exec-path-from-shell-setenv :around 'ad-exec-path-from-shell-setenv)))
    (setq shell-command-switch "-c"))

  (exec-path-from-shell-initialize)

  (when *is-a-win*
    (with-eval-after-load 'info
      (setq Info-additional-directory-list (split-string (msys2-cygpath-to-win-path (exec-path-from-shell-getenv "INFOPATH")) "[;]"))))

  (defalias 'maybe-copy-env-from-shell 'exec-path-from-shell-getenvs))

(unless (fboundp 'maybe-copy-env-from-shell)
  (defun maybe-copy-env-from-shell (names) t))

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available.
(setq package-check-signature (when (executable-find "gpg") 'allow-unsigned))

(provide 'init-bootstrap)
