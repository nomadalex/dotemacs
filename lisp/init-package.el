;;; Configure package.el and use-package.

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(package-maybe-enable-signatures)
(after-load 'init-exec-path
  (package-maybe-enable-signatures))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'bind-key)
  (package-install 'diminish)
  (package-install 'use-package))

(provide 'init-package)
;;; init-package ends here

