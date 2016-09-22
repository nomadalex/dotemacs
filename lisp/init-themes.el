(use-package monokai-theme
  :ensure t
  :init
  (if (and (> emacs-major-version 24) (eq system-type 'windows-nt))
      (add-hook 'window-setup-hook '(lambda () (load-theme 'monokai t)))
    (add-hook 'after-init-hook '(lambda () (load-theme 'monokai t)))))

(provide 'init-themes)
;;; init-themes ends here
