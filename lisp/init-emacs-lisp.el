(use-package lisp-mode
  :defer t

  :init
  (use-package rainbow-delimiters
    :ensure t
    :defer t)

  (use-package elisp-slime-nav
    :ensure t
    :defer t
    :diminish elisp-slime-nav-mode)

  (use-package cl-lib-highlight
    :ensure t
    :defer t)

  :config
  (cl-lib-highlight-initialize)

  (defun check-parens-after-save ()
    "Run `check-parens' if this is a lispy mode."
    (when (memq major-mode '(emacs-lisp-mode))
      (check-parens)))

  (add-hook 'after-save-hook #'check-parens-after-save)

  (define-key emacs-lisp-mode-map (kbd "C-x C-a") 'pp-macroexpand-last-sexp)

  (defun maybe-set-elisp-readonly ()
    "If this elisp appears to be part of Emacs or installed from elpa, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1)))

  (add-hook 'emacs-lisp-mode-hook 'maybe-set-elisp-readonly)

  ;; paredit's wrap-round.
  ;; https://github.com/Fuco1/smartparens/wiki/Permissions#pre-and-post-action-hooks
  (with-eval-after-load 'smartparens
    (sp-local-pair 'emacs-lisp-mode "(" nil :wrap "M-("
                   :post-handlers '(:add restore-paren-location))

    ;; sp-wrap-with-pair doesn't execute post-handlers?
    (defun restore-paren-location ()
      "Move preceding paren to the previous line if it is empty."
      (let ((empty-line-above (save-excursion
                                (forward-line -1)
                                (looking-at "^\\s-*$"))))
        (when empty-line-above
          (save-excursion
            (forward-line -1)
            (delete-region (point) (1+ (line-end-position))))
          (save-excursion
            (newline-and-indent))))))

  (defun setup-elisp-mode ()
    (elisp-slime-nav-mode 1)
    (eldoc-mode 1)
    (rainbow-delimiters-mode 1)
    (smartparens-strict-mode)
    ;; Complete symbol considering without namespace.
    (make-local-variable 'hippie-expand-try-functions-list)
    (add-to-list 'hippie-expand-try-functions-list
                 'try-complete-lisp-symbol-without-namespace t))

  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-lisp.el
  (defun try-complete-lisp-symbol-without-namespace (old)
    "Hippie expand \"try\" function which expands \"-foo\"
to \"modname-foo\" in elisp."
    (unless old
      (he-init-string (he-lisp-symbol-beg) (point))
      (when (string-prefix-p "-" he-search-string)
        (let ((mod-name (emacs-lisp-module-name)))
          (when mod-name
            (setq he-expand-list (list (concat mod-name
                                               he-search-string)))))))
    (when he-expand-list
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list nil)
      t))

  (defun emacs-lisp-module-name ()
    "Search the buffer for `provide' declaration."
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^(provide '" nil t)
        (symbol-name (symbol-at-point)))))

  (add-hook 'emacs-lisp-mode-hook #'setup-elisp-mode))

(provide 'init-emacs-lisp)
