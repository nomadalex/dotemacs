(use-package company
  :ensure t
  :diminish company-mode
  :bind (("M-C-/" . company-complete)
         :map
         company-mode-map
         ("M-/" . company-complete)
         :map
         company-active-map
         ("M-/" . company-select-next))
  :init
  ;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
  ;; TODO Default sort order should place [a-z] before punctuation

  (setq tab-always-indent 'complete)  ;; use 't when company is disabled
  (add-to-list 'completion-styles 'initials t)
  ;; Stop completion-at-point from popping up completion buffers so eagerly
  (setq completion-cycle-threshold 5)

  :config
  (setq-default company-backends '((company-capf company-dabbrev-code) company-dabbrev)
                company-dabbrev-other-buffers 'all)

  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (set (make-local-variable 'company-backends)
         (append (list backend) company-backends)))

  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode))

  (after-load 'page-break-lines-mode
    (defvar sanityinc/page-break-lines-on-p nil)
    (make-variable-buffer-local 'sanityinc/page-break-lines-on-p)

    (defun sanityinc/page-break-lines-disable (&rest ignore)
      (when (setq sanityinc/page-break-lines-on-p (bound-and-true-p page-break-lines-mode))
        (page-break-lines-mode -1)))

    (defun sanityinc/page-break-lines-maybe-reenable (&rest ignore)
      (when sanityinc/page-break-lines-on-p
        (page-break-lines-mode 1)))

    (add-hook 'company-completion-started-hook 'sanityinc/page-break-lines-disable)
    (add-hook 'company-completion-finished-hook 'sanityinc/page-break-lines-maybe-reenable)
    (add-hook 'company-completion-cancelled-hook 'sanityinc/page-break-lines-maybe-reenable)))

(global-company-mode)

(provide 'init-company)
;;; init-company ends here
