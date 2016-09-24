(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map
         company-mode-map
         ("M-/" . company-complete)
         ([remap indent-for-tab-command] . company-indent-for-tab-command)
         :map
         company-active-map
         ([tab] . company-complete-common-or-cycle)
         ("TAB" . company-complete-common-or-cycle))
  :init
  ;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
  ;; TODO Default sort order should place [a-z] before punctuation

  ;; https://github.com/company-mode/company-mode/issues/94
  (setq tab-always-indent 'complete)  ;; use 't when company is disabled
  (add-to-list 'completion-styles 'initials t)
  ;; Stop completion-at-point from popping up completion buffers so eagerly
  (setq completion-cycle-threshold 5)

  (defvar completion-at-point-functions-saved nil)

  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
          (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

  (defun company-complete-common-wrapper ()
    (let ((completion-at-point-functions completion-at-point-functions-saved))
      (company-complete-common)))

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

  ;; https://github.com/company-mode/company-mode/issues/180
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/54
  (after-load 'fill-column-indicator
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

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
