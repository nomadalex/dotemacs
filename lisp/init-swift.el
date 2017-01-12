(use-package company-sourcekit
  :disabled
  :if *is-a-mac*
  :ensure t
  :defer t

  :init
  (with-eval-after-load 'company
    (setq sourcekit-verbose t
          company-sourcekit-verbose t)
    (add-to-list 'company-backends 'company-sourcekit))

  :config
  ;; don't use sourcekit completion when there are no project for it
  (defun company-sourcekit--prefix-check-project (orig-fun &rest args)
    (and (eq major-mode 'swift-mode)
         (sourcekit-project)
         (apply orig-fun args)))
  (advice-add 'company-sourcekit--prefix :around 'company-sourcekit--prefix-check-project))

(use-package swift-mode
  :ensure t
  :defer t)

(provide 'init-swift)
