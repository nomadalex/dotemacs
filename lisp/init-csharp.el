(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :config
  (add-hook 'csharp-mode-hook (lambda ()
                                (setq tab-width 4)
                                (setq c-basic-offset 4)
                                (c-set-offset 'substatement-open 0)

                                (define-key csharp-mode-map [remap c-indent-line-or-region] 'company-indent-for-tab-command))))

(provide 'init-csharp)
;;; init-csharp ends here
