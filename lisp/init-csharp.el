(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"
  :config
  ;; fix revert-buffer advice
  ;; @see https://code.google.com/p/csharpmode/issues/detail?id=8
  (add-hook 'csharp-mode-hook (lambda ()
                                (setq tab-width 4)
                                (setq c-basic-offset 4)
                                (c-set-offset 'substatement-open 0)

                                (require 'flymake))))

(provide 'init-csharp)
;;; init-csharp ends here
