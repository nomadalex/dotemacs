(require 'cl)

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$"

  :init
  ;; https://github.com/OmniSharp/homebrew-omnisharp-roslyn
  ;; note: only omnisharp-roslyn v1.6.7.9 can work with unity3d
  ;;   https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.6.7.9/omnisharp.tar.gz
  (use-package omnisharp
    :ensure t
    :defer t
    :diminish omnisharp-mode
    :config
    (when *is-a-mac*
      (setq omnisharp-server-executable-path (executable-find "omnisharp")))
    (setq omnisharp-company-template-use-yasnippet nil)
    (add-to-list 'company-backends 'company-omnisharp))

  :config
  (define-key csharp-mode-map [remap c-indent-line-or-region] 'company-indent-for-tab-command)

  (defun setup-csharp-mode ()
    (omnisharp-mode)
    (setq tab-width 4)
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0))

  (add-hook 'csharp-mode-hook 'setup-csharp-mode))

(provide 'init-csharp)
;;; init-csharp ends here
