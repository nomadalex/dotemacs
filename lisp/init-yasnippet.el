(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode

  :init
  (add-hook 'after-init-hook 'yas-global-mode))

(provide 'init-yasnippet)
