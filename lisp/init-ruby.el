;; Auto completion for ruby
(use-package robe
  :ensure t
  :defer t
  :init
  (diminish 'robe-mode)
  (add-hook 'ruby-mode-hook 'robe-mode)
  (with-eval-after-load 'company
    (push 'company-robe company-backends)))

(use-package projectile-rails
  :ensure t
  :defer t
  :init
  (projectile-rails-global-mode))

(use-package rinari
  :disabled
  :ensure t
  :defer t)

(use-package flymake-ruby
  :ensure t
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'flymake-ruby-load))

(provide 'init-ruby)
