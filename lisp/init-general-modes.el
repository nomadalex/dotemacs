(use-package markdown-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package pkgbuild-mode
  :ensure t
  :defer t)

(use-package scala-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :defer t)

(use-package mmm-mode
  :ensure t
  :defer t)

(use-package shader-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.shader$" . shader-mode)))

(use-package protobuf-mode
  :ensure t
  :defer t)

(use-package fish-mode
  :ensure t
  :defer t
  :config
  (defun setup-fish-mode ()
    (setq tab-width 4))
  (add-hook 'fish-mode-hook 'setup-fish-mode))

(use-package go-mode
  :ensure t
  :defer t
  :config
  (defun setup-go-mode ()
    (setq tab-width 4))
  (add-hook 'go-mode-hook 'setup-go-mode)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package nginx-mode
  :ensure t
  :defer t)

(provide 'init-general-modes)
