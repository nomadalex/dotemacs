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

(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

(defun my-objc-mode-hook ()
  ;; Setup indentation
  (setq tab-width 4)
  (c-set-style "java")
  (c-set-offset 'brace-list-close '-)
  (c-set-offset 'brace-list-intro '0)
  (c-set-offset 'arglist-close '0)
  ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'substatement-open '0) 
  (c-set-offset 'inline-open '+)
  (c-set-offset 'block-open '+)
  ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'brace-list-open '+)   
  ; indent case labels by c-indent-level, too
  (c-set-offset 'case-label '+)

  ;; Fix override of tab switch keybinding
  (define-key objc-mode-map (kbd "M-j") nil))

(add-hook 'objc-mode-hook 'my-objc-mode-hook)

(provide 'init-general-modes)
