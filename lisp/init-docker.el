(use-package docker
  :ensure t
  :defer t
  :init
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package docker-compose-mode
  :ensure t
  :defer t)

(provide 'init-docker)
