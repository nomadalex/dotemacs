(defconst *need-copy-env-from-shell* (memq window-system '(mac ns x)))

(use-package exec-path-from-shell
  :if *need-copy-env-from-shell*
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(defun copy-env-if-need (vars)
  (when *need-copy-env-from-shell*
    (exec-path-from-shell-getenvs vars)))

(copy-env-if-need '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))

(provide 'init-exec-path)
