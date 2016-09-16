(eval-when-compile (require 'cl))

(use-package better-defaults
  :ensure t)

;; Remove splash screen and message, change major mode.
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode)

; "y or n" instead of "yes or no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable disabled commands.

;; Enable set-goal-column command.
(put 'set-goal-column 'disabled nil)

;; Enable narrowing.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Enable upcasing/downcasing a region.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Autosave settings.
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "autosaves/\\1") t)))
(let ((dir (expand-file-name "autosaves" user-emacs-directory)))
  (unless (file-accessible-directory-p dir)
    (make-directory dir)))

;; Backup files settings.
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      ;; Make backup files even when they're in version control
      vc-make-backup-files t)

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  (define-key key-translation-map "\e[21~" [f10])
  ;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
  (setq visible-bell nil)
  (setq ring-bell-function (lambda ()
                             (invert-face 'mode-line)
                             (run-with-timer 0.1 nil 'invert-face 'mode-line)))
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(0.001)))

(column-number-mode 1)
(delete-selection-mode 1)

(recentf-mode 1)
(setq-default recentf-max-saved-items 100
              recentf-exclude '("/tmp/" "/ssh:"))

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(use-package cl-lib :ensure t :defer t)
(use-package scratch :ensure t :defer t)
(use-package wgrep :ensure t :defer t)

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-x C-,") 'pop-global-mark)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-;" . ace-jump-mode)
         ("C-:" . ace-jump-line-mode)))

;; 2011-05-06 20:17
;; http://stackoverflow.com/questions/145291/smart-home-in-emacs
(defun move-beginning-of-line-smart ()
  "Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of
line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
	 (beginning-of-line))))

(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line-smart)

;;; desktop
;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(provide 'init-general)
;;; init-general ends here
