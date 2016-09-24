(use-package better-defaults
  :ensure t)

;; "y or n" instead of "yes or no".
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

;; Start maximized.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Smooth scrolling.
(setq scroll-conservatively 10000
      scroll-margin 3
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; More useful frame title.
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - " invocation-name))

;; Show keystrokes in progress.
(setq echo-keystrokes 0.1)

;; Sentences do not need double spaces to end.
(setq-default sentence-end-double-space nil)

;; Do not break lines.
(setq-default truncate-lines t)

;; Move files to trash when deleting.
(setq delete-by-moving-to-trash t)
(when *is-a-mac*
  (setq trash-directory (expand-file-name "~/.Trash")))

(column-number-mode 1)
(delete-selection-mode 1)

;; Handle camelCase words properly everywhere.
(global-subword-mode 1)

;; Automatically open compressed files.
(auto-compression-mode 1)

;; http://endlessparentheses.com/faster-pop-to-mark-command.html
;; When popping the mark, continue popping until the cursor actually moves.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        ad-do-it))))

(setq set-mark-command-repeat-pop t)

(defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
  "Cleanup white space after `kill-line' up to non white space character."
  (unless (bolp)
    (delete-region (point)
                   (progn (skip-chars-forward " \t") (point)))))

;; Package.el on steroids.
(use-package paradox
  :ensure t
  :defer t
  :config
  (setq paradox-execute-asynchronously nil))

(use-package hydra
  :ensure t
  :defer t)

(use-package scratch
  :ensure t
  :defer t)

;;; desktop
(use-package desktop
  :config
  ;; save a list of open files in ~/.emacs.d/.emacs.desktop
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 600)
  (desktop-save-mode 1))

;;; uniquify
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-ignore-buffers-re "^\\*"))

;; Visualize some kinds of blank.
(use-package whitespace
  :diminish whitespace-mode
  :config
  ;; Indicate empty lines after the buffer end.
  (setq-default indicate-empty-lines t)
  (setq whitespace-style '(face trailing))
  (global-whitespace-mode 1))

;; Smart mode line.
(use-package smart-mode-line
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'sml/setup))

;; fill-column-indicator
;; Enable only in text- and prog-modes.
(use-package fill-column-indicator
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'fci-mode)
  (add-hook 'text-mode-hook #'fci-mode)
  :config
  ;; Workarounds for popup library.

  (defadvice popup-create (before suppress-fci-mode activate compile)
    "Suspend fci-mode while popups are visible"
    (when fci-mode
      (turn-off-fci-mode)))

  (defadvice popup-delete (after restore-fci-mode activate compile)
    "Restore fci-mode when all popups have closed"
    (unless fci-mode
      (turn-on-fci-mode))))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

;; Set fill-column and comment-fill-column.
(setq-default fill-column 79)
(use-package newcomment
  :defer t
  :config
  (setq comment-fill-column 70))

;; Set default dictionary for flyspell-mode.
(use-package ispell
  :defer t
  :config
  (setq ispell-dictionary "english"))

;; Detect buffer language.
(use-package exttextcat
  :defer t
  :config
  (add-hook 'find-file-hook #'exttextcat-guess-language-buffer))

(use-package autorevert
  :config
  ;; Revert buffers automatically associated with files when the file changes
  ;; on disk.
  (global-auto-revert-mode 1)
  ;; Also auto refresh dired and be quiet.
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Don't use M-TAB to correct words in flyspell-mode.
(use-package flyspell
  :defer t
  :diminish flyspell-mode
  :config
  (setq flyspell-use-meta-tab nil))

(use-package hippie-exp
  :bind
  ("C-M-/" . hippie-expand-lines)
  :init
  ;; Enable dynamic expansion of words.
  (setq global-abbrev-table (make-abbrev-table)) ; Fix wrong type argument.
  (setq-default abbrev-mode t)
  (setq save-abbrevs 'silently)
  :config
  ;; Custom hippie-expand expansion functions.
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev-from-kill
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-all-abbrevs
                                           try-expand-list
                                           try-expand-line
                                           try-complete-lisp-symbol-partially
                                           try-complete-lisp-symbol)))

(use-package calendar
  :defer t
  :config
  ;; Start week at Monday.
  (setq calendar-week-start-day 1))

;; Visualization of undo tree.
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package browse-kill-ring
  :ensure t
  :bind
  ("C-x C-y" . browse-kill-ring)
  :config
  (push 'browse-kill-ring-mode page-break-lines-modes))

(use-package avy
  :ensure t
  :defer t
  :bind
  ("C-;" . avy-goto-char)
  :init
  (bind-keys :map goto-map
             ("c" . avy-goto-char)
             ("C" . avy-goto-char-2)
             ("m" . avy-goto-char-in-line)
             ("G" . avy-goto-line)
             ("w" . avy-goto-word-1)
             ("W" . avy-goto-word-0)
             ("s" . avy-goto-subword-1)
             ("S" . avy-goto-subword-0)))

;; Undo/redo window configuration with C-c <left>/<right>.
(use-package winner
  :config
  (winner-mode 1))

(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window))

;; http://endlessparentheses.com/exclude-directories-from-grep.html
(use-package grep
  :defer t
  :config
  (add-to-list 'grep-find-ignored-directories "auto")
  (add-to-list 'grep-find-ignored-directories "elpa"))

;; Writable grep buffer.
(use-package wgrep
  :ensure t
  :defer t)

;; Show number of search matches in mode line.
(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config
  (global-anzu-mode 1))

;; Visual query replace.
(use-package visual-regexp
  :ensure t
  :bind
  ("C-M-%" . vr/query-replace))

;; Savehist keeps track of some history.
(use-package savehist
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode 1))

(use-package recentf
  :config
  (setq-default recentf-max-saved-items 100
                recentf-exclude '("/tmp/" "/ssh:"))
  (recentf-mode 1))

;; Bookmarks.
(use-package bookmark
  :defer t
  :config
  (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
        bookmark-save-flag 1))

(use-package imenu
  :defer t
  :config
  ;; Always rescan buffer for imenu
  (setq-default imenu-auto-rescan t))

;; Display available keybindings after delay.
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Display major mode key bindings in popup menu.
(use-package discover-my-major
  :ensure t
  :defer t)

;; Paste buffers to refheap from emacs.
(use-package refheap
  :ensure t
  :defer t)

(use-package highlight-symbol
  :disabled
  :ensure t
  :defer t
  :diminish highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode))

;;; ibuffer
(use-package ibuffer
  :defer t
  :init
  (use-package ibuffer-vc :ensure t :defer t)
  (use-package ibuffer-tramp :ensure t :defer t)
  :config
  (require 'ibuffer-vc)
  (require 'ibuffer-tramp)

  (defun ibuffer-set-up-preferred-filters ()
    (setq ibuffer-filter-groups
          (append (ibuffer-vc-generate-filter-groups-by-vc-root)
                  (ibuffer-tramp-generate-filter-groups-by-tramp-connection)))
    (ibuffer-update nil t)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))

  (add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters))

;; key bindings

(bind-key* "C-," 'set-mark-command)
(bind-key* "C-x C-," 'pop-global-mark)

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

(global-set-key (kbd "C-c j") 'join-line)

;; Paragraph movement.
(bind-key "M-n" #'forward-paragraph)
(bind-key "M-p" #'backward-paragraph)

;; Launcher map.
;; `C-x l' is `count-lines-page' by default.
;; http://endlessparentheses.com/launcher-keymap-for-standalone-features.html
(bind-keys :prefix-map launcher-map
           :prefix "C-x l"
           ("c" . whitespace-cleanup)
           ("C" . calc)
           ("h" . man)
           ("f" . find-dired)
           ("l" . lgrep)
           ("r" . rgrep)
           ("o" . occur)
           ("m" . multi-occur)
           ("p" . paradox-list-packages)
           ("w" . webjump))

;; Toggle map.
;; http://endlessparentheses.com/the-toggle-map-and-wizardry.html
(bind-keys :prefix-map toggle-map
           :prefix "C-x t"
           ("e" . toggle-debug-on-error)
           ("r" . dired-toggle-read-only)
           ("w" . whitespace-mode)
           ("f" . auto-fill-mode)
           ("t" . truncate-lines)
           ("m" . menu-bar-mode))

;; Text mode
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'flyspell-mode)

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

(provide 'init-general)
;;; init-general ends here
