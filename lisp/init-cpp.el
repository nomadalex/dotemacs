(use-package modern-cpp-font-lock
  :ensure t
  :defer t)
(use-package google-c-style
  :ensure t
  :defer t)

;; This hack fixes indentation for C++11's "enum class" in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361

(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

;; https://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
(defun vr-c++-looking-at-lambda_as_param ()
  "Return t if text after point matches '[...](' or '[...]{'"
  (looking-at ".*[,(][ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun vr-c++-looking-at-lambda_in_uniform_init ()
  "Return t if text after point matches '{[...](' or '{[...]{'"
  (looking-at ".*{[ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun vr-c++-indentation-examine (langelem looking-at-p)
  (and (equal major-mode 'c++-mode)
       (ignore-errors
         (save-excursion
           (goto-char (c-langelem-pos langelem))
           (funcall looking-at-p)))))

(defun vr-c++-indentation-setup ()
  (require 'google-c-style)
  (google-set-c-style)

  (c-set-offset
   'block-close
   (lambda (langelem)
     (if (vr-c++-indentation-examine
          langelem
          #'vr-c++-looking-at-lambda_in_uniform_init)
         '-
       0)))

  (c-set-offset
   'statement-block-intro
   (lambda (langelem)
     (if (vr-c++-indentation-examine
          langelem
          #'vr-c++-looking-at-lambda_in_uniform_init)
         0
       '+)))

  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (vr-c++-indentation-examine
               langelem
               #'vr-c++-looking-at-lambda_as_param)
              0
            ad-do-it))))

(defun setup-cpp-mode ()
  (modern-c++-font-lock-mode)
  (vr-c++-indentation-setup)
  (fix-enum-class))

(add-hook 'c++-mode-hook #'setup-cpp-mode)

(provide 'init-cpp)
;;; init-cpp ends here
