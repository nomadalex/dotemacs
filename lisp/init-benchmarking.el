(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defvar sanityinc/require-times nil
  "A list of (FEATURE . LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defadvice require (around sanityinc/build-require-times (feature &optional filename noerror) activate)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        ad-do-it
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (cons feature time)
                       t))))))

(defun enable-log-init-time ()
  (add-hook 'after-init-hook
            (lambda ()
              (message "init completed in %.2fms"
                       (sanityinc/time-subtract-millis after-init-time before-init-time)))))

(defun enable-log-desktop-restored-time ()
  (defun desktop-read-debug-error-log-time (orig-fun &rest args)
    (let ((debug-on-error t)
          (start-time (current-time)))
      (apply orig-fun args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time))))

  (advice-add 'desktop-read :around 'desktop-read-debug-error-log-time)

  (defun desktop-create-buffer-log-time (orig-fun &rest args)
    (let ((start-time (current-time))
          (filename (nth 1 args)))
      (apply orig-fun args)
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename)))))

  (advice-add 'desktop-create-buffer :around 'desktop-create-buffer-log-time))

(provide 'init-benchmarking)
