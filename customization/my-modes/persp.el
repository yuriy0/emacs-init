(use-package persp-mode
  :ensure t
  :disabled
  :commands (persp-mode)

  :hook
  (persp-mode . persp-mode-setup-advice)

  :custom
  (persp-lighter
   '(:eval
     (format
      (propertize
       " #%.26s"
       'face (let ((persp (get-current-persp)))
               (if persp
                   (if (persp-contain-buffer-p (current-buffer) persp)
                       'persp-face-lighter-default
                     'persp-face-lighter-buffer-not-in-persp)
                 'persp-face-lighter-nil-persp)))
      (safe-persp-name (get-current-persp)))))
  )

(use-package treemacs-persp
  :ensure t
  :after (treemacs persp-mode)
  )

(defvar persp-mode-functions-to-advise
  '(next-buffer
    previous-buffer
    helm-mini
    helm-imenu-in-all-buffers
    helm-multi-occur-all
    helm-buffer-list)
  "List of functions which need additional advising when using `persp-mode'.")

;;;###autoload
(defun persp-mode-wrapper (wrapped-buffer-command &rest r)
  "Wrapper for commands which need advising for use with `persp-mode'.
Only for use with `advice-add'."
  (with-persp-buffer-list () (apply wrapped-buffer-command r)))

;;;###autoload
(defun persp-mode-setup-advice ()
  "Adds or removes advice on functions in `persp-mode-functions-to-advise'."
  (message (format "persp-mode-setup-advice %s" persp-mode))
  ;; (cl-loop for func in persp-mode-functions-to-advise
  ;;          do (if persp-mode
  ;;                 (advice-add func :around #'persp-mode-wrapper)
  ;;               (advice-remove func #'persp-mode-wrapper)))
)
