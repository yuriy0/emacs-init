;; Basic Configuration
(add-to-list 'default-frame-alist '(width . 85))
(add-to-list 'default-frame-alist '(font . "Iosevka Extended 11"))

(use-package all-the-icons
  :ensure
  :defer t ;; note: loading this only if making a GUI frame

  :init
  (add-hook-once
   'after-make-frame-functions
   (lambda (&optional _frame)
     (when (display-graphic-p)
       (require 'all-the-icons)
       t)))

  (setq all-the-icons-scale-factor 1.0)
  (custom-set-variables
   '(all-the-icons-scale-factor 1.0))

  :config
  (setq all-the-icons-scale-factor 1.0)
)

;; fixes an issue with emacs hanging when viewing some unicode files
;; see https://github.com/purcell/emacs.d/issues/273
(setq inhibit-compacting-font-caches t)

(use-package ucs-utils :ensure)

(use-package descr-text :ensure)


;;;###autoload
(defun any-non-displayable ()
  (require 'descr-text) ;; for `describe-char-display'
  (setq found nil)
  (save-excursion
    (goto-char (point-min))

    (while (and (not found) (not (eobp)))
      (if (or (eolp)
              (looking-at "\t")
              (describe-char-display (point) (char-after)))
          (forward-char)
        (setq found t)
        ))
    )
  found)

;;;###autoload
(defun maybe-unicode-fonts-setup (&rest _unused)
  (when (any-non-displayable)
    (run-with-timer 0.5 nil #'unicode-fonts-setup)
    t))

(use-package unicode-fonts
  :ensure
  :requires (descr-text)

  :config

  ;; the first time we open a buffer which has non-printing characters, we'll run `unicode-fonts-setup'
  ;; if we run it too early it won't pick up some future faces which might be added by deferred setup
  (add-hook-once 'window-buffer-change-functions #'maybe-unicode-fonts-setup)
)

(use-package highlight-indent-guides
  :ensure
  :hook ((prog-mode . highlight-indent-guides-mode))
  :diminish highlight-indent-guides-mode
  :config

  (setq highlight-indent-guides-method 
        ;; 'character ;; fastest and non intrusive but buggy??
        'bitmap ;; slower but less buggy
        )
  (setq highlight-indent-guides-responsive 'stack)
)

(use-package solaire-mode
  :ensure
  :defer 4
  :diminish
  :config
  (solaire-global-mode +1)

  ;; kind of a hack, this package seems to assume that we always use a theme, so
  ;; configures this variable in advice to load-theme.
  (setq solaire-mode--supported-p t)

  ;; customization for what is considered a "real" buffer here
  (defun my/solaire-mode-real-buffer-p()
    (let*
        ((buf (current-buffer))
         (buf-name (buffer-name buf))
         (base-buf (buffer-base-buffer))
         (base-buf-file (buffer-file-name base-buf))
         )
      (or
       ;; don't modify minibuffer
       (minibufferp buf)

       ;; don't modify faces for transient buffers, like helm buffers. they
       ;; arent file-visiting buffers, but they typically have their own color
       ;; conventions.
       (transitive-bufferp buf)

       ;; "scratch" buffer is real (we save it to disk!)
       (equal buf-name "*scratch*")

       ;; file visiting base buffer is real
       base-buf-file
      )))
  (setq solaire-mode-real-buffer-fn #'my/solaire-mode-real-buffer-p)

  :custom-face
  (solaire-default-face ((t (:background "grey93"))))
  ;; (solaire-default-face ((t (:background "orange"))))
  )
