;; Basic Configuration
(add-to-list 'default-frame-alist '(width . 85))
(add-to-list 'default-frame-alist '(font . "Iosevka Extended 11"))

(use-package all-the-icons
  :ensure
  :if (display-graphic-p))

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
  :config

  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
)
