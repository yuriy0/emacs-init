

(use-package treemacs
  :defer t
  :ensure t
  :commands (treemacs)

  :config

  ;; fixes a bug where the treemacs icons have the wrong background until you change
  ;; a theme or have set a theme before requiring treemacs. if we use the default theme
  ;; the icon colors are somehow wrong?
  (add-hook-once 'treemacs-mode-hook
                 (lambda()
                   (treemacs--updated-icon-background-colors)
                   ;; t
                   nil
                   ))

  (setq treemacs-follow-mode nil)
)

;;;###autoload
(defun treemacs--updated-icon-background-colors()
  ;; update the icon background: look at solaire mode BG face, if it exists, as
  ;; well as default face
  (setf treemacs--not-selected-icon-background
        (or
         (and
          (facep 'solaire-default-face)
          ;; solaire-mode ;; TODO seems to always be nil here when called from hook?
          (treemacs--get-local-face-background 'solaire-default-face)
          )
         (treemacs--get-local-face-background 'default)
         treemacs--not-selected-icon-background
         ))

  ;; this is taken exactly from `treemacs--setup-icon-background-colors` which
  ;; will only be called if changing themese
  (dolist (theme treemacs--themes)
    (treemacs--maphash (treemacs-theme->gui-icons theme) (_ icon)
      (treemacs--set-img-property
       (get-text-property 0 'img-selected icon)
       :background treemacs--selected-icon-background)
      (treemacs--set-img-property
       (get-text-property 0 'img-unselected icon)
       :background treemacs--not-selected-icon-background))))


(use-package treemacs-all-the-icons
  :ensure t
  :after (:all treemacs all-the-icons)

  )

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
)

(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t

  :config
  (treemacs-set-scope-type 'Tabs)
)
