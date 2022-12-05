

(use-package treemacs
  :defer t
  :ensure t
  :config

  ;; fixes a bug where the treemacs icons have the wrong background until you change
  ;; a theme or have set a theme before requiring treemacs. if we use the default theme
  ;; the icon colors are somehow wrong?
  (add-hook-once 'treemacs-mode-hook
                 (lambda() (treemacs--updated-icon-background-colors) t))
)

;;;###autoload
(defun treemacs--updated-icon-background-colors()
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
