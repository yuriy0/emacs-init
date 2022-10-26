(use-package all-the-icons
  :ensure
  :if (display-graphic-p))


;; fixes an issue with emacs hanging when viewing some unicode files
;; see https://github.com/purcell/emacs.d/issues/273
(setq inhibit-compacting-font-caches t)

(use-package unicode-fonts
  :ensure

  :config
  (unicode-fonts-setup)
)

(use-package ucs-utils :ensure)

