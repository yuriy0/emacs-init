;; see https://github.com/rksm/emacs-rust-config/blob/master/init.el

(add-hook 'rustic-mode-hook
  (lambda ()

    (setq indent-tabs-mode nil)
    (setq tab-width 2)
    (setq default-tab-width 2)
    (setq rust-indent-offset 2)
  )
)
