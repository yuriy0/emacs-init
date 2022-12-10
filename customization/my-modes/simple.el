(use-package simple
  :bind
  ("C-M-<backspace>" . pop-to-mark-command)
  ("C-M-\\" . pop-global-mark)

  :config

  ;; visual line mode
  (diminish 'visual-line-mode)
  ;; (add-hook 'prog-mode-hook 'visual-line-mode) ;; TODO we really want to use global visual line mode, but it breaks some special modes!
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  ;; (setq bidi-display-reordering t)

  (advice-add 'pop-global-mark :around #'my/pop-global-mark)
)

;;;###autoload
(defun my/pop-global-mark (fn &rest args)
  "Like `pop-global-mark', except if it tries to open a buffer, will use tab-bar-raise instead"
  (override-fun-nonrecursive switch-to-buffer tab-bar-raise-or-switch-to-buffer
    (apply fn args)))
