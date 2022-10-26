(use-package python-mode
  :ensure
  :commands (python-mode)

  :config

  ;; Don't use electric indent in python mode
  (add-hook 'python-mode-hook
            (lambda ()
              (electric-indent-mode -1)
              (setq electric-indent-chars (delq ?: electric-indent-chars))))
  
  ;; Override `py-indent-or-complete' to never call any completion function
  (defun py-indent-or-complete ()
    "Complete or indent depending on the context.
  
  If cursor is at end of a symbol, try to complete
  Otherwise call `py-indent-line'
  
  If `(use-region-p)' returns t, indent region.
  Use `C-q TAB' to insert a literally TAB-character
  
  In ‘python-mode’ `py-complete-function' is called,
  in (I)Python shell-modes `py-shell-complete'"
    (interactive "*")
    (setq py-last-window-configuration
          (current-window-configuration))
    (cond ((use-region-p)
  	 (py-indent-region (region-beginning) (region-end)))
          (t (py-indent-line))
    )
  )
  
  (add-to-list 'auto-mode-alist
     '("[\\/]wscript\\'" . python-mode)
  )
)
