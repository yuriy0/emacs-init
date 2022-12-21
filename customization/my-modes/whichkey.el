(use-package which-key
  :ensure
  :defer 10 ;; which-key mainly functions by an idle timer so we have no good way to determine when this is needed
  :delight which-key-mode

  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-show-prefix 'top
        which-key-side-window-max-width 0.45
        which-key-idle-delay 2.5
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h t
        which-key-unicode-correction 2
        )

  (define-key global-map (kbd "C-h /") 'which-key-show-major-mode)

  (defun add-strikethrough(s)
    (add-to-faces s '(:strikethrough "black")))

  ;; adds some extra formatting to keybinds which are displaed by whichkey
  (define-advice which-key--format-and-replace
      (:around (fn &rest args) extra-formatting)
    (let ((r (apply fn args)))
      (cl-map 'list (lambda(formatted unformatted)
                      (cond
                       ;; (cdr unformatted) is a string with special meaning
                       ;; based on the type of command keybind. see
                       ;; `which-key--get-keymap-bindings-1'
                       ((equal (cdr unformatted) "nil")

                        ;; this is a menu-item whose :filter property returned nil
                        ;; (the nil gets stringified via symbol-name)
                        (list
                         (add-strikethrough (nth 0 formatted))
                         (nth 1 formatted)
                         (concat (add-strikethrough (nth 2 formatted)) " ")
                         ))

                       ;; default: return unchanged
                       (t
                        formatted))
                      )
              r
              (car args))))
)
