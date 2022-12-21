;; -*- lexical-binding: t; -*-

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

  ;; makes it easier to advise this function via letf...
  (advice-add 'which-key--create-pages :filter-return
              (defun my/which-key--create-pages(pages-obj)
                pages-obj
                ))

  (with-eval-after-load 'repeat
    ;; taken from https://karthinks.com/software/it-bears-repeating/
    ;;
    ;; Disable the built-in repeat-mode hinting, instead spawn a which-key popup
    ;; when repeat mode activates.
    (defvar which-key-side-window-location)
    (defvar which-key-side-window-max-height)

    (setq repeat-echo-function #'ignore)
    (defun repeat-help--which-key-popup ()
      (if-let ((should-activate repeat-in-progress)
               (cmd (or this-command real-this-command))
               (keymap (or repeat-map (repeat--command-property 'repeat-map))))
          (run-at-time
           0 nil
           (lambda ()
             (cl-letf
                 (
                  ;; override configuration of which-key temporarily: show at
                  ;; bottom with a smaller window than normal; and add some text
                  ;; indicating that we're in repeat mode to the title area of
                  ;; the repeat mode buffer
                  (which-key-side-window-location 'bottom)
                  (which-key-side-window-max-height 0.1)
                  ((symbol-function 'my/which-key--create-pages)
                   (lambda (pages-obj)
                     (if pages-obj
                         (modf-v (which-key--pages-prefix-title pages-obj) title
                                 (concat "Repeating... " title)
                                 ))
                     pages-obj
                     ))
                  )
               (which-key--create-buffer-and-show
                nil (symbol-value keymap))
               )
             )
           )
        (which-key--hide-popup)))
    (advice-add 'repeat-post-hook :after #'repeat-help--which-key-popup)
  )
)


