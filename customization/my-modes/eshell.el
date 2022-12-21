;; eshell
(use-package eshell
  :ensure
  :commands (eshell eshell-mode eshell-command)

  :bind
  (:map eshell-mode-map
   ("M-]" . 'helm-eshell-history)
   ("M-/" . 'helm-esh-pcomplete))

  (:map eshell-rebind-mode-map
   ("C-c l" . eshell-lock-local-map))

  :config
  ;; just for *Help*
  (mapc #'require '(em-alias em-banner em-basic em-cmpl em-dirs
                             em-glob em-hist em-ls em-prompt em-rebind em-script
                             em-smart em-term em-tramp em-unix em-xtra esh-arg esh-cmd
                             esh-ext esh-groups esh-io esh-mode esh-module esh-opt
                             esh-proc esh-util esh-var eshell))

  (require 'helm-eshell)

  ;; modules
  (add-to-list 'eshell-modules-list 'eshell-rebind)
  (setq eshell-modules-list
        (delete 'eshell-pred eshell-modules-list))

  ;; don't rebind <up> and <down> as history scroll keys
  (setq eshell-hist-rebind-keys-alist
        (--remove (member (car it) '([up] [down]))
                  eshell-hist-rebind-keys-alist))

  ;; don't rebind some keys...
  (setq eshell-rebind-keys-alist
        (--remove (member (car it)
                          '([home] [backspace] [delete]))
                  eshell-rebind-keys-alist))

  ;; additional keybinds which move point but should not move into the prompt string
  (many 1 (lambda (c) (add-to-list 'eshell-cannot-leave-input-list c))
        'left-char
        'right-char
        'move-beginning-of-line
        )

  ;; prompt colour
  (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")

  ;; ignore case in completions
  (setq eshell-cmpl-ignore-case t)

  ;; customization of eshell
  (add-hook 'eshell-mode-hook
            (defun my/eshell-mode-hook()
              (eshell-cmpl-initialize)
              (push-mark) ;; adds point in ehsell to mark ring
              ))

  ;; customize prompt
  (setq eshell-prompt-function #'(lambda ()
     (let ((sp (propertize " " 'face 'solaire-default-face)))
       (concat
        sp (abbreviate-file-name (eshell/pwd)) "\n"
        sp (if (= (user-uid) 0) "#" "$") " ")) ))


  (advice-add 'eshell :override #'my--eshell)
)

;; copied almost verbatim from eshell.el (***). The behaviour which is
;; overridden is: "A nonnumeric prefix arg means to create a new session."
;;;###autoload
(defun my--eshell (&optional arg)
  "Create an interactive Eshell buffer.
The buffer used for Eshell sessions is determined by the value of
`eshell-buffer-name'.  If there is already an Eshell session
active in that buffer, Emacs will simply switch to it.
Otherwise, a new session will begin.  A numeric prefix arg (as in
`C-u 42 M-x eshell RET') switches to the session with that
number, creating it if necessary. Returns the buffer selected (or
created)."
  (interactive "P")
  (cl-assert eshell-buffer-name)
  (let (;; *** except this part ;;;;;;;;;;;;;;;;;;;
        (buf (get-buffer-create
              (format "%s<%d>"
                      eshell-buffer-name
                      (if (numberp arg) arg 0))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        )
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

;;; eshell functions
;;;###autoload
(defun eshell/buffer-contents (buf)
  "Echo the contents of a given buffer"
  (interactive)
  (eshell/echo (with-current-buffer buf (buffer-string))))
