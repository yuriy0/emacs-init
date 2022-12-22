;; eshell  -*- lexical-binding: t; -*-

(use-package eshell
  :ensure
  :commands (eshell eshell-mode eshell-command)

  :config
  ;; load eshell modules
  (mapc #'require
        '(helm-eshell
          esh-mode
          em-prompt
          em-rebind ;; change localmap based on whether the cursor is in the command area
          em-hist ;; provides command history
          em-cmpl ;; provides completions
          ))

  ;; customize keybinds
  ;; we don't use use-package for these because it ends up loading too much eagerly
  (bind-keys
   :map eshell-mode-map
   ("M-]" . helm-eshell-history)
   ("M-/" . helm-esh-pcomplete)
   ("C-w" . kill-region)

   :map eshell-rebind-mode-map
   ("C-c l" . eshell-lock-local-map))

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
                          '([home] [backspace] [delete]
                            [(control 97)]
                            [(control 119)]))
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
              (setenv "TERM" "xterm-256color") ;; used for xterm-color
              ))

  ;; customize prompt
  (setq eshell-prompt-function #'(lambda ()
     (let ((sp (propertize " " 'face 'solaire-default-face)))
       (concat
        sp (abbreviate-file-name (eshell/pwd)) "\n"
        sp (if (= (user-uid) 0) "#" "$") " ")) ))

  (advice-add 'eshell :override #'my--eshell)

  ;; ansi colors in eshell via xterm-color
  (require 'xterm-color)
  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
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
