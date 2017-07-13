;; eshell
(require 'helm-eshell)

;; just for *Help*
(mapc #'require '(em-alias em-banner em-basic em-cmpl em-dirs
      em-glob em-hist em-ls em-prompt em-rebind em-script
      em-smart em-term em-tramp em-unix em-xtra esh-arg esh-cmd
      esh-ext esh-groups esh-io esh-mode esh-module esh-opt
      esh-proc esh-util esh-var eshell))

;; modules
(add-to-list 'eshell-modules-list 'eshell-rebind)
(setq eshell-modules-list
   (delete 'eshell-pred eshell-modules-list))

(setq eshell-hist-rebind-keys-alist
  (--remove (member (car it) '([up] [down])) 
            eshell-hist-rebind-keys-alist))

(add-hook 'eshell-mode-hook 
  (lambda () 
    (setq eshell-cmpl-ignore-case t)
    (eshell-cmpl-initialize)
    (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")
    (push-mark)
    (define-keys eshell-mode-map 
      (kbd "M-]") 'helm-eshell-history
      [remap eshell-pcomplete] 'helm-esh-pcomplete
      (kbd "<home>") 'eshell-bol
      )
    (many 1 (lambda (c) (add-to-list 'eshell-cannot-leave-input-list c))
      'left-char 
      'right-char
      )
    ))

(setq eshell-prompt-function #'(lambda ()
   (let ((sp (propertize " " 'face '(:background "#fff"))))
     (concat
      sp (abbreviate-file-name (eshell/pwd)) "\n"
      sp (if (= (user-uid) 0) "#" "$") " ")) ))

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
(fset 'eshell 'my--eshell)

;;; eshell functions
(defun eshell/buffer-contents (buf)
  "Echo the contents of a given buffer"
  (interactive)
  (eshell/echo (with-current-buffer buf (buffer-string))))
