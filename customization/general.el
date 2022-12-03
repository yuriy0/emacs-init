;; -*- lexical-binding: t; -*-

(setq mouse-buffer-menu-mode-mult 0 ; right click menu
      inhibit-startup-screen t)     ; turn off startup screen

;; only angry people use capslock
(setq w32-enable-caps-lock nil)

;;;###autoload
(defun many (nargs fn &rest args) 
  "Apply FN of arity NARGS to each consecutive group of NARGS
values in the ARGS list. If the length of ARGS is not divisible
by NARGS, the final trailing group of length < NARGS is ignored."
  (--map (apply fn it) (-partition nargs args)))

(defalias 'global-set-keys (apply-partially 'many 2 'global-set-key))
(defalias 'customize-set-variables (apply-partially 'many 2 'customize-set-variable))
(defalias 'define-keys 
  (lambda(m &rest as) 
    (apply 'many (cons 2 (cons (apply-partially 'define-key m) as)))))

;; keep server alive
;; http://stackoverflow.com/questions/2001485/how-do-i-keep-emacs-server-running-when-the-current-window-is-closed-x-on-wind 
(defvar really-kill-emacs nil)

;;;###autoload 
(defun really-kill-emacs ()
 (interactive)
 (desktop-save-in-desktop-dir)
 (let ((really-kill-emacs t)) (save-buffers-kill-emacs)))

(defun non-kill-emacs()
  "Replacement action for trying to kill emacs when running in
server mode. For GUI, hides the current frame; for TTY, deletes
it, since making the frame invisible is impossible there"
  (if (display-graphic-p)
      (make-frame-invisible nil t)
    (delete-frame)))


(defun my/kill-emacs (fn &rest args)
  "Only kill emacs if the variable is true"
  (if really-kill-emacs
      (apply fn args)
    (message "kill-emacs - not killing, instead making frame invisible")
    (non-kill-emacs)))
(advice-add 'kill-emacs :around #'my/kill-emacs)

;; used by `save-buffers-kill-emacs' to avoid quitting emacs based on user defined conditions
;; strangely, somehow `save-buffers-kill-emacs' will quit even though it seems to call `kill-emacs'?
;; i.e. it is somehow skipping our advice?
(add-hook #'kill-emacs-query-functions
          (lambda()
            (if really-kill-emacs t
              (message "save-buffers-kill-emacs - not killing, instead making frame invisible")
              (non-kill-emacs)
              nil))
          99
          )

;; easy keyboard escape (2<Esc> instead of 3<Esc>)
(require 'gnutls)
(global-set-key (kbd "<ESC> <ESC>") 'keyboard-escape-quit)  

;; Copy-paste settings
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(global-set-keys
 (kbd "C-c c") 'copy-region-as-kill
 (kbd "C-v") 'yank)
(setq delete-selection-mode t) 

;; fill paragraphs at width 80 
(setq-default fill-column 80)

;; electric-indent sucks
(global-set-key (kbd "RET") 'electric-indent-just-newline)

;; backup files
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/emacs_backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath ) )
(setq make-backup-file-name-function 'my-backup-file-name)

(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-by-copying t 
      version-control t 
      kept-old-versions 2 
      kept-new-versions 20 
      delete-old-versions t
      tramp-backup-directory-alist backup-directory-alist
      auto-save-file-name-transforms nil)

;; no lock file
(setq create-lockfiles nil)

;; no autosave
(setq auto-save-default nil)

;; enable some disabled-by-default comands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

;; split vertically 
(setq split-height-threshold 0
      split-width-threshold nil)

;; i hate beeping
(setq visible-bell 1)

;; remove toolbar 
(tool-bar-mode -1)

;; set cursor to bar, and override multiple cursors function which distinguishes
;; between bar and other cursors.
(setq-default cursor-type 'bar) 

(defun mc/cursor-is-bar-fake () nil)
(advice-add 'mc/cursor-is-bar :override 'mc/cursor-is-bar-fake)

;; auto-revert
(setq revert-without-query (list ".+") ;; auto revert any file without confirmation
      auto-revert-interval 1
      auto-revert-use-notify nil)

;; ignore changes to file on disk (but warn about them)
(defun ask-user-about-supersession-threat (fn)
  (message "File %s changed on disk, overriding modifications!"
           (buffer-file-name)))

;; f5 to refresh 
(global-set-key (kbd "<f5>") (lambda () (interactive) (revert-buffer t t)))

;; tabs are EVIL 
(setq-default indent-tabs-mode nil)

;; window {un/re}do
(let ((map (make-sparse-keymap)))
  (progn
      (define-key map (kbd "C-c ,") 'winner-undo)
      (define-key map (kbd "C-c .") 'winner-redo)
      (setq winner-mode-map map)))
(winner-mode t)

;; who needs this...
(fset 'find-file-read-only 'find-file)

;; highlight parens
(setq show-paren-delay 0)
(add-hook 'prog-mode-hook (lambda () (show-paren-mode 1)))

;; display the next buffer in the same window if the current buffer is a help
;; buffer
(defun mode-of-buffer (buf) (with-current-buffer buf major-mode))
(defun transitive-bufferp (buf) 
  (or (memq (mode-of-buffer buf) 
            '(helm-major-mode) )
      (minibufferp buf)))
(defun same-window-buffers (buf ac)
  "Returns t if the given buffer is not a minibuffer type buffer 
(actual minibuffer or helm mode); and if the current buffer is a
help mode buffer (i.e. if the given buffer should be displayed in
the same window)."
  (and (memq (mode-of-buffer (current-buffer)) '(help-mode apropos-mode))
       (not (transitive-bufferp buf))))
(add-to-list 'display-buffer-alist '(same-window-buffers display-buffer-same-window))

;; confusing
(add-hook 'sh-mode-hook
  (lambda () (setq-local inhibit-eol-conversion t)))

;; column numbers
(column-number-mode t)

;; default emacs keys which I don't use which annoy me
(many 1 'global-unset-key
  (kbd "C-x <left>")
  (kbd "C-x <right>")
  (kbd "C-x C-b") )

;; use recycle bin
(setq delete-by-moving-to-trash t)

;; make frame
(define-key global-map (kbd "C-x 5 -") 'make-frame)

;; for WAF build system
(add-to-list 'auto-mode-alist
   '("\\.waf_files\\'" . text-mode)
)

;; HACK for Tramp
;; The default does not like the ~ before > so do a kludge
(setq shell-prompt-pattern '"^[^#$%>\n]*~?[#$%>] *")

;; HACK for Tramp to work with cygwin
;; For rationale, see:
;;   https://www.gnu.org/software/emacs/manual/html_node/tramp/Windows-setup-hints.html#Windows-setup-hints 
;;   https://www.emacswiki.org/emacs/SshWithNTEmacs
;; (cond
;;   ( (not (executable-find "fakecygpty"))
;;     (message "Skipping 'fakecygpty' since 'fakecygpty.exe' not found on path")
;;   )
;;   (t (progn
;;        (add-to-list 'load-path "~/.emacs.d/fakecygpty")
;;        (require 'fakecygpty)
;;        (fakecygpty-activate)
;;      )
;;   )
;; )

;; (eval-after-load "tramp"
;;   '(progn
;;      (add-to-list 'tramp-methods
;;                   (mapcar
;;                    (lambda (x)
;;                      (cond
;;                       ((equal x "sshx") "cygssh")
;;                       ((eq (car x) 'tramp-login-program) (list 'tramp-login-program "fakecygpty ssh"))
;;                       (t x)))
;;                    (assoc "sshx" tramp-methods)))
;;      (setq tramp-default-method "cygssh"))
;; )

;; Use utf-8 encoding by default
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)

(set-selection-coding-system
 (if (system-type-windowslike-p)
     'utf-16-le  ;; must use this because Windows uses UTF16 for clipboard...
   'utf-8-auto)
)

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; disables secondary selection keybinds
(global-unset-key [M-mouse-1] )
(global-unset-key [M-drag-mouse-1] )
(global-unset-key [M-down-mouse-1] )
(global-unset-key [M-mouse-3] )
(global-unset-key [M-mouse-2] )

;; disable undo/redo default binds
(global-unset-key (kbd "C-/"))
(global-unset-key (kbd "C-?"))

;; in tty mode disable the menu bar
(when (not (display-graphic-p))
  (menu-bar-mode -1))
