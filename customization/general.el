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
 (setq really-kill-emacs t)
 (desktop-save-in-desktop-dir)
 ;; (wg-save-session)
 (save-buffers-kill-emacs))

;;;###autoload 
(defadvice kill-emacs (around really-exit activate)
   "Only kill emacs if the variable is true"
   (if really-kill-emacs ad-do-it) (make-frame-invisible nil t))

;; easy keyboard escape (2<Esc> instead of 3<Esc>)
(require 'gnutls)
(global-set-key (kbd "<ESC> <ESC>") 'keyboard-escape-quit)  

;; Copy-paste settings 
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(global-set-keys 
 (kbd "C-z") 'undo
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

;; desktop 
(desktop-save-mode 1)
(setq desktop-load-locked-desktop t
      desktop-dirname "~/.emacs.d/desktop/"
      desktop-path (list desktop-dirname)
      desktop-save t 
      desktop-auto-save-timeout 120
      desktop-restore-eager 10)
;; disable modes on startup
(add-to-list 'desktop-minor-mode-table (list 'whitespace-mode nil)) 
(add-to-list 'desktop-minor-mode-table (list 'agda2-mode nil)) 

(add-hook 'desktop-after-read-hook 
  #'(lambda () 
      (delete-other-frames) ; single frame on startup 
))

(many 1 (apply-partially 'add-to-list 'desktop-globals-to-save) 
      'extended-command-history 
      'kill-ring)
             
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

;; cygwin
(require 'cygwin-mount)
(require 'setup-cygwin)

(cygwin-mount-activate)

(add-hook 'comint-output-filter-functions
    'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
    'comint-watch-for-password-prompt nil t)

;; external shell
(setq explicit-shell-file-name "bash.exe")

;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)
;;(setq-default coding-system-for-read 'utf-8-unix)
;;(setq-default coding-system-for-write 'utf-8)
;;(set-coding-system-priority 'utf-8-unix 'utf-8-dos)

;; window {un/re}do
(let ((map (make-sparse-keymap)))
  (progn
      (define-key map (kbd "C-c ,") 'winner-undo)
      (define-key map (kbd "C-c .") 'winner-redo)
      (setq winner-mode-map map)))
(winner-mode t)

;; require doesn't cut it
(smex-initialize)

;; who needs this...
(fset 'find-file-read-only 'find-file)

;; highlight parens
(setq show-paren-delay 0)
(add-hook 'prog-mode-hook '(lambda () (show-paren-mode 1)))

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
  '(lambda () (setq-local inhibit-eol-conversion t)))

;; save scratch file
(persistent-scratch-setup-default)

;; {frame+wind}move
(global-set-keys 
 (kbd "C-x C-<left>") 'windmove-left
 (kbd "C-x C-<right>") 'windmove-right
 (kbd "C-x C-<up>") 'windmove-up
 (kbd "C-x C-<down>") 'windmove-down)
(setq framemove-hook-into-windmove t)
(framemove-default-keybindings 'super)

;; which-key
(which-key-mode)
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'right)
(setq which-key-side-window-max-width 0.45)
(setq which-key-idle-delay 3.0)
(define-key global-map (kbd "C-h /") 'which-key-show-major-mode)

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
   '("[\\/]wscript\\'" . python-mode)
   '("\\.waf_files\\'" . text-mode)
)

;; HACK for Tramp to work with cygwin
;; For rationale, see:
;;   https://www.gnu.org/software/emacs/manual/html_node/tramp/Windows-setup-hints.html#Windows-setup-hints 
;;   https://www.emacswiki.org/emacs/SshWithNTEmacs
(cond
  ( (not (executable-find "fakecygpty"))
    (message "Skipping 'fakecygpty' since 'fakecygpty.exe' not found on path")
  )
  (t (progn
       (add-to-list 'load-path "~/.emacs.d/fakecygpty")
       (require 'fakecygpty)
       (fakecygpty-activate)
     )
  )
)

(eval-after-load "tramp"
  '(progn
     (add-to-list 'tramp-methods
                  (mapcar
                   (lambda (x)
                     (cond
                      ((equal x "sshx") "cygssh")
                      ((eq (car x) 'tramp-login-program) (list 'tramp-login-program "fakecygpty ssh"))
                      (t x)))
                   (assoc "sshx" tramp-methods)))
     (setq tramp-default-method "cygssh"))
)
