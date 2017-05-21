(setq mouse-buffer-menu-mode-mult 0 ; right click menu
      inhibit-startup-screen t)     ; turn off startup screen

;;;###autoload
(defun many (nargs fn &rest args) 
  "Apply FN of arity NARGS to each consecutive group of NARGS
values in the ARGS list. If the length of ARGS is not divisible
by NARGS, the final trailing group of length < NARGS is ignored."
  (--map (apply fn it) (-partition nargs args)))

(defalias 'global-set-keys (apply-partially 'many 2 'global-set-key))
(defalias 'customize-set-variables (apply-partially 'many 2 'customize-set-variable))

;; keep server alive
;; http://stackoverflow.com/questions/2001485/how-do-i-keep-emacs-server-running-when-the-current-window-is-closed-x-on-wind 
(defvar really-kill-emacs nil)

;;;###autoload 
(defun really-kill-emacs ()
 (interactive)
 (setq really-kill-emacs t)
 (desktop-save-in-desktop-dir)
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

;; enable {up/down}case-region (copied from somewhere...)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

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
      auto-revert-interval 1)

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