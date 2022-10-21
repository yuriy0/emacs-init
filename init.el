;;;;;;;;;;;
;; fonts ;;
;;;;;;;;;;;
(set-face-attribute 'default nil 
  :family "Courier New" 
  :foundry "outline" 
  :slant 'normal 
  :weight 'normal 
  :height 120
  :width 'normal)

;; same behaviour as default, but allows customization of 
;; the behaviour for specific modes by using add-to-list 
(setq font-lock-maximum-decoration '((t . t)))

;;;;;;;;;;;;;;;;;;;;
;; initialization ;;
;;;;;;;;;;;;;;;;;;;;
;; server start 
(server-start)

;; set home directory as emacs default (aka home) directory 
(setq default-directory (concat (getenv "HOME") "/"))

(defvar emacs-init-finished nil)
(defvar emacs-init-stage "~/.emacs.d/init.el")

;;;###autoload
(defun check-emacs-init-finished ()
  (interactive)
  (if emacs-init-finished 
      (message "emacs initialization finished.")
      (message "%s" 
               (propertize 
                (format "emacs initialization failed while loading %s!" emacs-init-stage)
                'face '(:foreground "red"))) ))

;; replace startup echo msg
(fset 'display-startup-echo-area-message
      'check-emacs-init-finished)

;; don't use custom
(setq custom-file "NUL")


(require 'cl-lib)
(cl-defstruct initmod
  name autoload)

;; Packages to be installed for this file to work. Emacs 25>
(setq package-selected-packages `(

  cl-lib            
  command-log-mode
  concurrent
  cmake-mode
  csharp-mode

  dash 
  dash-functional

  deferred				
  dired-hacks-utils
  dired-rainbow
  dired-subtree
  diminish
  elm-mode
  f
  font-utils        
  fringe-helper     
  fuzzy
  gnu-elpa-keyring-update
  haskell-mode     
  ido-yes-or-no
  json-mode
  list-utils
  load-dir
  lua-mode
  magit
  markdown-mode 
  multiple-cursors
  pcache
  persistent-scratch
  persistent-soft
  php-mode
  popup             
  powershell
  python-mode
  s
  shell-pop
  smex
  sml-mode
  ucs-utils
  unicode-fonts
  visual-regexp
  web-mode
  web-server
  which-key
))

;; suppress all warnings
(setq warning-minimum-level :error
      byte-compile-warnings nil)

;; packages
(require 'package)
(setq package-archives '(
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   )
)

(setq package-enable-at-startup nil)
(package-initialize)

;; local packages
(add-to-list 'load-path (concat user-emacs-directory "/emacswiki-pkg"))

;;;; cygwin bash hacks (required otherwise shell commands will always fail)
;; do this early because some package requires are going to try shell commands
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
;;;; end cygwin bash hacks

;; refresh package contents
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)


;; put packages in the correct form
(setq package-selected-packages
      (mapcar (lambda(p) (if (symbolp p) (make-initmod :name p :autoload t) p)) package-selected-packages)
)

;; install any missing packages
(dolist (package package-selected-packages)
  (unless (or (not (initmod-autoload package)) (package-installed-p (initmod-name package)))
    (package-install (initmod-name package))))

;; require user packages
(mapc (lambda (p)
	(if (package-installed-p (initmod-name p))
	    (require (initmod-name p)))
	)
      package-selected-packages)

;; whenever you install/update a package, the `package' package will (through
;; 'customize') automatically clobber `package-selected-packages'. 
;;;###autoload
(defun package--dont-save-selected-packages (&optional val) 
  (progn (message "Override package--dont-save-selected-packages called!") nil))
(advice-add 'package--save-selected-packages :override 'package--dont-save-selected-packages)

;;;;;;;;;;;;;;;;;;;
;; begin logging ;;
;;;;;;;;;;;;;;;;;;;

;; return `nil' from `load' instead of returning a void value/throwing an error,
;; etc. this might confuse other emacs functionality... 
;;;###autoload
(defun load-ret-nil (the-load &rest load-args) 
  (unwind-protect
      (let ((debug-on-error nil))
        (with-demoted-errors
            (setq load-ret-nil--ret-val (apply the-load load-args)) ))
    (boundp 'load-ret-nil--ret-val )))
(advice-add 'load :around 'load-ret-nil)
    
;; saves the loading file in `emacs-init-stage', then loads the file, then
;; resets the previous value of `emacs-init-stage'. Also forces the printing of
;; loading messages.
;;;###autoload
(defun log-and-load (the-load &rest load-args) 
  (let ((old-stage emacs-init-stage)
        (new-stage (nth 0 load-args))
        (old-force-load force-load-messages))
    (setq emacs-init-stage new-stage)
    (setq force-load-messages t)
    (let ((load-res (apply the-load load-args)))
      (when load-res 
        (setq emacs-init-stage old-stage))
      (setq force-load-messages old-force-load)
      load-res)))
(advice-add 'load :around 'log-and-load) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/customization/general.el")

;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/customization/interactive-functions.el")

;;;;;;;;;;;;;;;;;;;
;; Custom advice ;;
;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/customization/advice.el")

;;;;;;;;;;;;;;;;;;
;; Custom modes ;;
;;;;;;;;;;;;;;;;;;
(load-dir-one "~/.emacs.d/customization/my-modes/")


;;; fin
(setq emacs-init-finished t)
(advice-remove 'load 'log-and-load) ; end logging

;; this fails if the window is hidden because the user can't interact...
;; (package-autoremove) ;; remove packages which shouldn't be here
