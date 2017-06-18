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

;; Packages to be installed for this file to work. Emacs 25>
(setq package-selected-packages '(
  ;; jdee ; this replaced malabar-mode a while back, but it's huge and who uses
          ; java anyways
  ;; workgroups2
  ac-helm 
  anaphora
  auto-complete
  auto-complete-clang
  batch-mode
  cc-mode
  cl-lib            
  command-log-mode
  csharp-mode
  cygwin-mount      
  dash 
  dash-functional
  diminish
  elm-mode
  f
  font-utils        
  framemove
  fringe-helper     
  fuzzy
  haskell-mode     
  helm 
  helm-ag
  helm-descbinds
  ido-yes-or-no
  intero
  list-utils
  load-dir
  magit
  markdown-mode 
  multiple-cursors
  pcache
  persistent-scratch
  persistent-soft
  php-mode
  popup             
  s  
  setup-cygwin
  shell-pop
  smex
  sml-mode
  ucs-utils
  unicode-fonts
  visual-regexp
  web-mode
  yaml-mode
))

;; suppress all warnings
(setq warning-minimum-level :emergency
      byte-compile-warnings nil)

;; packages
(require 'package)
(setq package-archives '(
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "https://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.org/packages/")
   ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
   ))
(setq package-enable-at-startup nil)
(package-initialize)
(package-autoremove)

;; fetch the list of packages available and install the missing packages
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; require user packages 
(mapc #'require package-selected-packages)

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
