;;; init.el  -*- no-byte-compile: t; lexical-binding: t; -*-

(defvar profiler-emacs-init nil)
(when profiler-emacs-init
  (setq debug-on-error t)
  (profiler-start 'cpu))

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

;; custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)
(setq custom-safe-themes t)

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

(defun system-type-windowslike-p()
  (memq system-type '(ms-dos windows-nt cygwin)))

(defun system-type-wsl-p()
  "A predicate which is non-nil when we're running on WSL"
  (and (not (system-type-windowslike-p))
       (executable-find "cmd.exe")))

(when (system-type-windowslike-p)
  ;;;; cygwin bash hacks (required otherwise shell commands will always fail)
  ;; do this early because some package requires are going to try shell commands
  (require 'cygwin-mount)
  (require 'setup-cygwin)

  (cygwin-mount-activate)

  ;; remove windows line endings from comint shells
  (add-hook 'comint-output-filter-functions
            'shell-strip-ctrl-m nil t)

  ;; external shell
  (setq explicit-shell-file-name "bash.exe")

  ;; For subprocesses invoked via the shell
  ;; (e.g., "shell -c command")
  (setq shell-file-name explicit-shell-file-name)
  ;;(setq-default coding-system-for-read 'utf-8-unix)
  ;;(setq-default coding-system-for-write 'utf-8)
  ;;(set-coding-system-priority 'utf-8-unix 'utf-8-dos)
  ;;;; end cygwin bash hacks
  )

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt nil t)

;; refresh package contents
(unless package-archive-contents
  (package-refresh-contents))

(defun package-install-and-require(package)
  (unless (package-installed-p package)
    (package-install package))
  (require package))

;; install & require packages
(setq bootstrap-packages `(
  use-package
  anaphora
  dash
  delight ;; (used by use-package during construction phase)
  diminish
  list-utils
  gnu-elpa-keyring-update
  load-dir
  quelpa
  auto-compile
))
(mapc #'package-install-and-require bootstrap-packages)

;; autocompile for lisp files
;; as per its docs, run this as soon as possible
(setq load-prefer-newer t)
(if (require 'auto-compile nil t)
    (progn
      (setq auto-compile-display-buffer nil)
      (setq auto-compile-mode-line-counter t)
      (auto-compile-on-load-mode)
      (auto-compile-on-save-mode)

      (defvar-local my/auto-compile-pending-files nil)

      (defun my/auto-compile-later()
        (while my/auto-compile-pending-files
          (-let [(file nosuffix) (pop my/auto-compile-pending-files)]
            (unless (member file auto-compile--loading)
              (let ((auto-compile--loading (cons file auto-compile--loading))
                    byte-compile-verbose el elc el*)
                (with-demoted-errors (format "Byte compiling '%s' failed: %s" file "%s")
                  (when (setq el (auto-compile--locate-library file nosuffix))
                    (setq elc (byte-compile-dest-file el))
                    (when (not (file-exists-p elc))
                      (message "Compiling %s..." el)
                      (auto-compile--byte-compile-file el)
                      (message "Compiling %s...done" el)
                      )))
                ))
            )
          )
        )

      (run-with-idle-timer 0.5 t #'my/auto-compile-later)

      ;; the auto-compile package explicitly does not byte-compile files on load
      ;; unless they're already byte compiled. This adds the functionality to ALWAYS
      ;; byte compile loaded file
      (defun my/after-auto-compile-on-load (file &optional nosuffix)
        (push (list file nosuffix) my/auto-compile-pending-files))
      (advice-add 'auto-compile-on-load :after #'my/after-auto-compile-on-load)

      ;; advise `load-file' in the same was as `load'
      (define-advice load-file
          (:before (file))
        (when auto-compile-on-load-mode
          (auto-compile-on-load file t)))
      )
  (message "auto-compile not installed"))

;; bootstrap quelpa-use-package
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; use package loading statistics
(when profiler-emacs-init
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t))

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

;;;;;;;;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/customization/theme.el")

;;; fin
(setq emacs-init-finished t)
(advice-remove 'load 'log-and-load) ; end logging

;; this fails if the window is hidden because the user can't interact...
;; (package-autoremove) ;; remove packages which shouldn't be here

;; Display the init time in messages 
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(when profiler-emacs-init
  (profiler-stop))
