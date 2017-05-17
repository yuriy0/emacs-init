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

;;;;;;;;;;;;;;;;;;;;
;; initialization ;;
;;;;;;;;;;;;;;;;;;;;
(defvar emacs-init-finished nil)
;;;###autoload
(defun check-emacs-init-finished ()
  (interactive)
  (if emacs-init-finished 
      (message "emacs initialization finished.")
      (message "%s" 
               (propertize 
                "emacs initialization failed!"
                'face '(:foreground "red"))) ))
(add-hook 'after-init-hook 'check-emacs-init-finished)

;; whenever you install/update a package, the `package' package will (through
;; 'customize') automatically clobber `package-selected-packages'. 
;;;###autoload
(defun package--dont-save-selected-packages (&optional val) 
  (progn (message "Override package--dont-save-selected-packages called!") nil))
(advice-add 'package--save-selected-packages :override 'package--dont-save-selected-packages)

;; load custom before setting package-selected-packages
(setq custom-file "~/.emacs.d/custom-set.el")
(load custom-file 'noerror)

;; Packages to be installed for this file to work. Emacs 25>
(setq package-selected-packages '(
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
  diminish
  elm-mode
  f
  font-utils        
  fringe-helper     
  fuzzy
  haskell-mode     
  helm 
  helm-ag
  ido-yes-or-no
  intero
  list-utils        
  malabar-mode
  multiple-cursors
  pcache            
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

;; set home directory as emacs default (aka home) directory 
(setq default-directory (concat (getenv "HOME") "/"))

;; server start 
(server-start)

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

;; ido mode
(require 'ido)
(ido-mode t) 
(require 'ido-yes-or-no)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general customization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; easy keyboard escape (2<Esc> instead of 3<Esc>)
(require 'gnutls)
(global-set-key (kbd "<ESC> <ESC>") 'keyboard-escape-quit)  

;; Copy-paste settings 
;; (setq cua-keep-region-after-copy t) ;; Standard Windows behaviour
(global-set-key (kbd "C-z") 'undo) 
(global-set-key (kbd "C-c c") 'copy-region-as-kill)
(setq delete-selection-mode t) 
(global-set-key (kbd "C-v") 'yank)

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
      desktop-auto-save-timeout 120)
;; disable modes on startup
(add-to-list 'desktop-minor-mode-table (list 'whitespace-mode nil)) 
(add-to-list 'desktop-minor-mode-table (list 'agda2-mode nil)) 

(add-hook 'desktop-after-read-hook 
  #'(lambda () 
      (delete-other-frames) ; single frame on startup 
))

;; remove toolbar 
(tool-bar-mode -1)

;; set cursor to bar, and override multiple cursors function which distinguishes
;; between bar and other cursors.
(setq-default cursor-type 'bar) 

(defun mc/cursor-is-bar-fake () nil)
(advice-add 'mc/cursor-is-bar :override 'mc/cursor-is-bar-fake)

;; whitespace mode 
(require 'whitespace)
(setq whitespace-style '(tabs tab-mark trailing))
(setq-default whitespace-line-column 80)
(global-whitespace-mode 1)

;; auto-revert
(setq revert-without-query (list ".+") ;; auto revert any file without confirmation
      auto-revert-interval 1)

;; f5 to refresh 
(global-set-key (kbd "<f5>") (lambda () (interactive) (revert-buffer t t)))

;; tabs are EVIL 
(setq-default indent-tabs-mode nil)

;; misc.
(setq describe-char-unidata-list
  (quote
   (name old-name general-category decomposition decimal-digit-value digit-value numeric-value uppercase))
   mouse-buffer-menu-mode-mult 0
   inhibit-startup-screen t)

(defun display-startup-echo-area-message () nil)

;; same behaviour as default, but allows customization of 
;; the behaviour for specific modes by using add-to-list 
(setq font-lock-maximum-decoration '((t . t)))

;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun kill-all-buffers ()
  "Kill every buffer except the current one."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;;;###autoload
(defun shell-command-silent (cmd)
  "Execute the shell command but do not open the MESSAGES buffer even if there is output."
  (interactive "sCommand: ")
  (with-temp-buffer (shell-command cmd t)))
(global-set-key (kbd "M-#") 'shell-command-silent)

;;;###autoload
(defun toggle-window-split ()
  "If frame is split into two horizontally/vertically, split it vertically/horizontally instead."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                  (car (window-edges (next-window))))
               'split-window-horizontally
               'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
        (funcall splitter)
        (if this-win-2nd (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (if this-win-2nd (other-window 1))))))
(define-key ctl-x-4-map "t" 'toggle-window-split)

;;;###autoload
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;###autoload
(defun delete-dos-eol ()
  "Remove ^M at end of line in the whole buffer."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((remove-count 0))
        (goto-char (point-min))
        (while (re-search-forward (concat (char-to-string 13) "$") (point-max) t)
          (setq remove-count (+ remove-count 1))
          (replace-match "" nil nil))
        (message (format "%d ^M removed from buffer." remove-count))))))

;;;###autoload 
(defun canon-win-path (path)
  "Convert the given path to a canonical, Windows path"
  (interactive)
  (replace-regexp-in-string "/" "\\\\" (convert-standard-filename (expand-file-name path))))

;;;###autoload
(defun unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t))

;;;###autoload
(defun dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos t))

;;;###autoload
(defun mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-mac t))

;; http://batsov.com/articles/2011/11/12/emacs-tip-number-2-open-file-in-external-program/
;;;###autoload
(defun prelude-open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command-silent (concat "cmd /C start \"\" \"" buffer-file-name "\""))))
(global-set-key (kbd "C-c o") 'prelude-open-with)

;;;###autoload
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file (file-name-nondirectory name) new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;###autoload
(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; https://www.emacswiki.org/emacs/RevertBuffer#toc1
;;;###autoload
(defun revert-all-buffers ()
  "Iterate through the list of buffers and revert them, e.g. after a
    new branch has been checked out."
  (interactive)
  (when (yes-or-no-p "Are you sure - any changes in open buffers will be lost! ")
    (let ((frm1 (selected-frame)))
      (let ((frm2 (next-frame frm1)))
        (select-frame frm2)
        (make-frame-invisible)
        (dolist (x (buffer-list))
          (let ((test-buffer (buffer-name x)))
            (when (not (string-match "\*" test-buffer))
              (when (not (file-exists-p (buffer-file-name x)))
                (select-frame frm1)
                (when (yes-or-no-p (concat "File no longer exists (" (buffer-name x) "). Close buffer? "))
                  (kill-buffer (buffer-name x)))
                (select-frame frm2))
              (when (file-exists-p (buffer-file-name x))
                (switch-to-buffer (buffer-name x))
                (revert-buffer t t t)))))
        (select-frame frm1)
        (delete-frame frm2)))))

;;;###autoload
(defun enclose-region-in (before-str after-str)
  "Insert the given text pair before/after the region."
  (interactive "sBefore text: \nsAfter text: ")
  (progn
   (save-excursion
     (goto-char (region-beginning))
     (insert before-str))
   (save-excursion
     (goto-char (region-end))
     (insert after-str))))

;;;###autoload
(defun enclose-region (&optional sep-str-arg)
  "Enclose the marked region in a box made of `sep-str', 
   or the comment string if the arg is nil (otherwise does nothing).
   If the region is not at the beginning/end of the line, the enclosing 
   box seperates the previous/subsequent text with a newline. 
   Does not work very well if the region spans multiple lines."
  (interactive (list (if 
    current-prefix-arg 
    (read-from-minibuffer "Enclose region with: ") 
    nil))) 
  (let ((sep-str (or sep-str-arg 
                     (if (string= comment-end "") (s-trim-right comment-start) nil))))
    (if sep-str
      (if (use-region-p)
        (let*
         ((sep-str-sz (length sep-str))
          (beg (region-beginning)) (end (region-end))
          (region-sz (- end beg))
          (region-sep-sz (+ region-sz (* 2 sep-str-sz) 2) )
          (count-sep-str (/ region-sep-sz sep-str-sz))
          (enc-str (s-left region-sep-sz (s-repeat (1+ count-sep-str) sep-str)))
          (enc-strs (list enc-str "\n" sep-str " "))
          (before-str (apply 'concat (cons (if (eq beg (line-beginning-position)) "" "\n") enc-strs)))
          (after-str (apply 'concat (reverse (cons (if (eq end (line-end-position)) "" "\n") enc-strs))))
          )
        (progn (message "test") (enclose-region-in before-str after-str)))
        (error (message "No region selected.")))
      (error (message "No seperator string given and no single-line comment syntax defined.")))))

;;;###autoload
(defun reload-emacs ()
  "(Re)Loads the emacs init file"
  (interactive)
  (load-file "~/.emacs"))

;;;;;;;;;;;;;;;;;;;
;; Custom advice ;;
;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
  "Use cygwin's /dev/null as the null-device. Prevent issues with
   the Windows null device (NUL) when using cygwin find with rgrep."
  (let ((null-device "/dev/null"))
        ad-do-it))
(ad-activate 'grep-compute-defaults)

;;; Filters ido-matches setting acronynm matches in front of the results
;;; From the smex docs: http://www.emacswiki.org/emacs/Smex
;;;###autoload
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
           (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match ido-text item) ;; exact match
              (add-to-list 'acronym-matches item)
            (if (string-match (concat regex "[^-]*$") item) ;; strict match
                (add-to-list 'acronym-matches item)
              (if (string-match regex item) ;; appending relaxed match
                  (add-to-list 'acronym-matches item t)))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))

;;;;;;;;;;;;;;;;;;
;; Custom modes ;;
;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;
(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode t)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-y") 'helm-global-mark-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x q") 'helm-resume)

(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

;;;;;;;;;;;;;
;; helm-ag ;;
;;;;;;;;;;;;;
(require 'helm-ag)
(global-set-key (kbd "C-x / 1") 'helm-do-ag)
(global-set-key (kbd "C-x / 2") 'helm-do-ag-this-file)
(global-set-key (kbd "C-x / 3") 'helm-do-ag-project-root)
(global-set-key (kbd "C-x / 4") 'helm-do-ag-buffers)

(setq helm-ag-fuzzy-match t)

;; uniquify
(require 'uniquify) 
(setq uniquify-buffer-name-style 'forward)

;; Maple 
(setq load-path (cons "~/.emacs.d/maple" load-path))
(autoload 'maplev-mode "maplev" "Maple editing mode" t)
(add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))
(setq maplev-indent-level 2)

;; MATLAB
(setq octave-comment-start "%")
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; C 
(require 'cc-mode) 

;; Web mode 
(require 'web-mode)
(define-key web-mode-map (kbd "RET") 'electric-indent-just-newline)

;; PHP
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; hakaru
(load "~/hakaru/tools/hakaru")
 
;; cygwin
(require 'cygwin-mount)
(require 'setup-cygwin)

(cygwin-mount-activate)

(add-hook 'comint-output-filter-functions
    'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
    'comint-watch-for-password-prompt nil t)
(setq explicit-shell-file-name "bash.exe")
;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
(setq shell-file-name explicit-shell-file-name)

;; eshell
(require 'helm-eshell)
(fset 'shell 'eshell) ; replace shell with eshell 
(add-hook 'eshell-mode-hook 
  (lambda () 
    (define-key eshell-mode-map (kbd "M-]") 'helm-eshell-history)
    (setq eshell-cmpl-ignore-case t)
    (eshell-cmpl-initialize)
    (set-face-attribute 'eshell-prompt nil :foreground "chartreuse4")
    (push-mark)
    ))
(setq eshell-prompt-function #'(lambda nil
  (concat
    (getenv "USER") "@" (system-name) ":" (abbreviate-file-name (eshell/pwd))
    (if (= (user-uid) 0) " # " " $ "))))

;; shell pop + eshell 
(setq shell-pop-shell-type "eshell"
      shell-pop-universal-key "C-'")

;;;;;;;;;;;;;;;;;;
;; Haskell mode ;;
;;;;;;;;;;;;;;;;;;
(customize-set-variable 'haskell-process-suggest-remove-import-lines t) 
(customize-set-variable 'haskell-process-auto-import-loaded-modules t)
(customize-set-variable 'haskell-process-log t)
(customize-set-variable 'haskell-process-type 'auto)

(setq haskell-indent-offset 2
      haskell-indentation-left-offset 0
      haskell-literate-default (quote tex)
      haskell-process-show-debug-tips nil)
(add-to-list 'font-lock-maximum-decoration '(haskell-mode . 2))
     
;; Interactive Haskell mode 
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; Haskell mode bindings
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "RET") 'electric-indent-just-newline)

;;;;;;;;;;;;;;;
;; Agda mode ;;
;;;;;;;;;;;;;;;
(load-file (let ((coding-system-for-read 'utf-8))
                 (shell-command-to-string "agda-mode locate")))

(require 'agda2-mode)
(add-hook 'agda2-mode-hook '(lambda () (buffer-face-mode 0)))

(setq agda2-program-args
  (mapcar (lambda (q) (concat "--include-path=" (canon-win-path (concat "~/" q))))
    '("agda/newlib"
      "agda/categories" 
      "stdlib/agda-stdlib/src")))
  
(setq agda-input-user-translations '(
  ;; random symbols 
  ("Nat" "ℕ") 
  ("BBN" "ℕ") 
  ("BBZ" "ℤ") 
  ("BBQ" "ℚ")
  ("SCup" "⊔")
  ("<#" "⇐")
  ("La" "⇐")
  ("ddown" "⇩")
  ("|-" "⊢")
  ("[=" "⊑")
  ("eps" "ε")
  ("cv" "⋎")
  ("{" "｛")
  ("}" "｝")
  ;; greek letters 
  ("Del" "Δ")
  ("del" "δ")
  ("Gam" "Γ") 
  ("gam" "γ")
))

;;;###autoload
(defun agda-quit-if-no-agda-buffs ()
  "Kill the agda process if there are no buffers visiting agda files."
  (when
    (and (agda2-running-p)
         (-none? (lambda (buf) 
           (with-current-buffer buf 
             (if (buffer-file-name) 
                 (string= (file-name-extension (buffer-file-name)) "agda")
               nil))) 
                 (buffer-list)))
    (progn (agda2-quit) (message "No agda buffers remaining; quitting agda"))))
(add-hook 'agda2-mode-hook '(lambda () (add-hook 'buffer-list-update-hook 'agda-quit-if-no-agda-buffs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agda mode character mappings ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq agda-unicode-char-map '(
  ;; Greek Extended     U+1F00  U+1FFF  (233)
  ;; General Punctuation        U+2000  U+206F  (111)
  ;; Superscripts and Subscripts        U+2070  U+209F  (42)
  ( (#x1f00 . #x209f) "FreeMono" )  
 
  ;; Letterlike Symbols         U+2100  U+214F  (80)
  ;; Number Forms       U+2150  U+218F  (60)
  ;; Arrows     U+2190  U+21FF  (112)
  ( (#x2100 . #x21ff) "FreeMono" )

  ;; Mathematical Operators     U+2200  U+22FF  (256)
  ( (#x2200 . #x22ff) "DejaVu Sans Mono" )

  ;; Miscellaneous Technical    U+2300  U+23FF  (255)
  ( (#x2300 . #x23ff) "FreeMono" )
  
  ;; Enclosed Alphanumerics     U+2460  U+24FF  (160)
  ( (#x2460 . #x24ff) "FreeMono" )
  
  ;; Miscellaneous Symbols      U+2600  U+26FF  (256)
  ( (#x2600 . #x26ff) "FreeMono" )
  
  ;; Miscellaneous Mathematical Symbols-A       U+27C0  U+27EF  (48)
  ;; Supplemental Arrows-A      U+27F0  U+27FF  (16)
  ( (#x27c0 . #x27ff) "FreeMono" )
  
  ;; Supplemental Arrows-B      U+2900  U+297F  (128)
  ;; Miscellaneous Mathematical Symbols-B       U+2980  U+29FF  (128)
  ( (#x2900 . #x29ff) "FreeMono" )
  
  ;; Supplemental Mathematical Operators        U+2A00  U+2AFF  (256)
  ( (#x2a00 . #x2aff) "FreeMono" )
  
  ;; Miscellaneous Symbols and Arrows   U+2B00  U+2BFF  (206)
  ( (#x2b00 . #x2bff) "FreeMono" )
  
  ;; Superscripts and Subscripts - U+2070 To U+209F
  ( (#x2070 . #x209f) "FreeMono" )
  
  ;; LATIN SUBSCRIPT SMALL LETTER H (U+2095)
  ;; LATIN SUBSCRIPT SMALL LETTER K (U+2096)
  ;; LATIN SUBSCRIPT SMALL LETTER L (U+2097)
  ;; LATIN SUBSCRIPT SMALL LETTER M (U+2098)
  ;; LATIN SUBSCRIPT SMALL LETTER N (U+2099)
  ;; LATIN SUBSCRIPT SMALL LETTER P (U+209A)
  ;; LATIN SUBSCRIPT SMALL LETTER S (U+209B)
  ;; LATIN SUBSCRIPT SMALL LETTER T (U+209C)
  ( (#x2095 . #x209C) "DejaVu Sans Mono" )
  
  ;; angle brackets ⟨ ⟩
  ( (#x27e8 . #x27f0) "DejaVu Sans Mono" )
  ( (#x230a . #x230b) "DejaVu Sans Mono" )
  
  ;; double curly brace ⦃ ⦄  
  ( (#x2983 . #x2984) "DejaVu Sans" )
  
  ;; relation compositon ⨾  
  ( (#x2a3e . #x2a3e) "Cambria" )
))

(dolist (chm agda-unicode-char-map) 
  (set-fontset-font "fontset-default" (nth 0 chm) (nth 1 chm) nil 'prepend))

;;;;;;;;;;;;;;;;;;; 
;; doc-view mode ;;
;;;;;;;;;;;;;;;;;;;
(setq doc-view-continuous t
      doc-view-ghostscript-program "gswin64c") 

;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion ;;
;;;;;;;;;;;;;;;;;;;;;

;; auto complete
(require 'auto-complete)

(defun my-ac-mode()
  (auto-complete-mode) 
  (setq-default 
     ac-sources
     '(ac-source-words-in-same-mode-buffers
       ac-source-dictionary
       ac-source-abbrev)))

;; auto complete - haskell
(define-key haskell-mode-map (kbd "M-/") 'ac-complete)
;; http://pastebin.com/tJyyEBAS
(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (my-ac-mode)
  (setq ac-sources (append '(ac-source-ghc-mod) ac-sources)))
(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (my-ac-haskell-mode)))
(add-hook 'find-file-hook 'my-haskell-ac-init)

;; C/c++
;; (defun my-c-ac-init ()
;;   (when (member (file-name-extension buffer-file-name) '("c" "cpp" "h" "hpp"))
;;     (my-ac-mode))
;;  )
;; (add-hook 'c-mode-common-hook 'my-ac-mode)
;; (add-hook 'find-file-hook 'my-c-ac-init)

;;;;;;;;;;;;;;;;;;;
;; visual regexp ;; 
;;;;;;;;;;;;;;;;;;;
(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q r") 'vr/query-replace)

;;;;;;;;;;;;;;;;;;
;; multi-cursor ;;
;;;;;;;;;;;;;;;;;;
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this) 
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-.") 'mc/mark-next-symbol-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;;;;;;;;;;;;;;;;;;;
;; smooth scroll ;;
;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

;;;;;;;;;;;;;;;;;;
;; line numbers ;;
;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun enable-linum-mode () (linum-mode 1))
(add-hook 'prog-mode-hook 'enable-linum-mode)
(add-hook 'haskell-mode-hook 'enable-linum-mode)
(add-hook 'maplev-mode-hook 'enable-linum-mode)

;;;;;;;;;;;;
;; aspell ;;
;;;;;;;;;;;;

;; todo: this should be its own function (`find-binary-and-add-to-exec-path')
;; which uses persistent storage to not make the system call every startup
;; (add-to-list 'exec-path 
;;   (canon-win-path 
;;     (file-name-directory 
;;       (replace-regexp-in-string "\n" "" 
;;         (shell-command-to-string "where aspell"))))) 

(setq ispell-program-name "aspell")
(require 'ispell)
(global-set-key (kbd "<f8>") 'ispell-word)
(setq ispell-local-dictionary "british")

;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;
(require 'tex-mode)

(setq latex-run-command "pdflatex") 
(setq tex-start-commands "SumatraPDF")

;;;###autoload
(defun custom-tex-hooks () 
  (add-to-list 'tex-compile-commands 
               '("SumatraPDF.exe \"%r.pdf\"" nil nil))
  (add-to-list 'tex-compile-commands 
               '("pdflatex.exe -interaction=nonstopmode \"%r.tex\"" nil nil))
  (add-to-list 'tex-compile-commands 
               '("bibtex.exe \"%r\"" nil nil))
  (add-to-list 'tex-compile-commands 
               '((concat "pdflatex.exe -interaction=nonstopmode \"%r.tex\" && "
                         "bibtex.exe \"%r\" && "
                         "pdflatex.exe -interaction=nonstopmode \"%r.tex\" && "
                         "pdflatex.exe -interaction=nonstopmode \"%r.tex\"")
                 nil nil)) )
(add-hook 'tex-mode-hook 'custom-tex-hooks)

;;; fin 
(setq emacs-init-finished t)
