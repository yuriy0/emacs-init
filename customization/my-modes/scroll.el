;; smooth scroll
;; http://www.emacswiki.org/emacs/SmoothScrolling

(setq
 ;; seems to do nothing with `scroll-conservatively' set to a large value
 scroll-step 1

 ;; if point moves more than this many lines, recenter on point
 ;; this is mainly useful when jumping to a mark which is far away
 scroll-conservatively 200

 ;; scrolling for long lines (scroll as normal in visual line mode)
 auto-window-vscroll nil

 ;; dont accelerate scrolling
 mouse-wheel-progressive-speed nil)

;; this strange expression changes the scroll amount to 5 lines per mouse wheel
;; event. this form is needed because `mouse-wheel-scroll-amount' is in fact an
;; "almost alist" where one of the elements will be a numeric value (the scroll
;; amount) and the other elements are of the form `(keys . action)' and give
;; special meaning to mouse events under certain keybinds
(modf mouse-wheel-scroll-amount
      (mapcar
       (lambda(x)
         (pcase x
           ((pred numberp) 5)
           (_ x))
         )))
