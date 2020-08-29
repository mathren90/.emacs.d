;; minimal configuration, fast startup
;; author: Mathieu Renzo

(setq inhibit-startup-message t) ;; hide the startup message
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq frame-title-format '("" "%b @ Emacs " emacs-version))


(global-linum-mode t) ;; enable line numbers globally

(if (not (display-graphic-p))
    (load-theme 'wombat)
  ())



;; open .bash_ in sh-script-mode
(add-to-list 'auto-mode-alist '("/\.bash[^/]*$" . shell-script-mode))


;;; MESA STUFF https://github.com/jschwab/mesa-major-mode
(add-to-list 'load-path "~/.emacs.d/emacs_tools/mesa-major-mode/")
(require 'mesa-mode)
(require 'run-star-extras)
(setq mesa-default-version "12778")
(setq mesa-version-mesa-dir "~/codes/mesa/mesa_12778/mesa12778")
(setq mesa-mode-enforce-formatting-default t)


(add-to-list 'auto-mode-alist '("/inlist[^/]*$" . mesa-mode))
(add-to-list 'auto-mode-alist '("\\.defaults$" . (lambda () (mesa-mode) (f90-mode) (view-mode))))
(add-to-list 'auto-mode-alist '("/run_star_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode))))
(add-to-list 'auto-mode-alist '("/run_binary_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode))))

;; ;; hide show mode configuration                                                                                                                                                                               
(add-hook 'f90-mode-hook                                                                                                                                                                                         
          (lambda()                                                                                                                                                                                              
            (local-set-key (kbd "M-s s") 'hs-show-block)                                                                                                                                                         
            (local-set-key (kbd "M-s h") 'hs-hide-block)                                                                                                                                                         
            (hs-minor-mode t)))            

;; avoid large file warning
(setq large-file-warning-threshold nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
