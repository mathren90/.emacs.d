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
