(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

(setq frame-title-format '("Emacs %@ %*" (:eval (if (buffer-name)(abbreviate-file-name (buffer-name)) "%b %*"))))
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(set-fringe-mode 0)
(column-number-mode)
(global-display-line-numbers-mode t)
(load-theme 'wombat t)
(set-default 'truncate-lines t)

(add-hook 'before-save-hook '(lambda()
                              (when (not (or (derived-mode-p 'markdown-mode)))
                                (delete-trailing-whitespace))))

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

(add-to-list 'auto-mode-alist '("/\.bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/dot-bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/\.zsh[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/dot-zsh[^/]*$" . shell-script-mode))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun zoom-in ()
  (interactive)
  (let ((x (+ (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))
(define-key global-map (kbd "C-+") 'zoom-in)
(define-key global-map (kbd "C--") 'zoom-out)

(when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

 (global-set-key (kbd "C-<prior>") 'previous-buffer)
 (global-set-key (kbd "C-<next>") 'next-buffer)

(global-set-key (kbd "\C-x k") 'kill-this-buffer)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key global-map (kbd "RET") 'newline-and-indent)

(setq gc-cons-threshold 25000000)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path "~/.emacs.d/emacs_tools/mesa-major-mode/")
(require 'mesa-mode)
(require 'run-star-extras)
(setq mesa-default-version "15140")
(setq mesa-version-mesa-dir "~/Documents/Research/codes/mesa/mesa_15140/mesa15140/")
(setq mesa-mode-enforce-formatting-default t)

(add-to-list 'auto-mode-alist '("/inlist[^/]*$" . mesa-mode))
(add-to-list 'auto-mode-alist '("\\.defaults$" . (lambda () (mesa-mode) (f90-mode) (view-mode))))
(add-to-list 'auto-mode-alist '("\\.inc$" . (lambda () (f90-mode) (view-mode))))
(add-to-list 'auto-mode-alist '("/run_star_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode) (lsp-mode))))
(add-to-list 'auto-mode-alist '("/run_star_extras.f90$" . (lambda () (f90-mode) (run-star-extras-minor-mode) (lsp-mode))))
(add-to-list 'auto-mode-alist '("/run_binary_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode) (lsp-mode))))
(add-to-list 'auto-mode-alist '("/run_binary_extras.f90$" . (lambda () (f90-mode) (run-star-extras-minor-mode) (lsp-mode))))

;; ;; hide show mode configuration
(add-hook 'f90-mode-hook
	  (lambda()
	    (local-set-key (kbd "\M-ss") 'hs-show-block)
	    (local-set-key (kbd "\M-sh") 'hs-hide-block)
	    (hs-minor-mode t)))
