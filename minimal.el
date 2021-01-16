;; author: Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: files

;; Comment: minimal configuration for quickly looking up things

;; Copyright (C) 2019-2021 Mathieu Renzo

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;; startup
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
;; UI
(setq frame-title-format '("Emacs %@ %*" (:eval (if (buffer-name)(abbreviate-file-name (buffer-name)) "%b %*"))))
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(set-fringe-mode 0)

;; line and column number
(column-number-mode)
(global-display-line-numbers-mode t)
;; handling parenthesis
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; theme
(load-theme 'wombat t) ;; use whiteboard or default for light theme
;; open .bash_* in sh-script-mode
(add-to-list 'auto-mode-alist '("/\.bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/dot-bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/\.zsh[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/dot-zsh[^/]*$" . shell-script-mode))
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; zoom-in and out
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

;; define global zoom in/out
;; N.B. per buffer zoom in/out can be achieved with C-x C-+ and C-x C--
(define-key global-map (kbd "C-+") 'zoom-in)
(define-key global-map (kbd "C--") 'zoom-out)

;; use shift + arrows to change buffer
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; C-pgUp/pgDown to browse buffers
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)

;; allow more garbage before collection
(setq gc-cons-threshold 25000000) ;; 25Mb
;; no backup files
; (setq make-backup-files nil)
;; and move autosaves to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; ;; ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; autoindent with return key
(define-key global-map (kbd "RET") 'newline-and-indent)

;; delete trailing white spaces except for markdown
(add-hook 'before-save-hook '(lambda()
                              (when (not (or (derived-mode-p 'markdown-mode)))
                                (delete-trailing-whitespace))))

;; don't ask which buffer when killing
(global-set-key (kbd "\C-x k") 'kill-this-buffer)

;; open dot-bash* .bash* in sh-script-mode
(add-to-list 'auto-mode-alist '("/\.bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/dot-bash[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/\.zsh[^/]*$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("/dot-zsh[^/]*$" . shell-script-mode))

;; MESA STUFF https://github.com/jschwab/mesa-major-mode
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
