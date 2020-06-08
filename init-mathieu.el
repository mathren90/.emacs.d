;; author: Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: files

;; Copyright (C) 2019-2020 Mathieu Renzo

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

;; for full config see also:
;; ~/.bash_functions
;; ~/.bash_aliases
;; and for desktop launcher:
;; ~/.local/share/applications/emacsclient.desktop




(setq inhibit-startup-message t) ;; hide the startup message
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq frame-title-format '("" "%b  -  Emacs " emacs-version))

;; add MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; recent files https://www.emacswiki.org/emacs/RecentFiles
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; handling parenthesis, https://emacs.stackexchange.com/questions/28857/how-to-complete-brackets-automatically
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; see also ~/.emacs for latex config

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; ;; prevent linebreaks in math mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'fill-nobreak-predicate 'texmathp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;; Mathieu
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'zenburn t)
(if (not(display-graphic-p))		
    (load-theme 'wombat) ;; use whiteboard or default for light theme
)

;; open .bash_ in sh-script-mode
(add-to-list 'auto-mode-alist '("/\.bash[^/]*$" . shell-script-mode))


;; Activate toggle-truncate-lines from start
(set-default 'truncate-lines t)
(global-set-key (kbd "C-c C-t C-l") 'toggle-truncate-lines)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; org-mode
;; show inline images in org mode
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 400)
(define-key global-map "\C-c l" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)  ;; already set in init-org.e
(define-key global-map "\C-c r" 'org-capture)
(define-key global-map "\C-c t l" 'org-todo-list)

;; capture for quick notes
(setq org-capture-templates
      ' (("n" "NOTES" entry
          (file+headline "
/home/math/Documents/Research/Notes.org" "NOTES")
          "* %?\n")))


;;jump to last (but one) line asking for column
;; to define macro with user interaction
;; (defun my-macro-query (arg)
;;   "Prompt for input using minibuffer during kbd macro execution.
;;  With prefix argument, allows you to select what prompt string to use.
;;  If the input is non-empty, it is inserted at point."
;;   (interactive "P")
;;   (let* ((query (lambda () (kbd-macro-query t)))
;;          (prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
;;          (input (unwind-protect
;;                     (progn
;;                       (add-hook 'minibuffer-setup-hook query)
;;                       (read-from-minibuffer prompt))
;;                   (remove-hook 'minibuffer-setup-hook query))))
;;     (unless (string= "" input) (insert input))))

;; (global-set-key "\C-xQ" 'my-macro-query)
;; see http://www.emacswiki.org/emacs/KeyboardMacros#toc4 to have an idea of how I came up with this solution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; first define a function used in the combination below
(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))

(global-set-key (kbd "M-g TAB") 'go-to-column)


(fset 'last-line-which-col
      "\C-[>\C-[OA\C-a\C-[g\C-i\C-u\C-xq[OB")

(put 'last-line-which-col 'kmacro t)

(global-set-key (kbd "C-c l") 'last-line-which-col)



