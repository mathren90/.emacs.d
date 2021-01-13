;; author: Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: files

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

;; load the bare minimum
(load "~/.emacs.d/minimal.el")

;; avoid line numbers in some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; start in server-mode
(server-start)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(use-package no-littering)

(require 'use-package)

(use-package all-the-icons)

;; Dired file browser
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; use only one buffer for dired
(use-package dired-single)

;; improve mode-line at the bottom
(use-package doom-modeline
  :ensure t
  :custom ((doom-modeline-height 10))
  :init (doom-modeline-mode 1))

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\C-r" 'recentf-open-files)

;; handling parenthesis
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; auto-revert when a file changes
; (global-auto-revert-mode t)

;; Activate toggle-truncate-lines from start
(set-default 'truncate-lines t)

;; which key for suggestions on shortcuts
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; lsp mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; LaTeX configuration

;; spell checking
(dolist (hook '(text-mode-hook LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-sort-corrections nil)
(setq flyspell-issue-message-flag nil)

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography '("/home/math/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib"))
;; ;; prevent linebreaks in math mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'fill-nobreak-predicate 'texmathp)))

(use-package org
  :config
  (setq org-ellipsis " ▾ ")
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width 400)
  (setq org-hide-emphasis-markers t) ;; hide synthax markers
  (setq org-capture-templates
	'(("n" "Research note" entry
	   (file+headline "~/Documents/Research/Notes.org" "Research notes")
	   "* %?\n")
	  ("r" "Random throwaway" entry
	   (file+headline "/tmp/Random_notes.org" "Random throughaway notes")
	   "* %?\n")
	  ("p" "Personal note" entry
	   (file+headline "~/Documents/Mathieu/Notes.org" "Personal notes")
	   "* %?\n")
	  ))
    )

;; to paste inline images with org-download
;; pasting from clipboard requires "wl-paste" available on apt
;; and can be done with =M-x org-download-clipboard=
(use-package org-download
  :config
  (setq-default org-download-image-dir ".org_notes_figures/")
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "●" "○" "●" "○" "●" "○")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  ;; (visual-fill-column-mode 1)
  )

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;;;;; keybindings
;; org-related
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-ctl" 'org-todo-list)

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

(global-set-key (kbd "C-c C-l") 'last-line-which-col)

;; python autocompletion
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

; to format python code with black
(load "~/.emacs.d/emacs_tools/blacken.el")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))


(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; remember sorting across sessions
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package tramp
  :config
  (setq tramp-default-method "ssh"))
