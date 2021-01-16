(load "~/.emacs.d/minimal.el")

(server-start)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\C-r" 'recentf-open-files)

(set-default 'truncate-lines t)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

(use-package no-littering)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-single)

(use-package doom-modeline
  :ensure t
  :custom ((doom-modeline-height 10))
  :init (doom-modeline-mode 1))

;; these are configured in minimal.el
;; (electric-pair-mode 1)
;; (setq electric-pair-preserve-balance nil)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

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

(dolist (hook '(text-mode-hook LaTeX-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(setq flyspell-sort-corrections nil)
(setq flyspell-issue-message-flag nil)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-default-bibliography '("~/Documents/Research/Biblio_papers/bibtex/master_bibtex.bib"))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'fill-nobreak-predicate 'texmathp)))

(use-package org
  :config
  (define-key org-mode-map (kbd "<S-left>") nil)
  (define-key org-mode-map (kbd "<S-right>") nil)
  (define-key org-mode-map (kbd "<S-down>") nil)
  (define-key org-mode-map (kbd "<S-up>") nil)
  (setq org-ellipsis " ▾ ")
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width 400)
  (setq org-hide-emphasis-markers t) ;; hide synthax markers
  (setq org-capture-templates
	'(("n" "Research note" entry
	   (file+headline "~/Documents/Research/Notes.org" "Research notes")
	   "* %?\n %T")
	  ("r" "Random throwaway" entry
	   (file+headline "/tmp/Random_notes.org" "Random throughaway notes")
	   "* %?\n %T")
	  ("p" "Personal note" entry
	   (file+headline "~/Documents/Mathieu/Notes.org" "Personal notes")
	   "* %?\n %T")
	  ))
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

(use-package org-download
  :config
  (setq-default org-download-image-dir ".org_notes_figures/")
  )

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(load "~/.emacs.d/emacs_tools/blacken.el")

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

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; remember sorting across sessions
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

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

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-capture)
(define-key global-map "\C-ctl" 'org-todo-list)

(defun go-to-column (column)
  (interactive "nColumn: ")
  (move-to-column column t))
(global-set-key (kbd "M-g TAB") 'go-to-column)

(fset 'last-line-which-col
      "\C-[>\C-[OA\C-a\C-[g\C-i\C-u\C-xq[OB")

(put 'last-line-which-col 'kmacro t)

(global-set-key (kbd "C-c C-l") 'last-line-which-col)
