;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq centaur-logo nil)                        ; Logo file or nil (official logRestore previous sessiono)
(setq centaur-full-name "Mathieu Renzo")           ; User full name
(setq centaur-mail-address "mrenzo@flatironinstitute.org")   ; Email address
;; (setq centaur-proxy "127.0.0.1:1080")          ; Network proxy
;; (setq centaur-package-archives 'emacs-china)   ; Package repo: melpa, melpa-mirror, emacs-china netease or tuna
;; load better theme for terminal
;; (setq centaur-theme 'daylight)                    ; Color theme: default, classic, doom, dark, light or daylight
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-lsp nil)                         ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(when (display-graphic-p)
  ;; ;; Set default fonts
  ;; (cond
  ;; ((member "Source Code Pro" (font-family-list))
  ;;  (set-face-attribute 'default nil :font "Source Code Pro"))
  ;; ((member "Menlo" (font-family-list))
  ;; (set-face-attribute 'default nil :font "Menlo"))
  ;; ((member "Monaco" (font-family-list))
  ;; (set-face-attribute 'default nil :font "Monaco"))
  ;; ((member "DejaVu Sans Mono" (font-family-list))
  ;; (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
  ;; ((member "Consolas" (font-family-list))
  ;; (set-face-attribute 'default nil :font "Consolas"))
  ;; )

  ;; (cond
  ;;  (sys/mac-x-p
  ;;   (set-face-attribute 'default nil :height 130))
  ;;  (sys/win32p
  ;;   (set-face-attribute 'default nil :height 110)))

  ;; ;; Specify fonts for all unicode characters
  ;; (cond
  ;;  ((member "Apple Color Emoji" (font-family-list))
  ;;   (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
  ;;  ((member "Symbola" (font-family-list))
  ;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

  ;; ;; Specify fonts for Chinese characters
  ;;(cond
  ;; ((member "WenQuanYi Micro Hei" (font-family-list))
  ;;  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
  ;; ((member "Microsoft Yahei" (font-family-list))
  ;;  (set-fontset-font t '(#x4e00 . #x9fff) "Microsoft Yahei")))
  )

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
 '(TeX-view-program-selection
   (quote
    ((engine-omega "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(org-agenda-files (quote ("~/Documents/Mathieu/Research/Notes.org" "~/org")))
 '(package-selected-packages
   (quote
    (ztree youdao-dictionary yasnippet-snippets yari yard-mode yapfify yaml-mode xterm-color which-key web-mode web-beautify volatile-highlights vimrc-mode use-package treemacs-projectile treemacs-magit toml-mode toc-org tldr tide symbol-overlay swift-mode sudo-edit solaire-mode smart-region shell-pop shackle scss-mode rust-mode ruby-refactor rubocop rspec-mode rmsbolt rg request-deferred rainbow-mode rainbow-delimiters quickrun python-mode py-autopep8 projectile-rails powershell pomidor php-mode persp-mode-projectile-bridge persistent-scratch pdf-tools paradox pager origami org-tree-slide org-rich-yank org-preview-html org-pomodoro org-fancy-priorities org-download org-dashboard org-bullets olivetti ob-rust ob-ipython ob-go nov mwim multi-term modern-cpp-font-lock mocha-snippets mocha memory-usage markdown-toc magit-todos magit-popup macrostep lsp-ui lsp-java live-py-mode list-environment linum-off less-css-mode json-mode js2-refactor ivy-yasnippet ivy-xref ivy-rich ivy-hydra irony-eldoc iedit ibuffer-projectile hungry-delete htmlize hlinum highlight-indent-guides hide-mode-line helpful haml-mode govet goto-chg golint go-tag go-projectile go-playground go-impl go-gen-test go-fill-struct go-dlv gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger forge focus flyspell-correct-ivy flycheck-swift flycheck-pycheckers flycheck-pos-tip flycheck-popup-tip flycheck-irony flx fish-mode fd-dired fancy-narrow exec-path-from-shell esup eshell-z eshell-prompt-extras esh-help esh-autosuggest elpy elfeed ein editorconfig easy-kill-extras dumb-jump drag-stuff doom-themes doom-modeline dockerfile-mode docker-tramp discover-my-major diredfl dired-quick-sort diminish diffview diff-hl dashboard dap-mode daemons css-eldoc csharp-mode coverage counsel-world-clock counsel-tramp counsel-projectile counsel-osx-app copyit company-web company-shell company-quickhelp company-lsp company-jedi company-irony-c-headers company-irony company-go company-c-headers company-bibtex company-auctex company-anaconda comment-dwim-2 coffee-mode ccls cask-mode calfw-org calfw-ical calfw cal-china-x browse-at-remote bmx-mode benchmark-init bash-completion avy-zap atomic-chrome anzu amx all-the-icons-dired aggressive-indent ace-pinyin ace-link)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-dired-dir-face ((t (:foreground nil))))
 '(aw-leading-char-face ((t (:inherit error :bold t :height 1.1))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:background "#46D9FF"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit font-lock-string-face))))
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
 '(hl-todo ((t (:box t :inherit))))
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(linum-highlight-face ((t (\` (:inherit default :background nil :foreground nil)))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
 '(lsp-ui-sideline-code-action ((t (:inherit warning))))
 '(macrostep-expansion-highlight-face ((t (:background nil))))
 '(org-ellipsis ((t (:foreground nil))))
 '(org-pomodoro-mode-line ((t (:inherit warning))))
 '(org-pomodoro-mode-line-break ((t (:inherit success))))
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))))
 '(symbol-overlay-default-face ((t (:inherit (quote region)))))
 '(symbol-overlay-face-1 ((t (:inherit (quote highlight)))))
 '(symbol-overlay-face-2 ((t (:inherit (quote font-lock-builtin-face) :inverse-video t))))
 '(symbol-overlay-face-3 ((t (:inherit (quote warning) :inverse-video t))))
 '(symbol-overlay-face-4 ((t (:inherit (quote font-lock-constant-face) :inverse-video t))))
 '(symbol-overlay-face-5 ((t (:inherit (quote error) :inverse-video t))))
 '(symbol-overlay-face-6 ((t (:inherit (quote dired-mark) :inverse-video t :bold nil))))
 '(symbol-overlay-face-7 ((t (:inherit (quote success) :inverse-video t))))
 '(symbol-overlay-face-8 ((t (:inherit (quote dired-symlink) :inverse-video t :bold nil)))))
;==================================================================================================
;;; MESA STUFF

(add-to-list 'load-path "/home/math/.emacs.d/emacs_tools/mesa-major-mode/")
(require 'mesa-mode)
(require 'run-star-extras)
(setq mesa-default-version "12115")

(add-to-list 'auto-mode-alist '("/inlist[^/]*$" . mesa-mode))
(add-to-list 'auto-mode-alist '("\\.defaults$" . (lambda () (mesa-mode) (view-mode))))
(add-to-list 'auto-mode-alist '("/run_star_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode))))
(add-to-list 'auto-mode-alist '("/run_binary_extras.f$" . (lambda () (f90-mode) (run-star-extras-minor-mode))))

(setq
 mesa-version-mesa-dir "/home/math/Documents/Mathieu/Research/codes/mesa_11701/mesa11701")

;; LATEX stuff
(add-to-list 'load-path "/home/math/.emacs.d/emacs_tools/")
(add-hook 'Latex-mode-hook
          (lambda ()
            (require 'okular-search)
            ('TeX-PDF-mode)
            ('(LaTeX-command "latex -synctex=1"))
            '(TeX-output-view-style '(("^pdf$" "." "okular %s.pdf")))))
(add-hook 'LaTeX-mode-hook (lambda () (local-set-key "\C-x\C-j" 'okular-jump-to-line)))
(add-hook 'tex-mode-hook (lambda () (local-set-key "\C-x\C-j" 'okular-jump-to-line)))
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
;; prevent linebreaks in math mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'fill-nobreak-predicate 'texmathp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;; Mathieu
(if (not(display-graphic-p))
    (load-theme 'wombat)
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
;; (define-key global-map "\C-ca" 'org-agenda)  ;; already set in init-org.e
(define-key global-map "\C-c r" 'org-capture)
(define-key global-map "\C-c t l" 'org-todo-list)

;; capture for quick notes
(setq org-capture-templates
      ' (("n" "NOTES" entry
          (file+headline "/home/math/Documents/Mathieu/Research/Notes.org" "NOTES")
          "* %?\n")))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


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


(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;;; custom.el ends here
