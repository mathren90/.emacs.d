


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
    (load-theme 'wombat) ;; use whiteboard or default for light theme
 ;; (load-theme 'deeper-blue)
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
          (file+headline "/home/math/Documents/Mathieu/Research/Notes.org" "NOTES")
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
