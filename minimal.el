(global-set-key(kbd"C-c C-t C-l")'toggle-truncate-lines)

(if (not(display-graphic-p))
    (load-theme 'wombat)
  )

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
