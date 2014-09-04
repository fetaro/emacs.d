;; windows

;; background alpha
(set-frame-parameter nil 'alpha 85)

;; show file path in menu
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))
