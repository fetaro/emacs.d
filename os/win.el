;; windows

;; background alpha
(set-frame-parameter nil 'alpha 85)

;; show file path in menu
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

; server start for emacs-client
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) ; Suppress error "directory
                                                 ; ~/.emacs.d/server is unsafe"
                                                 ; on windows.
(server-start)


;; ZENKAKU space color
(defface my-face-zenkakuspace '((t (:background "bisque"))) nil)
;; tab color
(defface my-face-tab '((t (:background "bisque"))) nil)
