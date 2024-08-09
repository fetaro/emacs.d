
(define-key global-map "\C-h" 'delete-backward-char)
;; change home directory
(cd "~/")

;; open same window in MacOSX
(setq ns-pop-up-frames nil) 

;; scroll 1 line
(setq scroll-conservatively 1)

;; rectangle mark
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;-------------------------
;; OS
;;-------------------------
(cond
 ((string-match "i386-mingw-nt6.1.7601" system-configuration)
  (load "~/.emacs.d/os/win.el")
  )
 ((string-match "linux" system-configuration)
  (load "~/.emacs.d/os/linux.el")
  )
 ((string-match "darwin" system-configuration)
  (load "~/.emacs.d/os/mac.el")
  )
)

;;-------------------------
;; key map
;;-------------------------

(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c d") 'describe-bindings)
(global-set-key (kbd "C-c b") 'cua-set-rectangle-mark) ;rectangle
;(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer);; Fix Dakuten
(global-set-key (kbd "M-d") 'insert-date) 

;; Untab and Indent
(global-set-key (kbd "C-c i") 'untabify-and-indent-whole-buffer)
(defun untabify-and-indent-whole-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

;;-------------------------
;; external lib
;;-------------------------

;; import from ./lisp
(let ((default-directory (locate-user-emacs-file "./lisp")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; dmacro
(defconst *dmacro-key* "\C-o" "repeat key")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)


;;-------------------------
;; config
;;-------------------------

;; TAB width
(setq-default tab-width 4)

;; use Space instead of TAB
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'indent-relative-maybe)

;; can not make backup file like  *.~
;(setq make-backup-files nil)
;; make backup file to ~/.emacs.d/backup
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.emacs.d/backup"))
            backup-directory-alist))

;; can not make auto save file like .#*
(setq auto-save-default nil)

;; Highlight reasion
(setq-default transient-mark-mode t)

;; skip start page
(setq inhibit-startup-message t)

;; hide menu bar
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Do not make buffer , when directory traversal
(defvar my-dired-before-buffer nil)
(defadvice dired-advertised-find-file
  (before kill-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-advertised-find-file
  (after kill-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

(defadvice dired-up-directory
  (before kill-up-dired-buffer activate)
  (setq my-dired-before-buffer (current-buffer)))

(defadvice dired-up-directory
  (after kill-up-dired-buffer-after activate)
  (if (eq major-mode 'dired-mode)
      (kill-buffer my-dired-before-buffer)))

;; add -r to grep
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))


;;-------------------------
;; color 
;;-------------------------

;; Color
;(set-background-color "#000000")
;(set-foreground-color "#ffffff")
;(set-cursor-color "#ffffff")

;; theme
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-arjen)))
;; font-lock-keyword-face

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal))))
 '(font-lock-keyword-face ((t (:foreground "cyan" :bold t)))))


;; Show TAB and ZENKAKU space
;"　" <- ZENKAKU space
;"	" <- TAB
;; ZENKAKU space color
(defface my-face-zenkakuspace '((t (:background "gray10"))) nil)
;; tab color
(defface my-face-tab '((t (:background "gray10"))) nil)
(defvar my-face-zenkakuspace 'my-face-zenkakuspace)
(defvar my-face-tab 'my-face-tab)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-face-zenkakuspace append)
     ("\t" 0 my-face-tab append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; ------------------------------
;; prevent from killing *scratch* buffer
;; ------------------------------
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(add-hook 'kill-buffer-query-functions
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))

(add-hook 'after-save-hook
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(leaf)))


;;--------------------------------------------
;; package
;;--------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp")

;; dmacro
(defconst *dmacro-key* "\C-o" "repeat key")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

