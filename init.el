;; use backspace
(define-key global-map "\C-h" 'delete-backward-char)

;;-------------------------
;; package
;;-------------------------

;; set load path
(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

;; auto-install
;(when (require 'auto-install nil t)
;  (setq auto-install-directory"~/.emacs.d/elisp/")
;  (auto-install-update-emacswiki-package-name t)
;  (auto-install-compatibility-setup))

;;undo hist
(when (require 'undohist nil t)
  (undohist-initialize))

;;undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; redo+
(require 'redo)

;; anything
(require 'anything-config)

;;-------------------------
;; key map
;;-------------------------

(global-set-key (kbd "C-l") 'redo)
(global-set-key (kbd "C-c /") 'undo-tree-visualize)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c f") 'anything-for-files)
(global-set-key (kbd "C-c y") 'anything-show-kill-ring)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c b") 'describe-bindings)
(global-set-key (kbd "C-t" ) 'other-window)

;; C-F   Untab and Indent
(global-set-key (kbd "C-F") 'untabify-and-indent-whole-buffer)
(defun untabify-and-indent-whole-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))


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

;; Show TAB and ZENKAKU space
(defface my-face-b-1 '((t (:background "bisque"))) nil)
(defface my-face-b-2 '((t (:background "LemonChiffon2"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("　" 0 my-face-b-1 append)
     ("\t" 0 my-face-b-2 append)
     ("[ \t]+$" 0 my-face-u-1 append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; Color
;(set-background-color "#000000")
;(set-foreground-color "#ffffff")
;(set-cursor-color "#ffffff")

;; Buffer frame title 
;(setq frame-title-format
;      (concat  "%b - emacs@" (system-name)))


;; Change coursor with IME ON/OFF
;(add-hook 'mw32-ime-on-hook
;          (function (lambda () (set-cursor-color "SkyBlue"))))
;(add-hook 'mw32-ime-off-hook
;          (function (lambda () (set-cursor-color "LemonChiffon"))))
;(setq-default mw32-ime-mode-line-state-indicator "[--]")
;(setq mw32-ime-mode-line-state-indicator-list '("[--]" "[J]" "[--]"))
;(mw32-ime-initialize)  ;; IME の初期化
 
;; scroll by 1 line
;(setq scroll-conservatively 35
;       scroll-margin 0
;       scroll-step 1)
;(setq comint-scroll-show-maximum-output t)

;; turn on font-lock mode
;(when (fboundp 'global-font-lock-mode)
;  (global-font-lock-mode t))

;;--------------------------------------
;; File type
;;--------------------------------------

;; php-mode
(autoload 'php-mode "php-mode" )
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
;;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; pretty js2mode indent
; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


;;;;;; flymake for ruby
(require 'flymake)
;;  flymake color
(set-face-background 'flymake-errline "VioletRed4")
(set-face-foreground 'flymake-errline "snow")
(set-face-background 'flymake-warnline "VioletRed4")
(set-face-foreground 'flymake-warnline "snow")
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don't want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))
    ;; C-c d at error line , show error to minibuffer
    (define-key ruby-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)))

(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))