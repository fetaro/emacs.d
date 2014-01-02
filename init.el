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
;(when (require 'undohist nil t)
;  (undohist-initialize))

;;undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; redo+
(require 'redo)

;; anything
(require 'anything-config)
(require 'descbinds-anything)
(descbinds-anything-install)

;; rectangle mark
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; minibuf-isearch
;; length of buffer list : t means infinity
;(setq history-length t)

;; session.el
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; cursole restore
  (setq session-undo-check -1))

;; minibuf-isearch
;(require 'minibuf-isearch nil t)

;; coffee-mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-local-variable 'coffee-tab-width) 2))
  )

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;-------------------------
;; key map
;;-------------------------

(global-set-key (kbd "M-/") 'redo)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-q") 'kill-ring-save) ;copy
(global-set-key (kbd "C-c /") 'undo-tree-visualize)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c d") 'describe-bindings)
(global-set-key (kbd "C-c b") 'cua-set-rectangle-mark) ;rectangle
;;anything
(global-set-key (kbd "C-c f") 'anything-for-files)
(global-set-key (kbd "C-c y") 'anything-show-kill-ring)
(global-set-key (kbd "C-c t") 'anything-c-etags-select)

;; Untab and Indent
(global-set-key (kbd "C-c i") 'untabify-and-indent-whole-buffer)
(defun untabify-and-indent-whole-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

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

;; ruby on rails
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.css.erb$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css.scss$" . css-mode))


;; php-mode
(autoload 'php-mode "php-mode" )
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))
;;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 'js-indent-hook)

;; pretty js2mode indent
; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
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
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))
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
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))
(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;;; ruby
;; highlight ( )
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))
;; inf ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Rakefile" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("Gemfile" . ruby-mode)) auto-mode-alist))
(setq auto-mode-alist
      (append '(("\\.gemspec" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda ()
             (inf-ruby-keys)))


;; flymake for ruby
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
    (if (not (null buffer-file-name)) (flymake-mode))
    (define-key ruby-mode-map "\C-cd" 'credmp/flymake-display-err-minibuf)))
(defun credmp/flymake-display-err-minibuf ()
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
