;; use backspace
(define-key global-map "\C-h" 'delete-backward-char)
;; change home directory
(cd "~/")
;; set load path
(let ((default-directory (expand-file-name "~/.emacs.d/elisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

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
;; package
;;-------------------------

;; cl-lib
(require 'cl-lib)


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
;(require 'minibuf-isearch nil t)

;; session.el
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 10000)))
  (add-hook 'after-init-hook 'session-initialize)
  ;; cursole restore
  (setq session-undo-check -1))


;; wdired
;(require 'wdired)
;(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; magit
(require 'magit)
(eval-after-load 'magit
  '(progn
     (set-face-background 'magit-item-highlight "#202020")
     (set-face-foreground 'magit-diff-add "#40ff40")
     (set-face-foreground 'magit-diff-del "#ff4040")
     (set-face-foreground 'magit-diff-file-header "#4040ff")
     ))

(prefer-coding-system 'utf-8)
(setq default-process-coding-system 'utf-8)

;; open-junk-file
(require 'open-junk-file)
(setq open-junk-file-format "~/.emacs.d/junk/%Y-%m-%d-%H%M%S.txt")
(global-set-key (kbd "C-x j") 'open-junk-file)

;; rspec
(require 'rspec-mode)
(custom-set-variables '(rspec-use-rake-flag nil))

;;-------------------------
;; key map
;;-------------------------

(global-set-key (kbd "M-p") 'previous-line) 
(global-set-key (kbd "M-n") 'next-line) 
(global-set-key (kbd "M-/") 'comment-dwim) 
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-t") 'other-window)
(global-set-key (kbd "C-c /") 'undo-tree-visualize)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c d") 'describe-bindings)
(global-set-key (kbd "C-c b") 'cua-set-rectangle-mark) ;rectangle

;;anything
;(global-set-key (kbd "C-c f") 'anything-for-files)
;(global-set-key (kbd "C-c y") 'anything-show-kill-ring)
;(global-set-key (kbd "C-c t") 'anything-c-etags-select)

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

;; rspec-mode
(global-set-key (kbd "C-c s") 'rspec-verify-single) 
(global-set-key (kbd "C-c r") 'rspec-verify) 


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

;; scroll by 1 line
;(setq scroll-conservatively 35
;       scroll-margin 0
;       scroll-step 1)
;(setq comint-scroll-show-maximum-output t)

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
(custom-set-variables
 '(session-use-package t nil (session)))
(custom-set-faces
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


;; grep -r
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


;;--------------------------------------
;; File type
;;--------------------------------------

;;----------
;; PHP
;;----------

;; php-mode
(autoload 'php-mode "php-mode" )
(setq auto-mode-alist
      (cons '("\\.php\\'" . php-mode) auto-mode-alist))

; pretty php indent
(add-hook 'php-mode-hook (lambda ()
    (defun ywb-php-lineup-arglist-intro (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (+ (current-column) c-basic-offset))))
    (defun ywb-php-lineup-arglist-close (langelem)
      (save-excursion
        (goto-char (cdr langelem))
        (vector (current-column))))
    (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro)
    (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close)))


;; ;;----------
;; ;; javascript
;; ;;----------

;; ;;; js2-mode
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-hook 'js2-mode-hook 'js-indent-hook)

;; ;; pretty js2mode indent
;; ; refer to http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
;; (defun my-js2-indent-function ()
;;   (interactive)
;;   (save-restriction
;;     (widen)
;;     (let* ((inhibit-point-motion-hooks t)
;;            (parse-status (save-excursion (syntax-ppss (point-at-bol))))
;;            (offset (- (current-column) (current-indentation)))
;;            (indentation (espresso--proper-indentation parse-status))
;;            node)
;;       (save-excursion
;;         (back-to-indentation)
;;         (if (looking-at "case\\s-")
;;             (setq indentation (+ indentation (/ espresso-indent-level 2))))
;;         (setq node (js2-node-at-point))
;;         (when (and node
;;                    (= js2-NAME (js2-node-type node))
;;                    (= js2-VAR (js2-node-type (js2-node-parent node))))
;;           (setq indentation (+ 4 indentation))))
;;       (indent-line-to indentation)
;;       (when (> offset 0) (forward-char offset)))))
;; (defun my-indent-sexp ()
;;   (interactive)
;;   (save-restriction
;;     (save-excursion
;;       (widen)
;;       (let* ((inhibit-point-motion-hooks t)
;;              (parse-status (syntax-ppss (point)))
;;              (beg (nth 1 parse-status))
;;              (end-marker (make-marker))
;;              (end (progn (goto-char beg) (forward-list) (point)))
;;              (ovl (make-overlay beg end)))
;;         (set-marker end-marker end)
;;         (overlay-put ovl 'face 'highlight)
;;         (goto-char beg)
;;         (while (< (point) (marker-position end-marker))
;;           (beginning-of-line)
;;           (unless (looking-at "\\s-*$")
;;             (indent-according-to-mode))
;;           (forward-line))
;;         (run-with-timer 0.5 nil '(lambda(ovl)
;;                                    (delete-overlay ovl)) ovl)))))
;; (defun my-js2-mode-hook ()
;;   (require 'espresso)
;;   (setq espresso-indent-level 4
;;         indent-tabs-mode nil
;;         c-basic-offset 4)
;;   (c-toggle-auto-state 0)
;;   (c-toggle-hungry-state 1)
;;   (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
;;   (define-key js2-mode-map "\C-\M-\\"
;;     '(lambda()
;;        (interactive)
;;        (insert "/* -----[ ")
;;        (save-excursion
;;          (insert " ]----- */"))
;;        ))
;;   (define-key js2-mode-map "\C-m" 'newline-and-indent)
;;   (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
;;   (if (featurep 'js2-highlight-vars)
;;       (js2-highlight-vars-mode))
;;   (message "My JS2 hook"))
;; (add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;-----------
;; ruby
;;-----------

;; ruby on rails
(add-to-list 'auto-mode-alist '("\\.js.erb$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.css.erb$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.css.scss$" . css-mode))

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

;; for indent
(setq ruby-deep-indent-paren-style nil)
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

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

;;---------
;; coffee
;;---------
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-local-variable 'coffee-tab-width) 2))
  )

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;;---------
;; markdown
;;---------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


