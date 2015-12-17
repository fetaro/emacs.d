;; font
; http://tcnksm.sakura.ne.jp/blog/2012/04/02/emacs/

 ; english
 (set-face-attribute 'default nil
             :family "Menlo" ;; font
             :height 110)    ;; font size

; japanese
(set-fontset-font
 nil 'japanese-jisx0208
; (font-spec :family "Hiragino Mincho Pro")) ;; font
  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

; hankaku : zenkaku = 1 : 2
(setq face-font-rescale-alist
;        '((".*Hiragino_Mincho_pro.*" . 1.2)))
      '((".*Hiragino_Kaku_Gothic_ProN.*" . 1.2)));; macfont

