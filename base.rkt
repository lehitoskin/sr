#lang racket/base
; base.rkt
(require racket/class racket/gui/base)
(provide macosx?)

(define macosx?
  (eq? (system-type) 'macosx))

; implement common keyboard shortcuts
(define (init-editor-keymap km)
  
  (send km add-function "insert-clipboard"
        (lambda (editor kev)
          (send editor paste)))
  
  (send km add-function "insert-primary"
        (lambda (editor kev)
          (send editor paste-x-selection)))
  
  (send km add-function "cut"
        (lambda (editor kev)
          (send editor cut)))
  
  (send km add-function "copy"
        (lambda (editor kev)
          (send editor copy)))
  
  (send km add-function "select-all"
        (lambda (editor kev)
          (send editor move-position 'end)
          (send editor extend-position 0)))
  
  (send km add-function "delete-backward-char"
        (lambda (editor kev)
          (send editor delete)))
  
  (send km add-function "delete-forward-char"
        (lambda (editor kev)
          (define start (send editor get-start-position))
          (define end (send editor get-end-position))
          ; if we have something selected, only delete that part
          (send editor delete start (if (= start end)
                                        (+ end 1)
                                        end))))
  
  (send km add-function "backward-char"
        (lambda (editor kev)
          (send editor move-position 'left)))
  
  (send km add-function "backward-word"
        (lambda (editor kev)
          (send editor move-position 'left #f 'word)))
  
  (send km add-function "backward-kill-word"
        (lambda (editor kev)
          (define to (send editor get-start-position))
          (send editor move-position 'left #f 'word)
          (define from (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "forward-kill-word"
        (lambda (editor kev)
          (define from (send editor get-start-position))
          (send editor move-position 'right #f 'word)
          (define to (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "forward-kill-buffer"
        (lambda (editor kev)
          (send editor kill)))
  
  (send km add-function "backward-kill-buffer"
        (lambda (editor kev)
          (define to (send editor get-start-position))
          (send editor move-position 'home)
          (define from (send editor get-start-position))
          (send editor delete from to)))
  
  (send km add-function "mark-char-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'simple))))
  
  (send km add-function "mark-char"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'simple))))
  
  (send km add-function "mark-word-backward"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #t 'word))))
  
  (send km add-function "mark-word"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'right #t 'word))))
  
  (send km add-function "mark-whole-word"
        (lambda (editor kev)
          (let ([cur (send editor get-start-position)])
            (send editor move-position 'left #f 'word)
            (send editor move-position 'right #t 'word))))
  
  (send km add-function "forward-char"
        (lambda (editor kev)
          (send editor move-position 'right)))
  
  (send km add-function "forward-word"
        (lambda (editor kev)
          (send editor move-position 'right #f 'word)))
  
  (send km add-function "beginning-of-buffer"
        (lambda (editor kev)
          (send editor move-position 'home)))
  
  (send km add-function "end-of-buffer"
        (lambda (editor kev)
          (send editor move-position 'end)))
  
  ; from gui-lib/mred/private/editor.rkt
  (send km add-function "mouse-popup-menu"
        (lambda (edit event)
          (when (send event button-up?)
            (let ([a (send edit get-admin)])
              (when a
                (let ([m (make-object popup-menu%)])
                  (append-editor-operation-menu-items m)
                  ;; Remove shortcut indicators (because they might not be correct)
                  (for-each
                   (lambda (i)
                     (when (is-a? i selectable-menu-item<%>)
                       (send i set-shortcut #f)))
                   (send m get-items))
                  (let-values ([(x y) (send edit
                                            dc-location-to-editor-location
                                            (send event get-x)
                                            (send event get-y))])
                    (send a popup-menu m (+ x 5) (+ y 5)))))))))
  km)

(define (set-default-editor-bindings km)
  (cond [macosx?
         (send km map-function ":d:c" "copy")
         (send km map-function ":d:с" "copy") ;; russian cyrillic
         (send km map-function ":d:v" "insert-clipboard")
         (send km map-function ":d:м" "insert-clipboard") ;; russian cyrillic
         (send km map-function ":d:x" "cut")
         (send km map-function ":d:ч" "cut") ;; russian cyrillic
         (send km map-function ":d:a" "select-all")
         (send km map-function ":d:ф" "select-all") ;; russian cyrillic
         (send km map-function ":backspace" "delete-backward-char")
         (send km map-function ":d:backspace" "backward-kill-buffer")
         (send km map-function ":delete" "delete-forward-char")
         (send km map-function ":left" "backward-char")
         (send km map-function ":right" "forward-char")
         (send km map-function ":a:left" "backward-word")
         (send km map-function ":a:right" "forward-word")
         (send km map-function ":a:backspace" "backward-kill-word")
         (send km map-function ":a:delete" "forward-kill-word")
         (send km map-function ":c:k" "forward-kill-buffer")
         (send km map-function ":s:left" "mark-char-backward")
         (send km map-function ":s:right" "mark-char")
         (send km map-function ":a:s:left" "mark-word-backward")
         (send km map-function ":a:s:right" "mark-word")
         (send km map-function ":home" "beginning-of-buffer")
         (send km map-function ":d:left" "beginning-of-buffer")
         (send km map-function ":c:a" "beginning-of-buffer")
         (send km map-function ":end" "end-of-buffer")
         (send km map-function ":d:right" "end-of-buffer")
         (send km map-function ":c:e" "end-of-buffer")
         (send km map-function ":rightbuttonseq" "mouse-popup-menu")
         (send km map-function ":leftbuttondouble" "mark-whole-word")]
        [else
         (send km map-function ":c:c" "copy")
         (send km map-function ":c:с" "copy") ;; russian cyrillic
         (send km map-function ":c:v" "insert-clipboard")
         (send km map-function ":c:м" "insert-clipboard") ;; russian cyrillic
         (send km map-function ":c:x" "cut")
         (send km map-function ":c:ч" "cut") ;; russian cyrillic
         (send km map-function ":c:a" "select-all")
         (send km map-function ":c:ф" "select-all") ;; russian cyrillic
         (send km map-function ":backspace" "delete-backward-char")
         (send km map-function ":delete" "delete-forward-char")
         (send km map-function ":left" "backward-char")
         (send km map-function ":right" "forward-char")
         (send km map-function ":c:left" "backward-word")
         (send km map-function ":c:right" "forward-word")
         (send km map-function ":c:backspace" "backward-kill-word")
         (send km map-function ":c:delete" "forward-kill-word")
         (send km map-function ":s:left" "mark-char-backward")
         (send km map-function ":s:right" "mark-char")
         (send km map-function ":c:s:left" "mark-word-backward")
         (send km map-function ":c:s:right" "mark-word")
         (send km map-function ":home" "beginning-of-buffer")
         (send km map-function ":end" "end-of-buffer")
         (send km map-function ":rightbuttonseq" "mouse-popup-menu")
         (send km map-function ":middlebutton" "insert-primary")
         (send km map-function ":leftbuttondouble" "mark-whole-word")
         (send km map-function ":s:insert" "insert-primary")]))

(current-text-keymap-initializer
 (λ (keymap)
   (define km (init-editor-keymap keymap))
   (set-default-editor-bindings km)))
