#lang racket/base
; frame.rkt
(require db/base
         framework
         racket/class
         racket/gui/base
         "base.rkt"
         "db.rkt")
(provide sr-frame)

; begin framework
(application:current-app-name "sr")
(application-quit-handler exit:exit)
(void (exit:insert-on-callback (λ () (disconnect sqlc))))
; end framework

(define sr-frame
  (new frame%
       [label "sr"]
       [width 500]
       [height 600]))

; begin menu bar
(define menu-bar
  (new menu-bar%
       [parent sr-frame]))

; begin menu bar file
(define menu-bar-file
  (new menu%
       [parent menu-bar]
       [label "&File"]))

(define menu-bar-file-quit
  (new menu-item%
       [parent menu-bar-file]
       [label "&Quit"]
       [shortcut #\Q]
       [callback (λ (b evt)
                   (exit:exit))]))
; end menu bar file

; begin menu bar edit
(define menu-bar-edit
  (new menu%
       [parent menu-bar]
       [label "&Edit"]))

(define menu-bar-edit-add
  (new menu-item%
       [parent menu-bar-edit]
       [label "&Add Title"]
       [callback (λ (b evt) (void))]))

(define menu-bar-edit-del
  (new menu-item%
       [parent menu-bar-edit]
       [label "&Delete Title"]
       [callback (λ (b evt) (void))]))

(define menu-bar-edit-stats
  (new menu-item%
       [parent menu-bar-edit]
       [label "Statistics"]
       [callback (λ (b evt) (void))]))
; end menu bar edit
; end menu bar

; begin header content
(define header-hpanel
  (new horizontal-panel%
       [parent sr-frame]
       [alignment '(left center)]
       [stretchable-height #f]))

(define medium
  (new choice%
       [parent header-hpanel]
       [label "Medium"]
       [choices '("Anime"
                  "Light Novel"
                  "Manga"
                  "Visual Novel")]
       [callback (λ (choice evt)
                   ; modify title-lbox based on selection
                   (void))]))

(define add-title
  (new button%
       [parent header-hpanel]
       [label "+"]
       [callback (λ (button evt)
                   (void))]))

(define del-title
  (new button%
       [parent header-hpanel]
       [label "-"]
       [callback (λ (button evt)
                   (void))]))
; end header content

; begin body content
(define body-hpanel
  (new horizontal-panel% [parent sr-frame]))

(define title-lbox
  (new list-box%
       [label ""]
       [parent body-hpanel]
       [style '(single)]
       [choices (list "")]
       [callback (λ (lbox evt)
                   ; clear out all fields in the right side
                   ; and fill them with database entries
                   (void))]))

; begin info section
(define info-vpanel
  (new vertical-panel%
       [parent body-hpanel]
       [alignment '(left center)]))

(define rating
  (new choice%
       [parent info-vpanel]
       [label "Rating"]
       [choices (for/list ([i (in-range 5 0 -1)]) (number->string i))]
       ; select string "3"
       [selection 2]))

(define date-started
  (new text-field%
       [parent info-vpanel]
       [label "Date Started"]
       [style '(single vertical-label)]))

(define progress
  (new text-field%
       [parent info-vpanel]
       [label "Progress"]
       [style '(single vertical-label)]))

(define completed/dropped
  (new text-field%
       [parent info-vpanel]
       [label "Date Completed/Dropped"]
       [style '(single vertical-label)]))

(define comments-tfield
  (new text-field%
       [parent info-vpanel]
       [label "Comments"]
       [style '(multiple vertical-label)]))

; Ok/Clear buttons ordered by host system
; end info section
; end body content
