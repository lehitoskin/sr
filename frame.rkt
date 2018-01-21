#lang racket/base
; frame.rkt
(require db/base
         framework
         racket/class
         racket/gui/base
         srfi/13
         "base.rkt"
         "db.rkt")
(provide sr-frame)

; begin framework
(application:current-app-name "sr")
(application-quit-handler exit:exit)
(void (exit:insert-on-callback (λ () (disconnect sqlc))))
; end framework

(define medium-box (box 'anime))

(define closer-frame%
  (class frame%
    (super-new)
    ; clean up and exit the program
    (define (on-close) (exit:exit))
    (augment on-close)))

(define sr-frame
  (new closer-frame%
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

(define medium-choice
  (new choice%
       [parent header-hpanel]
       [label "Medium"]
       [choices '("Anime"
                  "Cartoon"
                  "Comic"
                  "LN"
                  "Manga"
                  "Television")]
       [callback (λ (choice evt)
                   ; modify title-lbox based on selection
                   (send title-lbox clear)
                   (set-box! medium-box
                             (string->symbol
                              (string-downcase
                               (send choice get-string-selection))))
                   (for ([title (in-list (sort (table-titles (unbox medium-box)) string<?))])
                     (send title-lbox append title))
                   ; set info side to defaults
                   (set-info-defaults!))]))

; begin title dialog box
(define title-dialog
  (new dialog%
       [label "sr - Add Title"]
       [width 400]
       [height 100]))

(define title-hpanel
  (new horizontal-panel% [parent title-dialog]))

(define title-tfield
  (new text-field%
       [parent title-hpanel]
       [label ""]
       [callback (λ (tfield evt)
                   (when (eq? (send evt get-event-type) 'text-field-enter)
                     (define title-text (send title-tfield get-value))
                     ; disallow the empty string
                     (unless (string-null? title-text)
                       (send title-lbox append title-text)
                       (send tfield set-value "")
                       (send title-dialog show #f))))]))

; Ok/Cancel order based on system
(cond [(system-position-ok-before-cancel?)
       ; ok-button
       (new button%
            [parent title-hpanel]
            [label "Ok"]
            [callback (λ (button evt)
                        (define title-text (send title-tfield get-value))
                        ; disallow the empty string
                        (unless (string-null? title-text)
                          (send title-lbox append title-text)
                          (send title-tfield set-value "")
                          (send title-dialog show #f)))])
       ; cancel-button
       (new button%
            [parent title-hpanel]
            [label "Cancel"]
            [callback (λ (button evt)
                        (send title-tfield set-value "")
                        (send title-dialog show #f))])
       (void)]
      [else
       ; cancel-button
       (new button%
            [parent title-hpanel]
            [label "Cancel"]
            [callback (λ (button evt)
                        (send title-tfield set-value "")
                        (send title-dialog show #f))])
       ; ok-button
       (new button%
            [parent title-hpanel]
            [label "Ok"]
            [callback (λ (button evt)
                        (define title-text (send title-tfield get-value))
                        ; disallow the empty string
                        (unless (string-null? title-text)
                          (send title-lbox append title-text)
                          (send title-tfield set-value "")
                          (send title-dialog show #f)))])
       (void)])
; end title dialog box

; add to bottom of the list and wait until user clicks "Save" button
(define add-title
  (new button%
       [parent header-hpanel]
       [label "+"]
       [callback (λ (button evt)
                   ; ask the user to enter the title of the series
                   (send title-dialog show #t))]))

; immediately
(define del-title
  (new button%
       [parent header-hpanel]
       [label "-"]
       [callback (λ (button evt)
                   (define title (send title-lbox get-string-selection))
                   ; don't do anything if there isn't anything selected
                   (when title
                     ; remove from lbox
                     (send title-lbox delete (send title-lbox get-selection))
                     ; don't do db stuff on an empty string
                     (unless (string-null? title)
                       ; remove from database
                       (del-title! (unbox medium-box) title))))]))
; end header content

; begin body content
(define body-hpanel
  (new horizontal-panel% [parent sr-frame]))

(define title-lbox
  (new list-box%
       [label ""]
       [parent body-hpanel]
       [style '(single)]
       ; if there are any anime entries then use those
       ; otherwise use default empty string
       [choices (let ([default (list "")]
                      [anime-lst (table-titles 'anime)])
                  (if (null? anime-lst)
                      default
                      (sort anime-lst string<?)))]
       [callback (λ (lbox evt)
                   (when (eq? (send evt get-event-type) 'list-box)
                     (define title (send lbox get-string-selection))
                     ; clear out all fields in the right side
                     (set-info-defaults!)
                     ; and fill them with database entries
                     (let ([rating (get-rating (unbox medium-box) title)]
                           [date-start (get-date-start (unbox medium-box) title)]
                           [progress (get-progress (unbox medium-box) title)]
                           [date-end (get-date-end (unbox medium-box) title)]
                           [comments (get-comments (unbox medium-box) title)])
                       (send rating-choice set-string-selection (number->string rating))
                       (send date-start-tfield set-value date-start)
                       (send progress-tfield set-value progress)
                       (send date-end-tfield set-value date-end)
                       (send comments-tfield set-value comments))))]))

; begin info section
(define info-vpanel
  (new vertical-panel%
       [parent body-hpanel]
       [alignment '(left center)]))

(define rating-choice
  (new choice%
       [parent info-vpanel]
       [label "Rating"]
       [choices (for/list ([i (in-range 5 0 -1)]) (number->string i))]
       ; select string "3" as default
       [selection 2]))

(define date-start-tfield
  (new text-field%
       [parent info-vpanel]
       [label "Date Started"]
       [style '(single vertical-label)]))

(define progress-tfield
  (new text-field%
       [parent info-vpanel]
       [label "Progress"]
       [style '(single vertical-label)]))

(define date-end-tfield
  (new text-field%
       [parent info-vpanel]
       [label "Date Completed/Dropped"]
       [style '(single vertical-label)]))

(define comments-tfield
  (new text-field%
       [parent info-vpanel]
       [label "Comments"]
       [style '(multiple vertical-label)]))

; set all the info fields to defaults
(define (set-info-defaults!)
  (send rating-choice set-selection 2)
  (send date-start-tfield set-value "")
  (send progress-tfield set-value "")
  (send date-end-tfield set-value "")
  (send comments-tfield set-value ""))

(define button-hpanel
  (new horizontal-panel%
       [parent info-vpanel]
       [alignment '(right center)]
       [stretchable-height #f]))

; Save/Clear buttons ordered by host system
(cond [(system-position-ok-before-cancel?)
       (new button%
            [parent button-hpanel]
            [label "Save"]
            [callback (λ (button evt)
                        ; if the title is an empty string or
                        ; there's no title selected, do nothing
                        (define title (send title-lbox get-string-selection))
                        (when (and title (not (string-null? title)))
                          (define rating
                            (string->number
                             (send rating-choice get-string-selection)))
                          (define date-start (send date-start-tfield get-value))
                          (define progress (send progress-tfield get-value))
                          (define date-end (send date-end-tfield get-value))
                          (define comments (send comments-tfield get-value))
                          (set-info! (unbox medium-box)
                                     title
                                     rating
                                     date-start
                                     progress
                                     date-end
                                     comments)))])
       (new button%
            [parent button-hpanel]
            [label "Clear"]
            [callback (λ (button evt)
                        (set-info-defaults!))])
       (void)]
      [else
       (new button%
            [parent button-hpanel]
            [label "Clear"]
            [callback (λ (button evt)
                        (set-info-defaults!))])
       (new button%
            [parent button-hpanel]
            [label "Save"]
            [callback (λ (button evt)
                        ; if the title is an empty string or
                        ; there's no title selected, do nothing
                        (define title (send title-lbox get-string-selection))
                        (when (and title (not (string-null? title)))
                          (define rating
                            (string->number
                             (send rating-choice get-string-selection)))
                          (define date-start (send date-start-tfield get-value))
                          (define progress (send progress-tfield get-value))
                          (define date-end (send date-end-tfield get-value))
                          (define comments (send comments-tfield get-value))
                          (set-info! (unbox medium-box)
                                     title
                                     rating
                                     date-start
                                     progress
                                     date-end
                                     comments)))])
       (void)])
; end info section
; end body content
