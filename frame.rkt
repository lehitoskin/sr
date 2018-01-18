#lang racket/base
; frame.rkt
(require framework
         racket/class
         racket/gui/base
         "db.rkt")

; begin framework
(application:current-app-name "sr")
(application-quit-handler exit:exit)
#| (void (exit:insert-on-callback (λ () (disconnect sqlc)))) |#
; end framework

(define frame
  (new frame%
       [label "sr"]
       [width 400]
       [height 600]))

; begin menu bar
(define menu-bar
  (new menu-bar%
       [parent frame]))

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
(send frame show #t)
