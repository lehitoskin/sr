#lang racket/base
; db.rkt
(require racquel db/base db/sqlite3 "files.rkt")
(provide sqlc)

(define sqlc
  (sqlite3-connect
   #:database db-path
   #:mode 'create))
