#lang racket/base
; db.rkt
(require racket/class
         racket/contract
         racket/string
         racquel
         db/base
         db/sqlite3
         "files.rkt")
(provide db-has-key?
         del-title!
         get-comments
         get-date-end
         get-date-start
         get-rating
         get-progress
         set-info!
         sqlc
         table-titles)

(define sqlc
  (sqlite3-connect
   #:database db-path
   #:mode 'create))

(define tables '("Anime"
                 "Cartoon"
                 "Comic"
                 "LN"
                 "Manga"
                 "Television"))

; make sure the table and columns exist
(for ([medium (in-list tables)])
  (define str
    (format "create table if not exists ~a(Title string not null, Rating integer,
             Date_Start string, Progress string, Date_End string, Comments string);" medium))
  (query-exec sqlc str))

(define anime%
  (data-class object%
              (table-name "Anime")
              (init-column (title "" "Title"))
              (column (rating 3 "Rating")
                      (date-start "" "Date_Start")
                      (progress "" "Progress")
                      (date-end "" "Date_End")
                      (comments "" "Comments"))
              (primary-key title)
              (super-new)

              (define/public (set-info! r ds p de c)
                (set! rating r)
                (set! date-start ds)
                (set! progress p)
                (set! date-end de)
                (set! comments c))))

(define cartoon%
  (data-class object%
              (table-name "Cartoon")
              (init-column (title "" "Title"))
              (column (rating 3 "Rating")
                      (date-start "" "Date_Start")
                      (progress "" "Progress")
                      (date-end "" "Date_End")
                      (comments "" "Comments"))
              (primary-key title)
              (super-new)

              (define/public (set-info! r ds p de c)
                (set! rating r)
                (set! date-start ds)
                (set! progress p)
                (set! date-end de)
                (set! comments c))))

(define comic%
  (data-class object%
              (table-name "Comic")
              (init-column (title "" "Title"))
              (column (rating 3 "Rating")
                      (date-start "" "Date_Start")
                      (progress "" "Progress")
                      (date-end "" "Date_End")
                      (comments "" "Comments"))
              (primary-key title)
              (super-new)

              (define/public (set-info! r ds p de c)
                (set! rating r)
                (set! date-start ds)
                (set! progress p)
                (set! date-end de)
                (set! comments c))))

(define ln%
  (data-class object%
              (table-name "LN")
              (init-column (title "" "Title"))
              (column (rating 3 "Rating")
                      (date-start "" "Date_Start")
                      (progress "" "Progress")
                      (date-end "" "Date_End")
                      (comments "" "Comments"))
              (primary-key title)
              (super-new)

              (define/public (set-info! r ds p de c)
                (set! rating r)
                (set! date-start ds)
                (set! progress p)
                (set! date-end de)
                (set! comments c))))

(define manga%
  (data-class object%
              (table-name "Manga")
              (init-column (title "" "Title"))
              (column (rating 3 "Rating")
                      (date-start "" "Date_Start")
                      (progress "" "Progress")
                      (date-end "" "Date_End")
                      (comments "" "Comments"))
              (primary-key title)
              (super-new)

              (define/public (set-info! r ds p de c)
                (set! rating r)
                (set! date-start ds)
                (set! progress p)
                (set! date-end de)
                (set! comments c))))

(define television%
  (data-class object%
              (table-name "Television")
              (init-column (title "" "Title"))
              (column (rating 3 "Rating")
                      (date-start "" "Date_Start")
                      (progress "" "Progress")
                      (date-end "" "Date_End")
                      (comments "" "Comments"))
              (primary-key title)
              (super-new)

              (define/public (set-info! r ds p de c)
                (set! rating r)
                (set! date-start ds)
                (set! progress p)
                (set! date-end de)
                (set! comments c))))

; scrub some inputs before adding to database
; sql queries will complain for several reasons:
; - if a tag has spaces, but no quotes around it
; - if the tag contains quotes, so add single quotes around the entire thing
; - if the tag contains ', so replace it with ''
(define (input-scrub str)
  (format "'~a'" (string-replace str "'" "''")))

(define/contract (medium->class medium)
  (-> (or/c or/c 'anime 'cartoon 'comic 'ln 'manga 'television) data-class?)
  (case medium
    [(anime) anime%]
    [(cartoon) cartoon%]
    [(comic) comic%]
    [(ln) ln%]
    [(manga) manga%]
    [(television) television%]))

(define/contract (db-has-key? #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       boolean?)
  (define mc (medium->class medium))
  (define objs (select-data-objects db-conn mc (where (= title ?)) title))
  (not (null? objs)))

(define/contract (table-titles #:db-conn [db-conn sqlc] medium)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television])
       (#:db-conn connection?)
       list?)
  (define mc (medium->class medium))
  (for/list ([db-obj (select-data-objects db-conn mc)])
    (get-column title db-obj)))

; default to 3 if title does not exist in medium
(define/contract (get-rating #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       integer?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else #f]))
  (if db-obj
      (get-column rating db-obj)
      3))

(define/contract (get-date-start #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       string?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else #f]))
  (cond [db-obj
         (define date-start (get-column date-start db-obj))
         (if (number? date-start)
             (number->string date-start)
             date-start)]
        [else ""]))

(define/contract (get-progress #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       string?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else #f]))
  (cond [db-obj
         (define progress (get-column progress db-obj))
         (if (number? progress)
             (number->string progress)
             progress)]
        [else ""]))

(define/contract (get-date-end #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       string?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else #f]))
  (cond [db-obj
         (define date-end (get-column date-end db-obj))
         (if (number? date-end)
             (number->string date-end)
             date-end)]
        [else ""]))

(define/contract (get-comments #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       string?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else #f]))
  (cond [db-obj
         (define comments (get-column comments db-obj))
         (if (number? comments)
             (number->string comments)
             comments)]
        [else ""]))

; create a new entry in the correct table
; if the entry already exists, don't do much of anything
(define/contract (add-title! #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       void?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else (new mc [title title])]))
  (save-data-object db-conn db-obj))

(define/contract (del-title! #:db-conn [db-conn sqlc] medium title)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television] string?)
       (#:db-conn connection?)
       void?)
  (when (db-has-key? #:db-conn sqlc medium title)
    (define db-obj (make-data-object db-conn (medium->class medium) title))
    (delete-data-object sqlc db-obj)))

(define/contract (set-info! #:db-conn [db-conn sqlc] medium title r ds p de c)
  (->* ([or/c 'anime 'cartoon 'comic 'ln 'manga 'television]
        string?
        integer?
        string?
        string?
        string?
        string?)
       (#:db-conn connection?)
       void?)
  (define mc (medium->class medium))
  (define db-obj
    (cond [(db-has-key? #:db-conn db-conn medium title)
           (make-data-object db-conn mc title)]
          [else
           (new mc [title title])]))
  (set-column! rating db-obj r)
  (set-column! date-start db-obj ds)
  (set-column! progress db-obj p)
  (set-column! date-end db-obj de)
  (set-column! comments db-obj c)
  (save-data-object db-conn db-obj))
