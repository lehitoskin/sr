#lang racket/base
; files.rkt
(require racket/file)
(provide sr-path db-path)

; base directory where sr will put all of its files
(define sr-path
  (case (system-type)
    [(unix)
     ; check XDG variable first, then default to ~/.config/sr
     (let ([xdg (getenv "XDG_CONFIG_HOME")])
       (if xdg
           (build-path xdg "sr")
           (build-path (find-system-path 'home-dir)
                       ".config/sr")))]
    [(windows)
     (normal-case-path
      (build-path (find-system-path 'home-dir)
                  "appdata/local/sr"))]
    [(macosx)
     (build-path (find-system-path 'home-dir)
                 "Library/Application Support/sr")]))

(define db-path (build-path sr-path "catalog.sqlite"))

; create the config directory
(unless (directory-exists? sr-path)
  (make-directory* sr-path))
