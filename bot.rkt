#lang racket/base

(require net/http-client json net/url racket/port web-server/private/timer)

(define server (getenv "SERVER"))
(define token(getenv "ACCESS_TOKEN"))
(define safebooru "safebooru.donmai.us")
(define limit "20")
(define page "2")
(define tags "mario_(series)")
(define pos (random 20))


(define (search-safebooru)
  (define-values (status header response)(http-sendrecv safebooru (string-append "/posts.json?limit=" limit "&page=" page "&tags="tags ) #:ssl? #t))
  (define posts (read-json response))
 (list-ref posts pos))
  

(define (download-image)
  (define post (search-safebooru))
  (define url (hash-ref post 'file_url))
  (define md5 (hash-ref post 'md5))
  (define file-ext (hash-ref post 'file_ext))
  (define filename (string-append md5 "." file-ext))
  (call-with-output-file filename
                        (lambda (in) (port->bytes (get-pure-port (string->url url))))
                        #:exists 'replace))



(define (upload-image)
  (define-values (status headers response)
    (http-sendrecv server (string-append "/api/v1/media") #:ssl? #t #:method "POST" #:headers (list (string-append "Authorization: Bearer " token))  #:data (download-image)))
  (define data (read-json response))
  (displayln data))


(upload-image)


;define tm (start-timer-manager))
;define (loop)
 ;(start-timer tm  600 (upload-image))
  ;loop))

;loop)
