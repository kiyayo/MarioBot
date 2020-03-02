
#lang racket/base

(require net/http-client json net/url racket/port racket/file racket/bytes file/md5)

(define server (getenv "SERVER"))
(define token (getenv "TOKEN"))
(define safebooru "safebooru.donmai.us")
(define limit "20")
(define tags "mario_(series)")

(define CRLF "\r\n")
(define boundary (bytes->string/utf-8 (md5 (number->string (current-seconds)))))
(define boundary-line (string-append "--" boundary CRLF))
; a table to map file extensions to MIME types:
(define ext=>mime-type
  #hash(("jpg" . "image/jpeg")
        ("png"  . "image/png")
        ("gif"  . "image/Gif")
        ("swf"  . "application/x-shockwave-flash")
        ("mp4"  . "video/mp4")
        ("webm" . "video/webm")))                                        
                
                                        


(define (search-safebooru)
  (define pos (random 19))
  (define page (number->string (random 1 350)))
  (define-values (status header response)(http-sendrecv safebooru (string-append "/posts.json?limit=" limit "&page=" page "&tags="tags ) #:ssl? #t))
  (define posts (read-json response))
 (list-ref posts pos))
  

(define (attach-media)
  (displayln "Obtaining media id")
  (define post (search-safebooru))
  (define url (hash-ref post 'file_url))
  (define md5 (hash-ref post 'md5))
  (define file-ext (hash-ref post 'file_ext))
  (define mime-type (hash-ref ext=>mime-type file-ext))
  (define filename (string-append md5 "." file-ext))
  (displayln (format "Downloading ~a from ~a" filename url))
  (call-with-output-file filename
    (lambda (in) (display (port->bytes (get-pure-port (string->url url)))in))
    #:exists 'replace)
  
 (define data
  (bytes-append
   (string->bytes/utf-8 (string-append boundary-line "Content-Disposition: form-data; name=\"file\"; filename=" "\"" filename "\"" CRLF
                                       "Content-Type: " mime-type CRLF CRLF))
                    (file->bytes filename) 
   (string->bytes/utf-8 (string-append CRLF "--" boundary "--" CRLF))))
 (define-values (status headers response)
  (http-sendrecv server (string-append "/api/v1/media") #:ssl? #t #:method #"POST"  #:headers (list (string-append "Content-Type: multipart/form-data; boundary=" boundary) (string-append "Authorization: Bearer " token)) #:data data))
  (read-json response))
  

 

(define (upload-attachment)
(define attachment (attach-media))
  (define id (hash-ref attachment 'id))
   (define data
  (bytes-append
   (string->bytes/utf-8 (string-append boundary-line "Content-Disposition: form-data; name=\"media_ids[]\"" CRLF CRLF))
   (string->bytes/utf-8 id)
   (string->bytes/utf-8 (string-append CRLF "--" boundary "--" CRLF))))
  (define-values (status headers response)
    (http-sendrecv server (string-append "/api/v1/statuses") #:ssl? #t #:method #"POST" #:headers (list (string-append "Content-Type: multipart/form-data; boundary=" boundary) (string-append "Authorization: Bearer " token)) #:data data)) 
    (displayln status)
  (displayln (read-json response))
  (displayln "Uploaded post!"))

(define (loop)
  (sleep 600)
(upload-attachment)
(loop))

(loop)


