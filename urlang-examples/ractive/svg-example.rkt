#lang racket

(require "spa.rkt" net/sendurl)

(define-title "SVG test")

(define-template
  `((h1 "SVG test")
    (div
     (input (@ (type "range") (value ,($ radius))
               (min "10") (max "95") (step "5")))
     (br) (br)
     (button (@ (on-click "swap")) "Change color"))
    
    (svg (@ (width "200") (height "200"))
         (circle (@ (cx "100") (cy "100") (r ,($ radius))
                    (stroke "green") (stroke-width "4")
                    (fill ,($ fill)))))))

(define-code
  (model [fill "yellow"]
         [radius 50])

  (persistence #f #f)
  
  (events [swap swap-colors])

  (helpers)
  
  (define (swap-colors e)
    (set-model-fill!
          (if (= (get-model-fill) "yellow")
              "red"
              "yellow"))))


(send-url/contents (make-spa))