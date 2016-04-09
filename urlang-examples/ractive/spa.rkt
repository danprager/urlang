#lang racket

(require html-writing urlang urlang/extra
         (for-syntax syntax/parse racket/syntax
                     racket/format racket/list racket/string))

(provide define-title
         define-template
         define-code
         include-js
         include-css
         $
         $section         
         make-spa

         (all-from-out urlang))

(current-urlang-run?                           #f) ; run using Node?
(current-urlang-echo?                          #t) ; print generated JavaScript?
(current-urlang-console.log-module-level-expr? #f) ; print top-level expression?
(current-urlang-beautify?                      #t)

(define ractive-title "")
(define ractive-template 'template-undefined-html-template-expected)

(define (define-title title)
  (set! ractive-title title))
  
(define ($section section-name #:iterator [it #f] . expressions)
  `(,(~a "{{#" section-name (if it (~a ":" it) "") "}}")
    ,@expressions
    ,(~a "{{/" section-name "}}")))

(define (define-template html-template)
  (set! ractive-template html-template))

(define js-imports '("http://cdn.ractivejs.org/latest/ractive.min.js"))
(define (include-js . js)
  (set! js-imports (append js-imports js)))

(define css-imports '())
(define (include-css . css)
  (set! css-imports (append css-imports css)))

(define (make-spa)
  (xexp->html
   `((html (head (title ,ractive-title)
                 ,@(map (λ (j) `(script (@ (src ,j)))) js-imports)
                 ,@(map (λ (cs) `(link (@ (rel "stylesheet")
                                          (href ,cs)))) css-imports))
           (body
            (div (@ (id "ractive-container")))
            (script (@ (id "ractive-template") (type "text/ractive"))
                    ,@ractive-template)
            (script ,(file->string "ractive.js")))))))

(define-syntax ($ stx)
  (syntax-parse stx
    [(_ var:id)
     (with-syntax ([v (datum->syntax stx
                                     (format "{{~a}}" (syntax->datum #'var)))])
       #'v)]
    [(_ (fn:id args:id ...))
     (with-syntax ([js (datum->syntax stx
                                      (format "{{~a(~a)}}"
                                                  (syntax->datum #'fn)
                                                  (string-join
                                                   (map (compose
                                                         (λ (x) (format "~a" x))
                                                         syntax->datum)
                                                        (syntax->list #'(args ...)))
                                                   ", ")))])
       #'js)]))

(define-syntax (define-code stx)
  (syntax-parse stx
    [(_ (model [m-key:id m-init-value:expr] ...)
        (persistence persist-store persist-load)
        (events [event:id action:expr] ...)
        (helpers [h-name:id h-body:expr] ...)
        body ...)
     #`(urlang
        (urmodule ractive
                  (import Ractive localStorage JSON eval)

                  (define (model-init) (object [m-key m-init-value] ...))
                  
                  (define (model-saved)
                    (if persist-load
                        (let ([s (localStorage.getItem persist-store)])
                          (if (= s (eval "null")) #f (JSON.parse s)))
                        #f))
                  
                  (define (save-model!)
                    (when persist-store
                      (localStorage.setItem persist-store
                                            (JSON.stringify model))))
                  
                  (define model (or (model-saved) (model-init)))
                  (define (model-reset!) (:= model (model-init)))
                  
                  (define helpers Ractive.defaults.data)
                  #,@(for/list ([name (syntax->list #'(h-name ...))]
                                [body (syntax->list #'(h-body ...))])
                       (with-syntax ([quoted-h (datum->syntax
                                                stx (format "~a" (syntax->datum name)))]
                                     [body body])
                         #'(:= helpers quoted-h body)))
                  
                  (define ractive
                    (new Ractive
                         (object [el "#ractive-container"]
                                 [template "#ractive-template"]
                                 [data model])))

                  (ractive.observe (λ (data) (when persist-store (save-model!))))
                  
                  #,@(for/list ([k (syntax->list #'(m-key ...))])
                       (with-syntax ([set-k! (format-id stx "set-model-~a!" k)]
                                     [quoted-k (datum->syntax
                                                stx (format "~a" (syntax->datum k)))])
                         #`(define (set-k! v) (ractive.set quoted-k v))))
                  #,@(for/list ([k (syntax->list #'(m-key ...))])
                       (with-syntax ([get-k (format-id stx "get-model-~a" k)]
                                     [quoted-k (datum->syntax
                                                stx (format "~a" (syntax->datum k)))])
                         #`(define (get-k) (ractive.get quoted-k))))
                  
                  (ractive.on (object [event action] ...))
                  
                  body ...))]))
