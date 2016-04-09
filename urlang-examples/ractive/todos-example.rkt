#lang racket

(require net/sendurl "spa.rkt")

(define-title "TODO test")

(define-template
  `((input (@ (value ,($ description))))
    (button (@ (on-click "add")) "Add")
    (table
     ,($section "todos" #:iterator "i"
                `(tr (td (input (@ (type "checkbox") (checked ,($ done)))))
                     (td (@ (style "text-decoration: " ,($ (strike done))))
                         ,($ description)))))))

(define-code
  (model [description ""]
         [todos (array)])

  (persistence "todo-test" #t)
  
  (events [add add-todo])
  
  (helpers [strike (λ (done) (if done "line-through" "none"))])
  
  (define (add-todo e)
    (let ([todos (get-model-todos)])
      (todos.push
       (object [done #f]
               [description (get-model-description)])))
    (set-model-description! "")))

(send-url/contents (make-spa))
