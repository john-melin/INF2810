;; Oppgave 1 
;; a)

(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define c1 (make-counter))
(c1)
(c1)
(c1)
(c1)

;; b)


;; Oppgave 2
;; a)

(define (make-stack STACK)
  (define (push! args)
    (set! STACK (append (reverse args) STACK)))
  (define (dispatch message . args)
    (cond ((eq? message 'pop!) 
           (cond ((null? STACK) '())              
                 (else (set! STACK (cdr STACK)))));; we want this internally but it sucks
          ((eq? message 'push!) (push! args))
          ((eq? message 'stack) STACK)))
  dispatch)

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'stack)
(s1 'pop!)
(s1 'stack)
(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack) 

;; b)
(define (pop! stck)
  (stck 'pop!))

(define (push! stck . args)
  (define (push-iter elements)
    (if (null? elements) '()
        (begin (stck 'push! (car elements))
               (push-iter (cdr elements)))))
  (push-iter args))


(define (stack stck)
  (stck 'stack))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1) 

;; Oppgave 3
;; a)

;; c)

(define (cycle? LIST)
  (define (cycle-iter next-pair)
    (cond ((or (null? next-pair)
               (null? (cdr next-pair)))
           #f)
          ((eq? LIST (cdr next-pair)) #t)
          (else (cycle-iter (cdr next-pair)))))
  (cycle-iter (cdr LIST)))


(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

bar

(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
;;(cycle? bar)

;; e)
(define (make-ring x)
  (define (internal first next)
    (if (null? (cdr next))
        (set-cdr! next first)
        (internal first (cdr next))))
  (internal (car x) (cdr x)))

(define (get-top x)
  (car x))

(define (left-rotate! x)
  (set-car! x (car (cdr x))))



