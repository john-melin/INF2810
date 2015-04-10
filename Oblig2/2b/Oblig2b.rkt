;; Oppgave 1
;; a)

(define (make-counter)
  (let ((counter 0))
    (lambda ()
      (set! counter (+ counter 1))
      counter)))

;;alternativ
(define (make-counter)
  ((lambda (counter)
    (lambda ()
      (set! counter (+ counter 1))
      counter))
   0))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))
(c1)
(c1)
(c1)
count
(c2)

;; b)
;; ? (c2) -> 1
;;                _____________________________________
;;               |                                     |
;; Global env -> | make-counter:...                    |
;;               | c2:----|                            |
;;               |________|____________________________|
;;                        |
;;                        |
;;                        |          ______________
;;                        |     E1->|counter: 0    |
;;                     [.][.]------>|______________|
;;                      |                     /\            ______________
;;                     \/                     |____________|              |
;;                   parameter:                            |______________|
;;                   body:                                 (set! counter (+ counter))
;;                                                         counter))
;;                     (set! counter (+ counter 1))                 
;;                       counter))

;; Oppgave 2
;; a)

(define (make-stack stack)
  (define (push! args)
    (set! stack (append (reverse args) stack)))
  (define (pop!)
    (if (null? stack)
        (set! stack stack);; stygg løsning for å ikke gjøre noe:P
        (set! stack (cdr stack))))
  (define (dispatch message . args)
    (cond ((eq? message 'pop!) (pop!))
          ((eq? message 'stack) stack)
          ((eq? message 'push!) (push! args))))
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

(define (pop! stack-list);; pop doesnt work for some stupid reason
  (stack-list 'pop!))

(define (stack stack-list)
  (stack-list 'stack))

(define (push! stack-list . args)
  (define (push-iter elements)
    (cond ((null? elements) (values));;how to make something stop without printing anything
          ((stack-list 'push! (car elements))
           (push-iter (cdr elements)))))
  (push-iter args))

(pop! s1)
(stack s1)
(push! s1 'foo 'faa)
(stack s1)

;; Oppgave 3
;; a)

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cddddr bar) (cdr bar))

;;           _________________
;;          |                 |
;;         \/                 |
;;[.][.]->[.][.]->[.][.]->[.][.]
;; |       |       |       |
;; \/      \/      \/      \/
;;'a      'b      'c       'd
;;
;;list-ref er designet til å gå til elementet på plass x og så returnere det
;;Ettersom listen vår nå looper tilbake til b etter vil alle tall 3 gå i loop rundt
;;'b 'c 'd beroende på hva x er.

;; b)

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah


;;  
;;             
;;                       
;;[.][.]->[.][.]->[.][/]
;; |       |       |     
;; \/      \/      \/      
;; bring   'a      'towel       


;;  _______
;; |       |                 
;; |      \/                 
;;[.][.]->[.][.]->[.][/]
;;         |       |     
;;         \/      \/      
;;        '42      'towel   

;; I det siste set-car! kallet, henter vi først opp cons-cellen (car bah) som er listen (a towel).
;; Så tar vi og bytter ut car av (car bah) som er 'a til 42.
;; Ettersom både car og cdr av bah peker på (a towel) vil vi nå se ((42 towel) (42 towel))

;; c)

(define (cycle? LIST)
  (define (cycle-iter slow-pair fast-pair)
    (cond ((or (null? fast-pair)
               (null? (cdr fast-pair))
               (null? (cddr fast-pair)))
           #f)
          ((eq? slow-pair fast-pair) #t)
          (else (cycle-iter (cdr slow-pair) (cddr fast-pair)))))
  (cond ((or (null? LIST)
             (null? (cdr LIST))
             (null? (cddr LIST))) 
         #f)
        (else (cycle-iter (cdr LIST) (cddr LIST)))))


(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
(cycle? bar)

;; d)
;; procedyren list? leter etter den tomme listen for at det skal regnes som en liste.
;; (list? (cons 1 2)) returnerer #f nettop derfor. Detsamme gjelder (list? bar) da den 
;; aldri returnerer den tomme listen og heller finner et duplikat av (cdr bar) noe sted
;; i listen.


;; e)
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;; returns the last pair of a ring
(define (last-pair-ring ring)
  (define (cycle-iter cmp-ring temp2)
    (cond ((eq? cmp-ring (cdr temp2)) temp2)
          (else (cycle-iter cmp-ring (cdr temp2)))))
  (cycle-iter ring (cdr ring)))


(define (make-ring ring)
  (set-cdr! (last-pair ring) ring)
  (define (left-rotate!)
    (set! ring (cdr ring))
    (car ring))
  (define (right-rotate!)
    (set! ring (last-pair-ring ring))
    (car ring))
  (define (delete!)
    (set-cdr! (last-pair-ring ring) (cdr ring))
    (set-car! ring (cdr ring))
    (set! ring (car ring))
    (car ring))
  (define (insert! element) 
    (let ((tmp-ring (cons element ring)))
      (set-cdr! (last-pair-ring ring) tmp-ring)
      (right-rotate!));; mongoløsning
    (car ring))
  (define (dispatch message . args)
    (cond ((eq? message 'top) (car ring))
          ((eq? message 'left-rotate) (left-rotate!))
          ((eq? message 'right-rotate) (right-rotate!))
          ((eq? message 'delete) (delete!))
          ((eq? message 'insert) (insert! (car args)))))
  dispatch)

(define (top ring) (ring 'top))
(define (right-rotate! ring) (ring 'right-rotate))
(define (left-rotate! ring) (ring 'left-rotate))
(define (delete! ring) (ring 'delete))
(define (insert! ring element) (ring 'insert element))


(define r1 (make-ring '(1 2 3 4)))
(define r2 (make-ring '(a b c d)))
(top r1)
(top r2)
(right-rotate! r1)
(left-rotate! r1)
(left-rotate! r1)
(delete! r1)
(left-rotate! r1)
(left-rotate! r1)
(left-rotate! r1)
(insert! r2 'x)
(right-rotate! r2)
(left-rotate! r2)
(left-rotate! r2)
(top r1)
(display "done")

;; f)
;; "left-rotate" har i vår implementasjon en lav kompleksitet da den bare setter pekern
;; til ringen til å være neste-pekeren til ringen.
;; Dette vil da bare være en konstant O(1).
;; "right-rotate" på en annen side vil kreve litt mer. Her finner vi siste-paret 
;; til ringen og setter pekeren til ringen til å peke på dette paret.
;; Kompleksiteten vil da være O(N)



(define (print-test-result expression expect res)
  (if (equal? res expect)
      (for-each display (list "Success: " expression " -> " res "\n"))
      (for-each display (list "Error: " expression " -> " res
                              ", but expected " expect "\n"))))
(define-syntax test
  (syntax-rules ()
    ((test expression expect)
     (print-test-result 'expression expect expression))))

;; Test `make-ring'
(define (test-ring)
  (define r1 (make-ring (list 1 2 3 4)))
  (define r2 (make-ring (list 'a 'b 'c 'd)))
  (test (top r1) 1)
  (test (top r2) 'a)
  (test (right-rotate! r1) 4)
  (test (left-rotate! r1) 1)
  (test (left-rotate! r1) 2)
  (test (delete! r1) 3)
  (test (left-rotate! r1) 4)
  (test (left-rotate! r1) 1)
  (test (left-rotate! r1) 3)
  (test (insert! r2 'x) 'x)
  (test (right-rotate! r2) 'd)
  (test (left-rotate! r2) 'x)
  (test (left-rotate! r2) 'a)
  (test (top r1) 3)
  (define lst (list 'a 'b 'c))
  (define r3 (make-ring lst))
  (test lst '(a b c))
  (top r3)
  (insert! r3 'e)
  (insert! r3 'f)
  (delete! r3)
  (right-rotate! r3)
  (right-rotate! r3)
  (left-rotate! r3)
  (test lst '(a b c)))