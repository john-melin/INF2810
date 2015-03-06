;;Oblig 2a


;;Oppgave 1a
(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (car (proc cons)))

(define (p-cdr proc)
  (cdr (proc cons)))

(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))

;;Oppgave 1b
(define foo 30)

;; Her setter vi x til å peke på den eksterne verdien til foo som er 30
;; y tar verdien 20 og plusses så sammen med foo og x til 80.
(let ((x foo)
      (y 20))
  (+ foo x y))

;; Lambda versjon
((lambda (z) 
  (define x foo) 
  (define y 20) 
  (+ foo x y)) foo)

;;
;;

;; Her lager vi en intern varibel vi kaller foo og setter den til å peke på verdien 10
;; x tar så verdien til den interne variabelen foo og plusser sammen med y som evalueres til 40.
(let ((foo 10))
  (let ((x foo)
        (y 20))
    (+ foo x y)))

;; Lambda versjon
((lambda (foo) 
   ((lambda (foo)
     (define x foo)
     (define y 20)
     (+ foo x y))foo))10)

;; Hvis vi isteden hadde skrevet det slik som nedenfor vil utrykket isteden evaluere seg til 60. Dette fordi her
;; og i det første utrykket hentes den eksterne verdien når vi kaller på foo fordi den er 
;; innenfor en ny let klasur. 
;; (let ((foo 10)
;;       (x foo)
;;       (y 20))
;;     (+ foo x y))

;; Oppgave 1c
;; Resultatet her blir en list med (6 -4 21 1/2)
;; map setter her argumentene x y z som blir alle de tre car verdiene til de tre 
;; listene. (første gang (1 + 5)). Så brukes de på utrykket (y x z) som derfor evalueres til (+ 1 5) = 6.
;; Videre cons'es dette med resten av de tre listene.
(define a1 (list 1 2 3 4))
(define a2 (list + - * /))
(define a3 (list 5 6 7 8))

((lambda (x y z) ;;argumentene
   (y x z))      ;;utrykket
 (car a1) (car a2) (car a3)) ;;
;; evalueres til 6


;; Oppgave 2a



