;;-[Oppgave 1]-
;;
;; a)
(display " oppgave 1a)")
(newline)
(display "->[.][.]->11")
(newline)
(display "   |")
(newline)
(display "   v")
(newline)
(display "   47")

;; b)
(newline)
(newline)
(display " oppgave 1b)")
(newline)
(display "->[.][/]")
(newline)
(display "   |")
(newline)
(display "   v")
(newline)
(display "   47")

;; c)
(newline)
(newline)
(display " oppgave 1c)")
(newline)
(display "->[.][.]->[.][/]")
(newline)
(display "   |       |")
(newline)
(display "   v       v")
(newline)
(display "   47      11")

;; d)
(newline)
(newline)
(display " oppgave 1d)")
(newline)
(display "->[.][.]->[.][/]")
(newline)
(display "   |       |")
(newline)
(display "   v       v")
(newline)
(display "   47      [.][.]->[.][/]")
(newline)
(display "            |       |")
(newline)
(display "            v       v")
(newline)
(display "            11      12")

;; e)
(newline)
(newline)
(display " oppgave 1e)")
(newline)
(display "->[.][.]->[.][.]->[.][.]->[.][/]")
(newline)
(display "   |       |       |       |")
(newline)
(display "   |       v       v       v")
(newline)
(display "   |       1       2       3")
(newline)
(display "   v")
(newline)
(display "  [.][.]->[.][.]->[.][/]")
(newline)
(display "   |       |       |")
(newline)
(display "   v       v       v")
(newline)
(display "   1       2       3")

;; f)
;; for å trekke ut 3 fra listen (1 2 3 4)
;; (car(cdr(cdr '(1 2 3 4))))
;;
;; g)
;; (car(car(cdr '((1 2) (3 4)))))
;;
;; h)
;; 
;; (car(car(cdr(cdr '((1) (2) (3) (4))))))
;;
;; i)
;; Med cons: 
;; (cons (cons 1 (cons 2 '())) 
;;       (cons (cons 3 (cons 4 '())) '()))
;;
;; Med list:
;; (list (list 1 2)
;;       (list 3 4))
;; 


;; -[Oppgave 2]-
;; a)
(define (length2 items)
  (define (lengthCnt2 items counter)
  (if (equal? items '())
      counter
      (lengthCnt2 ((cdr items) (+ counter 1)))))
  (lengthCnt2 items 0))
 
;; b)
;; Jeg har brukt halerekursjon da dette vil spare minne. (Se oppgave c)
;;
(define (rev-list items)
  (define (reverse-list in out)
    (if (null? in)
        out
        (reverse-list (cdr in) (cons (car in) out))))
  (reverse-list items '()))
  
;; c)
;; Jeg brukte her halerekursjon. Ved halerekursjon i Scheme vil et nytt metodekall ikke legges til i "stacken" men
;; gjøre det forrige metodekallet ferdig og kun kalle et nytt et isteden for å 
;; vente på det siste metodekallet for å begynne å legge sammen svaret. 
;; Dette fordi Scheme krever "tail-recursion-optimisation" ved hver implementasjon.
;; I praksis betyr det at alle metoder vi kaller vil bruke samme minne.

(define (ditch y items)
  (define (filter pred items)
  (cond ((null? items) '())
        ((pred (car items))
         (cons (car items)
               (filter pred (cdr items))))
        (else (filter pred (cdr items)))))
  (filter (lambda (x) (not (= x y)) ) items))

;; d)
(define (nth y items)
  (define (findNth items counter)
  (cond ((= counter y) (car items))
        (else (findNth (cdr items) (+ 1 counter)))))
  (findNth items 0))

;; e)
(define (where y items)
  (define (where-iter pred items counter)
  (cond ((null? items) #f)
        ((pred (car items)) counter)
        (else (where-iter pred (cdr items) (+ 1 counter)))))
  (where-ite (lambda (x) (= x y)) items 0))

;; f)
;;Ettersom map2 skal brukes i mange procedyrer er det bedre å definere den utenfor for å slippe kopiert kode.
(define (map2 proc items1 items2)
  (if (or (null? items1) 
          (null? items2))
      '()
      (cons (proc (car items1) 
                  (car items2))
            (map2 proc 
                  (cdr items1) 
                  (cdr items2)))))
;add-lists 
(define (add-lists items1 items2)
  (map2 + items1 items2))  

;; g)

;;mean-lists
(define (mean-lists items1 items2)
  (map2 (lambda (x y) 
          (/ (+ x y) 2)) items1 items2))  

;;even-lists
(define (even-lists items1 items2)
  (map2 (lambda (x y) 
          (and (even? x ) 
               (even? y))) items1 items2))  

;; h)
(define (both? pred)
  (lambda (x y) 
    (and (pred x) 
         (pred y))))
         
;; i)
(define (self proc)
  (lambda (x) 
    (proc x x)))