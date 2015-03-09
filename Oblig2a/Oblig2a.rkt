(load "huffman.scm")

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

(define (member? symbol lst) 
  (cond ((null? lst) #f) 
        ((equal? symbol (car lst)) #t) 
        (else (member? symbol (cdr lst))))) 

;; Oppgave 2b

;; Svaret ligger i varibelen tree som brukes i den interne procedyren decode-1 etter at 
;; symbolet fra en 'leaf' har blitt consa' sammen med cdr av 'bits'. Den brukes da til å komme
;; tilbake til toppen av treet da current-branch kun går nedover i treet.

;; Oppgave 2c

(define (decode-tail bits tree)
  (define (decode-1 bits current-branch output-list)
    (if (null? bits)
        output-list
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits) tree (append (symbol-leaf next-branch) 
                                                (list (car output-list)))) ;; fix later sux
              (decode-1 (cdr bits) next-branch output-list)))))
  (decode-1 bits tree '()))

;; Oppgave 2d
;; (ninjas fight ninjas by night)


;; Oppgave 2e
;; To display error messages
(define (error message)
  (display message))

;; To iterate and make a encode list of all the symbols 
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond((not (member? symbol (symbols tree))) (error "symbol not in tree")) ;; if the symbol is not in the tree-list display error
       ((leaf? tree) '())
       ((member? symbol (symbols (left-branch tree))) ;; if the symbol is a member of the left branch add 0
        (cons 0 (encode-symbol symbol (left-branch tree))))
       (else  ;; else it must be in the right branch since we already check at the beginning that the element actually is in the list
        (cons 1 (encode-symbol symbol (right-branch tree))))))

;; Oppgave 2f

(define (grow-huffman-tree freqs) 
  (succesive-merge (make-leaf-set freqs))) ;;Sorts a leaf-list and starts succesive merge

(define (succesive-merge tree)
  (if (= (length tree) 1) 
      (car tree)
      (let ((current-tree (make-code-tree (car tree) (cadr tree)))) ;; make a tree of the current car and cadr
        (succesive-merge (adjoin-set current-tree (cddr tree))))))

;; Oppgave 2g
(define alfabet '((ninjas 57) (samurais 20) (fight 45) 
                              (night 12) (hide 3) (in 2) (ambush 2) (defeat 1) 
                              (the 5) (sword 4) (by 12) (assassin 1) (river 2) 
                              (forest 1) (wait 1) (poison 1)))

(define codebook (grow-huffman-tree alfabet))

(encode '(ninjas fight 
                 ninjas fight ninjas 
                 ninjas fight samurais
                 samurais fight
                 samurais fight ninjas
                 ninjas fight ninjas by night) 
        codebook)

;; Gjennomsnittet per kodeord som brukes i akkurat denne meldinger er (14 bits)/(5 ord) = 3bits
;; Listen med hele koden gir oss 
;; (1 1 1 0 1 1 1 0 1 1 1 1 1 0 0 1 0 0 1 0 1 0 0 1 0 1 0 1 1 1 1 1 0 1 1 0 1 1 1 0 0 0) Nemlig 42 bits.
;; Hvis vi skulle bruke fixed-length kode ville dette isteden blitt litt lenger.
;; Vi har her 16 forskjell bokstaver (ord) det vil si at vi trenger 16 forskjellige bit mønster.
;; For å få til dette trenger vi 4 bits per bokstav (2^4 = 16).
;; I kodelisten er 17 ord, så bitkoden ville blitt 4*17 = 68bits

;; Oppgave 2h
(define (huffman-leaves tree)
  (cond ((null? tree) )
        ((leaf? tree) (list (symbol-leaf tree) (weight-leaf tree)))
        (else (cons (huffman-leaves (left-branch tree))
                    (huffman-leaves (right-branch tree))))));; wrong output jumbled

(huffman-leaves sample-tree)
(newline)
(huffman-leaves codebook)
(newline)

;; Oppgave 2i

