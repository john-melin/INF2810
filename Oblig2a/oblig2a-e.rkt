(load "huffman.scm")

;; Oppgave 2e
(define (encode-message message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode-symbol (cdr message) tree))))

(define (is-in-node? symbol tree-symbols)
  (cond ((null? tree-symbols) #f)
        ((equal? symbol (car tree-symbols)) #t)
        (else (is-in-node? symbol (cdr tree-symbols)))))

(define (encode-symbol symbol tree)
  (cond (not (is-in-node? symbol (symbols tree)) '())
        (else (encode-symbol-iter symbol tree))))
        
(define (encode-symbol-iter symbol tree)
  (cond((leaf?) '())
       (cond ((is-in-node? symbol (symbols (left-branch tree))) 
              (append 
               0 
               (encode-symbol-iter symbol (left-branch tree))))
             ((else (append 1 
                            (encode-symbol-iter symbol (right-branch tree))))))))