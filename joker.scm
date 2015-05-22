(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) 'p1 'p2 )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 54) )

;;;new best-total arguments

(define (best-total sent)
	(ace-total-helper (aces-counter sent) (total-helper sent)))

(define (new-best-total sent)
  (cond
    ((> (best-total sent) 21) (best-total sent))
    ((= 0 (joker-counter sent)) (best-total sent))
    ((and (< 10 (best-total sent)) (= 1 (joker-counter sent))) '21)
    ((and (>= 10  (best-total sent)) (= 1 (joker-counter sent))) (+ 10 (best-total sent)))
    ((= 2 (joker-counter sent)) '21) 
    (else error "error message")))

 ;; (joker-total-helper (joker-counter sent) (best-total-with-aces sent)));;



;;;new best-total arguments

;;joker procedures

(define (joker-total-helper joker-counter value)
       (if
	(and (>= value 22) (>= joker-counter 1))
  	(joker-total-helper (- joker-counter 1) (- value 1))
	value))

(define (joker-counter sent)
       (cond
	((empty? sent) 0)
	((equal? (first (first sent)) 'p) (+ 1 (joker-counter (bf sent))))
	(else (joker-counter (bf sent)))))
;;;;;joker procedures

(define (ace-total-helper aces-counter value)
       (if
	(and (>= value 22) (>= aces-counter 1))
	(ace-total-helper (- aces-counter 1) (- value 10))
	value))

(define (aces-counter sent)
       (cond
	((empty? sent) 0)
	((equal? (first (first sent)) 'a) (+ 1 (aces-counter (bf sent))))
	(else (aces-counter (bf sent)))))

(define (total-helper sent)
       (if
	(empty? sent)
	0
	(+ (value-helper (first sent)) (total-helper (bf sent)))))
	
(define (value-helper wrd)
       (cond
	((equal? (first wrd) 'p) 1)
	((equal? (first wrd) 'a) 11)
	((equal? (first wrd) 'k) 10)
	((equal? (first wrd) 'q) 10)
	((equal? (first wrd) 'j) 10)
	((equal? (first wrd) '1) 10)
	((equal? (first wrd) 9) 9)
	((equal? (first wrd) 8) 8)
	((equal? (first wrd) 7) 7)
	((equal? (first wrd) 6) 6)
	((equal? (first wrd) 5) 5)
	((equal? (first wrd) 4) 4)
	((equal? (first wrd) 3) 3)
	((equal? (first wrd) 2) 2)
	(else (error "error message"))))

(define (stop-at-17 customer-hand-so-far dealer-up-card)
	(if
	(<= (best-total customer-hand-so-far) 17)
	#t
	#f))

(define (play-n strategy n)
  (if (= n 1)
      (twenty-one strategy)
     ( +  (twenty-one strategy) (play-n  strategy (- n 1)))))


(define (dealer-sensitive customer-hand-so-far dealer-up-card)
	(if
	(or
	(and (< (best-total customer-hand-so-far) 17) (>= (value-helper dealer-up-card) 7))
	(and (< (best-total customer-hand-so-far) 12) (<= (value-helper dealer-up-card) 6)))
	#t
	#f))


(define (stop-at n customer-hand-so-far dealer-up-card)
	(if
	(<= (best-total customer-hand-so-far) n)
	#t
	#f))

(define (heart? sent)
  (if
    (empty? sent) 
      #f
  (if   
    (member? 'h  (last (first sent)))
    #t
    (heart? (bf sent)))))

(define (valentine customer-hand-so-far dealer-up-card)
  (if
   (and  (heart? customer-hand-so-far) (< (best-total customer-hand-so-far) 19))
   #t
   (stop-at-17 customer-hand-so-far dealer-up-card)))
    
(define (suit? s sent)
  (if
     (empty? sent)
     #f
   (if
    (member? s (last (first sent))) 
    #t
    (suit? s (bf sent)))))

(define (suit-strategy s strategy1 strategy2)
  (define (use-strategy customer-hand-so-far dealer-up-card)
    (if
    (suit? s  customer-hand-so-far)
    (strategy1 customer-hand-so-far dealer-up-card)
    (strategy2 customer-hand-so-far dealer-up-card))) use-strategy)

(define (majority strategy1 strategy2 strategy3)
  (define (use-strategy customer-hand-so-far dealer-up-card)
      (or
        (and  (strategy1 customer-hand-so-far dealer-up-card) (strategy2 customer-hand-so-far dealer-up-card))
	(and  (strategy1 customer-hand-so-far dealer-up-card) (strategy3 customer-hand-so-far dealer-up-card))
        (and  (strategy2 customer-hand-so-far dealer-up-card) (strategy3 customer-hand-so-far dealer-up-card)))) use-strategy) 

(define (reckless strategy)
  (define (reckless-strategy customer-hand-so-far dealer-up-card)
    (strategy (butlast customer-hand-so-far) dealer-up-card))
  reckless-strategy)
