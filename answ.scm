(;7;

;;1
(define-class (person name)
(instance-vars (last-thing-said '()))
(method (say stuff) (set! last-thing-said stuff) stuff)
(method (ask stuff) (ask self 'say (se '(would you please) stuff)))
(method (greet) (as self 'say (se '(hello my name is) name)))
(method (repeat) (ask self 'say last-thing-said)))

;;2
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se (usual 'say stuff) (ask self 'repeat))) )
;;;creates an infinite loop since passing the message 'repeat will call the method "say" in the subclass
(define-class (double-talker name)
  (parent (person name))
  (method (say stuff) (se stuff stuff)) )
; Method works fine for repeating things when aked to say something or when asking to greet, but it wont work for 'repeat since the superclass' say method is never used which is where set! changes the state variable last-thing-said

(define-class (double-talker name)
  (parent (person name))
(method (say stuff) (usual 'say (se stuff stuff))) )
; works as it should

;;3
(define-class (random-generator n)
  (instance-vars (count 0))
  (method (number) (set! sount (+ count 1))
	  (random n))
  (method (count) count))

;;4
(define-class (coke-machine cmax cprice)
  (instance-vars (cnumber 0) (amount-deposited 0))
  (method (deposit amount) (set! amount-deposited
				 (+ amount amount-deposited)))
  (method (coke) (cond ((>= amount-deposited cprice)
			(if (> cnumber 0)
			    (- amount-deposited cprice)
			    (print "Machine empty")))
		       (else (print "Not enough money"))))
  (method (fill n) (if (<= n (- cmax cnumber))
		       (set! cnumber (+ cnumber n))
		       (print "Error: too many cokes"))))

;5;
(define-class (deck deck-of-cards)
  (shuffle deck-of-cards)
  (method (deal) (car deck-of-cards)
	  (set! deck-of-cards (cdr deck-of-cards)))
  (method (empty?) (empty? deck-of-cards)))

(define my-deck1 (instantiate deck '(AH 2H AD 2D AS 2S AC 2C)))

(ask mydeck1 'deal)

(define-class (deck deck-of-cards)
  (instantiate-vars (deal-card '()))
  (method (deal) (cond ((ask self 'empty?)
			(print "Error: No more cards!") )
		       (else (set! deck-of-cards (shuffle deck-of-cards))
			     (set! deal-card (car deck-of-cards))
			     (set! deck-of-cards (cdr deck-of-cards))
			     deal-card)))
	  (method (empty?) (null? duck-of-cards)))
			    
;6;
(define-class (miss-manners obj)
  (instance-vars (message '()) (arg '()))
  (method (please message arg) (ask obj message arg)))

;;;;;8;;;;;;
;;Printed
