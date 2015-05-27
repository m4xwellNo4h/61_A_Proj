(load-option 'format)

(define nil '())

(define (val-of-card card) (if (<= card 10) card 10))
(define (is-ace? card) (= (val-of-card card) 1))

(define twenty-one 21)

;;; a hand of cards is implemented in a message-passing style
(define (make-hand)
  (define cards nil)
  (define number-of-cards 0)
  (define ace #f)
  (define least-value 0)
  (define greatest-value 0)

  (define (dispatch msg)
    (cond ((eq? msg 'add-a-card)
	   (lambda (card)
	     (set! cards (cons card cards))
	     (set! number-of-cards (1+ number-of-cards))
	     (if (is-ace? card) (set! ace #t))
	     (set! least-value
		   (let ((sum (+ least-value (val-of-card card))))
		     (if (> sum twenty-one) 'bust sum)))
	     (set! greatest-value
		   (cond ((eq? least-value 'bust) 'bust)
			 (ace (let ((sum (+ least-value 10)))
				(if (> sum twenty-one) least-value sum)))
			 (else least-value)))))
          ((eq? msg 'ace) ace)
	  ((eq? msg 'least-value) least-value)
	  ((eq? msg 'greatest-value) greatest-value)
	  ((eq? msg 'cards) cards)
	  ((eq? msg 'number-of-cards) number-of-cards)
	  (else (error "make-hand: dispatch: unknown message: " msg))))

  dispatch)

;;; a bet-hand is a hand for a player
(define (make-bet-hand bet split-ace)
  (define hand (make-hand))

  (define (dispatch msg)
    (cond ((eq? msg 'bet) bet)
	  ((eq? msg 'double-down) (set! bet (+ bet bet)))
	  ((eq? msg 'split-ace) split-ace)
	  (else (hand msg))))

  dispatch)

(define (is-a-hand-a-blackjack? hand)
  (and (eq? (hand 'number-of-cards) 2)
       (eq? (hand 'greatest-value) twenty-one)))

(define (is-surrender-allowed? bet-hand)
  (eq? (bet-hand 'number-of-cards) 2))

;;; to check if split is allowed
(define (is-split-allowed? bet-hand)
  (and (eq? (bet-hand 'number-of-cards) 2)
       (let ((cards (bet-hand 'cards)))
	 (eq? (first cards) (second cards)))))

;;; to check if "double down" is allowed
(define (is-double-down-allowed? bet-hand)
  (eq? (bet-hand 'number-of-cards) 2))

;;; to check if the player-hand is a blackjack

(define (is-a-bet-hand-a-blackjack? bet-hand)
  (and (not (bet-hand 'split-ace))
       (is-a-hand-a-blackjack? bet-hand)))

(define (play-blackjack times player-initial player)

  (define number-of-decks 6)
  (define number-of-all-cards (* 52 number-of-decks))
  (define reshuffle-threshold (- number-of-all-cards 39))

  (define lowest-allowed-bet 10)
  (define highest-allowed-bet 500)

  (define player-account 0)

  (define player-insurance-account 0)
  (define player-total-insurance-bet 0)
  (define (increase-insurance-account-bet bet)
    (set! player-insurance-account (+ player-insurance-account bet)))
  (define (decrease-insurance-account-bet bet)
    (set! player-insurance-account (- player-insurance-account bet)))

;;; decrease the player's acount if the hand loses
  (define (decrease-account-bet bet)
    (set! player-account (- player-account bet)))

  (define (decrease-account bet-hand)
    (decrease-account-bet (bet-hand 'bet)))

;;; increase the player's acount if the hand wins
  (define (increase-account-bet bet)
    (set! player-account (+ player-account bet)))

  (define (increase-account bet-hand)
    (increase-account-bet (bet-hand 'bet)))

  (define player-total-bet 1)

  (define all-cards (make-vector number-of-all-cards 0))
  (define number-of-used-cards 0)
  (define used-cards (make-vector number-of-all-cards 0))

  ;;; shuffling cards
  (define (shuffle)
    (define (initialize)
      (define (aux1 i j v)
	(if (zero? j)
	    i
	    (begin (vector-set! all-cards i v)
		   (aux1 (1+ i) (- j 1) v))))

      (define (aux2 i v)
	(if (<= v 13)
	    (aux2 (aux1 i (* 4 number-of-decks) v) (1+ v))))

      (define (aux3 i)
	(if (< i number-of-all-cards)
	    (begin (vector-set! used-cards i 0) (aux3 (1+ i)))))

      (aux2 0 1)
      (aux3 0))

    (define (randomize)
      (define (swap i j)
	(if (eq? i j) nil
	    (let ((t (vector-ref all-cards i)))
	      (vector-set! all-cards i (vector-ref all-cards j))
	      (vector-set! all-cards j t))))
      (define (aux3 i)
	(if (> i 0)
	    (begin (swap (- i 1) (random i)) (aux3 (- i 1)))))
      (aux3 number-of-all-cards))

    (set! *random-state* (make-random-state #t))
    (initialize)
    (randomize))

  ;;; reshuffle cards
  (define (reshuffle)
    (define (aux i)
      (if (>= i 0)
	  (begin (vector-set! used-cards i 0) (aux (- i 1)))))
    (aux (- number-of-used-cards 1))
    (set! number-of-used-cards 0)
    (shuffle))

  ;;; cards are shuffled now.
  (define initial-shuffle (shuffle))

  ;;; (newcard) returns the next card
  (define (newcard)
    (let ((v (vector-ref all-cards number-of-used-cards)))
      (vector-set! used-cards number-of-used-cards v)
      (set! number-of-used-cards (1+ number-of-used-cards)) v))

  (define secret-card-position)

  (define (secret-newcard)
    (let ((v (vector-ref all-cards number-of-used-cards)))
      (set! secret-card-position number-of-used-cards)
      (vector-set! used-cards number-of-used-cards 'unknown)
      (set! number-of-used-cards (1+ number-of-used-cards)) v))

  (define (verify-bet bet)
    (if (or (< bet lowest-allowed-bet)
	    (> bet highest-allowed-bet))
	(error "The amount of bet is not allowed" bet)))

  (define (split-bet-hand bet-hand)
    (let* ((card (first (bet-hand 'cards)))
	   (split-ace (= 1 (val-of-card card)))
	   (bet-hand-1 (make-bet-hand (bet-hand 'bet) split-ace))
	   (bet-hand-2 (make-bet-hand (bet-hand 'bet) split-ace)))
      ((bet-hand-1 'add-a-card) card)
      ((bet-hand-1 'add-a-card) (newcard))
      ((bet-hand-2 'add-a-card) card)
      ((bet-hand-2 'add-a-card) (newcard))
      (cons bet-hand-1 bet-hand-2)))


  (define (play-one-hand)
    (define player-bet-hand)
    (define player-original-bet)
    (define player-bet-hands nil)
    (define settled-player-bet-hands nil)

    (define dealer-hand)
    (define dealer-up-card)
    (define dealer-down-card)

    (define insured #f)
    (define (is-insurance-allowed?)
      (and (not insured)
	   (eq? dealer-up-card 1)
	   (eq? (player-bet-hand 'number-of-cards) 2)))

    (define (error-msg msg)
      (format #t "dealer's hand: ~S\nplayer's hand: ~S\n"
	      (dealer-hand 'cards)
	      (player-bet-hand 'cards))
      (error msg))

    (define (increase-all-player-bet-hands)
      (define (aux bet-hands)
	(if (null? bet-hands)
	    nil
	    (begin (increase-account (car bet-hands))
		   (aux (cdr bet-hands)))))
      (aux settled-player-bet-hands))

    (define (compare-all-player-bet-hands)
      (define (aux-bj bet-hands)
	(if (null? bet-hands)
	    nil
	    (begin
	      (let ((bet-hand (car bet-hands)))
		(let ((bet (bet-hand 'bet))
		      (value (bet-hand 'greatest-value)))
		  (if (is-a-bet-hand-a-blackjack? bet-hand)
		      nil
		      (decrease-account-bet bet))))
	      (aux-bj (cdr bet-hands)))))

      (define (aux-non-bj dealer-value bet-hands)
	(if (null? bet-hands)
	    nil
	    (begin
	      (let ((bet-hand (car bet-hands)))
		(let ((bet (bet-hand 'bet))
		      (value (bet-hand 'greatest-value)))
		  (cond ((is-a-bet-hand-a-blackjack? bet-hand)
			 (increase-account-bet (* 1.5 bet)))
			((< dealer-value value)
;;; player wins
			 (increase-account-bet bet))
			((> dealer-value value)
;;; dealer wins
			 (decrease-account-bet bet))
;;; it is a draw
			(else nil))))
	      (aux-non-bj dealer-value (cdr bet-hands)))))
      (if (is-a-hand-a-blackjack? dealer-hand)
	  (aux-bj settled-player-bet-hands)
	  (aux-non-bj (dealer-hand 'greatest-value)
		      settled-player-bet-hands)))

    (define (initial-deal)
      (let ((bet (player-initial used-cards player-account)))
	(verify-bet bet)
	(set! insured #f)
	(set! player-original-bet bet)
	(set! dealer-hand (make-hand))

;;; the hand is not splitted
	(set! player-bet-hand (make-bet-hand bet #f))

	(set! player-total-bet (+ player-total-bet bet))

;;; dealing dealer's down card
        (set! dealer-down-card (secret-newcard))
	((dealer-hand 'add-a-card) dealer-down-card)

;;; dealing the player's first card
	((player-bet-hand 'add-a-card) (newcard))

;;; dealing dealer's up card
	(set! dealer-up-card (newcard))
	((dealer-hand 'add-a-card) dealer-up-card)

;;; dealing the player's second card
	((player-bet-hand 'add-a-card) (newcard))))

    (define (loop-dealer-hand)
      (define (aux dealer-value)
	(cond ((eq? dealer-value 'bust)
	       (increase-all-player-bet-hands))
	      ((< dealer-value 17)
;;; dealer needs to hit again
	       (begin ((dealer-hand 'add-a-card) (newcard))
		      (aux (dealer-hand 'greatest-value))))
	      (else (compare-all-player-bet-hands))))

      (vector-set! used-cards secret-card-position dealer-down-card)
      (if (not (null? settled-player-bet-hands))
	  (aux (dealer-hand 'greatest-value))))

    (define (loop-player-bet-hand)
      (let ((msg (player player-bet-hand dealer-up-card
			 used-cards player-account)))
	(cond ;;; buy insurance
	      ((eq? msg 'insure)
	       (if (is-insurance-allowed?)
		   (begin (set! insured #t)
			  (set! player-total-insurance-bet
				(+ player-total-insurance-bet
				   player-original-bet)))
		   (error-msg "insurance is not allowed"))
	       (loop-player-bet-hand))

              ;;; one more card
	      ((eq? msg 'hit)
	       ((player-bet-hand 'add-a-card) (newcard))
	       (if (eq? (player-bet-hand 'least-value) 'bust)
;;; player is bust
		   (begin (decrease-account player-bet-hand)
			  (loop-player-bet-hands))
;;; the hand is not finished yet
		   (loop-player-bet-hand)))

              ;;; no more card
	      ((eq? msg 'stand)
	       (set! settled-player-bet-hands
		     (cons player-bet-hand settled-player-bet-hands))
	       (loop-player-bet-hands))

              ;;; split a hand
	      ((eq? msg 'split)
;	     (format #t "action split is taken\n")
;	     (format #t "player-hand:~S = ~S\n" player-hand player-sum)
;	     (format #t "dealer-hand:~S = ~S\n" dealer-hand dealer-sum)
	       (if (not (is-split-allowed? player-bet-hand))
		   (error-msg "split is not allowed"))
	       (let ((pair (split-bet-hand player-bet-hand)))
		 (set! player-bet-hand (car pair))
		 (set! player-bet-hands (cons (cdr pair) player-bet-hands)))
	       (loop-player-bet-hand))

              ;;; late-surrender
	      ((eq? msg 'surrender)
	       (if (not (is-surrender-allowed? player-bet-hand))
		   (error-msg "late surrender is not allowed"))
	       (if (is-a-hand-a-blackjack? dealer-hand)
		   (decrease-account-bet (player-bet-hand 'bet))
		   (decrease-account-bet (/ (player-bet-hand 'bet) 2)))
	       (loop-player-bet-hands))

              ;;; double down
	      ((eq? msg 'double)
	       (if (not (is-double-down-allowed? player-bet-hand))
		   (error-msg "double down is not allowed"))
	       (player-bet-hand 'double-down)
	       ((player-bet-hand 'add-a-card) (newcard))
	       (if (eq? (player-bet-hand 'greatest-value) 'bust)
		   (decrease-account player-bet-hand)
		   (set! settled-player-bet-hands
			 (cons player-bet-hand settled-player-bet-hands)))
	       (loop-player-bet-hands))

              ;;; unrecognized command
	      (else (error "loop-player-bet-hand: unknown message: " msg)))))

  (define (loop-player-bet-hands)
    (if (null? player-bet-hands)
	(begin
	  (if insured
	      (if (is-a-hand-a-blackjack? dealer-hand)
		  (begin
		    (increase-insurance-account-bet player-original-bet)
		    (increase-account-bet player-original-bet))
		  (begin
		    (decrease-insurance-account-bet (/ player-original-bet 2))
		    (decrease-account-bet (/ player-original-bet 2)))))
	  (loop-dealer-hand))
	(begin
	  (set! player-bet-hand (car player-bet-hands))
	  (set! player-bet-hands (cdr player-bet-hands))
	  (loop-player-bet-hand))))

  (initial-deal)
  (loop-player-bet-hand))

  (define (play-many-hands)
    (if (> times 0)
	(begin
	  (if (eq? 0 (remainder times 1000))
	      (begin (format #t "\nremaining times = ~S\n" times)
		     (format #t "player's total bet = ~S\n" player-total-bet)
		     (format #t "player's total insurance bet = ~S\n" player-total-insurance-bet)
		     (format #t "player's insurance balance = ~S\n" player-insurance-account)
		     (format #t "player's final balance = ~S\n" player-account)
		     (format #t "player ratio = ~S\n"
			     (* 1.0 (/ player-account player-total-bet)))))
	  (set! times (- times 1))
	  (if (> number-of-used-cards reshuffle-threshold) (reshuffle))
	  (play-one-hand)
	  (play-many-hands))
	(begin
	  (format #t "\n\nFinal Result:\nplayer's total bet = ~S\n" player-total-bet)
	  (format #t "player's insurance balance = ~S\n" player-insurance-account)
	  (format #t "player's final balance = ~S\n" player-account)
	  (format #t "player ratio = ~S"
		  (* 1.0 (/ player-account player-total-bet))))))
  (play-many-hands)
)


;;;
;;;
;;; player-initial is a function that takes the used-cards and the
;;; player's account balance as its arguments and returns a number
;;; (between 10 and 100) as the initial amount of bet.
;;;
;;; player takes the player's current hand of card (bet-hand)
;;; the value of dealer's up card (dealer-up-card), the used
;;; cards (used-cards) and the player's account balance, and returns
;;; one of the following symbols:
;;;
;;; double, hit, split, stand and surrender
;;;
;;;

;;; here is a simple strategy where the player always bets $10
;;; and does not stand until hit 17 or above
;(define player-initial-and-player
;  (let ((insured))
;    (define (player-initial used-cards player-account)
;      (set! insured #f) 10)
;    (define player
;      (lambda (bet-hand dealer-up-card used-cards player-account)
;	(if (and (not insured)
;		 (eq? dealer-up-card 1)
;		 (eq? (bet-hand 'number-of-cards) 2))
;	    (begin (set! insured #t) 'insure)
;	    (if (< (bet-hand 'greatest-value) 17) 'hit 'stand))))
;    (cons player-initial player)))

;(define player-initial (car player-initial-and-player))
;(define player1 (cdr player-initial-and-player))

;(define (player2 bet-hand dealer-up-card used-cards player-account)
;  (if (< (bet-hand 'greatest-value) 17) 'hit 'stand))

(define (test times)
  (play-blackjack times player-initial player))
