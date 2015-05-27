(define-class (cd id-number timing-list)
  (set! number id-number)
  (method (id) id-number)
  (method (list) timing-list)
  (method (songs)
	  (- (length timing-list) 1))
  (method (index n)
	 (list-ref timing-list n)))

  
  
(define-class (cd-player)
  (method (load cd-name)
	  (ask cd-name 'id))
  (method (goto n)
	  (list-ref (ask cd-name 'list) (- n 1))))
	  

	  
	  

(define-class (mailbox address)
  (instance-vars (state '()) (history '()))
  (set! address address)
  (method (address) address)
  (method (flag boolean)
	  (set! state boolean))
  (method (flag?) state)
  (method (put letter)
	  (set! history (cons letter history)))
  (method (get) history))

(define-class (movable-mailbox address)
  (parent (mailbox address))
  (instance-vars (new-address '()))
  (method (new-address new)
	  (define-class (movable-mailbox new))))
  

  
	  
 





(define (make-account balance password)
  (define trans '())
  (define init-balance balance)
  (define (withdraw amount) 
    (set! trans (cons (list 'withdraw amount) trans))
    (set! balance (- balance amount)) balance) 
  (define (deposit amount) 
    (set! trans (cons (list 'deposit amount) trans))
    (set! balance (+ balance amount)) balance) 
  (define (dispatch pass msg) 
    (begin (cond 
      ((and (eq? msg 'withdraw) (eq? pass password)) withdraw) 
      ((and (eq? msg 'deposit) (eq? pass password)) deposit)
      ((and (eq? msg 'init-balance) (eq? pass password)) init-balance)
      ((and (eq? msg 'balance) (eq? pass password)) balance)
      ((and (eq? msg 'transactions) (eq? pass password)) trans))
   (print "Hot damn. Wrong!"))) 
  dispatch) 
