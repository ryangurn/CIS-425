#lang racket

; hw #6
; #1

(define (sequence spacing low high)
	(if (> low high)
			'()
			(cons low (sequence spacing (+ low spacing) high))))

(display "#1\n")
(sequence 1 0 10)

; #2

(define (string-append-map xs suff)
	(map (lambda (x) (string-append x suff)) xs ))
(display "#2\n")
(string-append-map '("1" "2" "3" "4" "5") "$$$$")

; #3

(define (list-nth-mod xs n)
	(if (negative? n)
			(error "list-nth-mod: negative number")
			(if (null? xs)
					(error "list-nth-mod: empty list")
					(car (list-tail xs (remainder n (length xs))))
			)
	)
)
(display "#3\n")
(list-nth-mod '(1 2 3 4) 2)

; #4
(define (fns-helper n)
	(cons (if (= 0 (remainder n 6)) (- n) n) (lambda () (fns-helper (+ n 1)))))

(define (funny-number-stream)
	(fns-helper 0))
(display "#4\n")
(funny-number-stream)
((cdr (funny-number-stream)))

; #5
; we use a thunk so that this does not
; loop forever to fill the list
(define (serious-number-stream n)
	(cons n (lambda () (serious-number-stream (+ n 1)))))
(display "#5\n")
(serious-number-stream 0)
((cdr (serious-number-stream 0)))

; #6
(define (stream-for-k-steps s k)
	(if (= 0 k)
			'()
			(cons (car s) (stream-for-k-steps ((cdr s)) (- k 1)))))
(display "#6\n")
(stream-for-k-steps (funny-number-stream) 13)

; #7
(define (va-helper v vec n)
	(cond [(= n (- 1)) #f]
				[(and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v)) (cdr (vector-ref vec n))]
				[else (va-helper v vec (- n 1))]))

(define (vector-assoc v vec)
	(letrec ([loop (lambda (i)
									 (cond
										 [(= i (- 1)) #f]
										 [(and (pair? (vector-ref vec i)) (equal? v (car (vector-ref vec i)))) (vector-ref vec i)]
										 [else (loop (- i 1))]))])
		(loop (- (vector-length vec) 1))))

(display "#7\n")
(vector-assoc 3 (vector '(1 2) 4 '(3 4)))

; #8
(define-syntax while-greater
	(syntax-rules (do) ((while-greater e1 do e2)
											(letrec ([loop (lambda ()
																			 (let ([y e2])
																				 (if (<= y e1)
																						 #t
																						 (loop))))]) (loop)))))

(define (TR e)
	(cond
		[(number? e) (quasiquote (unquote e))]
		[(equal? (car e) '+) (quasiquote (* ,(TR (cadr e)) ,(TR (caddr e))))]
		[(equal? (car e) '*) (quasiquote (+ ,(TR (cadr e)) ,(TR (caddr e))))]))


(display "#8\n")
(define a 7)
(while-greater 2 do (begin (set! a (- a 1)) (print "x") a))

(quote (+ (+ 1 1) 2))
(quasiquote (+ (unquote (+ 1 1)) 2))
