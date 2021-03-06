;bprint: print the board or not
;x-range: the range of `x'
;y-range: the range of `y'
;board: the board matrix: 0 means no stone; 1 means a black stone; 2 means a white stone
;recv: In regard to the input characters
;step: step
;dead-black: the number of the dead black stones
;dead-white: the number of the dead white stones
;last-two-board: the pair of the last two boards
(define (play-go bprint x-range y-range board recv cursor step dead-black dead-white manual)

 ;An alias of `play-go'
 (define (playgo bprint . lst)
  (apply play-go bprint x-range y-range lst))

 ;Make arguments fewer
 (define (pmask bprint mask . value)
  (define (set lst mask value)
   (my-fold-right (lambda (m l) (apply my-list-set l m)) lst (map list mask value )))
  (let ((lst (list board recv cursor step dead-black dead-white manual)))
   (apply playgo bprint (set lst mask value))))

 ;Is clearing stones?
 (define (clr_stone?) (and (> (string-length recv) 0) (char=? (string-ref recv 0) #\1)))

 ;Print the board
 (define (print)
  (define display-char (lambda (c) (case c ((0) "+") ((1) "O") (else "X"))))
  (define display-char-color (lambda (c) (case c ((0) "+") ((1) "\033[41mO\033[0m") (else "\033[44mX\033[0m"))))
  (begin
   (display "\033[3;J\033[H\033[2J\r\n")
   (for-each
    (lambda (n)
     (for-each
      (lambda (m)
       (display
	(string-append
         (if (zero? m) "    " "-")
         (if (equal? (cons m n) cursor)
          (string-append "\033[42m" (display-char (list-ref-2d board m n)) "\033[0m")
          (display-char-color (list-ref-2d board m n))))))
      (myrange x-range))
     (display "\r\n"))
    (myrange y-range))
   (display
    (string-append
     "    step:"
     (number->string step)
     "("
     (if (odd? step) "white" "black")
     ")\tblack:"
     (number->string dead-black)
     "\twhite:"
     (number->string dead-white)
     "\033[?25l\r\n"
     (if (clr_stone?)
      "Leaves the dead stones\r\n"
      "")))))

 (define in-range? (lambda (delt-xy x y) (and (<= 0 (+ x (car delt-xy)) (- x-range 1)) (<= 0 (+ y (cdr delt-xy)) (- y-range 1)))))

 (define (cmp board next-board) 
  (my-fold-right
   (lambda (xy r)
    (let (
      (old (list-ref-2d board (car xy) (cdr xy)))
      (new (list-ref-2d next-board (car xy) (cdr xy))))
     (if (< old new)
      (cons (list xy) (cdr r))
      (if (< new old)
       (cons (car r) (cons xy (cdr r)))
       r))))
   '(())
   (apply append (map (lambda (y) (map (lambda (x) (cons x y)) (myrange x-range))) (myrange y-range)))))

 ;A high-order function which can traverse the connected area including [x,y] and the Qi(a term of weiqi) by your own rule and returns a pair
 ;reulst => (flag-tab . init_neighbor)
 ;f-neighbor (3 arguments) : color1 color2 acc
 (define (cal-connected board x y result connect? f-neighbor)
  (my-fold-right
   (lambda (delt-xy r)
    (if (and
         (in-range? delt-xy x y)
         (connect? (list-ref-2d board x y) (list-ref-2d board (+ x (car delt-xy)) (+ y (cdr delt-xy))))
         (zero? (list-ref-2d (car r) (+ x (car delt-xy)) (+ y (cdr delt-xy)))))
     (cal-connected board (+ x (car delt-xy)) (+ y (cdr delt-xy)) r connect? f-neighbor)
     r))
   (cons
    (list-set-2d (car result) x y 1)
    (my-fold-right
     (lambda (delt-xy r)
      (if (in-range? delt-xy x y)
       (f-neighbor
         (list-ref-2d board x y)
         (list-ref-2d board (+ x (car delt-xy)) (+ y (cdr delt-xy)))
         r)
       r))
     (cdr result)
     '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))))
   '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))))

 (define (cut-recv)
  (cond
   ((zero? (string-length recv)) "")
   ((char=? (string-ref recv 0) #\1) (substring recv 0 1))
   (else "")))

 ;In the end, we count the stones to decide who wins the game
 (define (count-stones board)
  (define (seek0 board)
   (define (seek0-line line r)
    (cond
     ((null? line) -1)
     ((zero? (car line)) r)
     (else (seek0-line (cdr line) (+ r 1)))))
   (define (seek0-iter board y)
    (if (null? board)
     #f
     (let ((x (seek0-line (car board) 0)))
      (if (>= x 0)
       (cons x y)
       (seek0-iter (cdr board) (+ y 1))))))
   (seek0-iter board 0))
  (define (mark-iter board)
   (let ((cord (seek0 board)))
    (if cord
     (let*
       ((nb (lambda (m n r) (cond ((= n 1) (cons #t (cdr r))) ((= n 2) (cons (car r) #t)) (else r))))
       (s (cal-connected board (car cord) (cdr cord) (cons (make-list y-range (make-list x-range 0)) '(#f . #f)) = nb))
       (color (+ (if (cadr s) 1 0) (if (cddr s) 2 0))))
      (mark-iter (map (lambda (b-line flag-line) (map (lambda (b flag)(if (zero? flag) b color)) b-line flag-line)) board (car s))))
     board)))
  (let*
    ((board2 (mark-iter board))
    (count (lambda (color b) (apply + (map (lambda (line) (apply + line)) (map (lambda (line) (map (lambda (c) (if (= color c) 1 0)) line)) b)))))
    (count1 (count 1 board2))
    (count2 (count 2 board2))
    (other (- (* x-range y-range) count1 count2)))
   (cons (+ count1 (/ other 2)) (+ count2 (/ other 2)))))

 ;A new step
 (define (put-stone cursor xy)
  (let*
    ((board2 (list-set-2d board (car xy) (cdr xy) (if (odd? step) 2 1)))
    (nb (lambda (m n r) (or r (zero? n))))
    (s (cal-connected board2 (car xy) (cdr xy) (cons (make-list y-range (make-list x-range 0)) #f) = nb))
    (s2
     (my-fold-right
      (lambda (delt-xy r)
       (if
        (and
         (in-range? delt-xy (car xy) (cdr xy))
         (= 3 (+ (list-ref-2d board2 (car xy) (cdr xy)) (list-ref-2d board (+ (car delt-xy) (car xy)) (+ (cdr delt-xy) (cdr xy)))))
         (zero? (list-ref-2d (car r) (+ (car delt-xy) (car xy)) (+ (cdr delt-xy) (cdr xy)))))
        (let*
	 ((nb (lambda (m n r) (or r (zero? n))))
	  (s3 (cal-connected board2 (+ (car delt-xy) (car xy)) (+ (cdr delt-xy) (cdr xy)) (cons (make-list y-range (make-list x-range 0)) #f) = nb)))
         (if (cdr s3)
          r
          (cons
           (map (lambda (m n) (map (lambda (mm nn) (if (zero? (+ mm nn)) 0 1))  m n)) (car r) (car s3))
           #f)))
        r))
      (cons (make-list y-range (make-list x-range 0)) #f)
      '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))))
    (s2-cnt (apply + (map (lambda (m) (apply + m)) (car s2))))
    (next-board (map (lambda (b-line flag-line) (map (lambda (b flag) (if (zero? flag) b 0)) b-line flag-line)) board2 (car s2))))
   (if (and (not (cdr s))(or (zero? s2-cnt) (equal? (cmp board next-board) ((lambda (x) (cons (cdr x) (car x))) (car manual)))))
    (pmask #f '(1) (cut-recv))
    (let
     ((next-dead-black (if (odd? step) (+ dead-black s2-cnt) dead-black))
     (next-dead-white (if (odd? step) dead-white (+ dead-white s2-cnt)))
     (next-manual (cons (cmp board next-board) manual)))
    (pmask #t '(0 1 3 4 5 6) next-board (cut-recv) (+ step 1) next-dead-black next-dead-white next-manual)))))

 ;Return a pair of the next `recv' argument and the action
 (define (new-input x)
  (case x
   ((#\b) (cons "" 'B )) ;B for back
   ((#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S)
    (if (and (> (string-length recv) 0) ((lambda (x) (and (char>=? x #\A) (char<=? x #\S))) (string-ref recv (- (string-length recv) 1))))
     (cons
      (cut-recv)
      (cons
       (- (char->integer (string-ref recv (- (string-length recv) 1))) (char->integer #\A) (car cursor))
       (- (char->integer x) (char->integer #\A) (cdr cursor))))
     (cons (string-append recv (make-string 1 x)) '())))
   ((#\w) (cons (cut-recv) (if (zero? (cdr cursor)) '() '(0 . -1))))
   ((#\s) (cons (cut-recv) (if (= (- y-range 1) (cdr cursor)) '() '(0 . 1))))
   ((#\d) (cons (cut-recv) (if (= (- x-range 1) (car cursor)) '() '(1 . 0))))
   ((#\a) (cons (cut-recv) (if (zero? (car cursor)) '() '( -1 . 0))))
   ((#\return #\newline)
    (let* (
           (new-recv (cut-recv))
           (xy (string->numberpair recv))
           (x-r (if xy (car xy) (car cursor)))
           (y-r (if xy (cdr xy) (cdr cursor))))
     (cons new-recv
      (cond
       ((and (not xy) (or (< (car cursor) 0) (< (cdr cursor) 0))) '())
       ((and (string=? new-recv "") (zero? (list-ref-2d board x-r y-r))) (if xy (cons 'P xy) 'P))
       ((string=? new-recv "1")
        (let ((c (list-ref-2d board x-r y-r)))
        (cond
         ((= c 1) (if xy (cons 'C1 xy) 'C1))
         ((= c 2) (if xy (cons 'C2 xy) 'C2))
         (else '()))))
       (else '())))))
   ((#\1) (if (or (zero? step) (clr_stone?)) (cons (cut-recv) '()) (cons "1" 'X)))
   ((#\2) (cons (cut-recv) (if (clr_stone?) 'Z '())))
   (else (cons (cut-recv) '()))))

 ;deal the pair of `s' returned by the `new-input' function
 (define (play s)
  (cond
   ((pair? (cdr s))
    (cond
     ((number? (cadr s))
      (pmask #t '(1 2) (car s)
       (let ((new-x (+ (car cursor) (cadr s))) (new-y (+ (cdr cursor) (cddr s))))
        (if (or (< new-x 0) (< new-y 0))
         '(0 . 0)
         (cons new-x new-y)))))
     ((member (cadr s) '(C1 C2))
      (let*
	((connect? (lambda (m n) (or (= n (if (eq? (cadr s) 'C1) 1 2)) (= n 0))))
	(nb (lambda l #t))
	(q (cal-connected board (caddr s) (cdddr s) (cons (make-list y-range (make-list x-range 0)) #f) connect? nb))
	(next-board (map (lambda (b-line flag-line) (map (lambda (b flag) (if (zero? flag) b 0)) b-line flag-line)) board (car q))))
       (pmask #t '(0 1 2 4 5) next-board (car s) '(-1 . -1) 0 0)))
     ((eq? (cadr s) 'P) (put-stone '(-1 . -1) (cddr s)))))
   ((eq? (cdr s) 'B)
    (if (zero? step)
     (pmask #f '(1) (car s))
     (let*
       ((clr_stone (clr_stone?))
       (next-manual (if clr_stone manual (cdr manual)))
       (next-step (if clr_stone step (- step 1)))
       (stat
        (my-fold-right
	 (lambda (s r)
	  (cons
	   (list-set-2d
	    (my-fold-right (lambda (xy b) (list-set-2d b (car xy) (cdr xy) 0)) (car r) (cdr s))
	    (caaar s)
	    (cdaar s)
	    (cadddr r))
	   (if (= 1 (cadddr r))
	    (list (cadr r) (+ (caddr r) (length (cdr s))) 2 (caar s))
	    (list (+ (cadr r) (length (cdr s))) (caddr r) 1 (caar s)))))
	 (cons (make-list y-range (make-list x-range 0)) '(0 0 1 (-1 . -1)))
	 next-manual)))
      (playgo #t (car stat) (car s) (car (cddddr stat)) next-step (cadr stat) (caddr stat) next-manual))))
   ((eq? (cdr s) 'X) (pmask #t '(1 6) (car s) manual))
   ((or (eq? (cdr s) 'C1) (eq? (cdr s) 'C2))
    (let*
      ((connect? (lambda (m n) (or (= n (if (eq? (cdr s) 'C1) 1 2)) (= n 0))))
      (nb (lambda l #t))
      (q (cal-connected board (car cursor) (cdr cursor) (cons (make-list y-range (make-list x-range 0)) #f) connect? nb)))
     (pmask #t '(0 1 4 5) (map (lambda (b-line flag-line) (map (lambda (b flag) (if (zero? flag) b 0)) b-line flag-line)) board (car q))(car s) 0 0)))
   ((eq? (cdr s) 'P) (put-stone cursor cursor))
   ((eq? (cdr s) 'Z)
    (let ((stones (count-stones board)))
     (display (string-append "result:\tblack:" (number->string (exact->inexact (car stones))) "\twhite:" (number->string (exact->inexact (cdr stones))) "\r\n"))))
   (else (pmask #f '(1) (car s)))))

;Here is the implement of 'play'
 (begin
  (if bprint (print) (display ""))
  (let ((x (read-char)))
   (if (eof-object? x)
    (display "\033[?25h")
    (play (new-input x))))))

;Let's go
(let ((x-range 19) (y-range 19))
 (play-go
  #t
  x-range
  y-range
  (make-list y-range (make-list x-range 0))
  ""
  '(-1 . -1)
  0
  0
  0
  '()))
