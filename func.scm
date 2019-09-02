;Return a list which is from 0 to n-1
(define (myrange n)
 (define (myrange2 a b)
  (if (>= a b)
   '()
   (cons a (myrange2 (+ a 1) b))
  )
 )
 (myrange2 0 n)
)

;The fold high-order function
(define my-fold-right
 (lambda (op init lst)
  (if (null? lst)
   init
   (op (car lst) (my-fold-right op init (cdr lst)))
  )
 )
)

;Return a new list which's position of index `x' is replaced to `value'
(define (my-list-set lst index value)
 (if (zero? index)
  (cons value (cdr lst))
  (cons (car lst) (my-list-set (cdr lst) (- index 1) value))
 )
)

;`m' is a matrix
;Return the new matrix(set m[x,y] to `v')
(define (list-set-2d m x y v)
 (my-list-set m y (my-list-set (list-ref m y) x v))
)

;`m' is a matrix
;Return m[x,y]
(define (list-ref-2d m x y)
 (list-ref (list-ref m y) x)
)

;Translate string to the pair of numbers
;For example: "10 18" => '(10 . 18) "X11 8 " =>'(11 . 8)
(define (string->numberpair s)
 (define (get-number-string s len)
  (cond
   ((zero? len) (cons 0 ""))
   ((or (char<? (string-ref s 0) #\0) (char>? (string-ref s 0) #\9)) (let ((r (get-number-string (substring s 1 len) (- len 1)))) (cons (+ 1 (car r)) (cdr r))))
   ((= len 1) (cons 0 s))
   ((or (char<? (string-ref s 1) #\0) (char>? (string-ref s 1) #\9)) (cons 0 (make-string 1 (string-ref s 0))))
   (else (cons 0 (string-append (make-string 1 (string-ref s 0)) (cdr (get-number-string (substring s 1 len) (- len 1))))))
  )
 )
 (define gn (lambda (s) (get-number-string s (string-length s))))
 (let* (
        (len (string-length s))
        (a (get-number-string s len))
        (snd-start (+ (car a) (string-length (cdr a))))
        (b (get-number-string (substring s snd-start len) (- len snd-start)))
        )
  (if (string=? (cdr b) "")
   #f
   (cons (string->number (cdr a)) (string->number (cdr b)))
  )
 )
)
