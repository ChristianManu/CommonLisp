;Problem 1
;This function receives an empty argument list.
;Since the returned value is described in the body of the function,
;a new object is created every time this function is called.
(defun myList () '(4 (7 22) "art" ("math" (8) 99) 100))

;Problem 2
(defun leapYear
    (&optional

     ;Starts searching for years from this value. Must be a multiple of 4.
     (fromYear 1800))
  (if (> fromYear 2021)
      '()
    (let (
          (years (leapYear (+ fromYear 4))))
      (if (or (not (zerop (mod fromYear 100))) (zerop (mod fromYear 400)))
          (cons fromYear years) 
        years 
        ))))

;Problem 3
;The result contains all elements of `xs` and `ys` except for elements of `zs`.
;The result is without duplicates.
(defun union- (xs ys &optional zs)
  ;In the recursive call, `zs` collects elements of `xs` and `ys`
  ;returned before. With the help of `zs`,
  ;duplicates don't make it to the result.
  (if xs ;If `xs` contains an element, try to put this element into the result.
      (if (member (car xs) zs)
          (union- (cdr xs) ys zs) ;`(car xs)` is a duplicate

        ;`(car xs)` is not a duplicate. Returned and inserted into `zs`.
        (cons (car xs)
              (union- (cdr xs) ys (cons (car xs) zs))))
    (if ys ;If `ys` contains an element, try to put this element
                                        ;into the result.
        (if (member (car ys) zs)
            (union- xs (cdr ys) zs)
          (cons (car ys)
                (union- xs (cdr ys) (cons (car ys) zs))))
      '())))

;Problem 4
;Returns `(/ sum1 n1)` where `sum1` is
;the sum of `sum` and all elements of `xs`
;and `n1` is the sum of `n` and the number of elements in `xs`.
;If `n1` is zero, returns `nil`.
(defun avg (xs &optional (sum 0) (n 0))
  ;`sum` and `n` are the sum and the number (respectively)
  ;of elements consumed before.
  (if xs
      ;This call to `avg` is in a tail-recursive position.
      (avg (cdr xs) (+ sum (car xs)) (+ n 1))

    ;If `n1` is zero, returns `nil`.
    (if (zerop n) nil (/ sum n))))

;Problem 5
;Returns a function that accepts `x` and returns
;whether `x` has type `dataType`.
(defun isType (dataType) (lambda (x) (typep x dataType)))

;Problem 6
;The concatenation of the reversed list `values1`
;and of the list `values` with all elements transformed as follows:
;if an element is greater than `limit`, it is multiplied by `rate`.
(defun taxCalculator (limit rate values &optional (values1 '()))


  ;The following implementation is tail-recursive.
  (if values
      ;Extracts the head of `values`, transforms it like in the specification,
      ;and adds it to `values1`.
      (taxCalculator limit rate (cdr values)
                     (cons (let ((x (car values)))
                             (if (> x limit) (* x rate) x))
                           values1))
    ;In order to make this function tail-recursive, elements of `values`
    ;were added to `values1` in reverse order.
    ;Fixing this by calling `reverse` which is also tail-recursive.
    (reverse values1)))

;Problem 7
(defun clean (f xs)
  (if xs
      ;if `xs` is not empty
      (if (consp xs)
          (let ((x (car xs))

                ;ys` is a tail of `xs` transformed with `clean`
                (ys (clean f (cdr xs))))
            
            (if (consp x)
                ;Transforms `x` with `clean` too if it is a `cons`.
                (cons (clean f x) ys)

              ;Return `x' if and only if `f` on `x` is true.
              (if (funcall f x) (cons x ys) ys)))

        '())
    
    ;`xs` is empty
    '()))

;Problem 8
(defmacro threeWayBranch (branchX branchY branchZ)

  ;`(car branchX)` is a conditional expression,
  ;and `(cdr branchX)` are expressions that should be executed
  ;if the conditional expression is true.
  `(if ,(car branchX)
       ,(cons 'progn (cdr branchX))

     (if ,(car branchY)
         ,(cons 'progn (cdr branchY))
       (if ,(car branchZ)
           ,(cons 'progn (cdr branchZ))
         nil))))
