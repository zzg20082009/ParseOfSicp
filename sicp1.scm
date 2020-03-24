(define sqrt-r
  (lambda (x)
    (letrec
	((sqrt-iter (lambda (guess)
		      (if (good-enough? guess) guess (sqrt-iter (improve guess)))))
	 (good-enough? (lambda (guess)
			 (< (abs (- (square-r guess) x)) 0.001)))
	 (improve (lambda (guess)
		    (average-r guess (/ x guess))))
	 (average-r (lambda (m n)
		      (/ (+ m n) 2)))
	 (square-r (lambda (m)
		     (* m m))))
      (cond
       ((= x 0) 0)
       ((< x 0) (sqrt-r (abs x)))
       (else
	(sqrt-iter 1.0))))))

(define cuberoot
  (lambda (x)
    (letrec
	((cr-iter (lambda (guess)
		    (if (good-enough? guess) guess (cr-iter (improve guess)))))
	 (good-enough? (lambda (guess)
			 (< (abs (- (cube guess) x)) 0.001)))
	 (improve (lambda (guess)
		    (/ (+ (/ x (square-r guess)) (* 2 guess)) 3)))
	 (square-r (lambda (m)
		     (* m m)))
	 (cube (lambda (m)
		 (* m m m))))
      (cr-iter 1.0))))

(define sqrt-r
  (lambda (x)
    (letrec
	((sqrt-iter (lambda (guess)
		      (if (good-enough? guess) guess (sqrt-iter (improve guess)))))
	 (good-enough? (lambda (guess)
			 (< (abs (- guess (improve guess))) 0.001)))
	 (improve (lambda (guess)
		    (average-r guess (/ x guess))))
	 (average-r (lambda (m n)
		      (/ (+ m n) 2)))
	 (square-r (lambda (m)
		     (* m m))))
      (sqrt-iter 1.0))))

(define sqrt-r
  (lambda (x)
    (letrec
	((sqrt-iter (lambda (guess)
		      (let ((nextguess (improve guess)))
			(if (good-enough? guess nextguess) nextguess (sqrt-iter nextguess)))))
	 (good-enough? (lambda (guess nextguess)
			 (< (abs (- guess nextguess)) 0.001)))
	 (improve (lambda (guess)
		    (average-r guess (/ x guess))))
	 (average-r (lambda (m n)
		      (/ (+ m n) 2)))
	 (square-r (lambda (m)
		     (* m m))))
      (sqrt-iter 1.0))))
;;----------------------------------------------------------------------------------------------------
;;---------------------------------chapter 1.2--------------------------------------------------------
(define factorial
  (lambda (n)
    (cond
     ((= n 1) 1)
     (else
      (* n (factorial (- n 1)))))))

(define factorial
  (lambda (n)
    (letrec
	((fac (lambda (x m)
		(cond
		 ((> x n) m)
		 (else
		  (fac (+ x 1) (* x m)))))))
      (cond
       ((= n 0) 1)
       ((< n 0) (display "error of the argument"))
       (else
	(fac 1 1))))))
;;迭代计算过程就是那种其状态可以用固定数目的状态变量描述的计算过程，同时，又存在着一套固定的规则，描述了计算过程在从一个状态到
;;下一个状态转换时，这些变量的更新方式。还有一个结束检测。在迭代的情况里，在计算过程中的任何一点，那几个程序变量都提供了有关计
;;算状态的一个完整描述。如果我们令上述计算过程在某两个步骤之间停下来，要想重新唤醒这一计算，只需要为解释器提供有关这三个变量的
;;值。而对于递归计算过程而言，这里还存在着另个的一些“隐含”信息，它们并未保存在程序变量里，而是由解释器维持着，指明了在所推迟的
;;运算所形成的链条里的漫游中，这一计算过程处在何处。这个链条越长，需要保存的信息也就越多。
;;---------------------------------------------------------------------------------------------------------
;;在scheme的实现中，即使语法的形式是递归的，但其计算过程可能是迭代的。具有这一特性的实现称为尾递归的。有了一个尾递归的实现，我
;;们就可以利用常规的过程调用机制表述迭代。

(define inc
  (lambda (n)
    (+ n 1)))

(define dec
  (lambda (n)
    (- n 1)))

(define ackman
  (lambda (x y)
    (cond
     ((= y 0) 0)
     ((= x 0) (* 2 y))
     ((= y 1) 2)
     (else
      (ackman (- x 1) (ackman x (- y 1)))))))
;;----------------------------------------------------------------------------------------------------------
(define fib
  (lambda (n)
    (cond
     ((= n 0) 0)
     ((= n 1) 1)
     (else
      (+ (fib (- n 1)) (fib (- n 2)))))))
;;这是一个递归的计算过程，不是迭代的计算过程。下面的是迭代的计算过程。设几个变量，保存计算过程中的状态，每一个计算的中间过程，都可
;;以用这几个状态变量来表示。
(define fib
  (lambda (n)
    (letrec
	((fibnacci (lambda (p1 sum m)      ;;m用来计数，表示是否达到了计算的终点n，sum则用来保存计算的中间结果。确定的中间结果
		     (if (= m n) sum
			 (fibnacci sum (+ p1 sum) (+ m 1))))))
      (fibnacci 0 1 1))))
;;-----------------------------------------------------------------------------------------------------------
;;換零钱方式的统计：给了50美分，25美分，10美分，5美分和1美分的零钱若干，将1美元換成零钱，共有多少不同方式。能否列出这些不同的換零钱方式
(define count-change
  (lambda (amount)
    (letrec
	((cc (lambda (amount-of-money kinds-of-coins)
	       (cond
		((= amount-of-money 0) 1)
		((or (= kinds-of-coins 0) (< amount-of-money 0)) 0)
		(else
		 (+ (cc amount-of-money (- kinds-of-coins 1)) (cc (- amount-of-money (value-of-coins kinds-of-coins)) kinds-of-coins))))))
	 (value-of-coins (lambda (kinds-of-coins)
			   (cond
			    ((= kinds-of-coins 1) 1)
			    ((= kinds-of-coins 2) 5)
			    ((= kinds-of-coins 3) 10)
			    ((= kinds-of-coins 4) 25)
			    ((= kinds-of-coins 5) 50)))))
      (cc amount 5))))

(define count-change
  (lambda (amount)
    (letrec
	((cc (lambda (solutions halfdollar quarter dime nickel penny)
	       (cond
		((= halfdollar (/ amount 50)) (inc solutions))
		((> quarter (/ amount 25)) (cc solutions (inc halfdollar) 0 0 0 0))
		((> dime (/ amount 10)) (cc solutions halfdollar (inc quarter) 0 0 0))
		((> nickel (/ amount 5)) (cc solutions halfdollar quarter (inc dime) 0 0))
		((> penny (/ amount 1)) (cc solutions halfdollar quarter dime (inc nickel) 0))
		((= amount (+ (* halfdollar 50) (* quarter 25) (* dime 10) (* nickel 5) penny)) (let ()
												  (display (cons halfdollar (cons quarter (cons dime (cons nickel (cons penny '())))))))
												  (newline)
												  (cc (inc solutions) halfdollar quarter dime nickel (+ penny 5)))
		(else
		 (cc solutions halfdollar quarter dime nickel (+ penny 5))))))
	 (inc (lambda (n)
		(+ n 1))))
      (cc 0 0 0 0 0 5))))
;;这个实现是迭代的，但是算法的效率非常低下，没有递归的实现的效率高。
;;-----------------------------------------------------------------------------------------------------------
(define count-change
  (lambda (amount)
    (letrec
	((cc (lambda (solutions halfdollar quarter dime nickel penny)
	       (cond
		((= halfdollar (/ amount 50)) (+ solutions 5))
		((= quarter (/ amount 25)) (cc solutions (inc halfdollar) 0 0 0 0))
		((= dime (/ amount 10)) (cc solutions halfdollar (inc quarter) 0 0 0))
		((= nickel (/ amount 5)) (cc solutions halfdollar quarter (inc dime) 0 0))
		((= penny (/ amount 1)) (cc solutions halfdollar quarter dime (inc nickel) 0))
		((= amount (+ (* halfdollar 50) (* quarter 25) (* dime 10) (* nickel 5) penny)) (let ()
												  (display (cons halfdollar (cons quarter (cons dime (cons nickel (cons penny '())))))))
												  (newline)
												  (cc (inc solutions) halfdollar quarter dime nickel (+ penny 5)))
		(else
		 (cc solutions halfdollar quarter dime nickel (+ penny 5))))))
	 (inc (lambda (n)
		(+ n 1))))
      (cc 0 0 0 0 0 5))))

(define count-change
  (lambda (amount)
    (letrec
	((cc (lambda (solutions halfdollar quarter dime nickel penny)
	       (cond
		((= halfdollar (/ amount 50)) (+ solutions 5))
		((= quarter (/ amount 25)) (cc solutions (inc halfdollar) 0 0 0 0))
		((= dime (/ amount 10)) (cc solutions halfdollar (inc quarter) 0 0 0))
		((= nickel (/ amount 5)) (cc solutions halfdollar quarter (inc dime) 0 0))
		((= penny (/ amount 1)) (cc solutions halfdollar quarter dime (inc nickel) 0))
		((= amount (+ (* halfdollar 50) (* quarter 25) (* dime 10) (* nickel 5) penny)) (cc (inc solutions) halfdollar quarter dime nickel (+ penny 5)))
		(else
		 (cc solutions halfdollar quarter dime nickel (+ penny 5))))))
	 (inc (lambda (n)
		(+ n 1))))
      (cc 0 0 0 0 0 5))))
;;-------------------------------------------------------------------------------------------------------------
(define f
  (lambda (n)
    (cond
     ((< n 3) n)
     (else
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))))

(define f
  (lambda (n)
    (letrec
	((fa (lambda (x n1 n2 n3)
	       (let ((nx (+ n1 (* 2 n2) (* 3 n3))))
		 (cond
		  ((= x n) nx)
		  (else
		   (fa (inc x) nx n1 n2))))))
	 (inc (lambda (m)
		(+ m 1))))
      (cond
       ((< n 3) n)
       (else
	(fa 3 2 1 0))))))
;;---------------------------------------------------------------------------------------------------------------
(define pascal
  (lambda (m n)
    (cond
     ((= n 1) 1)
     ((= m n) 1)
     (else
      (+ (pascal (- m 1) (- n 1)) (pascal (- m 1) n))))))

(define pascal
  (lambda (m n)
    (letrec
	((yanhui (lambda (x l)
		   (cond
		    ((= x m) (pick n l))
		    (else
		     (yanhui (inc x) (update l))))))
	 (inc (lambda (x)
		(+ x 1)))
	 (update (lambda (l)
		   (letrec
		       ((u (lambda (lat)
			     (cond
			      ((null? (cdr lat)) lat)
			      (else
			       (cons (+ (car lat) (car (cdr lat))) (u (cdr lat))))))))
		     (cons 1 (u l)))))
	 (pick (lambda (x lat)
		 (cond
		  ((= x 1) (car lat))
		  (else (pick (- x 1) (cdr lat)))))))
      (cond
       ((or (= m 1) (= m 2)) 1)
       (else
	(yanhui 2 '(1 1)))))))
;;-----------------------------------------------------------------------------------------------------------------
(define print-pascal
  (lambda (m)
    (letrec
	((update (lambda (l)
		   (letrec
		       ((u (lambda (lat)
			     (cond
			      ((null? (cdr lat)) lat)
			      (else
			       (cons (+ (car lat) (car (cdr lat))) (u (cdr lat))))))))
		     (cons 1 (u l)))))
	 (print (lambda (x l)
		  (cond
		   ((<= x m) (let ()
			       (display l)
			       (newline)
			       (print (+ x 1) (update l))))
		   (else
		    (display "end of print"))))))
      (cond
       ((= m 1) (display 1))
       (else
	(print 2 '(1 1)))))))
;;------------------------------------------------------------------------------------------------------------------
(define sine
  (lambda (x)
    (letrec
	((p (lambda (m)
	      (- (*  3 m) (* 4 (cube m)))))
	 (cube (lambda (m)
		 (* m m m)))
	 (sin (lambda (m)
		(cond
		 ((< m 0.1) m)
		 (else
		  (p (sin (/ m 3))))))))
      (sin x))))

(define sine
  (lambda (x)
    (letrec
	((coft (lambda (n m)
		 (cond
		  ((<= n 0.1) (cons n (cons m '())))    ;;n代表某数最后小于0.1的具体数值，m代表需要做几次迭代。
		  (else
		   (coft (/ n 3) (+ m 1))))))
	 (sin (lambda (m value counter)                             ;;m代表需要迭代的次数，他是上面coft函数返回值中的那个m，n的初值则是上面coft中的n
		  (cond
		   ((= m counter) value)
		   (else
		    (sin (+ m 1) (p value) counter)))))
	 (p (lambda (m)
	      (- (* 3 m) (* 4 (cube m)))))
	 (cube (lambda (m)
		 (* m m m))))
      (let
	  ((vac (coft x 0)))
	(cond
	 ((<= x 0.1) x)
	 (else
	  (sin 0 (car vac) (car (cdr vac)))))))))
;;以上的是计算sine值的迭代版本。原理也比较简单，先将参数不断的除3，计算出需要除多少次能才使得值小于0.1（m），保存计算的次数及小于0.1的那个具体的数值，这是由coft来完成的。
;;然后就是sin不断的从0开始迭代，迭代的终止条件就是迭代次数等于coft计算出来的m，迭代的公式就是由p算出来的。也就是反向来推，计算值。
;;---------------------------------------------------------------------------------------------------------------------
(define fast-exp
  (lambda (b n)
    (cond
     ((= n 0) 1)
     ((even? n) (square (fast-exp b (/ n 2))))
     (else (* b (fast-exp b (- n 1)))))))

(define fast-exp
  (lambda (b n)
    (letrec
	((fe (lambda (product m)
	       (cond
		((= m n) product)
		((> m n) (fe (/ product b) (- m 1)))
		(else
		 (fe (square product) (* m 2)))))))
      (fe b 1))))
;;这个迭代的实现效率不高。因为如果方次大的话，会有许多的多余计算。划不来。
;;---------------------------------------------------------------------------------------------------------------------
(define fast-exp
  (lambda (b n)
    (letrec
	((iter (lambda (result product a)
		 (cond
		  ((= a 0) result)
		  ((even? a) (iter result (square product) (/ a 2)))
		  (else
		   (iter (* result product) product (- a 1)))))))
      (iter 1 b n))))
;;这个方法是很好的方法，其中的一个参数值product可以在某一时刻不连续的改变！这是不太容易想到的。
;;----------------------------------------------------------------------------------------------------------------------
;;在上面的那个算法中，result*product*(b^a)保持不变。渐渐的减小a的值，增加product的值，当a奇数时，将result的值更新，使result=result*product
;;例如(2^2)^5=(2^2)*(2^2)^4，如此不断累积result的值。直至a的值为1，为0……这个算法是很巧妙的。所以下面的练习是巩固自己的理解。
(define fast-exp
  (lambda (b n)
    (letrec
	((iter (lambda (accu product exp)
		 (cond
		  ((= exp 0) accu)
		  ((even? exp) (iter accu (square product) (/ exp 2)))
		  (else
		   (iter (* accu product) product (- exp 1)))))))
      (iter 1 b n))))
;;利用关系(b^(n/2))^2=(b^2)^(n/2)，除了指数n和基数b之外，还应维持一个附加的状态变量a，并定义好状态转变，使得从一个状态到另一个状态转变时，a*b^n
;;不变。在计算过程的开始时令a取值1，并用计算过程结束时a的值作为回答。一般来说，定义一个不变量，要求它在状态之间保持不变，这一技术是思考迭代算法设计
;;问题时的一个非常强有力的方法。（在上面的例子中，a的值不断变化（就是accu)，product的值也不断变化，exp的值也随之改变，但其总的accu*(product^exp)的值
;;是始终不变，都是等于a*b^n的。
;;-----------------------------------------------------------------------------------------------------------------------
(define double
  (lambda (n)
    (+ n n)))

(define halve
  (lambda (n)
    (/ n 2)))

(define multi
  (lambda (n m)
    (cond
     ((= m 0) 0)
     (else (+ n (multi n (- m 1)))))))   ;;这个递归的过程中，计算乘的时间复杂度是线性的o(m)

(define multi
  (lambda (n m)
    (letrec
	((ml (lambda (accu middle count)
	       (cond
		((= count 0) accu)
		((even? count) (ml accu (double middle) (/ count 2)))
		(else (ml (+ accu middle) middle (- count 1)))))))
      (ml 0 n m))))                     ;;这个迭代的计算过程中，时间复杂度是log(m).

(define multi
  (lambda (a b)
    (cond
     ((= b 0) 0)
     ((even? b) (double (multi a (/ b 2))))
     (else (+ a (multi a (- b 1)))))))   ;;递归计算过程，时间复杂度是logb
;;-------------------------------------------------------------------------------------------------------------------------
;;the following application is battery included
(define multi
  (lambda (a b)
    (letrec
	((ml (lambda (accu middle count)
	       (cond
		((= count 0) accu)
		((even? count) (ml accu (double middle) (halve count)))
		(else (ml (+ accu middle) middle (- count 1))))))
	 (double (lambda (x)
		   (+ x x)))
	 (halve (lambda (x)
		  (/ x 2))))
      (ml 0 a b))))

;;another aggressive method of the russia peasant algorithm
(define multi
  (lambda (a b)
    (letrec
	((ml (lambda (accu middle count)
	       (cond
		((= count 0) accu)
		((even? count) (ml accu (double middle) (halve count)))
		(else
		 (ml (+ accu middle) (double middle) (- (halve count) 0.5))))))
	 (double (lambda (x)
		 (+ x x)))
	 (halve (lambda (x)
		  (/ x 2))))
      (ml 0 a b))))
;;-------------------------------------------------------------------------------------------------------------------------
(define fib
  (lambda (n)
    (letrec
	((iter (lambda (a b p q m)
		 (cond
		  ((= m 0) a)
		  ((even? m) (iter (u a b p q) (v a b p q) (w p q) (z p q) (/ m 2)))
		  (else
		   (iter (u a b p q) (v a b p q) p q (- m 1))))))
	 (u (lambda (a b p q)
	      (+ (* b q) (* a q) (* a p))))
	 (v (lambda (a b p q)
	      (+ (* b p) (* a q))))
	 (w (lambda (p q)
	      (+ (double p) (double q))))
	 (z (lambda (p q)
	      (+ (* 2 p q) (double q))))
	 (double (lambda (a)
		   (* a a))))
      (iter 1 0 0 1 n))))              ;;this version is write by me.and this version is not correct.
;;--------------------------------------------------------------------------------------------------------------------------
(define fib
  (lambda (n)
    (letrec
	((iter (lambda (a b p q m)
		 (cond
		  ((= m 0) b)          ;;this is one difference
		  ((even? m) (iter a b (w p q) (z p q) (/ m 2)))    ;;the second difference: we did not update p and qxs
		  (else
		   (iter (u a b p q) (v a b p q) p q (- m 1))))))
	 (u (lambda (a b p q)
	      (+ (* b q) (* a q) (* a p))))
	 (v (lambda (a b p q)
	      (+ (* b p) (* a q))))
	 (w (lambda (p q)
	      (+ (double p) (double q))))
	 (z (lambda (p q)
	      (+ (* 2 p q) (double q))))
	 (double (lambda (a)
		   (* a a))))
      (iter 1 0 0 1 n))))                                   ;; p=p^2+ q^2    q=2pq+q^2
;;---------------------------------------------------------------------------------------------------------------------------
(define gcd1
  (lambda (m n)
    (cond
     ((= n 0) m)
     (else
      (gcd1 n (remainder m n))))))

(define gcd1
  (lambda (m n k)
    (cond
     ((= n 0) k)
     (else
      (gcd1 n (remainder m n) (+ k 1))))))
;;lame定理：如果欧几里德算法需要用k步计算出一对整数的gcd，那么这对数中较小的那个数必然大于或等于第k个斐波那契数。
(define expmod
  (lambda (base n m)
    (cond
     ((= n 0) 1)
     ((even? n) (remainder (square (expmod base (/ n 2) m)) m))
     (else (remainder (* base (expmod base (- n 1) m)) m)))))
;;----------------------------------------------------------------------------------------------------------------------------
(define fermat-test
  (lambda (n count)
    (letrec
	((expmod (lambda (base exp m)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
		    (else (remainder (* base (expmod base (- exp 1) m)) m)))))
	 (try-it (lambda (ra)
		   (= (expmod ra n n) (remainder ra n))))
	 (fast-prim? (lambda (m times)
		       (cond
			((= times 0) #t)
			((try-it (+ 1 (random (- m 1)))) (fast-prim? (- m 1) (- times 1)))
			(else #f)))))
      (fast-prim? n count))))           ;;2147483647=2^31-1 is a big prime. you can test the time consumed by the two method.

(define t-prime?
  (lambda (n)
    (letrec
	((smallest-divsor (lambda (m)
			    (cond
			     ((= (remainder n m) 0) m)
			     ((> (square m) n) n)
			     (else (smallest-divsor (+ m 1)))))))
      (cond
       ((= (smallest-divsor 2) n) #t)
       (else #f)))))

(define timed-prime-test
  (lambda (primfunc n)
    (letrec
	((timeused (lambda (start)
		     (display (primfunc n))
		     (newline)
		     (display n)
		     (newline)
		     (display (- (runtime) start)))))
      (timeused (runtime)))))
		       
;;------------------------------------------------------------------------------------------------------------------------------
;;上面的两个计算过程时间相差还是比较大的。在这个机器上，判断2^31-1是否是素数，fermat-test用的时间大约是0.1秒，而t-prim?用的时间大约是0.5秒（或许是毫秒）
;;上面的递归计算过程很自然，想改成迭代的计算过程， 来改写expmod
(define modlst
  (lambda (n)
    (cond
     ((= n 1) '(1))
     ((even? n) (cons n (modlst (/ n 2))))
     (else (cons n (modlst (- n 1)))))))

(define expmod
  (lambda (base exp m result rtm)
    (cond
     ((= (+ rtm 1) exp) (let ()
			  (display "+ rtm 1")
			  (newline)
			  (remainder (* base result) m)))
     ((> (* rtm 2) exp) (let ()
			  (display "* rtm 2")
			  (newline)
			  (expmod base exp m (remainder (* base result) m) (+ rtm 1))))
    (else
     (let ()
       (display (cons result (cons rtm '())))
       (newline)
       (expmod base exp m (remainder (square result) m) (* rtm 2)))))))


(define expmod
  (lambda (base exp m result rtm)
    (cond
     ((= (+ rtm 1) exp) (remainder (* base result) m))
     ((> (* rtm 2) exp) (expmod base exp m (remainder (* base result) m) (+ rtm 1)))
     (else
      (expmod base exp m (remainder (square result) m) (* rtm 2))))))

;;(expmod 2 19 19 2 1) test function
;;这个版本的实现太低效，尤其是当exp非常大时，计算到一半就得线性推进了。
;;--------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------
(define expmod
  (lambda (base exp m result rtm)
    (cond
     ((= (+ rtm 1) exp) (remainder (* base result) m))
     ((> (* rtm 2) exp) (expmod (square base) rtm m (remainder (* result base) m) (+ rtm )))
     (else
      (expmod base exp m (remainder (square result) m) (* rtm 2))))))  ;;没有想好，所有不是个完整的实现，不能运行。目的：迭代计算过程来求expmod
;;--------------------------------------------------------------------------------------------------------------------------------
;;--------------------------------------------------------------------------------------------------------------------------------
(define search-for-primes
  (lambda (first last)
    (letrec
	((timed-prime-test (lambda (n)
			     (newline)
			     (start-prime-test n (runtime))))
	 (start-prime-test (lambda (n start-time)
			     (if (prime? n)
				 (let ()
				   (newline)
				   (display n)
				   (newline)
				   (report-prime (- (runtime) start-time))))))
	 (report-prime (lambda (elapsed-time)
			 (display "******")
			 (display elapsed-time))))
      (if (< first last) (let ()
			   (timed-prime-test first)
			   (search-for-primes (+ first 2) last))))))
;;---------------------------------------------------------------------------------------------------------------------------------
(define fermat-test
  (lambda (n)
    (letrec
	((expmod (lambda (base exp m)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
		    (else (remainder (* base (expmod base (- exp 1) m)) m)))))
	 (try-it (lambda (a)
		   (if (< a n)
		       (if (= (expmod a n n) (remainder a n)) (try-it (+ a 1)) #f) #t))))
      (try-it 2))))
;;这个实现是从2开始，依次调用fermat小定理。 对应书上的练习1.27用来测试那些所谓的carmichael数。实际判断一个数是不是素数，还是费马小定理，概率测试一下，
;;不需要从头到尾声都做测试。
;;----------------------------------------------------------------------------------------------------------------------------------
(define miler-robin
  (lambda (n)
    (letrec
	((expmod (lambda (base exp m)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp)
		     (let ((v (expmod base (/ exp 2) m)))
		       (if (mr v) 0 (remainder (square v) m))))
		    (else (remainder (* base (expmod base (- exp 1) m)) m)))))
	 (mr (lambda (v)
	       (cond
		((= v 1) #f)
		((= v (- n 1)) #f)
		(else
		 (= (remainder (square v) n) 1)))))
	 (slow-prim (lambda (a)
		      (if (< a n) (let ()
				    (if (= (expmod a (- n 1) n) 0) #f
					(slow-prim (+ a 1))))
			  #t))))
      (slow-prim 2))))
;;-------------------------------------------------------------------------------------------------------------------------------------
;;----------------使用miler-robin算法测试，进行概率性的测试一个数是不是素数--------------------------------------------------------------------
(define miler-robin
  (lambda (n times)         ;;进行times轮的测试，看其结果是不是符合miler-robin算法
    (letrec
	((expmod (lambda (base exp m)
		   (cond
		    ((= exp 0) 1)
		    ((even? exp) (let ((v (expmod base (/ exp 2) m)))
				   (if (miler-test v) 0 (remainder (square v) m))))
		    (else (remainder (* base (expmod base (- exp 1) m)) m)))))
	 (miler-test (lambda (v)
		       (cond
			((= v 1) #f)
			((= v (- n 1)) #f)
			(else (= (remainder (square v) n) 1)))))
	 (prim? (lambda (count)
		  (cond
		   ((> count 0) (if (= (expmod (+ (random (- n 1)) 1) (- n 1) n) 0) #f (prim? (- count 1)))) 
		   (else #t)))))
      (prim? times))))
;;miler-robin测试基于以下的定理：如果n是素数，那么任何小于n的整数的（n－1）次幂模n等于1，即a^(n-1) mod n = 1 (a<n的整数)
;;-----------------------------------------------1.3 用高阶函数做抽象----------------------------------------------------------------------
;;过程也就是一类抽象，过程描述的是一类方法，如果程序设计语言没有提供过程，就会迫使我们永远在语言恰好提供的那些特定基本操作的层面上工作，而不能基于更高级的操作工作。
;;过程最重要的作用是：表述方法，表述某种概念，比如立方，平方，求导等。人们对于功能强大的程序设计语言有一个必然要求，就是能为公共的模式命名，建立抽象，而后直接在抽象
;;的层次上工作。然而即使在数值计算过程中，如果将过程限制为只能以数作为参数，也会严重地限制我们建立“抽象”的能力。经常会有一些同样的程序设计模式能用于若干不同的过程。
;;为了把这种模式描述为相应的“概念”，我们需要构造出这样的过程：让它们以过程作为参数，或者以过程作为返回值。这类能操作过程的过程称为“高阶过程”，高阶过程能成为强有力
;;的抽象机制，极大地增强语言的表述能力。
(define sum-integers
  (lambda (a b)
    (if (> a b)
	0
	(+ a (sum-integers (+ a 1) b)))))

(define sum-cubes
  (lambda (a b)
    (if (> a b)
	0
	(+ (cube a) (sum-cubes (+ a 1) b)))))

;;calculate 1/1*3 + 1/5*7 + 1/9*11...
(define pi-sum
  (lambda (a b)
    (if (< a b)
	0
	(+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b)))))
;;可以看出，这三个过程共享着一种公共的基础模式。它们的很大一部分是共同的。只在所用的过程名字上不一样：用于从a算出需要加的项的函数，还有用于提供下一个a值的函数。！！！
;;所以可以提供一种抽象，提供这种共同的模式所表达的更一般性的概念：加，某种模式的加。
(define sum
  (lambda (term a next b)
    (if (> a b)
	0
	(+ (term a) (sum term (next a) next b)))))
;;有了上面的更一般的过程定义，它的抽象层次更高，提供了更一般的概念（某个范围内的累加）。我们只要提供不同的term next就ok了。
;;-----------------------------------------------------------------------------------------------------------------------------------------
;;-----------------------------------------------------------------------------------------------------------------------------------------
(define identity
  (lambda (x)
    x))

(define cube
  (lambda (x)
    (* x x x)))

(define pi-related
  (lambda (x)
    (/ 1.0 (* x (+ x 2)))))

(define next-related
  (lambda (x)
    (+ x 1)))

(define next-related
  (lambda (x)
    (+ x 4)))   ;;according to calculate pi-sum
;;------------------------------------------------------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------------------------------------------------------
;;once we have sum, we will be able to formular other concept
(define integral
  (lambda (f a b dx)
    (letrec
	((sum (lambda (term a next b)
		(if (> a b)
		    0
		    (+ (term a) (sum term (next a) next b)))))
	 (next (lambda (x)
		 (+ x dx))))
      (* (sum f a next b) dx))))


(define simpson
  (lambda (f a b n)
    (letrec
	((sum (lambda (k h)
		(cond
		 ((= k n) (f b))
		 ((even? k) (+ (* 2 (f (+ a (* k h)))) (sum (+ k 1) h)))
		 (else (+ (* 4 (f (+ a (* k h)))) (sum (+ k 1) h)))))))
      (let ((h (/ (- b a) n)))
	(/ (* h (+ (f a) (sum 1 h))) 3)))))
;;这个实现是能正确工作的，但实在是太恶心了，基本上看不出来什么逻辑结构，程序的结构非常的差。下面要想办法改进
;;------------------------------------------------------------------------------------------------------------------------------------------
(define simpson
  (lambda (f a b n)
    (let ((h (/ (- b a) n)))
      (letrec
	  ((sum-b (lambda (k)
		    (let ((y (f (+ a (* k h)))))
		      (cond
		       ((= k n) y)
		       ((even? k) (+ (* 2 y) (sum-b (+ k 1))))
		       (else (+ (* 4 y) (sum-b (+ k 1)))))))))
	(/ (* h (+ (f a) (sum-b 1))) 3)))))       ;;并没有充分利用上面刚刚定义的sum过程。还是只能孤立的写这个程序，有违练习的初衷。
;;------------------------------------------------------------------------------------------------------------------------------------------
(define product
  (lambda (term a next b)
     (if (> a b)
	 1
	 (* (term a)
	    (product term (next a) next b)))))
;;这里定义了一个一般性的乘法算式。算式有term参数，这个参数可以对不同的乘数要求做处理。

(define product
  (lambda (term a next b)
    (letrec
	((iter (lambda (cur result)
		 (if (> cur b)
		     result
		     (iter (next cur) (* result (term cur)))))))
      (iter a 1))))
;;这是个迭代算法，和上面的是一样的。

(define pi-term
  (lambda (n)
    (if (even? n)
	(/ (+ n 2) (+ n 1))
	(/ (+ n 1) (+ n 2)))))

(define next
  (lambda (x)
    (+ x 1)))
;;----------------------------------------------------------------------------------------------------------------------------------------------
(define accumulate
  (lambda (combiner null-value term a next b)
    (if (> a b)
	null-value
	(combiner (term a)
		  (accumulate combiner null-value term (next a) next b)))))

(define accumulate
  (lambda (combiner null-value term a next b)
    (letrec
	((iter (lambda (it)
		 (if (> it b)
		     null-value
		     (combiner (term it)
			       (iter (next it)))))))
      (iter a))))
;;上面的是递归的计算过程，下面要写出迭代的计算过程
(define accumulate
  (lambda (combiner null-value term a next b result)
    (if (> a b)
	result
	(accumulate combiner null-value term a next b
		    (combiner (term a) result)))))  ;;需要在调用accumulate时，将result的初始值设置为和null-value相同。

(define accumulate
  (lambda (combiner null-value term a next b result)
    (letrec
	((iter (lambda (cur result)
		 (if (> cur b)
		     result
		     (iter (next cur) (combiner result (term cur)))))))
      (iter a null-value))))
;;--------------------------------------------------------------------------------------------------------------------------------------------------
(define fiter-accumulate
  (lambda (fiter combiner null-value term a next b)
    (letrec
	((iter (lambda (cur)
		 (if (>  cur b)
		     null-value
		     (if (fiter cur)
			 (combiner (term cur) (iter (next cur)))
			 (iter (next cur)))))))
      (iter a))))

(define fiter-accumulate
  (lambda (fiter combiner null-value term a next b)
    (letrec
	((iter (lambda (cur result)
		 (if (> cur b)
		     result
		     (if (fiter cur)
			 (iter (next cur) (combiner result (term cur)))
			 (iter (next cur) result))))))
      (iter a null-value))))
;;----------------------------------------------------------------------------------------------------------------------------------------------------
(define search
  (lambda (func neg-p pos-p)
    (letrec
	((average (lambda (first end)
		    (/ (+ first end) 2)))
	 (close-enough (lambda (first end)
			 (< (abs (- first end)) 0.001))))
      (if (close-enough neg-p pos-p)
	  (average neg-p pos-p)
	  (let ((mid-p (average neg-p pos-p)))
	    (if (< (func mid-p) 0)
		(search func mid-p pos-p)
		(search func neg-p mid-p)))))))
;;====================================================================================================================================================
(define half-interval-method
  (lambda (f a b)
    (letrec
	((search (lambda (neg pos)
		   (let ((mid (average neg pos)))
		     (if (close-enough? neg pos)
			 mid
			 (cond
			  ((> (f mid) 0) (search neg mid))
			  ((< (f mid) 0) (search mid pos))
			  (else mid))))))
	 (close-enough? (lambda (neg pos)
			  (< (abs (- pos neg)) 0.001)))
	 (average (lambda (neg pos)
		    (/ (+ neg pos) 2))))
      (let ((a-value (f a))
	    (b-value (f b)))
	(cond
	 ((and (negative? a-value) (positive? b-value)) (search a b))
	 ((and (negative? b-value) (positive? a-value)) (search b a))
	 (else
	  (display "values are not of opposite sign")))))))
;;-----------------------------------------------------------------------------------------------------------------------------------------------------
(define fixed-point
  (lambda (f start)
    (let ((guess (f start)))
      (if (< (abs (- guess start)) 0.00001)
	  guess
	  (fixed-point f guess)))))
;;-----------------------------------------------------------------------------------------------------------------------------------------------------
(define fixed-point
  (lambda (f start)
    (letrec
	((try (lambda (guess)
		(let ((next (f guess)))
		  (if (tolerance guess next)
		      next
		      (try next)))))
	 (tolerance (lambda (guess next)
		      (< (abs (- guess next)) 0.00001))))
      (try start))))
;;可以利用这个求函数不动点的过程来求出平方根函数。因为y^2=x,要求出y的值，可以认为是y=x/y(x是已知的值),将y当作自变量，这个公式就是求函数的不动点。
(define sqrt-by-fix
  (lambda (x)
    (fixed-point (lambda (y) (/ x y)) 1.0)))
;;上面的这个定义会使得函数不收敛，使得猜测的值太偏离最初的值了，一直在两个值之间振荡。解决这个问题的方法是，平均阻尼。即让猜测的值不发生太大的振荡。由于最终的值是在y和x/y之间的，每次
;;猜测值为x/y的变化幅度太大，所以可以改为在(average (y x/y))
(define sqrt-by-fix
  (lambda (x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0)))

(define cube-by-fix
  (lambda (x)
    (fixed-point (lambda (y) (average y (/ x (square y)))) 1.0)))  ;;这里也使用了平均阻尼技术，但是可以换另外一种方法来用。

(define cube-by-fix
  (lambda (x)
    (fixed-point (lambda (y) (/ (+ (* 2 y) (/ x (square y))) 3)) 1.0)))    ;;这里的思想和平均阻尼技术相似，但是每一部分的权重是不一样的。
;;======================================================================================================================================================
;;如果要显示每次猜测的值，可以改的是fixed-point函数。
(define fixed-point
  (lambda (f start)
    (letrec
	((try (lambda (guess)
		(let ((next (f guess)))
		  (display next)
		  (newline)
		  (if (tolerance guess next)
		      next
		      (try next)))))
	 (tolerance (lambda (guess next)
		      (< (abs (- guess next)) 0.00001))))
      (try start))))
;;求函数不动点的方法，形式化的统一了一些东西，比如，不断的试探某些值，看一看猜测的值是不是更接近最终的答案等这类问题的解决。
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
(define x-to-x
  (lambda (x)
    (fixed-point (lambda (y) (/ (log x) (log y))) 2.0)))               ;;没有使用平均阻尼方法，计算x^x=1000的根时，用了34次猜测。

(define x-to-x
  (lambda (x)
    (fixed-point (lambda (y) (average y (/ (log x) (log y)))) 2.0)))   ;;使用了平均阻尼方法，猜测的次数非常的少。在计算x^x=1000的值时，用了9次，而上例则用了34次。

(define average
  (lambda (x y)
    (/ (+ x y) 2)))
;;=========================================================================================================================================================
(define cont-frac       ;;(practice 1.37)递归形式
  (lambda (n d k)
    (if (= k 0)
	0
	(/ (n (- k 1)) (+ (d (- k 1)) (cont-frac n d (- k 1)))))))

;;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 5)
;;之所以将以函数做为参数，传递给cont-frac，是因为无穷连分式当中，不一定值是1，可以灵活的根据参数的值改变其要返回的内容，而无穷连分式的形式则是不变的。例如，下式，没有以函数做为参数
;;所以下面的函数就不是非常的灵活，只能计算某种特定形式的无穷连分式。

(define cont-frac
  (lambda (n d k)
    (if (= k 0)
	0
	(/ n (+ d (cont-frac n d (- k 1)))))))

;;以下为迭代方式的函数定义
(define cont-frac
  (lambda (n d k r)
    (if (= k 0)
	r
	(cont-frac n d (- k 1)
		   (/ (n (- k 1)) (+ r (d (- k 1))))))))
;;-------------------------------------------------------------------------------------------------------------------------------------------------------
;;上面的关于cont-frac的定义是倒着来的，对于某些场合可能不是太习惯这种写法。可以定义如下的方法
;;递归的写法如下：
(define cont-frac
  (lambda (n d k)
    (letrec
	((iter (lambda (i)
		(if (= i k)
		    0
		    (/ (n i) (+ (d i) (iter (+ i 1))))))))
      (iter 1))))

(define n
  (lambda (i)
    (square i)))

(define d
  (lambda (i)
    (+ (* 2 i) 1)))
;;上面的这两个函数用来计算pi/4（其无穷连分式的形式见印象笔记里的那个收藏）
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
(define cont-frac
  (lambda (n d k)
    (letrec
	((iter (lambda (i result)
		 (if (= i 0)
		     result
		     (iter (- i 1)
			   (/ (n (- i 1)) (+ result (d (- k 1)))))))))
      (iter k 0.0))))
;;迭代的方法没办法做到像递归那样，这个迭代算法和上面的那个迭代算法是一样的，只不过用到了letrec
;;=========================================================================================================================================================
;;practice 1.38
(define e-euler
  (lambda (k)
    (+ 2.0
       (cont-frac (lambda (i) 1)
		  (lambda (i)
		    (if (= (remainder i 3) 2)
			(/ (+ i 1) 1.5)
			1))
		  k))))
;;这个问题的关键是求拟合函数。就是要找到d的那个通项范式。各d取值如下：1 （2 1 1 4 1 1 6 1 1 8 1 1），每3个数为一个周期，不太好想到……
;;practice 1.39
(define tan-cf
  (lambda (x k)
    (/ (cont-frac (lambda (i) (- 0 (square x)))
		  (lambda (i) (- (* 2 i) 1))
		  k)
       (- 0 x))))
;;关键还是求出n d函数的通项。这个是我自己想出来的，但是上面的那个就比较难想了。
;;----------------------------------------------------------------------------------------------------------------------------------------------------------
;;将过程作为参数传递，能够显著增强我们的程序设计语言的表达能力。通过创建另一种其返回值本身也是过程的过程，我们还能得到进一步的表达能力。
;;牛顿法：如果g(x)是一个可微函数，那么方程g(x)=0的一个解就是函数f(x)=x-g(x)/g'(x)的一个不动点。因为根据推导，x(n+1)=xn-g(xn)/g'(xn)。根据不动点的原理，在本例中，其实就是两个猜测
;;的值越来越近了。最终不变化，收缩到了那个0点。因为x(n+1)可以认为是g(x)=0的更好的解（相比与xn来说）
;;为了将牛顿方法实现为一个过程，我们首先必须描述导数的思想。请注意，“导数”不像平均阻尼，它是从函数到函数的一种变换。g'(x)=(g(x+dx)-g(x))/dx。其中dx趋向于0
(define deriv
  (lambda (g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx))))

(define dx 0.00001)

(define newton
  (lambda (g)
    (letrec
	((f (lambda (x)
	      (- x (/ (g x) ((deriv g) x))))))
      (fixed-point f 1.0))))
;;----------------------------------------------------------------------------------------------------------------------------------------------------------
;;将所有的东西综合在一起，试着写出来牛顿法求函数根的形式。（battery included）
(define newton
  (lambda (g)
    (letrec
	((fixed-point (lambda (func guess)
			(letrec
			    ((iter (lambda (guess)
				     (let ((next (func guess)))
				       (if (close-enough? next guess)
					   next
					   (iter next)))))
			     (close-enough? (lambda (next guess)
					      (< (abs (- next guess)) 0.00001))))
			  (iter guess))))   ;;求不动点的函数
	 (newton-method (lambda (g)
			  (lambda (x)
			    (- x (/ (g x) ((deriv g) x))))))
	 (deriv (lambda (g)
		  (lambda (x)
		    (let ((dx 0.00001))
		      (/ (- (g (+ x dx)) (g x)) dx))))))
      (fixed-point (newton-method g) 1.0))))
;;注意上式中所涉及到的相关的函数，都是对某些东西的抽象！首先是求函数不动点的fixed-point过程的抽象定义，然后是牛顿法对函数所做的变形，变形的过程中由于用到了求导，再定义deriv过程
;;在上面的例子中，当我专心的写一个过程时，我不要考虑其它过程，也就是说，当我集中注意一个概念时，将这个概念定义好了就可以了。
;;不论是牛顿法还是平均阻尼法，最终都是需要用到求函数不动点的方法，所以求函数的不动点才是最实质的，牛顿法或者平均阻尼，只是对相关的函数做了变形，然后交给函数不动点过程去求解。所以我们
;;可以将这一具有普遍性的思想表达为一个函数：
(define fixed-point-of-transform
  (lambda (g transform guess)
    (fixed-point (transform g) guess)))      ;;这里做到了概念的提升，也就是又做了一层的抽象，因为transform可以有多种变化。不一定就是牛顿或平均阻尼。如此求平方根就可以定义成如下的形式
;;===============================================================================================================================================================
(define sqrt-fix
  (lambda (x)
    (fixed-point-of-transform (lambda (y) (/ x y))
			      average-damp ;;求平均函数，需要自己定义，又生成一个新的函数
			      1.0)))
(define average-damp
  (lambda (f)
    (lambda (x)
      (/ (+ x (f x)) 2))))

(define sqrt-fix
  (lambda (x)
    (fixed-point-of-transform (lambda (y) (- (square y) x))
			      newton-transform
			      1.0)))
(define newton-transform
  (lambda (g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x))))))

(define deriv
  (lambda (f)
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))

(define dx 0.00001)         ;;只是为了记忆，练习而已。
;;===============================================================================================================================================================
;;我们将复合过程，看作一种至关重要的抽象机制，因为它使我们能将一般性的计算方法，用程序语言里的元素明确描述，高阶函数能如何去操作这些一般性的方法，建立起进一步的抽象来。抽象的层次越高，函数阶越高
;;作为编程者，我们应该对这类可能性保持高度敏感（创建复合过程，抽象...）设法从中识别出程序里的基本抽象，基于它们去进一步构造，并推广它们以创建威力更加强大的抽象。当然，这并不是说总应该采用尽可能
;;抽象的方式去写程序，程序设计专家知道如何根据工作中的情况，去选择合适的抽象层次，但是，能基于这种抽象去思考确实是最重要的，只有这样才可能在新的上下文中去应用它们。高阶过程的重要性，就在于使我们
;;能显式的用程序设计语言的要素去描述这些抽象，使我们能像操作其他计算元素一样去操作它们。
;;一般而言，程序设计语言总会对计算元素的可能使用方式强加上某些限制。带有最少限制的元素被称为第一级的状态（一等公民）。第一级元素的某些“权利或特权”包括：1、可以用变量命名;2、可以提供给过程作为参数
;;3、可以由过程作为结果返回；4、可以包含在数据结构中。在scheme中，过程是作为一等公民存在的，所以给有效的实现提出了挑战，但由此所获得的描述能力却是惊人的。（将过程作为一等公民的主要代价是，为使
;;过程能够作为值返回，我们就需要为过程里的自由变量保留空间，即使这一过程并不执行。这些变量都被存储在过程的环境里。
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;;practice 1.40
(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (cube x) (* a (square x)) (* b x) c ))))

(define cube
  (lambda (x)
    (* x x x)))

;;practice 1.41
(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define inc
  (lambda (x)
    (+ x 1)))

;;practice 1.42
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))         ;;函数的复合。f g是单参数函数，能过这种方式，能复合两个函数。

;;practice 1.43
(define repeated
  (lambda (f n)
      (if (= n 0)
	  (lambda (x) x)
	  (compose f (repeated f (- n 1))))))

(define repeat
  (lambda (f n)
    (letrec
	((iter (lambda (k result)
		 (if (= k 0)
		     result
		     (iter (- k 1) (compose f result)))))
	 (compose (lambda (g h)
		    (lambda (x)
		      (g (h x)))))
	 (identity (lambda (x)
		     x)))
      (iter n identity))))                ;;这个迭代的实现也不错。

(define (repeated f n) 
  (if (= n 1) 
      f 
      (compose f (repeated f (- n 1))))) ;;这个实现是更优雅的。
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
(define smooth
  (lambda (f)
    (lambda (x)
      (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3))))

(define n-smooth
  (lambda (f n)
    ((repeated smooth n) f)))
;;需要仔细的理解：过程是所谓的一等公民，可以和数据一样操作。
;;=========================================================================================================================================================
;;practice 1.44
(define average
  (lambda (x y)
    (/ (+ x y) 2)))

(define average-damp
  (lambda (f)
    (lambda (x)
      (average x (f x)))))

(define tolerance 0.00001)

(define fixed-point
  (lambda (f first-guess)
    (letrec
	((iter (lambda (guess)
		 (let ((next (f guess)))
		   (if (close-enough? next guess)
		       next
		       (iter next)))))
	 (close-enough? (lambda (prev succ)
			  (< (abs (- succ prev)) tolerance))))
      (iter first-guess))))

(define repeat
  (lambda (f n)
    (if (= n 1)
	f
	(lambda (x)
	  (f ((repeat f (- n 1)) x))))))

(define repeat
  (lambda (f n)
    (letrec
	((compose (lambda (g h)
		   (lambda (x)
		     (g (h x)))))
	 (iter (lambda (time)
		 (if (= time 1)
		     f
		     (compose f (iter (- n 1)))))))
      (iter n))))

(define get-max-pow
  (lambda (base n)
    (if (< (- n base) 0)
	0
	(+ 1 (get-max-pow base (/ n base))))))

(define pow
  (lambda (base n)
    (if (= n 0)
	1
	(if (even? n)
	    (square (pow base (/ n 2)))
	    (* base (pow base (- n 1)))))))

(define pow
  (lambda (base n)
    (letrec
	((iter (lambda (result a nr)
		 (if (= nr 0)
		     result
		     (if (even? nr)
			 (iter result (square a) (/ nr 2))
			 (iter (* result a) a (- nr 1)))))))
      (iter 1 base n))))

(define nth-root
  (lambda (n x)
    (fixed-point ((repeat average-damp (get-max-pow 2 n)) (lambda (y) (/ x (pow y (- n 1))))) 1.0)))
;;(nth-root 5 32)
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
(define iterative-improve
  (lambda (good-enough? improve)
    (lambda (x)
      (if (good-enough? x (improve x))
	  (improve x)
	  ((iterative-improve good-enough? improve) (improve x))))))

(define iterative-improve
  (lambda (good-enough? improve)
    (lambda (x)
      (let ((xim (improve x)))
	(if (good-enough? x xim)
	    xim
	    ((iterative-improve good-enough? improve) xim))))))

(define good-enough?
  (lambda (guess1 next)
    (< (abs (- next guess1)) 0.00001)))

(define sqrt-t
  (lambda (x)
    ((iterative-improve good-enough? (lambda (y) (/ (+ (/ x y) y) 2))) 1.0)))
