;; 符号数据
;; 到目前为止，我们已经使用过的所有复合数据，最终都是从数据值出发构造起来的。在这一节里，我们要扩充所用语言的表述能力，引进将任意符号作为数据的能力。
;; 为了能够操作这些符号，我们的语言里就需要有一种新元素：为数据对象加引号的能力。假定我们希望构造出表(a b)，当然不能用(list a b)完成这件事，因为这一表达式将要构造出的是a和b的值
;; 的表，而不是这两个符号本身的表。在自然语言的环境中，这种情况也是众所周知的，在那里的单词和句子可能看作语义实体，也可以看作是字符的序列（语法实体）。在自然语言里，常见的方式就是
;; 用引号表明一个词或者一个句子应作为文字看待，将它们直接作为字符的序列。例如说，“John”的第一个字母显然是“J”（这样我们说的是单词John本身，而不是John所指向的某个人）。如果我们对
;; 某人说，“大声说你的名字”，此时希望听到的是那个人的名字，如果说，“大声说 ‘你的名字”，此时希望听到的就是词组“你的名字”。请注意，我们在这里不得不嵌套的引号去描述别人应该说的东西。
;; 允许在一个语言中使用引号，将会极大地损害根据简单词语在语言中做推理的能力，因为它破坏了对等的东西可以相互替换的观念。举个例子，三等于二加一，但是“三”这个字却不等于“二加一”这个短
;; 语。引号是很有威力的东西，因为它使我们可以构造起一种能操作其他表达式的表达式。正如我们将在第4章里看到的那样。但是，在一种语言里允许用语句去讨论这一语言里的其他语句，那么有关“对
;; 等的东西可以相互代換究竟是什么意思，我们就很难给任何具有内在统一性的说法了。举例来说，如果我们知道长庚星就是启明星，那么我们就可以从句子“长庚星就是金星”推导出“启明星就是金星”。
;; 然而，即使有“张三知道长庚星就是金星”（这代表的可能只是一句语法形式上的），我们也无法推论说“张三知道启明星就是金星”（没法做等价代换）

;; 我们可以按照同样的方式，将表和符号标记为应该作为数据对象看待，而水是作为应该求值的表达式。然而，这里所用的引号形式与自然语言中的不同，我们只在被引用对象的前面放一个引号（按照习惯
;; 在这里用单引号）。在Scheme里可以不写结束引号，因为这里已经靠空白和括号将对象分隔开，一个单引号的意义就是引用下一个对象。

;; 严格地说，引号的这种使用方式，违背了我们语言中所有复合表达式都应该由括号限定，都具有表的形式的普遍性原则。通过引进特殊形式quote就可以恢复这种一致性，这种特殊形式的作用与引号完全
;; 一样。因此我们完全可以用（quote a）代替‘a。这也就是解释器的实际工作方式。引号只不过是一种将(quote <expression>)包裹起来的单字符缩写形式。这一点非常重要，因为它维持了我们的
;; 原则：解释器看到的所有表达式都可以作为数据对象去操作。例如，我们可以构造出表达式(car '(a b c))，它就等同于通过对表达式(list 'car (list 'quote '(a b c)))的求值而得到的
;; (car (quote (a b c)))

;; 为了能对符号做各种操作，我们还需要用另一个基本过程eq?，这个过程以两个符号作为参数，检查它们是否为同样的符号（我们可以认为，两个符号是“同样的”，如果它们是由同样字符按照同样顺序构
;; 成。这一定义回避了一个我们目前尚且无法去探讨的深入问题：程序设计语言里“同样”的意义问题）。利用eq？可以实现一个称为memq（在MIT SCHEME里是一个基本过程）的有用过程，它以一个符号
;; 和一个表为参数。如果这个符号不包含在这个表里（也就是说，它与表里的任何项目都不eq？），memq就返回假；否则就返回该表的由这个符号的第一次出现开始的那个子表。
(define memq
  (lambda (item seq)
    (if (null? seq)
	#f
	(if (eq? item (car seq))
	    seq
	    (memq item (cdr seq))))))

(define equal1?
  (lambda (item1 item2)
    (if (pair? item1)
	(if (pair? item2)
	    (cond
	     ((null? item1) (null? item2))
	     ((pair? (car item1)) (and (equal1? (car item1) (car item2))
				       (equal1? (cdr item1) (cdr item2))))
	     (else (and (eq? (car item1) (car item2))
			(equal1? (cdr item1) (cdr item2)))))
	    #f)
	(eq? item1 item2))))
;; 我上面的这个实现有点复杂了。可以用多个and串连起来
(define (equal1? list1 list2)
  (cond
   ((and (not (pair? list1)) (not (pair? list2))) (eq? list1 list2))
   ((and (pair? list1) (pair? list2)) (and (equal1? (car list1) (car list2))
					   (equal1? (cdr list1) (cdr list2))))
   (else #f)))
;; 我觉得上面的这个实现逻辑上更清楚
;;-------------------------------------------------------------------------------------------------------------------------------------------------------
;; practice 2.55
(car ''abracadabra)
;; is equvalent to (car (quote (quote abracadabra)))
;; 2.3.2符号求导
;; 为了开发出一个符号计算程序，我们将按照2.1.1节开发有理数系统那样，采用同样的数据抽象策略。也就是说，首先定义一个求导算法，令它在一些抽象对象上操作，例如“和”、“乘积”和“变量”，并不
;; 考虑这些对象实际上如何表示，以后才去关心具体表示的问题
;; 为了使有关的讨论简单化，我们在这里考虑一个非常简单的符号求导程序，它处理的表达式都是由对于两个参数的加和乘运算构造起来的。对于这种表达式求导的工作可以通过下面几条归约规则完成：
;;   1. dc/dx=0  当c是一个常量，或者一个与x不同的变量
;;   2. dx/dx=1
;;   3. d(u+v)/dx = du/dx + dv/dx
;;   4. d(uv)/dx = u(dv/dx) + v(du/dx)
;; 可以看到，这里的最后两条规则具有递归的性质，也就是说，要想得到一个和式的导数，我们首先要找出其中各个项的导数，而后将它们相加。这里的每个项又可能是需要进一步分解的表达式。通过这种分解
;; 我们能得到越来越小的片段，最终将产生出常量或者变量，它们的导数就是0或者1.
;; 为了能在一个过程中体现这些规则，我们用一下按愿望思维，就像在前面设计有理数的实现时所做的那样。如果现在有了一种表示代数表达式的方式，我们一定能判断出某个表达式是否为一个和式、乘式、常
;; 量或者变量，也能提取出表达式里的各个部分。对于一个和式，我们可能希望取得其被加项（第一项）和加项（第二个项）。我们还需要能从几个部分出发构造出整个表达式。让我们假定现在已经有了一些过
;; 程，它们实现了下述的构造函数、选择函数和谓词。

;; (variable? e)                  e是变量吗？
;; (same-variable? v1 v2)         v1 v2是同一个变量吗？
;; (sum? e)                       e是和式吗？
;; (addend e)                     e的被加数
;; (augend e)                     e的加数
;; (make-sum a1 a2)               构造起a1与a2的和式

;; (product? e)                   e是乘式吗？
;; (multiplier e)                 的被乘数
;; (multiplicand e)               e的乘数
;; (make-product m1 m2)           构造起m1与m2的乘数

;; 利用这些过程，以及判断表达式是否数值的基本过程number? 我们就可以将各种求导规则用下面的过程表达出来了.
;; 下式中存在着某些递归的成份，所以，要先写出递归的最终出口
(define deriv
  (lambda (exp var)
    (cond
     ((number? exp) 0)
     ((variable? exp)
      (if (same-variable? exp var) 1 0))
     ((sum? exp) (make-sum (deriv (addend exp) var)
			   (deriv (augend exp) var)))
     ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
			       (make-product (multiplicand exp) (deriv (multiplier exp) var))))
     (else
      (error "Unknow expression type ---DERIV" exp)))))
;; 过程deriv里包含了一个完整的求导算法。因为它是基于抽象数据表述的，因此，无论我们如何选择代数表达式的具体表示，只要设计了一组正确的选择函数和构造函数，这个过程都可以工作。表示的问题是下
;; 面必须考虑的问题：代数表达式的表示
;; 我们可以设想出许多表结构表示代数表达式的方法。（下面的所有东西，都是考虑具体的代数表达式的表示问题）
(define (variable? x) (symbol? x))          ;;用基本过程symbol?来判断变量x是否是变量

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (addend s) (car (cdr s)))

(define (augend s) (car (cdr (cdr s))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (car (cdr p)))

(define (multiplicand p) (car (cdr (cdr p))))

;; 使用上面的程序，产生出的结果是对的，但是它们没有经过化简。当表达式变得更加复杂时，这一情况可能变成严重的问题（没有经过化简的结果很不直观）。现在所面临的困难很像我们在做有理数时所遇到的问
;; 题：希望将结果化简到最简单的形式。为了完成有理数的化简，我们只需要修改构造函数和选择函数的实现。这里也可以采取同样的策略。这里也可以采取同样的策略。我们在这里也完全不必修改deriv，只需要
;; 修改make-sum，使得当两个求和对象都是数时，make-sum求出它们的和返回。还有，如果其中的一个求和对象是0，那么make-sum就直接返回另一个对象。
(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else (list '+ a1 a2))))

;; (define (make-sum a1 a2)
;;   (cond
;;    ((=number? a1 0) a2)
;;    ((=number? a2 0) a1)
;;    ((and (number? a1) (number? a2)) (+ a1 a2))
;;    ((equal? a1 a2) (list '* 2 a1))                     ;; added here
;;    (else (list '+ a1 a2))))

(define (=number? x v)
  (if (number? x)
      (eq? x v)
      #f))

;; 与此类似，我们也需要修改make-product，高潮引进下面的规则：0与任何东西的乘积都是0， 1与任何东西的乘积都是那个东西：
(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else (list '* m1 m2))))

;; 在deriv过程中，要想做出一个程序，使它能将表达式做成我们都能同意的“最简单”的形式，前面还有很长的路要走。代数化简是一个非常复杂的问题，除了其他各种因素之外，还有另一个根本性的问题：对于某种
;; 的最简形式，对于另一种用途可能就不是最简形式。
;; practice 2.56
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (car (cdr exp)))

(define (exponent exp) (car (cdr (cdr exp))))

(define (make-exponentiation base n)
  (cond
   ((= n 0) 1)
   ((= n 1) base)
   (else
    (list '** base n))))
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------
(define deriv
  (lambda (exp var)
    (cond
     ((number? exp) 0)
     ((variable? exp)
      (if (same-variable? exp var) 1 0))
     ((exponentiation? exp) (make-product (exponent exp) (make-product (make-exponentiation (base exp) (- (exponent exp) 1)) (deriv (base exp) var))))
     ((sum? exp) (make-sum (deriv (addend exp) var)
			   (deriv (augend exp) var)))
     ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
			       (make-product (multiplicand exp) (deriv (multiplier exp) var))))
     (else
      (error "Unknow expression type ---DERIV" exp)))))
;; ---------------------------------------------------------------------------------------------------------------------------------------------------------------
;; practice 2.57 扩充求导程序，使之能处理任意项（两项或者更多项）的和与乘积。例：(deriv '(* x y (+ x 3)) 'x)
(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))

(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else
    (make-sum-list (list a1 a2)))))

(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cdr l)))))

(define (make-product m1 m2)
  (cond
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else (make-product-list (list m1 m2)))))

(define (augend s)
  (let ((a (cddr s)))
    (if (= (length a) 1)
	(car a)
	(make-sum-list a))))

(define (multiplicand p)
  (let ((m (cddr p)))
    (if (= (length m) 1)
	(car m)
	(make-product-list m))))
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;; practice 2.58 假定我们希望修改求导程序，使唤它能用于常规数学公式，其中＋和＊采用的是中缀运算而不是前缀。由于求导程序是基于抽象数据定义的，要修改它，使唤之能用于另一种不同的表达形式表示，我
;; 们只需要换一套工作在新的，求导程序需要使用的代数表达式的表示形式上的谓词、选择函数和构造函数
;; Part A is pretty straight forward. we will just change the representation
(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   (else (list a1 '+ a2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((or (=number? m1 0) (=number? m2 0)) 0)
   (else
    (list m1 '* m2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier x) (car x))

(define (multiplicand x) (caddr x))
;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------
;; Part B. 如果允许标准的代数写法，例如(x + 3 * (x + y + 2))
;; The main problem is essentially to recognize whether a given expression is a sum or product. Now, keep in mind that, despite our moving to a representation that
;; is more orthodox to traditional notation, we are still playing pun: the parentheses which in one sense are used as mathematical groupings are the same time sub-lists
;; in list-structure. We can assume that these sub-lists will be valid expressions, so they will also be self-contained expressions. The upshot of this is that we
;; need to concern ourselves with only the topmost "layer" of an expression
(define (sum? expr)
  (eq? '+ (smallest-op expr)))

(define (product? expr)
  (eq? '* (smallest-op expr)))

;; Where smallest-op searches an expression for the lowest-precedence operator, which can be done as an accumulation.
;; While the following solution is not correct, I mean the procedure smallest-op is not correct,because you can't assure a is + or *
;; ---------------------------------------------------------------------------------------
(define (smallest-op expr)
  (accumulate (lambda (a b)
		(if (operator? b)
		    (min-precedence a b)
		    a))
	      'maxop
	      expr))


;; The following smallest-op is enough, the most important is precedence<? proc
(define (smallest-op expr)
  (accumulate (lambda (a b)
		(min-precedence a b))
	      'maxop
	      expr))
;; ---------------------------------------------------------------------------------------

(define (accumulate op init seq)
   (if (null? seq)
       init
       (op (car seq) (accumulate op init (cdr seq)))))

;; We need a predicate operator?
(define *precedence-table* '((maxop . 10000) (minop . -10000) (+ . 0) (* . 1)))

(define (operator? x)
  (define (loop op-pair)
    (cond
     ((null? op-pair) #f)
     ((eq? x (caar op-pair)) #t)
     (else (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (min-precedence a b)
  (if (precedence<? a b)
      a
      b))
;; -----------------------------------------------------------------------------------------
(define (precedence<? a b)
  (< (precedence a) (precedence b)))


(define (precedence<? a b)
  (cond
   ((and (operator? a) (operator? b)) (< (precedence a) (precedence b)))
   ((operator? a) #t)
   ((operator? b) #f)
   (else #f)))
;; This version of precedence<? is correct, cause it consider the symbol that is not operator
;; ------------------------------------------------------------------------------------------
(define (precedence op)
  (define (loop op-pair)
    (cond
     ((null? op-pair) (error "Operator not defined -- PRECEDENCE:" op))
     ((eq? op (caar op-pair)) (cdar op-pair))
     (else
      (loop (cdr op-pair)))))
  (loop *precedence-table*))

(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
    (if (singleton? a)
	(car a)
	a)))

(define (prefix sym seq)
  (if (or (null? seq) (eq? sym (car seq)))
      '()
      (cons (car seq) (prefix sym (cdr seq)))))   ;; 中序表达式

(define (addend expr)
  (let ((a (prefix '+ expr)))
    (if (singleton? a)
	(car a)
	a)))

(define (make-sum a1 a2)
  (cond
   ((=number? a1 0) a2)
   ((=number? a2 0) a1)
   ((and (number? a1) (number? a2)) (+ a1 a2))
   (else (list a1 '+ a2))))

(define (multiplier expr)
  (let ((m (prefix '* expr)))
    (if (singleton? m)
	(car m)
	m)))

(define (make-product m1 m2)
  (cond
   ((=number? m1 1) m2)
   ((=number? m2 1) m1)
   ((or (=number? m1 0) (=number? m2 0)) 0)
   ((and (number? m1) (number? m2)) (* m1 m2))
   (else
    (list m1 '* m2))))

(define (singleton? x)
  (= (length x) 1))

;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------
;; 集合的表示 1.集合作为未排序的表
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((equal? (car set) x) #t)
   (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; 实现intersection-set时可以采用递归策略：如果我们已知如何做出set2与set1的cdr的交集，那么就只需要确定是否应将set1的car包含到结果之中了，而这依赖于（car set1）是否也在set2里。
(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
   (else (intersection-set (cdr set1) set2))))

;; 在设计一种表示形式时，有一件必须关注的事情是效率问题。为考虑这一问题，就需要考虑上面定义的各集合操作所需要的工作步数。因为它们都使用了element-of-set?，这一操作的速度对整个集合的实现效率将有
;; 重大影响。在上面这个实现里，为了检查某个对象是否为一个集合的成员，element-of-set?可能不得不扫描整个集合(最坏情况是这一元素恰好不在集合里)。因此，如果集合有n个元素，element-of-set?就可能
;; 需要n步才能完成。这样，这一操作所需的步数交为O(n)的速度增长。adjoin-set使用了这个操作，因此它所需的步数也以O(n)的速度增长。而对于intersection-set，它需要对set1的每个元素做一次element-of-set
;; 检查，因此所无原则步数将按所涉及的两个集合的大小之乘积增长，或者说，在两个集合大小都为n时，就是O(n^2)。union-set的情况也是如此。
;; practice 2.59 union-set
(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
   (else (cons (car set1) (union-set (cdr set1) set2)))))
;; practice 2.60
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((equal? x (car set)) #t)
   (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (remove-set-element x set)
  (define (remove-set-element-iter acc rest)
    (cond
     ((null? rest) acc)
     ((equal? x (car rest)) (append acc (cdr rest)))
     (else (remove-set-element-iter (adjoin-set (car rest) acc) (cdr rest)))))
  (remove-set-element-iter '() set))

(define (intersection-set set1 set2)
  (cond
   ((or (null? set1) (null? set2)) '())
   ((element-of-set? (car set1) set2) (cons (car set1)
					    (intersection-set (cdr set1) (remove-set-element (car set1) set2))))
   (else (intersection-set (cdr set1) set2))))
;; -------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; 集合作为排序的表
;; 加带集合操作的一种方式是改变表示方式，使集合元素在表中按上升序排列。为此，我们就需要有某种方式来比较两个元素，以便确定哪个元素更大一些。例如，我们可以按字典做符号的比较；或者同意采用某种方式为每个
;; 对象关联一个唯一的数，在比较元素的时候就比较与之对应的数。为了简化这里的讨论，我们将仅仅考虑集合元素是数值的情况，这样就可以用> <做元素的比较了。下面将数的集合表示为元素按照上升顺序排列的表。
(define element-of-set?
  (lambda (x set)
    (cond
     ((null? set) #f)
     ((= x (car set)) #t)
     ((< x (car set)) #f)
     (else (element-of-set? x (cdr set))))))
;; 从操作element-of-set?可以看到采用有序表示的一个优势：为了检查一个项目的存在性，现在不必扫描整个表了。如果检查中遇到的某个元素大于当时要找的东西，那么就可以断定这个东西根本不在表里。这样能节约多
;; 少步数？在最坏情况下，我们要找的项目可能是集合中最大的元素，此时所需步数与采用未排序的表示时一样。但在另一方面，如果需要查找许多不同大小的项，我们总可以期望，有此时候这一检索可在接近表开始处的某一
;; 点停止，也有些时候需要检查表的一大部分。平均而言，我们可以期望需要检查表中的一半元素，这样，平均所无需的步数就是大约n/2。这仍然是O(n)的增长速度，但与前一实现相比，平均来说，现在我们节约了大约一半
;; 的步数
;; 操作intersection-set的加整党情况更使人印象深刻。在未排序的表示方式里，这一操作需要O(n^2)的步数，因为对set1的每个元素，我们都需要对set2做一次完全的扫描。对于排序表示则可以有一种更聪明的方法。
;; 我们在开始时比较两个集合的起始元素，例如X1和X2。如果X1与X2相等，那么这样就得到了交集的一个元素，而交集的其他元素就是这两个集合的cdr的交集。如果此时的情况是X1小于X2，由于X2是集合set2的最小元素，
;; 我们立即可以断定X1不会出现在集合set2里的任何地方，因此它不应该在交集里。这样，这两集合的交集就等于集合set2与set1的cdr的交集。与此类似，如果X2小于X1，那么两集合的交集就等于集合set1与set2的cdr
;; 的交集。与此类似，如果X2小于X1，那么两集合的交集就等于集合set1与set2的cdr的交集。
(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond
	 ((equal? x1 x2) (cons x1 (intersection-set-list (cdr set1) (cdr set2))))
	 ((< x1 x2) (intersection-set-list (cdr set1) set2))
	 (else
	  (intersection-set-list set1 (cdr set2)))))))
;; 在上面的这个程序里，没有使用element-of-set?这个函数
;; 为了估计出这一过程所需的步数，请注意，在每个步骤中，我们都将求交集问题归结到更小集合的交集计算问题－－去掉了set1和set2之一或者是两者的第一个元素。这样，所需步数至多等于set1与set2的大小之和。而不
;; 像在未排序表示中它们的乘积。这也就是O(n)的增长速度，这一加速非常明显，即使对中等大小的集合也是如此。
;;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; practice 2.61 给出采用排序表示时adjoin-set的实现。通过类似element-of-set?的方式说明，可以如何利用排序的优势得到一个过程，其平均所需的步数是采用未排序表示时的一半
(define (adjoin-set-list x set)
  (define (adjoin-set-iter result set-it)
    (if (equal? x (car set-it))
	(append result set-it)
	(if (< x (car set-it))
	    (append result (cons x set-it))
	    (adjoin-set-iter (append result (list (car set-it))) (cdr set-it)))))
  (if (null? set)
      (cons x set)
      (adjoin-set-iter '() set)))

(define (adjoin-set x set)
  (define (adjoin-set-iter result set-it)
    (cond
     ((null? set-it) (append result (list x)))
     ((equal? x (car set-it)) (append result set-it))
     ((< x (car set-it)) (append result (cons x set-it)))
     (else
      (adjoin-set-iter (append result (list (car set-it))) (cdr set-it)))))
  (if (null? set)
      (cons x set)
      (adjoin-set-iter '() set)))    ;; 这个实现考虑了边界条件，即元素将来要插入到第一个或最后一个的情况。
;; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; 上面的两个实现是迭代式的，也是非常笨的。
(define (adjoin-set x set)
  (cond
   ((null? set) (cons x set))
   ((equal? x (car set)) set)
   ((< x (car set)) (cons x set))
   (else
    (cons (car set) (adjoin-set x (cdr set))))))
;; -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; (define adjoin-set
;;   (lambda (x set)
;;     (call-with-current-continuation (lambda (skip)
;; 				      (letrec
;; 					  ((adjoin-iter (lambda (element result set-it)
;; 							  (if (equal? element (car set-it))
;; 							      (skip set)
;; 							      (if (< element (car set-it))
;; 								  (skip (append result (cons element set-it)))
;; 								  (adjoin-iter element (append result (list (car set-it))) (cdr set-it)))))))
;; 					(adjoin-iter x '() set))))))    ;; The same as the first adjoin-set
;;------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (union-set-list set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   (else
   (let ((x1 (car set1))
	 (x2 (car set2)))
     (if (< x1 x2)
	 (cons x1 (cons x2 (union-set-list (cdr set1) (cdr set2))))
	 (if (> x1 x2)
	     (cons x2 (cons x1 (union-set-list (cdr set1) (cdr set2))))
	     (cons x1 (union-set-list (cdr set1) (cdr set2)))))))))
;; 集合作为二叉树
;; 如果将集合元素安排成一棵树的形式，我们还可以得到比排序表表示更好的结果。树中每个结点保存集合中的一个元素， 称为该结点的“数据项”，它还链接到另外的两个结点（可能为空）。其中“左边”的链接所指向的所有元素
;; 均小于本结点的元素，而“右边”链接到的元素都大于本结点里的元素。树表示方法的优点在于：假定我们希望检查某个数X是否在一个集合里，那么就可以用X与树顶结点的数据项比较。如果X小于它，我们就知道现在只需要搜索
;; 左子树；如果X比较大，那就只需要搜索右子树。在这样做时，如果该树是“平衡的”，也就是说，每棵子树大约是整个树的一半大，那么，这样经过一步，我们就将需要搜索规模为N的树的问题，归约为搜索规模为N/2的树的问题
;; 由于经过每个步骤能够使唤权的大小减小一半，我们可以期望搜索规模为N的树的计算步数以O(log n)的速度增长。在集合很大时，相对于原来的表示，现在的操作速度将明显快得多。

;; 我们可以用表来表示树，将结点表示为三个元素的表：本结点中的数据项，其左子树和右子树。以空表作为左子树或者右子树，就表示没有子树连接在那里。我们可以用下面过程描述这种表示
;; ========================================================================================================================================================================
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))
;; 先设计顶层的数据结构，再细化每个过程的实现。定义最基本的构造函数和选择函数，再依次的细化。
;; 现在我们就可以采用上面描述的方式实现过程element-of-set?了
(define (element-of-set? x set)
  (cond
   ((null? set) #f)
   ((= x (entry set)) #t)
   ((< x (entry set)) (element-of-set? x (left-branch set)))
   (else
    (element-of-set? x (right-branch set)))))
;; 向集合里加入一个项的实现方式与此类似，也需要O(log n)步。为了加入元素X，我们需要将X与结点数据项比较，以便确定X应该加入右子树还是左子树中。在将X加入适当的分支之后，我们将新构造出的这个分支，原来的数据项
;; 与另一分支放到一起。如果X等于这个数据项，那么就直接返回这个结点。如果需要将X加入一个空子树，那么我们就生成一棵树，以X作为数据项，并让它具有空的左右分支。
(define (adjoin-set x set)
  (cond
   ((null? set) (make-tree x '() '()))
   ((= x (entry set)) set)
   ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
   (else
    (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set))))))

;; practice 2.63  The following two functions turn tree to list
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list wtree result-list)
    (if (null? wtree)
	result-list
	(copy-to-list (left-branch wtree)
		      (cons (entry wtree)
			    (copy-to-list (right-branch wtree) result-list)))))
  (copy-to-list tree '()))

(define tree (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))
;; The following is the practice of tree-list-2
(define (tree->list-2 tree)
  (define (copy-to-list wtree result-list)
    (if (null? wtree)
	result-list
	(copy-to-list (left-branch wtree)
		      (cons (entry wtree)
			    (copy-to-list (right-branch wtree) result-list)))))
  (copy-to-list tree '()))
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))
;; practice 2.65
(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list-2 set1) (tree->list-2 set2))))
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; 集合与信息检索
;; 我们考察了用表表示集合的各种选择，并看到了数据对象表示的选择可能如何深刻地影响到使用数据的程序的性能。关注集合的另一个原因是，这里所讨论的技术在涉及信息检索的各种应用中将会一再地出现
(define (lookup given-key set-of-records)
  (cond
   ((null? set-of-records) #f)
   ((= given-key (key (entry set-of-records))) (entry (set-of-records)))
   ((< given-key (key (entry set-of-records))) (lookup given-key (left-branch set-of-records)))
   (else (lookup given-key (right-branch set-of-records)))))
;; ============================================================================================================================================================================
;; 生成哈夫曼树的算法实际上十分简单，其想法就是设法安排这棵树，使唤得那些带有最低频度的牌号出现在离树根最远的地方。这一构造过程从叶结点的集合开始，这种结点中包含各个符号和它们的频度，这就是开始构造编码的初始数据。
;; 现在要找出两个具有最低权重的叶，并归并它们，产生出一个以这两个结点为左右分支的结点。新结点的权重就是那两个结点的权重之和。现在我们从原来集合里删除前面的两个叶结点，并用这一新结点代替它们。随后继续这一过程，在
;; 其中的每一步都归并两个具有最小权重的结点，将它们从集合中删除，并用一个以这两个结点作为左右分支的新结点取而代之。当集合中只剩下一个结点时，这一过程终止，而这个结点就是树根。
;; Huffman树的表示 设计树的表示方式是很重要的，需要认真思考的事情，然后就是围绕表示方式，设计相关的构造函数和选择函数
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (equal? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;; 一棵一般的树也是一个表，其中包含一个左分支、一个右分支、一个符号集合和一个权重。符号集合就是符号的表，这里没有用更复杂的集合表示。在归并两个结点做出一棵树时，树的权重也就是这两个结点的权重之和，其符号集就是两个
;; 结点的符号集的并集。
(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
;; 在对树叶或者一般树调用过程symbols和weight时，它们需要做的事情有一点不同。这些不过是通用型过程（可以处理多于一种数据的过程）的简单实例。
;; 解码过程	  
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond
   ((= bit 0) (left-branch branch))
   ((= bit 1) (right-branch branch))
   (else (error "bad bit"))))
;; 带权重元素的集合
(define (adjoin-set x set)
  (cond
   ((null? set) (list x))
   ((< (weight x) (weight (car set))) (cons x set))
   (else (cons (car set)
	       (adjoin-set x (cdr set))))))

;; 下面过程以一个符号－权重对偶的表为参数，例如（（A 4）（B2）（C 1）（D 1））,它构造出树叶的初始排序集合，以便HUFFMAN算法能够去做归并
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))
;; practice 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; practice 2.68 encode过程，以一个消息和一棵树为参数，产生出被编码消息所对应的二进制位的表
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol char tree)
  (if (leaf? tree)
      '()
      (if (element-of-set? char (symbols tree))
	  (let ((left (left-branch tree))
		(right (right-branch tree)))
	    (if (element-of-set? char (symbols left))
		(cons 0 (encode-symbol char left))
		(cons 1 (encode-symbol char right))))
	  (error "not contain the char"))))
;;--------------------------------------------------------------------------------------------------------------------------------------------------------
;; practice 2.69下面过程以一个符号－频度对偶表为参数（其中没有任何符号出现在多于一个对偶中），并根据HUFFMAN算法生成出HUFFMAN编码树。
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs) (car (cdr pairs))) (cdr (cdr pairs))))))

;; The result of practice 2.70 is 84; and if we use fixed-length encoding, the result will b 36*3=108. (length (encode message (generate-huffman-tree pairs)))
;; 在哈夫曼编码的这个例子中，首先设计哈夫曼树的表示方式，再依次设计相关的选择函数。我们可以看到make-code-tree的实现很简单，make-leaf的实现也很简单，其它相关的函数的实现都不难，然而
;; 有了这些基本的组合，我们逐步得到了相对难的哈夫曼树的编码过程，解码过程，以及构造哈夫曼树的方式。感觉很自然的一个过程，然而，让我自己想，估计是很验证的。所以，组合，设计真的很重要！
;; 需要不断的回味这个渐渐明朗的过程是如何得到的。

;; practice 2.71 假定我们有一棵N个符号的字母表的HUFFMAN树，其中各符号的相对频度分别是1 2 4…，2^(n-1)。请对N＝5和N＝10勾勒出有关的树的样子。对于这样的树，编码出现最频繁的符号用多少个二进制位？最不频繁的呢？
;; A:编码最频繁的符号用1个二进制位，最不频繁的用N-1个二进制位。（根据HUFFMAN树的生成过程，我们可以得到树的结构大概是什么样子的）
;; 2.72 网上亦无答案，留作后来思考吧
;; ==============================================================================================================================================================================
;; 2.4抽象数据的多重表示
;; 我们已经介绍过数据抽象，这是一种构造系统的方法学，采用这种方法，将使一个程序中的大部分描述能与这一程序所操作的数据对象的具体表示的选择无关。如，我们曾经看到如何将一个使用有理数的程序的设计与有理数的实现我工作相互
;; 分享，具体实现中采用的是计算机语言所提供的构造复合数据的基本机制。这里的关键性思想就是构筑起一道抽象屏障－－对于上面情况，也就是有理数的选择函数和构造函数(make-rat, number, demon)，它能将有理数的使用方式与其
;; 借助于表结构的具体表示形式隔离开。与此类似的抽象屏障，也把执行有理数算术的过程(add-rat, sub-rat, mul-rat,div-rat)与使用有理数的“高层”过程隔离开。这样做出的程序所具有的结构更好。
;; 数据抽象屏障是控制复杂性的强有力工具。通过对数据对象基础表示的屏蔽，我们就可以将设计一个大程序的任务，分割为一组可以分别处理的较小任务。但是，这种类型的数据抽象还不够强大有力，因为在这里说数据对象的“基础表示”并不
;; 一定总有意义。从一个角度看，对于一个数据对象也可能存在多种有用的表示方式，而且我们也可能希望所设计的系统能处理多种表示形式。例如，得数就可以表示为两种几乎等价的形式：直角坐标形式（实部和虚部）和极坐标形式（模和角）
;; 有时采用直角坐标形式更合适，有时极坐标形式更方便。的确，我们完全可能设想一个系统，其中的复数同时采用了两种表示形式，而其中的过程可以对具有任意表示形式的复数工作。更重要的是，一个系统的程序设计常常是由许多人通过一
;; 个相当长时期的工作完成的。系统的需求也在承受着时间而不断变化。在这样一种环境里，要求每个人都在数据表示的选择上达成一致是根本就不可能的事情。因此，除了需要将表示与使用相隔离的数据抽象屏障之外，我们还需要有抽象屏障
;; 去隔离互不相同的设计选择，以便允许不同的设计选择在同一个程序里共存。进一步说，由于大型程序常常是通过组合起一些现存模块构造起来的，而这些系统又是独立设计的，我们也需要一些方法，使程序员可能逐步地将许多模块结合成一
;; 个大型系统，而不必去重新设计或者重新实现这些模块。在这一节里，我们将学习如何去处理数据，使它们可能在一个程序的不同部分中采用不同的表示方式。这就需要我们去构造通用型过程－－也就是那种可以在不止一种数据表示上操作的
;; 过程。这里构造通用型过程所采用的主要技术，是让它们在带有类型标志的数据对象上工作。也就是说，让这些数据对象包含着它们应该如何处理的明确信息。我们还要讨论数据导向的程序设计，这是一种用于构造采用了通用型操作的系统有
;; 力而且方便的技术。关键词：（1）通用型过程 （2）数据对象带有类型标志 （3）数据导向的程序设计技术

;; 我们将从简单的复数实例开始，看看如何采用类型标志和数据导向的风格，为复数分别设计出直角坐标表示和极坐标表示，而又维持一种抽象的“复数”数据对象的概念。做到这一点的方式就是定义基于通用型选择函数定义复数的算术运算：
;; (add-complex, sub-complex, mul-complex, div-complex)这些函数的实现，是在通用型选择函数的基础之上实现的。而通用型选择函数就需要去处理复数的多种表示方式。通用型选择函数向add-complex屏蔽了具体的复数表示
;; 方式。但要求通用型选择函数需要理解不同的复数的表示是如何实现的（有时，可能是不可能的，因为这个组件可能是第三方提供的）

;; 2.4.1 复数的表示：（1）极坐标表示； （2）直角坐标表示
;; 我们假定所有复数运算的实现都基于如下四个选择函数：real-part, imag-part, magnitude和angle;还要假定有两个构造复数的过程:make-from-real-imag返回一个采用实部和虚部描述的复数，make-from-mag-ang返加一个
;; 采用模和幅角描述的复数。这些过程的性质是，对于任何得数Z，下面两者：(make-from-real-imag (real-part z) (imag-part z))和(make-from-mag-ang (magnitude z) (angle z))产生出的复数都等于z.利用这些构造
;; 函数和选择函数，我们就可以实现复数算术了，其中使用这些构造函数和选择函数所刻画的“抽象数据”，就像前面针对有理数所做的那样。复数的加法采用实部和虚部的方式描述，而乘法和除法采用模和幅角的方式描述
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))
;; 为完成这一复数包，我们必须选择一种表示方式，而且必须基于基本的数值和基本表结构，基于它们实现各个构造函数和选择函数。现在假定有两个程序员，Ben和Alyssa，他们正在分别独立地设计这一得数系统的具体表示形式。Ben选择了
;; 复数的直角坐标形式，采用这一选择，Ben给出了下面的的选择函数和构造函数的实现：
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z) (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
;; Alyssa选择了复数的极坐标形式，她的表示就是：
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
	(atan y x)))
(define (make-from-mag-ang r a) (cons r a))
;; 数据抽象的规则保证了add-complex, sub-complex, mul-complex和div-complex的同一套实现对于Ben的表示或者Alyssa的表示都能正常工作
;; =================对于不同的表示方式，定义了两套不同的选择函数和构造函数，她们在各自独立的系统里使用，没有问题====================================================================================
;; 2.4.2 带标志数据
;; 认识数据抽象的一种方式是将其看作“最小允诺原则”的一个应用。由选择函数和构造函数形成的抽象屏障，使我们可以把为自己所用数据对象选择具体表示形式的事情，尽量向后推，而且还能保持系统设计的最大灵活性。最小允诺原则还可以
;; 推进到更极端的情况。如果我们需要的话，那么还可以在设计完成选择函数和构造函数，并决定了同时使用Ben的表示和Alyssa的表示之后，仍然维持所用表示方式的不确定性。如果要在同一个系统里包含这两种不同表示形式，那么就需要有
;; 一种方式，将极坐标形式的数据与直角坐标形式的数据区分开。否则的话，如果现在要找出对偶（3，4）的magnitude，我们就无法知道答案是5还是3.完成这种区分的一种方式，就是在每个复数里包含一个类型标志部分－用符号rectangular
;; 或者polar。此后如果我们需要操作一个复数，借助于这个标志就可以确定应该使用的选择函数了。
;; 为了能对带标志数据进行各种操作，我们将假定有过程type-tag和contents，它们分别从数据对象中提取出类型标志和实际内容（对于复数的情况，其中的极坐标或者直角坐标）。还要假定有一个过程attach-tag，它以一个标志和实际内容
;; 为参数，生成出一个带标志的数据对象。实现这些的直接方式就是采用普通的表结构
(define (attach-tag type-tag contents) (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum --TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum --contents" datum)))
;; 利用这些过程，我们就可以定义出谓词rectangular?和polar?，它们分别辨识直角坐标和极坐标的复数：
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))
;; 有了类型标志之后，Ben和Alyssa现在就可以修改自己的代码，使唤它们的两种不同表示能共存于同一个系统中了。当Ben构造一个复数时，总为它加上标志，说明采用的是直角坐标；而当Alyssa构造复数时，总将其标志设置为极坐标。此外
;; Ben和Alyssa还必须保证它们所用的过程名并不冲突。保证这一点的一种方式是，Ben总为在他的表示上操作的过程名字加上后缀rectangular,而Alyssa为她的过程名加上后缀polar
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
	(real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y) (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))
;; 同样有修改后的极坐标表示
(define (real-part-polar z) (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
								 (atan y x))))
(define (make-from-mag-ang-polar r a) (attach-tag 'polar (cons r a)))
;; 每个通用型选择函数都需要实现为这样的过程，它首先检查参数的标志，而后去调用处理该类数据的适当过程。例如，为了得到一个复数的实部，real-part需要通过检查，设法确定是去使用Ben的real-part-rectangular，还是去使用Alyssa
;; 的real-part-polar。在这两种情况下，我们都用contents提取出原始的无标志数据，并将它送给所需要的直角坐标过程或者极坐标过程。
(define (real-part z)
  (cond
   ((rectangular? z) (real-part-rectangular (contents z)))
   ((polar? z) (real-part-polar (contents z)))
   (else (error "Unknow type" z))))

;; (define (imag-part z) ...)
;; (define (magnitude z) ...)
;; (define (angle z) ...)
;; 在实现复数算术运算时，我们仍然可以采用同样的过程add-complex, sub-complex, mul-complex和div-complex，因为它们所调用的选择函数现在都是通用型的，对任何表示都能工作。
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))
;; 这样得到的复数x繁育所具有的结构如P122图，这一系统已经分解成为三个相对独立的部分：复数算术运算，Alyssa的极坐标实现和Ben的直角坐标实现。极坐标或直角坐标的实现可以是Ben和Alyssa独立工作写出的东西，这两部分又被第三个程序员
;; 作为基础表示，用于在抽象构造函数和选择函数界面之上实现各种复数算术过程。
;; 因为每个数据对象都以其类型作为标志，选择函数就能在不同的数据上以一种通用的方式操作。也就是说，每个选择函数的定义行为依赖于它操作其上的特定的数据类型。请注意这里建立不同表示之间的界面的一般性机制：在一种给定的表示实现中
;; （例如Alyssa的极坐标包），复数是一种无类型的对偶（模，幅角），当通用型选择函数对一个polar类型的复数进行操作时，它会剥去标志并将基相应内容传给Alyssa的代码。与此对应，当Alyssa去构造一个供一般性使用的复数时，她也为其加上
;; 类型标志，使这个数据对象可以为高层过程所识别。在将数据对象从一个层次传递到另一个层次的过程中，这种剥去和加上标志的规范方式可以成为一种重要的组织策略。
;; 2.4.3数据导向的程序设计和可加性
;; 检查一个数据项的类型，并据此去调用某个适当过程称为基于类型的分派。在系统设计中，这是一种获得模块性的强有力策略。而在另一方面，像上面提到的分派有两个显著的弱点。（1）第一个弱点是，其中的这些通用型界面过程(real-part, ...)
;; 必须知道所有的不同表示。举例来说，假定现在希望能为前面的复数系统增加另一种表示，我们就必须将这一新表示方式标识为一种新类型，而且要在每个通用界面过程里增加一个子句，检查这一新类型，并对这种表示形式使用适当的选择函数（也就是说
;; 通用型界面过程要做的事，当表示方式增加时，影响通用型选择函数的实现）（2）这一技术还有另一个弱点。即使这些独立的表示形式可以分别设计，我们也必须保证在整个系统里不存在两个名字相同的过程。正因为这一原因，Ben和Alyssa必须去修改
;; 原来给出的那些过程的名字。们于这两个弱点之下的基础问题是，上面这种实现通用型界面的技术不具有可加性。在每次增一种新表示形式时，实现通用选择函数的人都必须修改他们的过程，而那些做独立表示的界面的人也必须修改其代码，以避免名字冲突
;; 问题。在做这些事情时，所有修改都必须直接对代码去做，而且必须准确无误，这当然会带来极大的不便，而且还很容易引进错误。
;; 现在我们需要的是一种能够将系统设计进一步模块化的方法。一种称为数据导向的程序设计的编程技术提供了这种能力。我们将操作与数据的类型组织为一个表格。这样，组织为表格之后，相关的操作不再散布在各个通用的选择函数中，而是集中在表格之中
;; 之后，再对表格做处理。数据导向的程序设计就是一种使程序能直接利用这种表格工作的程序设计技术。在我们前面的实现里，是采用一组过程做为复数算术与两个表格包之间的界面（通用选择函数，构造函数），并让这些过程中的每一个去做基于类型的的
;; 显式分派。下面我们要把这一界面实现为一个过程，由它用操作名和参数类型的组合到表格中查找，以便找出应该调用的适当过程，并将这一过程应用于参数的内容。如果能做到这些，再把一种新的表示包加入系统里，我们就不需要任何现存的过程，而只要
;; 在这个表格里添加一些新的项目即可。
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;; 这里的所有内部过程，与Ben在自己独立工作中写出的过程完全一样，在将它们与系统的其他部分建立联系时，也不需要做任何修改。进一步说，由于这些过程定义都是上述安装过程内部的东西，ben完全不必担心它们的名字会与直角坐标程序包外的其他过程
;; 的名字相互冲突。为了能与系统里的其他部分建立起联系，ben将他的real-part过程安装在操作名字real-part和类型（rectangular）之下，其他选择函数的情况也都与此类似。这一界面还定义了提供给外部系统的构造函数。
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;; 虽然ben和alyssa两个人仍然使用着他们原来的过程定义，这些过程也有着同样的名字（real-part），但对于其他过程而言，这些定义都是内部的（他们都是函数，估计系统会给内部函数分配不同的地址），所以在这里不会出现名字冲突问题。
;; 复数算术的选择函数通过一个通用的名为apply-generic的“操作”过程，访问有关表格，这个过程将通用型操作应用于一些参数。apply-generic在表格中用操作名和参数类型查找，如果找到，就去应用查找中得到的过程。
(define (apply-generic op . args)            ;; 需要注意一点细节问题，这里使用了带点尾部记法，所以，对于args参数，将会是以列表的形式，整体传递给apply-generic函数
  (let ((type-tags (map type-tag args)))     ;; 比如给的参数args是 (rectangular (3 4)),但是，由于采用的是带点尾部记法，所以args在apply-generic里就是((rectangular (3 4)))，所以才有这里的map写法。
    (let ((proc (get op type-tags)))         ;; 提醒我们，细节的问题要注意 map操作的最终结果是一个表，不是原子数据
      (if proc
	  (apply proc (map contents args))
	  (error "No method for these types" (list op type-tags))))))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
;; 使用哪个软件包，哪个软件包有自己的一套应用，复数的表示，为自动被相应的软件包打上标志。
;; ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;; practice 2.73 在2.3.2节描述了一个执行符号求导的程序：
(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   ((sum? exp) (make-sum (deriv (addend exp) var)
			 (deriv (augend exp) var)))
   ((product? exp) (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var ))
			     (make-product (deriv (multiplier exp) var) (multiplicand exp))))
   (else (error "Unknow expression type --DERIV" exp))))
;; 可以认为，这个程序是在执行一种基于被求导表达式类型的分派工作。在这里，数据的“类型标志”就是代数运算符（例如＋），需要执行的操作是deriv。我们也可以将这一程序变换到数据导向的风格，将基本求导过程重新写成：
(define (deriv exp var)
  (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   (else
    ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;; (a) 上面的程序根据被求表达式的类型做分派工作！谓词number?和same-variable?只仅仅是谓词，不是具体的操作符，所以没有办法加入数据导向分派中。
;; (b) 请写出针对和式与积式的求导过程，并把它们安装到表格里
(define (make-sum a b)
  (cond
   ((=number? a 0) b)
   ((=number? b 0) a)
   ((and (number? a) (number? b)) (+ a b))
   (else
    (list '+ a b))))
(define (make-product a b)
  (cond
   ((or (=number? a 0) (=number? b 0)) 0)
   ((=number? a 1) b)
   ((=number? b 1) a)
   ((and (number? a) (number? b)) (* a b))
   (else (list '* a b))))

(define (=number? a b)
  (if (and (number? a) (number? b))
      (eq? a b)
      #f))
      
(put 'deriv '+ (lambda (x y) (make-sum (deriv (addend x) y)
				       (deriv (augend x) y))))
