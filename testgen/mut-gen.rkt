#lang rosette/safe

(require rosette/lib/match
         "../ast/spec.rkt"
         "../ast/syntax.rkt")

(provide (all-defined-out))

(define (mutate-js-expr js-expr symbol-list inputs prev-depth)
  (define (remove-prev arg)
    (match arg
      [(list 'prev arg1) (remove-prev arg1)]
      [_ arg]))
  (define (prev-num arg)
    (match arg
      [(list 'prev arg1) (+ 1 (prev-num arg1))]
      [_ 0]))
  (define (make-prev arg num)
    (if (= num 0)
        arg
        (list 'prev (make-prev arg (- num 1)))))
  (define (mutate-arguments js-expr)
    (define (iter args)
      (match args
        [(list) '()]
        [(cons arg rest)
         (append (map (lambda (x) (cons x rest))
                      (filter (lambda (x) (not (eq? x arg))) symbol-list))
                 (map (lambda (x) (cons arg x)) (iter rest)))]))
    (match js-expr
      [(cons func args)
       (map (lambda (x) (cons func x)) (iter args))]
      [(list) '()]
      [sym (filter (lambda (x) (not (eq? x sym))) symbol-list)]))
  (define (mutate-input js-expr)
    (define (iter js-expr)
      (define (iter-one arg cur-num ori-num rest)
        (cond [(> cur-num prev-depth) '()]
              [(= cur-num ori-num) (iter-one arg (+ 1 cur-num) ori-num rest)]
              [else (cons (cons (make-prev arg cur-num) rest) (iter-one arg (+ 1 cur-num) ori-num rest))]))
            
      (match js-expr
        [(list) '()]
        [(cons arg rest)
         (let ([non-prev-arg (remove-prev arg)]
               [prev-num-arg (prev-num arg)])
           (if (memq non-prev-arg inputs)
               (append (iter-one non-prev-arg 0 prev-num-arg rest)
                       (map (lambda (x) (cons arg x)) (iter rest)))
               (map (lambda (x) (cons arg x)) (iter rest))))]))
    (match js-expr
      [(cons func args) (iter js-expr)]
      [(list) '()]
      [sym (map car (iter (list sym)))]))
               
  (append (mutate-arguments js-expr)
          (mutate-input js-expr)))

(define (mutate-inst inst symbol-list const-list inputs prev-depth)
  ((cond [(if? inst) mutate-if]
         [(if-multi? inst) mutate-if-multi]
         [(if-else-multi? inst) mutate-if-else-multi]
         [(case-multi? inst) mutate-case-multi]
         [(return? inst) mutate-return]
         [(if-else? inst) mutate-if-else]
         [(bind? inst) mutate-bind]
         [(begin? inst) mutate-begin]
         [(split? inst) mutate-split]
         [(return-empty? inst) (lambda lst '())]
         [else 'not-implemented]) inst symbol-list const-list inputs prev-depth))

(define (mutate-inst-imp inst const-list inputs prev-depth)
  ((cond [(if? inst) mutate-if-imp]
         [(if-else? inst) mutate-if-else-imp]
         [(if-multi? inst) mutate-if-multi-imp]
         [(if-else-multi? inst) mutate-if-else-multi-imp]
         [(case-multi? inst) mutate-case-multi-imp]
         [(bind? inst) mutate-bind-imp]
         [(empty-stream? inst) mutate-empty-stream]
         [(new-stream? inst) mutate-new-stream]
         [(new-stream-initial? inst) mutate-new-stream-initial]
         [(new-stream-seed? inst) mutate-new-stream-seed]
         [else 'not-implemented]) inst const-list inputs prev-depth))

(define (mutate-empty-stream inst const-list inputs prev-depth)
  (list))

(define (mutate-new-stream inst const-list inputs prev-depth)
  (match inst
    [(list 'new-stream body)
     (define mutated-body (map (lambda (x) (list 'new-stream x)) (mutate-body body const-list inputs prev-depth)))
     (define to-empty (list (list 'empty-stream)))
     (append
      mutated-body
      to-empty)]))

(define (mutate-new-stream-initial inst const-list inputs prev-depth)
  (match inst
    [(list 'new-stream-initial body js-expr)
     (define mutated-body (map (lambda (x) (list 'new-stream-initial x js-expr)) (mutate-body body const-list inputs prev-depth)))
     (define mutate-initial (map (lambda (x) (list 'new-stream-initial body x)) (mutate-js-expr js-expr const-list inputs prev-depth)))
     (define to-seed (list (list 'new-stream-seed body js-expr)))
     (define to-empty (list (list 'empty-stream)))
     (append
      mutated-body
      mutate-initial
      to-seed
      to-empty)]))

(define (mutate-new-stream-seed inst const-list inputs prev-depth)
  (match inst
    [(list 'new-stream-seed body js-expr)
     (define mutated-body (map (lambda (x) (list 'new-stream-seed x js-expr)) (mutate-body body const-list inputs prev-depth)))
     (define mutate-seed (map (lambda (x) (list 'new-stream-seed body x)) (mutate-js-expr js-expr const-list inputs prev-depth)))
     (define to-initial (list (list 'new-stream-initial body js-expr)))
     (define to-empty (list (list 'empty-stream)))
     (append
      mutated-body
      mutate-seed
      to-initial
      to-empty)]))

(define (mutate-split inst symbol-list const-list inputs prev-depth)
  (define (mutate-bindings bindings)
    (match bindings
      [(list) 'should-not-happen]
      [(list (list name js-expr)) (map (lambda (x) (list (list name x))) (mutate-js-expr js-expr symbol-list inputs prev-depth))]
      [(cons (list name js-expr) rest)
       (append (map (lambda (x) (cons (list name x) rest)) (mutate-js-expr js-expr symbol-list inputs prev-depth))
               (map (lambda (x) (cons (list name js-expr) x)) (mutate-bindings rest)))]))
  (match inst
    [(list 'split (list (list)) body) (map (lambda (x) (list 'split (list (list)) x)) (mutate-inst-imp body const-list inputs prev-depth))]
    [(list 'split bindings body)
     (append (map (lambda (x) (list 'split x body)) (mutate-bindings bindings))
             (map (lambda (x) (list 'split bindings x)) (mutate-inst-imp body (append (map car bindings) const-list) inputs prev-depth)))]))


(define (mutate-if inst symbol-list const-list inputs prev-depth [imp #f])
  (match inst
    [(list 'if js-expr branch)
     (define mutate-if-arg
       (map (lambda (x) (list 'if x branch)) (mutate-js-expr js-expr (if imp const-list symbol-list) inputs prev-depth)))
     (define remove-if (list branch))
     (define flip-if (list (list 'if (list 'not js-expr) branch)))
     (define mutate-branch (map (lambda (x) (list 'if js-expr x))
                                (if imp
                                    (mutate-inst-imp branch const-list inputs prev-depth)
                                    (mutate-inst branch symbol-list const-list inputs prev-depth))))
     (append mutate-if-arg
             remove-if
             flip-if
             mutate-branch)]))

(define (mutate-if-imp inst const-list inputs prev-depth)
  (mutate-if inst '() const-list inputs prev-depth #t))

(define (mutate-if-multi inst symbol-list const-list inputs prev-depth [imp #f])
  ; remove & flip are handled by mutating the mapping
  (define js-list (if imp const-list symbol-list))
  (match inst
    [(list 'if-multi args branch mapping)
       (define (iter args)
         (match args
           [(list) 'should-not-happen]
           [(list js-expr) (mutate-js-expr js-expr js-list inputs prev-depth)]
           [(cons js-expr rest)
            (append (map (lambda (x) (cons x rest)) (mutate-js-expr js-expr js-list inputs prev-depth))
                    (map (lambda (x) (cons js-expr x)) (iter rest)))]))
       (define mutate-if-args
         (map (lambda (x) (list 'if-multi x branch mapping)) (iter args)))
     (define mutate-branch (map (lambda (x) (list 'if-multi args x mapping))
                                (if imp
                                    (mutate-inst-imp branch const-list inputs prev-depth)
                                    (mutate-inst branch symbol-list const-list inputs prev-depth))))
     (append mutate-if-args mutate-branch)]))

(define (mutate-if-multi-imp inst const-list inputs prev-depth)
  (mutate-if-multi inst '() const-list inputs prev-depth #t))

(define (mutate-if-else-multi inst symbol-list const-list inputs prev-depth [imp #f])
  ; remove & flip are handled by mutating the mapping
  (define js-list (if imp const-list symbol-list))
  (match inst
    [(list 'if-else-multi args then-branch else-branch mapping)
     (define (iter args)
       (match args
         [(list) 'should-not-happen]
         [(list js-expr) (mutate-js-expr js-expr js-list inputs prev-depth)]
         [(cons js-expr rest)
          (append (map (lambda (x) (cons x rest)) (mutate-js-expr js-expr js-list inputs prev-depth))
                  (map (lambda (x) (cons js-expr x)) (iter rest)))]))
     (define mutate-if-else-args
       (map (lambda (x) (list 'if-else-multi x then-branch else-branch mapping)) (iter args)))
     (define mutate-then-branch (map (lambda (x) (list 'if-else-multi args x else-branch mapping))
                                (if imp
                                    (mutate-inst-imp then-branch const-list inputs prev-depth)
                                    (mutate-inst else-branch symbol-list const-list inputs prev-depth))))
     (define mutate-else-branch (map (lambda (x) (list 'if-else-multi args then-branch x mapping))
                                (if imp
                                    (mutate-inst-imp else-branch const-list inputs prev-depth)
                                    (mutate-inst else-branch symbol-list const-list inputs prev-depth))))
     (append mutate-if-else-args mutate-then-branch mutate-else-branch)]))

(define (mutate-if-else-multi-imp inst const-list inputs prev-depth)
  (mutate-if-else-multi inst '() const-list inputs prev-depth #t))

(define (mutate-case-multi inst symbol-list const-list inputs prev-depth [imp #f])
  (define js-list (if imp const-list symbol-list))
  (define (mutate-func inst)
    (if imp
        (mutate-inst-imp inst const-list inputs prev-depth)
        (mutate-inst inst symbol-list const-list inputs prev-depth)))
  (match inst
    [(list 'case-multi args branchs mapping)
     (define (iter args)
       (match args
         [(list) 'should-not-happen]
         [(list js-expr) (mutate-js-expr js-expr js-list inputs prev-depth)]
         [(cons js-expr rest)
          (append (map (lambda (x) (cons x rest)) (mutate-js-expr js-expr js-list inputs prev-depth))
                  (map (lambda (x) (cons js-expr x)) (iter rest)))]))
     (define mutate-case-args
       (map (lambda (x) (list 'case-multi x branchs mapping)) (iter args)))
     (define (iter-branchs branchs)
       (match branchs
         [(list) 'should-not-happen]
         [(list branch) (mutate-func branch)]
         [(cons branch rest)
          (append (map (lambda (x) (cons x rest)) (mutate-func branch))
                  (map (lambda (x) (cons branch x)) (iter-branchs rest)))]))
     (define mutate-branchs (map (lambda (x) (list 'case-multi args x mapping)) (iter-branchs branchs)))
     (append mutate-case-args mutate-branchs)]))

(define (mutate-case-multi-imp inst const-list inputs prev-depth)
  (mutate-case-multi inst '() const-list inputs prev-depth #t))

(define (mutate-if-else inst symbol-list const-list inputs prev-depth [imp #f])
  (match inst
    [(list 'if-else js-expr then-branch else-branch)
     (define mutate-if-else-arg
       (map (lambda (x) (list 'if x then-branch else-branch)) (mutate-js-expr js-expr (if imp const-list symbol-list) inputs prev-depth)))
     (define remove-if-else (list then-branch else-branch))
     (define flip-if-else (list (list 'if-else js-expr else-branch then-branch)))
     (define delete-branch
       (list
        (list 'if js-expr then-branch)
        (list 'if js-expr else-branch)
        (list 'if (list 'not js-expr) then-branch)
        (list 'if (list 'not js-expr) else-branch)))
     (define mutate-then (map (lambda (x) (list 'if-else js-expr x else-branch))
                              (if imp
                                  (mutate-inst-imp then-branch const-list inputs prev-depth)
                                  (mutate-inst then-branch symbol-list const-list inputs prev-depth))))
     (define mutate-else (map (lambda (x) (list 'if-else js-expr then-branch x))
                              (if imp
                                  (mutate-inst-imp else-branch const-list inputs prev-depth)
                                  (mutate-inst else-branch symbol-list const-list inputs prev-depth))))
     (append mutate-if-else-arg
             remove-if-else
             flip-if-else
             delete-branch
             mutate-then
             mutate-else)]))

(define (mutate-if-else-imp inst const-list inputs prev-depth)
  (mutate-if-else inst '() const-list inputs prev-depth #t))

(define (mutate-bind inst symbol-list const-list inputs prev-depth [imp #f])
  (match inst
    [(list 'bind name js-expr next)
     (define mutate-bind-arg
       (map (lambda (x) (list 'bind name x next) (mutate-js-expr js-expr (if imp const-list symbol-list) inputs prev-depth))))
     (define mutate-next
       (map (lambda (x) (list 'bind name js-expr x)
              (if imp
                  (mutate-inst-imp next (cons name const-list) inputs prev-depth)
                  (mutate-inst next (cons name symbol-list) const-list inputs prev-depth)))))
     (append mutate-bind-arg
             mutate-next)]))

(define (mutate-bind-imp inst const-list inputs prev-depth)
  (mutate-bind inst '() const-list inputs prev-depth #t))

(define (mutate-return inst symbol-list const-list inputs prev-depth)
  (match inst
    [(list 'return js-expr)
     (map (lambda (x) (list 'return x))
          (mutate-js-expr js-expr symbol-list inputs prev-depth))]))

(define (mutate-begin inst symbol-list const-list inputs prev-depth)
  (define (mutate-let let-inst symbol-list)
    (match let-inst
      [(list 'let name value) (map (lambda (x) (list 'let name x)) (mutate-js-expr value symbol-list inputs prev-depth))]))
  (define (mutate-seq seq symbol-list)
    (match seq
      [(list) 'should-not-happen]
      [(list last-inst) (map (lambda (x) (list x)) (mutate-inst last-inst symbol-list const-list inputs prev-depth))]
      [(cons let-inst rest)
       (append
        (map (lambda (x) (cons x rest)) (mutate-let let-inst symbol-list))
        (map (lambda (x) (cons let-inst x)) (mutate-seq rest (cons (let-name let-inst) symbol-list))))]))
   (match inst
     [(cons 'begin seq)
      (map (lambda (x) (cons 'begin x)) (mutate-seq seq symbol-list))]))

(define (mutate-begin-imp inst const-list inputs prev-depth)
  (define (mutate-let let-inst const-list)
    (match let-inst
      [(list 'let name value) (map (lambda (x) (list 'let name x)) (mutate-js-expr value const-list inputs prev-depth))]))
  (define (mutate-seq seq const-list)
    (match seq
      [(list) 'should-not-happen]
      [(list last-inst) (map (lambda (x) (list x)) (mutate-inst-imp last-inst const-list inputs prev-depth))]
      [(cons let-inst rest)
       (append
        (map (lambda (x) (cons x rest)) (mutate-let let-inst const-list))
        (map (lambda (x) (cons let-inst x)) (mutate-seq rest (cons (let-name let-inst) const-list))))]))
  (match inst
    [(cons 'begin seq)
     (map (lambda (x) (cons 'begin x)) (mutate-seq seq const-list))]))



(define (mutate-body body const-list inputs prev-depth)
  (match body
    [(list) (list)]
    [(list (list name inst)) (map (lambda (x) (list (list name x))) (mutate-inst inst (append const-list inputs) const-list inputs prev-depth))]
    [(cons (list name inst) rest)
     (append
      (map (lambda (x) (cons (list name x) rest)) (mutate-inst inst (append const-list inputs) const-list inputs prev-depth))
      (map (lambda (x) (cons (list name inst) x)) (mutate-body rest const-list inputs prev-depth)))]))

(define (mutate-spec spec-input prev-depth)
  (match spec-input
    [(spec inputs output funclist const-list defaultval body)
     (map (lambda (x) (spec inputs output funclist const-list defaultval x)) (mutate-body body const-list inputs prev-depth))]))
           

(define (main-mut-gen)
  (mutate-js-expr '(f a b c) '(a b c) '(a b c) 2)
  (mutate-js-expr '(f (prev a) b c) '(a b c) '(a b c) 2)
  (mutate-js-expr 'a '(a b c) '(a b c) 2)

  (mutate-inst '(if (f a) (return (g b))) '(a b c) '() '(a b) 2)
  (mutate-inst '(begin (let x (f a)) (let y (g b)) (return c)) '(a b c) '() '(a b) 2)

  (mutate-inst '(if (f a) (return (g c b))) '(a b c) '(c) '(a b) 2)

  (mutate-body
   '((a (if (f a) (return (g c b))))
     (b (if (f b) (return a))))
   '(c)
   '(a b)
   2)

  (mutate-body
   '((a (split ((t (f a))) (if d (new-stream ((b (if t (return b)))))))))
   '(d)
   '(a b c)
   1)

  (mutate-body
   '((mode
      (split ((mode_snapshot mode)
              (down_snapshot down))
             (if-else (f mode_snapshot)
                      (new-stream
                       ((move (if-else (g drawing)
                                       (return (l down_snapshot move))
                                       (return (n drawing (prev move) move))))))
                      (empty-stream)))))
   '()
   '(mode down move)
   1)
   
  )