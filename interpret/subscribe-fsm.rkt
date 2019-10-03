#lang rosette/safe

(require rosette/lib/match)

(provide (all-defined-out))

(struct stream-dependency (stream father sibling son direct-son) #:transparent)
(struct empty-dependency (ref father sibling) #:transparent)

(define (collect-son body)
  (define (iter body)
    (append-map (lambda (x) (collect-son-inst (cadr x))) body))
  (define (collect-son-inst inst)
    (match inst
      [(list 'if _ branch) (collect-son-inst branch)]
      [(list 'if-else _ then-branch else-branch)
       (append (collect-son-inst then-branch) (collect-son-inst else-branch))]
      [(list 'if-multi _ branch _) (collect-son-inst branch)]
      [(list 'if-else-multi _ then-branch else-branch _)
       (append (collect-son-inst then-branch) (collect-son-inst else-branch))]
      [(list 'case-multi _ branchs _) (append* (map collect-son-inst branchs))]
      [(list 'return _) '()]
      [(list 'return-empty) '()]
      [(cons 'begin lst) (collect-son-inst (last lst))]
      [(list 'bind _ _ next) (collect-son-inst next)]
      [(list 'split _ body) (collect-son-inst body)]
      [(list 'new-stream body) (cons body (collect-son body))]
      [(list 'new-stream-initial body _) (cons body (collect-son body))]
      [(list 'new-stream-seed body _) (cons body (collect-son body))]
      [(list 'empty-stream) (list)]))
  (iter body))

(define (collect-direct-son-grouped body)
  (define (iter body)
    (map (lambda (x) (collect-direct-son-inst (cadr x))) body))
  (define (collect-direct-son-inst inst)
    (match inst
      [(list 'if _ branch) (collect-direct-son-inst branch)]
      [(list 'if-else _ then-branch else-branch)
       (append (collect-direct-son-inst then-branch) (collect-direct-son-inst else-branch))]
      [(list 'if-multi _ branch _) (collect-direct-son-inst branch)]
      [(list 'if-else-multi _ then-branch else-branch _)
       (append (collect-direct-son-inst then-branch) (collect-direct-son-inst else-branch))]
      [(list 'case-multi _ branchs _)
       (append* (map collect-direct-son-inst branchs))]
      [(list 'return _) '()]
      [(list 'return-empty) '()]
      [(cons 'begin lst) (collect-direct-son-inst (last lst))]
      [(list 'bind _ _ next) (collect-direct-son-inst next)]
      [(list 'split _ body) (collect-direct-son-inst body)]
      [(list 'new-stream body) (list body)]
      [(list 'new-stream-initial body _) (list body)]
      [(list 'new-stream-seed body _) (list body)]
      [(list 'empty-stream) (list)]))
  (iter body))

(define (collect-sibling inst-imp)
  (match inst-imp
    [(list 'if _ branch) (collect-sibling branch)]
    [(list 'if-else _ then-branch else-branch)
     (append (collect-sibling then-branch)
             (collect-sibling else-branch))]
    [(list 'if-multi _ branch _) (collect-sibling branch)]
    [(list 'if-else-multi _ then-branch else-branch _)
     (append (collect-sibling then-branch)
             (collect-sibling else-branch))]
    [(list 'case-multi _ branchs _)
     (append* (map collect-sibling branchs))]
    [(list 'bind _ _ next) (collect-sibling next)]
    [(cons 'begin lst) (collect-sibling (last lst))]
    [(list 'new-stream body) (list body)]
    [(list 'new-stream-initial body _) (list body)]
    [(list 'new-stream-seed body _) (list body)]
    [(list 'empty-stream) (list)]))

(define (collect-streams body built)
  (define (collect-streams-inst inst father sibling built)
    (define (build-dependency stream)
      (stream-dependency stream (list father) (remq stream sibling)
                         (collect-son stream) (collect-direct-son-grouped stream)))
    (define (build-empty-dependency ref)
      (empty-dependency ref (list father) sibling))
    (match inst
      [(list 'if _ branch) (collect-streams-inst branch father sibling (cons (build-empty-dependency inst) built))]
      [(list 'if-else _ then-branch else-branch)
       (collect-streams-inst else-branch father sibling
                             (collect-streams-inst then-branch father sibling built))]
      [(list 'if-multi _ branch _) (collect-streams-inst branch father sibling (cons (build-empty-dependency inst) built))]
      [(list 'if-else-multi _ then-branch else-branch _)
       (collect-streams-inst else-branch father sibling
                             (collect-streams-inst then-branch father sibling built))]
      [(list 'case-multi _ branchs _)
       (define (iter branchs)
         (match branchs
           [(list) built]
           [(cons cur rest)
            (collect-streams-inst cur father sibling (iter rest))]))
       (iter branchs)]
      [(list 'return _) built]
      [(list 'return-empty) built]
      [(cons 'begin lst) (collect-streams-inst (last lst) father sibling built)]
      [(list 'bind _ _ next) (collect-streams-inst next father sibling built)]
      [(list 'split _ body)
       (collect-streams-inst body father (collect-sibling body) built)]
      [(list 'new-stream body) (collect-in-new-stream body (cons (build-dependency body) built))]
      [(list 'new-stream-initial body _) (collect-in-new-stream body (cons (build-dependency body) built))]
      [(list 'new-stream-seed body _) (collect-in-new-stream body (cons (build-dependency body) built))]
      [(list 'empty-stream) (cons (build-empty-dependency inst) built)]))
  (define (collect-in-new-stream father built)
    (define (iter body built)
      (match body
        [(list) built]
        [(cons (list _ inst-body) rest)
         (iter rest (collect-streams-inst inst-body father '()#|won't be used|# built))]))
    (iter father built))
  (reverse (collect-in-new-stream body (cons (stream-dependency body '() '() (collect-son body) (collect-direct-son-grouped body)) built))))

(define (dependency-enumerate-state dependency-list)
  (define non-empty-dependency-list (filter stream-dependency? dependency-list))
  (define initial-mapping (map (lambda (x) (cons x #f)) non-empty-dependency-list))
  (define (update dependency-map)
    (define (already-built? stream)
      (if (cdar (filter (lambda (x) (eq? stream (stream-dependency-stream (car x)))) dependency-map))
          #t
          #f))
    (define (already-built-deep-list? lst)
      (null? (filter (lambda (x) (not x)) (append-map (lambda (x) (map already-built? x)) lst))))
    (define (get-result stream)
      (define (iter dependency-map)
        (match dependency-map
          [(list) 'error]
          [(cons (cons (stream-dependency cur _ _ _ _) result) rest)
           (if (eq? cur stream)
               result
               (iter rest))]))
      (iter dependency-map))
    (define (cart-product-append list1 list2)
      (append-map (lambda (x) (map (lambda (y) (append x y)) list2)) list1))
    (define (cart-product-append-list-of-list list-of-list)
      (match list-of-list
        [(list) '(())]
        [(cons cur rest)
         (cart-product-append cur (cart-product-append-list-of-list rest))]))
    (define (build stream direct)
      (define built-result-for-direct (map (lambda (x) (cons '() (append-map get-result x))) direct))
      (map (lambda (x) (cons stream x)) (cart-product-append-list-of-list built-result-for-direct)))
    (define (iter lst built)
      (match lst
        [(list) (reverse built)]
        [(cons cur rest)
         (match cur
           [(cons sd result)
            (match sd
              [(stream-dependency stream _ _ _ direct)
               (if (or result (not (already-built-deep-list? direct)))
                   (iter rest (cons cur built))
                   (update (append (reverse built) (list (cons sd (build stream direct))) rest)))])])]))
    (iter dependency-map '()))
  (update initial-mapping))

(struct stream-bv-rep (self unsub) #:transparent)
(struct empty-bv-rep (unsub) #:transparent)

(define (get-unsub rep)
  (match rep
    [(stream-bv-rep _ unsub) unsub]
    [(empty-bv-rep unsub) unsub]))

(define (get-self rep)
  (match rep
    [(stream-bv-rep self _) self]))

(define (build-bitvector-transition dependency-list)
  (define non-empty-dependency-list (filter stream-dependency? dependency-list))
  (define stream-list (map stream-dependency-stream non-empty-dependency-list))
  (define len (length stream-list))
  (define (build-mapping stream-list last-bv)
    (match stream-list
      [(list) '()]
      [(cons stream rest)
       (cons (cons stream last-bv) (build-mapping rest (bvshl last-bv (bv 1 len))))]))
  (define mapping (build-mapping stream-list (bv 1 len)))
  (define (get-sibling-sons sibling)
    (remove-duplicates
     (append sibling
             (append-map stream-dependency-son
                         (filter (lambda (x) (memq (stream-dependency-stream x) sibling))
                                 non-empty-dependency-list))) eq?))
  (define (stream-list-to-bv lst)
    (if (null? lst)
        (bv 0 len)
        (apply bvor (map (lambda (x) (cdr (assoc x mapping))) lst))))
  (define (iter dependency-list)
    (match dependency-list
      [(list) '()]
      [(cons cur rest)
       (cons (match cur
               [(stream-dependency stream _ sibling _ _)
                (stream-bv-rep (cdr (assoc stream mapping)) (bvnot (stream-list-to-bv (get-sibling-sons sibling))))]
               [(empty-dependency _ _ sibling)
                (empty-bv-rep (bvnot (stream-list-to-bv (get-sibling-sons sibling))))])
             (iter rest))]))
  (iter dependency-list))

(define (build-symbol-bv-mapping dependency-list)
  (define non-empty-dependency-list (filter stream-dependency? dependency-list))
  (define stream-list (map stream-dependency-stream non-empty-dependency-list))
  (define len (length stream-list))
  (define (build-mapping stream-list last-bv)
    (match stream-list
      [(list) '()]
      [(cons stream rest)
       (cons (cons stream last-bv) (build-mapping rest (bvshl last-bv (bv 1 len))))]))
  (define mapping (build-mapping stream-list (bv 1 len)))
  (define (walk-mapping mapping)
    (match mapping
      [(list) '()]
      [(cons (cons stream bv) rest)
       (append (map (lambda (x) (cons (car x) bv)) stream) (walk-mapping rest))]))
  (walk-mapping mapping))
         

(define (test-collect)
  (define (get-name body)
  (if (null? body) 'nobody
    (caar body)))
  (define (print-dependency lst)
    (match lst
      [(list) (void)]
      [(cons (stream-dependency stream father sibling son direct-son) rest)
       (displayln (format "~a :: father: ~a, sibling: ~a, son: ~a, direct: ~a"
                          (get-name stream)
                          (map get-name father)
                          (map get-name sibling)
                          (map get-name son)
                          (map (lambda (x) (map get-name x)) direct-son)))
       (print-dependency rest)]
      [(cons (empty-dependency ref father sibling) rest)
       (displayln (format "empty :: father: ~a, sibling: ~a" (map get-name father) (map get-name sibling)))
       (print-dependency rest)]))
  (define (print-enumerate lst)
    (match lst
      [(list) (void)]
      [(cons (cons (stream-dependency stream _ _ _ _) result) rest)
       (displayln (format "~a :: result: ~a" (get-name stream) (map (lambda (x) (map get-name x)) result)))
       (print-enumerate rest)]))
  (define stream3
    '((s1 (split ((sb s1))
                 (if-else sb
                          (new-stream ((s2 (return s2))))
                          (new-stream
                           ((s3 (split ((sb3 s3))
                                       (if sb3
                                       (if-else sb3
                                                (if-else sb3
                                                         (new-stream ((s5 (return s5))))
                                                         (new-stream ((s6 (split ((sx6 s6)) (new-stream ((s7 (return s7))))))
                                                                      (sx (split ((sxx sx)) (new-stream ((s8 (return s8)))))))))
                                                (empty-stream))))))))))))
  (define dependency-list (collect-streams stream3 '()))
  (print-dependency dependency-list)
  (define all-state (dependency-enumerate-state dependency-list))
  (print-enumerate all-state)
  (displayln (build-bitvector-transition dependency-list))
  (displayln (build-symbol-bv-mapping dependency-list))

  (define drawing-spec
  '((mode
          (split ((mode_snapshot mode)
                  (down_snapshot down))
                 (if-else (curve-drawing? mode_snapshot)
                          (new-stream
                           ((move (if-else (undefined? drawing)
                                           (return (list (segment down_snapshot move)))
                                           (return (append-one drawing (segment (prev move) move)))))))
                          (new-stream-initial () (list)))))))
 (define dependency-list1 (collect-streams drawing-spec '()))
  (print-dependency dependency-list1)
  (define all-state1 (dependency-enumerate-state dependency-list1))
  (print-enumerate all-state1)
  (displayln (build-bitvector-transition dependency-list1))
  (displayln (build-symbol-bv-mapping dependency-list1)) 
  )

      