#lang rosette/safe

(require "../ast/syntax.rkt")
(require rosette/lib/match)

(provide (all-defined-out))

(struct stream-dependency (stream father sibling son direct-son) #:transparent)
(struct empty-dependency (ref father sibling) #:transparent)

(define (collect-son body)
  (define (iter body)
    (append-map (lambda (x) (collect-son-inst (cadr x))) body))
  (define (collect-son-inst inst)
    (cond [(if? inst) (collect-son-inst (if-branch inst))]
          [(if-else? inst)
           (append (collect-son-inst (if-else-then-branch inst))
                   (collect-son-inst (if-else-else-branch inst)))]
          [(if-multi? inst) (collect-son-inst (if-multi-branch inst))]
          [(if-else-multi? inst)
           (append (collect-son-inst (if-else-multi-then-branch inst))
                   (collect-son-inst (if-else-multi-else-branch inst)))]
          [(case-multi? inst)
           (append* (map collect-son-inst (case-multi-branchs inst)))]
          [(return? inst) '()]
          [(return-empty? inst) '()]
          [(begin? inst) (collect-son-inst (last (begin-seq inst)))]
          [(bind? inst) (collect-son-inst (bind-inst inst))]
          [(split? inst) (collect-son-inst (split-body inst))]
          [(new-stream? inst)
           (let ([body (new-stream-body inst)])
             (cons body (collect-son body)))]
          [(new-stream-initial? inst)
           (let ([body (new-stream-initial-body inst)])
             (cons body (collect-son body)))]
          [(new-stream-seed? inst)
           (let ([body (new-stream-seed-body inst)])
             (cons body (collect-son body)))]
          [(empty-stream? inst) '()]))
  (iter body))

(define (collect-direct-son-grouped body)
  (define (iter body)
    (map (lambda (x) (collect-direct-son-inst (cadr x))) body))
  (define (collect-direct-son-inst inst)
    (cond [(if? inst) (collect-direct-son-inst (if-branch inst))]
          [(if-else? inst)
           (append (collect-direct-son-inst (if-else-then-branch inst))
                   (collect-direct-son-inst (if-else-else-branch inst)))]
          [(if-multi? inst) (collect-direct-son-inst (if-multi-branch inst))]
          [(if-else-multi? inst)
           (append (collect-direct-son-inst (if-else-multi-then-branch inst))
                   (collect-direct-son-inst (if-else-multi-else-branch inst)))]
          [(case-multi? inst)
           (append* (map collect-direct-son-inst (case-multi-branchs inst)))]
          [(return? inst) '()]
          [(return-empty? inst) '()]
          [(begin? inst) (collect-direct-son-inst (last (begin-seq inst)))]
          [(bind? inst) (collect-direct-son-inst (bind-inst inst))]
          [(split? inst) (collect-direct-son-inst (split-body inst))]
          [(new-stream? inst)
           (let ([body (new-stream-body inst)])
             (list body))]
          [(new-stream-initial? inst)
           (let ([body (new-stream-initial-body inst)])
             (list body))]
          [(new-stream-seed? inst)
           (let ([body (new-stream-seed-body inst)])
             (list body))]
          [(empty-stream? inst) '()]))
  (iter body))

(define (collect-sibling inst)
  (cond [(if? inst) (collect-sibling (if-branch inst))]
        [(if-else? inst)
         (append (collect-sibling (if-else-then-branch inst))
                 (collect-sibling (if-else-else-branch inst)))]
        [(if-multi? inst) (collect-sibling (if-multi-branch inst))]
        [(if-else-multi? inst)
         (append (collect-sibling (if-else-multi-then-branch inst))
                 (collect-sibling (if-else-multi-else-branch inst)))]
        [(case-multi? inst)
         (append* (map collect-sibling (case-multi-branchs inst)))]

        [(begin? inst) (collect-sibling (last (begin-seq inst)))]
        [(bind? inst) (collect-sibling (bind-inst inst))]
        [(new-stream? inst)
         (let ([body (new-stream-body inst)])
           (list body))]
        [(new-stream-initial? inst)
         (let ([body (new-stream-initial-body inst)])
           (list body))]
        [(new-stream-seed? inst)
         (let ([body (new-stream-seed-body inst)])
           (list body))]
        [(empty-stream? inst) '()]))

(define (collect-streams body built)
  (define (collect-streams-inst inst father sibling built)
    (define (build-dependency stream)
      (stream-dependency stream (list father) (remq stream sibling)
                         (collect-son stream) (collect-direct-son-grouped stream)))
    (define (build-empty-dependency ref)
      (empty-dependency ref (list father) sibling))
    (cond [(if? inst)
           (let ([branch (if-branch inst)])
             (collect-streams-inst branch father sibling (cons (build-empty-dependency inst) built)))]
          [(if-else? inst)
           (let ([then-branch (if-else-then-branch inst)]
                 [else-branch (if-else-else-branch inst)])
             (collect-streams-inst else-branch father sibling
                             (collect-streams-inst then-branch father sibling built)))]
          [(if-multi? inst)
           (let ([branch (if-multi-branch inst)])
             (collect-streams-inst branch father sibling (cons (build-empty-dependency inst) built)))]
          [(if-else-multi? inst)
           (let ([then-branch (if-else-multi-then-branch inst)]
                 [else-branch (if-else-multi-else-branch inst)])
             (collect-streams-inst else-branch father sibling
                             (collect-streams-inst then-branch father sibling built)))]
          [(case-multi? inst)
           (let ([branchs (case-multi-branchs inst)])
             (define (iter branchs)
               (if (null? branchs)
                   built
                   (let ([cur (car branchs)]
                         [rest (cdr branchs)])
                     (collect-streams-inst cur father sibling (iter rest)))))
             (iter branchs))]
          [(return? inst) built]
          [(return-empty? inst) built]
          [(begin? inst)
           (let ([lst (begin-seq inst)])
             (collect-streams-inst (last lst) father sibling built))]
          [(bind? inst)
           (let ([next (bind-inst inst)])
             (collect-streams-inst next father sibling built))]
          [(split? inst)
           (let ([body (split-body inst)])
             (collect-streams-inst body father (collect-sibling body) built))]
          [(new-stream? inst)
           (let ([body (new-stream-body inst)])
             (collect-in-new-stream body (cons (build-dependency body) built)))]
          [(new-stream-initial? inst)
           (let ([body (new-stream-initial-body inst)])
             (collect-in-new-stream body (cons (build-dependency body) built)))]
          [(new-stream-seed? inst)
           (let ([body (new-stream-seed-body inst)])
             (collect-in-new-stream body (cons (build-dependency body) built)))]
          [(empty-stream? inst)
           (cons (build-empty-dependency inst) built)]))
  (define (collect-in-new-stream father built)
    (define (iter body built)
      (if (null? body)
          built
          (let* ([cur (car body)]
                 [rest (cdr body)]
                 [inst-body (cadr cur)])
            (iter rest (collect-streams-inst inst-body father '()#|won't be used|# built)))))
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
        (if (null? dependency-map)
            'error
            (let* ([cur-dep (car dependency-map)]
                   [rest (cdr dependency-map)]
                   [result (cdr cur-dep)]
                   [cur (stream-dependency-stream (car cur-dep))])
              (if (eq? cur stream)
                  result
                  (iter rest)))))
      (iter dependency-map))
    (define (cart-product-append list1 list2)
      (append-map (lambda (x) (map (lambda (y) (append x y)) list2)) list1))
    (define (cart-product-append-list-of-list list-of-list)
      (if (null? list-of-list)
          '(())
          (let ([cur (car list-of-list)]
                [rest (cdr list-of-list)])
            (cart-product-append cur (cart-product-append-list-of-list rest)))))
    (define (build stream direct)
      (define built-result-for-direct (map (lambda (x) (cons '() (append-map get-result x))) direct))
      (map (lambda (x) (cons stream x)) (cart-product-append-list-of-list built-result-for-direct)))
    (define (iter lst built)
      (if (null? lst)
          (reverse built)
          (let* ([cur (car lst)]
                 [rest (cdr lst)]
                 [sd (car cur)]
                 [result (cdr cur)]
                 [stream (stream-dependency-stream sd)]
                 [direct (stream-dependency-direct-son sd)])
            (if (or result (not (already-built-deep-list? direct)))
                (iter rest (cons cur built))
                (update (append (reverse built) (list (cons sd (build stream direct))) rest))))))
    (iter dependency-map '()))
  (update initial-mapping))

(struct stream-bv-rep (self unsub) #:transparent)
(struct empty-bv-rep (unsub) #:transparent)

(define (get-unsub rep)
  (if (stream-bv-rep? rep)
      (stream-bv-rep-unsub rep)
      (empty-bv-rep-unsub rep)))

(define (get-self rep)
  (stream-bv-rep-self rep))

(define (build-bitvector-transition dependency-list)
  (define non-empty-dependency-list (filter stream-dependency? dependency-list))
  (define stream-list (map stream-dependency-stream non-empty-dependency-list))
  (define len (length stream-list))
  (define (build-mapping stream-list last-bv)
    (if (null? stream-list)
        '()
        (let ([stream (car stream-list)]
              [rest (cdr stream-list)])
          (cons (cons stream last-bv) (build-mapping rest (bvshl last-bv (bv 1 len)))))))
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
    (if (null? dependency-list)
        '()
        (let ([cur (car dependency-list)]
              [rest (cdr dependency-list)])
          (cons
           (if (stream-dependency? cur)
               (stream-bv-rep
                (cdr (assoc (stream-dependency-stream cur) mapping))
                (bvnot (stream-list-to-bv (get-sibling-sons (stream-dependency-sibling cur)))))
               (empty-bv-rep (bvnot (stream-list-to-bv (get-sibling-sons (empty-dependency-sibling cur))))))
           
           (iter rest)))))
  (iter dependency-list))

(define (build-symbol-bv-mapping dependency-list)
  (define non-empty-dependency-list (filter stream-dependency? dependency-list))
  (define stream-list (map stream-dependency-stream non-empty-dependency-list))
  (define len (length stream-list))
  (define (build-mapping stream-list last-bv)
    (if (null? stream-list)
        '()
        (let ([stream (car stream-list)]
              [rest (cdr stream-list)])
          (cons (cons stream last-bv) (build-mapping rest (bvshl last-bv (bv 1 len)))))))
  (define mapping (build-mapping stream-list (bv 1 len)))
  (define (walk-mapping mapping)
    (if (null? mapping)
        '()
        (let* ([cur (car mapping)]
               [stream (car cur)]
               [bv (cdr cur)]
               [rest (cdr mapping)])
          (append (map (lambda (x) (cons (car x) bv)) stream) (walk-mapping rest)))))
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

      