;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname perm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require racket/list)

;; list-remove : (listof X) nat -> (listof X)
(define (list-remove l i)
  (append (take l i) (drop l (+ i 1)) ))

;; perm : (listof X) num -> (listof (listof X))
(define (perm l m)
  (local
    {;; lp : (listof X) num num (listof X) (listof (listof X)) -> (listof (listof X))
     ;;   - l is the list to choose from
     ;;   - n is the length of the list
     ;;   - m is the number of elements to choose
     ;;   - current is the list of elements so far
     ;;   - acc is the list of combinations so far
     (define (lp l n m current acc)
       (if (= m 0)
           (cons current acc)  ;; return
           (if (= n 0)
               (error 'perm "not enough elements!")
               (local
                 {;; f : nat (listof (listof X)) -> (listof (listof X))
                  (define (f i acc)
                    (lp (list-remove l i) (- n 1) (- m 1)
                        (cons (list-ref l i) current) acc)
                  )}
                 (foldr f acc (build-list n identity))
               ))
       ))}
    (lp l (length l) m '() '())
  ))

;; comb : (listof X) num -> (listof (listof X))
(define (comb l m)
  (local
    {;; lp : (listof X) num num (listof X) (listof (listof X)) -> (listof (listof X))
     ;;   - l is the list to choose from
     ;;   - n is the length of the list
     ;;   - m is the number of elements to choose
     ;;   - current is the list of elements so far
     ;;   - acc is the list of combinations so far
     (define (lp l n m current acc)
       (if (= m 0)
           (cons current acc)  ;; success
           (if (= n 0)
               acc  ;; fail: omitted too much
               (local
                 {;; f : bool -> (listof (listof X))
                  (define (f take?)
                    (lp (rest l) (- n 1) (if take? (- m 1) m)
                        (if take? (cons (first l) current) current) acc))}
                  (append (f #t) (f #f))
               ))))}
    (lp l (length l) m '() '())
  ))

;; subsets : (listof X) -> (listof (listof X))
(define (subsets l)
  (foldr (λ (m acc) (append (comb l m) acc))
         '() (build-list (+ 1 (length l)) identity) ))

(subsets '(A B C D E F))
