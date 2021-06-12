#!/bin/sh

CURRENT=$(dirname $(readlink -f "$0"))

INPUT1="
(let
 ((true (x y : x))
  (false (x y : y))

  (zero (f : (x : x)))
  (succ (n f x : f (n f x)))
  (pred (n f x : n (g h : h (g f)) (u : x) (u : u)))
  ;; (zero-p (n : n (x : false) true))
  (zero-p (n a b : n (x : b) a))

  (Y (f : ((x : (f (x x))) (x : (f (x x)))))) ;; original Y combinator
  ;; (Y (f : (m : m m) (x : (f (x x))))) ;; simplified version

  ;; TODO: why below combinator does not work in most cases:
  ;;           (add (x : x) (x : x))            -*-> f : (x : (f ((f x (f : (x : x))) f x)))
  ;;           (church 4)                       -*-> f : (x : (f ((f 3) f x)))
  ;;           (add (church 0) (church 0))      -*-> some garbage
  ;;           (ith 3 3 a b c)                  -*-> diverges
  ;;       but sometimes does:
  ;;           (n-fib 7)                        -*-> 21 (correct result)
  ;; (Y ((x : (f : (f ((x x) f)))) (x : (f : (f ((x x) f)))))) ;; Turing UU combinator

  (add (Y (f x y : (zero-p y) x (succ (f x (pred y))))))

  ;; encodes machine numers as church numerals
  (church (Y (f n : (n-zero-p n) (g x : x) (succ (f (n-pred n))))))

  ;; returns nth fibonacci number
  (n-fib (Y (f n : (n-zero-p n) 1 ((n-zero-p (n-pred n)) 1 ((n-add (f (n-add -2 n)) (f (n-add -1 n))))))))

  ;; return factorial
  (n-fact (Y (f n : (n-zero-p n) 1 (n-mul n (f (n-add -1 n))))))

  (dropkp (x : (Y (f k y : (n-zero-p k) x (f (n-pred k))))))
  (dropk (x k : (dropkp x k) x))
  (ith0 (Y (f n k x : (n-zero-p n) (dropk x (n-pred k)) (f (n-pred n) (n-pred k)))))
  (ith (n : (ith0 (n-pred n))))     ;; semantic: 'ith(i, k, x_1, x_2, ..., x_k) = x_i'
  )

;; add (church 2) (church 3)
((add (x : x)) (x : x))

;; (church 1)
;; (church 4)

;; (zero-p (church 3)) a b

;; (add (church 0)) (church 1)
;; (add (church 2)) (church 3)

;; (numbers
;;   (church 0)
;;   (church 1)
;;   (church 2))

;; (church 0)

;; (n-fib 0)
;; (n-fib 1)
;; (n-fib 2)
;; (n-fib 3)
;; (n-fib 4)
;; (n-fib 5)
;; (n-fib 6)
;; (n-fib 7)

;; (ith 1 a b)
;; (((((ith 1) a) b) c) d)
;; (ith 3 3 a b c)

;; (n-fact 1)

)
"

STRATEGY="
sugar*
;; loop* ^*
;; loop|^*
^*
;; ^|loop*
;; (* loop* ^)
;; end*
"

exec zc \
	rules= "$CURRENT/lambda-calc.szcalc" \
	strategy= "$STRATEGY" \
	"$INPUT1"
