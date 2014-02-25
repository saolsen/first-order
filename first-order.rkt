#lang racket
(require "miniKanren/mk.rkt")

(define (key-not-boundo key env)
  (conde
   [(== '() env)]
   [(fresh (k v rest)
      (== `((,k . ,v) . ,rest) env)
      (=/= k key)
      (key-not-boundo key rest))]))

(define (get-varo key env val)
  (fresh (k v rest)
    (== `((,k . ,v) . ,rest) env)
    (conde
     [(== k key)
      (== v val)]
     [(=/= k key)
      (get-varo key rest val)])))

(define (ando a b r)
  (conde
   [(== a #t) (== b #t) (== r #t)]
   [(== a #f) (== b #t) (== r #f)]
   [(== a #t) (== b #f) (== r #f)]
   [(== a #f) (== b #f) (== r #f)]))

(define (oro a b r)
  (conde
   [(== a #t) (== b #t) (== r #t)]
   [(== a #f) (== b #t) (== r #t)]
   [(== a #t) (== b #f) (== r #t)]
   [(== a #f) (== b #f) (== r #f)]))

(define (noto x v)
  (conde
   [(== x #t) (== v #f)]
   [(== x #f) (== v #t)]))

(define (boolo b)
  (conde
   [(== b #t)]
   [(== b #f)]))

(define (expressiono exp env-in env-out val)
  (conde
   [(boolo exp)
    (== env-in env-out)
    (== exp val)]
   [(symbolo exp)
    (conde
     [(key-not-boundo exp env-in)
      (== env-out `((,exp . ,val) . ,env-in))]
     [(get-varo exp env-in val)
      (== env-in env-out)])]
   [(fresh (e v)
      (== `(NOT ,e) exp)
      (noto v val)
      (expressiono e env-in env-out v))]
   [(fresh (e1 e2 v1 v2 env*)
      (== `(AND ,e1 ,e2) exp)
      (ando v1 v2 val)
      (expressiono e1 env-in env* v1)
      (expressiono e2 env* env-out v2))]
   [(fresh (e1 e2 v1 v2 env*)
      (== `(OR ,e1 ,e2) exp)
      (oro v1 v2 val)
      (expressiono e1 env-in env* v1)
      (expressiono e2 env* env-out v2))]))

(define (assertiono a env-in env-out)
  (conde
   [(fresh (e1 e2 v env*)
      (== `(IFF ,e1 ,e2) a)
      (expressiono e1 env-in env* v)
      (expressiono e2 env* env-out v))]
   [(fresh (e1 e2 v1 v2 env*)
      (== `(IF ,e1 ,e2) a)
      (conde
       [(== #t v1) (== #t v2)]
       [(== #f v1) (== #t v2)]
       [(== #f v1) (== #f v2)])
      (expressiono e1 env-in env* v1)
      (expressiono e2 env* env-out v2))]))

(define (eval-manyo as env-in env-out)
  (conde
   [(== `() as)
    (== env-in env-out)]
   [(fresh (f r env* v)
      (== (cons f r) as)
      (assertiono f env-in env*)
      (eval-manyo r env* env-out))]))

(define (evaluate-assertions . assertions)
  (run* (q)
    (eval-manyo assertions `() q)))

(module+ test (require rackunit)
  (check-equal? (run* (q) (oro #t #f q))
                `(#t))
  (check-equal? (run* (q) (ando #t #f q))
                `(#f))
  (check-equal? (run* (q) (ando #t #t q))
                `(#t))
  (check-equal? (run* (q) (noto #t q))
                `(#f))
  (check-equal? (run* (q) (expressiono `foo `() q #t))
                `(((foo . #t))))
  (check-equal? (run* (q) (expressiono #t `() `() q))
                `(#t))
  (check-equal? (run* (q) (fresh (a) (expressiono `foo `((foo . #f)) a q)))
                `(#f))
  (check-equal? (run* (q) (fresh (a) (expressiono `foo `((foo . #f)) a q)))
                `(#f))
  (check-equal? (run* (q) (fresh (a) (expressiono `(AND foo #t) `((foo . #f)) a q)))
                `(#f))
  (check-equal? (run* (q) (fresh (a) (expressiono `(AND foo #t) `((foo . ,q)) a #f)))
                `(#f))
  (check-equal? (run* (q) (expressiono `(AND foo #t) `() q #f))
                `(((foo . #f))))
  (check-equal? (run* (q) (expressiono `(AND foo (NOT bar)) `((bar . #t)) q #t))
                `())
  (check-equal? (run* (q) (assertiono `(IF #t #t) `() `()))
                `(_.0))
  (check-equal? (run 1 (q) (assertiono `(IF #t ,q) `() `()))
                `(#t))
  (check-equal? (run 2 (q) (assertiono `(IF #f ,q) `() `()))
                `(#t #f))
  (check-equal? (run* (q) (eval-manyo `((IF a b) (IF b c) (IFF a #t)) `() q))
                `(((c . #t) (b . #t) (a . #t))))
  )