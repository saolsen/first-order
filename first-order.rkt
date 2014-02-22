#lang racket
(require "miniKanren/mk.rkt")

;;expression
;;assertion

(define (new-varo key env)
  (conde
   [(fresh (k v rest)
      (== `((,k . ,v) . ,rest) env)
      (=/= k key)
      (new-varo key rest))]
   [(== '() env)]))

(define (get-varo key env val)
  (fresh (k v rest)
    (== `((,k . ,v) . ,rest) env)
    (conde
     [(== k key)
      (== v val)]
     [(get-varo key rest val)])))

(define (ando a b r)
  (conde
   [(== a #t) (== b #t) (== r #t)]
   [(== a #f) (== b #t) (== r #f)]
   [(== a #t) (== b #f) (== r #f)]
   [(== a #f) (== b #f) (== r #f)]))

;; unifies r with (or a b)
(define (oro a b r)
  (conde
   [(== a #t) (== b #t) (== r #t)]
   [(== a #f) (== b #t) (== r #t)]
   [(== a #t) (== b #f) (== r #t)]
   [(== a #f) (== b #f) (== r #f)]))

;; unifies v with (not x)
(define (noto x v)
  (conde
   [(== x #t) (== v #f)]
   [(== x #f) (== v #t)]))

(define (boolo b)
  (conde
   [(== b #t)
    (== b #f)]))

(define (expressiono exp env-in env-out val)
  (conde
   [(symbolo exp)
    (new-varo exp env-in)
    (fresh (v)
      (== val v)
      (== env-out `((,exp . ,v) . ,env-in)))]
   [(symbolo exp)
    (get-varo exp env-in val)
    (== env-in env-out)]
   [(fresh (e1 e2 v1 v2 env*)
        (== `(AND ,e1 ,e2) exp)
        (expressiono e1 env-in env* v1)
        (expressiono e2 env* env-out v2)
        (ando v1 v2 val))]
   [(fresh (e1 e2 v1 v2 env*)
        (== `(OR ,e1 ,e2) exp)
        (expressiono e1 env-in env* v1)
        (expressiono e2 env* env-out v2)
        (oro v1 v2 val))]
   [(fresh (e v)
        (== `(NOT ,e) exp)
        (expressiono e env-in env-out v)
        (noto v val))]
   [(boolo exp)
    (== env-in env-out)
    (== exp val)]))

(define (assertiono a env-in env-out)
  (conde
   [(fresh (e1 e2 v1 v2 env*)
      (== `(IF ,e1 ,e2) a)
      (expressiono e1 env-in env* v1)
      (expressiono e2 env* env-out v2)
      (conde
       ;; TODO: this isn't right
       [(== #t v1) (== #t v2)]
       [succeed]))]
   [(fresh (e1 e2 v1 v2 env*)
      (== `(IFF ,e1 ,e2) a)
      (expressiono e1 env-in env* v1)
      (expressiono e2 env* env-out v2)
      (== v1 v2))]))

(define (eval-manyo as env-in env-out)
  (conde
   [(fresh (f r env* v)
           (== (cons f r) as)
           (assertiono f env-in env*)
           (eval-manyo r env* env-out))]
   [(== `() as)
    (== env-in env-out)]))

(define (evaluate-assertions . assertions)
  (run* (q)
        (eval-manyo assertions `() q)))

(module+ test (require rackunit)
  (check-equal? `(#t)
    (run* (q) (oro #t #f q))
    )
  )
