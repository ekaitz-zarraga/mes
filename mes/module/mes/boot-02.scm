;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of GNU Mes.
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; read-0.mes - bootstrap reader.  This file is read by a minimal
;;; core reader.  It only supports s-exps and line-comments; quotes,
;;; character literals, string literals cannot be used here.

;;; Code:

;; boot-00.scm
(define mes %version)

(define (defined? x)
  ((lambda (v)
     (if v (if (eq? (variable-ref v) *undefined*) #f #t) #f))
   (core:hashq-ref (initial-module) x #f)))

(define (cond-expand-expander clauses)
  (if (defined? (car (car clauses)))
      (cdr (car clauses))
      (cond-expand-expander (cdr clauses))))

(define-macro (cond-expand . clauses)
  (cons 'begin (cond-expand-expander clauses)))
;; end boot-00.scm

;; boot-01.scm
(define (not x) (if x #f #t))

(define (display x . rest)
  (if (null? rest) (core:display x)
      (core:display-port x (car rest))))

(define (write x . rest)
  (if (null? rest) (core:write x)
      (core:write-port x (car rest))))

(define (newline . rest)
  (core:display "\n"))

(define (cadr x) (car (cdr x)))

(define (map1 f lst)
  (define (loop lst acc)
    (if (null? lst)
        (core:reverse! acc (list))
        (loop (cdr lst) (cons (f (car lst)) acc))))
  (loop lst (list)))

(define (map f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map f (cdr lst)))))

(define (cons* . rest)
  (define (loop lst acc)
    (if (null? (cdr lst))
        (core:reverse! acc (car lst))
        (loop (cdr lst) (cons (car lst) acc))))
  (loop rest (list)))

(define (apply f h . t)
  (if (null? t) (core:apply f h (current-environment))
      (apply f (apply cons* (cons h t)))))

(define (append . rest)
  (define (loop lst acc)
    (if (null? (cdr lst))
        (core:reverse! acc (car lst))
        (loop (cdr lst) (append-reverse (car lst) acc))))
  (if (null? rest)
      '()
      (loop rest (list))))
;; end boot-01.scm

;; boot-02.scm
(define-macro (and . x)
  (if (null? x) #t
      (if (null? (cdr x)) (car x)
          (list (quote if) (car x) (cons (quote and) (cdr x))
                #f))))

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list (quote lambda) (list (quote r))
                      (list (quote if) (quote r) (quote r)
                            (cons (quote or) (cdr x))))
                (car x)))))

(define-macro (mes-use-module module)
  #t)

(define-macro (define-module module . rest)
  #t)

;; end boot-02.scm

(primitive-load 0)
