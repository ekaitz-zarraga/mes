;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Mes.
;;;
;;; Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Mes.  If not, see <http://www.gnu.org/licenses/>.

;; boot-00.scm
(define mes %version)

(define (defined? x)
  (assq x (current-module)))

(define (cond-expand-expander clauses)
  (if (defined? (car (car clauses)))
      (cdr (car clauses))
      (cond-expand-expander (cdr clauses))))

(define-macro (cond-expand . clauses)
  (cons 'begin (cond-expand-expander clauses)))
;; end boot-00.scm

;; boot-01.scm
(define <cell:character> 0)
(define <cell:pair> 7)
(define <cell:string> 10)

(define (pair? x) (eq? (core:type x) <cell:pair>))
(define (not x) (if x #f #t))

(define (display x . rest)
  (if (null? rest) (core:display x)
      (core:display-port x (car rest))))

(define (write x . rest)
  (if (null? rest) (core:write x)
      (core:write-port x (car rest))))

(define (list->string lst)
  (core:make-cell <cell:string> lst 0))

(define (integer->char x)
  (core:make-cell <cell:character> 0 x))

(define (newline . rest)
  (core:display (list->string (list (integer->char 10)))))

(define (string->list s)
  (core:car s))

(define (cadr x) (car (cdr x)))

(define (map1 f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define map map1)

(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (core:apply cons* (cdr rest) (current-module)))))

(define (apply f h . t)
  (if (null? t) (core:apply f h (current-module))
      (apply f (apply cons* (cons h t)))))

(define (append . rest)
  (if (null? rest) '()
      (if (null? (cdr rest)) (car rest)
          (append2 (car rest) (apply append (cdr rest))))))
;; end boot-01.scm

(primitive-load 0)
