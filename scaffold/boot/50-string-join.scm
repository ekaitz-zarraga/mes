;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(cond-expand
 (guile)
 (mes
  (define (cons* . rest)
    (if (null? (cdr rest)) (car rest)
        (cons (car rest) (core:apply cons* (cdr rest) (current-environment)))))

  (define (apply f h . t)
    (if (null? t) (core:apply f h (current-environment))
        (apply f (apply cons* (cons h t)))))

  (define (append . rest)
    (if (null? rest) '()
        (if (null? (cdr rest)) (car rest)
            (append2 (car rest) (apply append (cdr rest))))))

  (define (string . lst)
    (list->string lst))

  (define (map1 f lst)
    (if (null? lst) (list)
        (cons (f (car lst)) (map1 f (cdr lst)))))

  (define map map1)))

(define (string-join lst infix)
  (if (null? (cdr lst)) (car lst)
      (string-append (car lst) infix (string-join (cdr lst) infix))))

(core:display "string-join\n")
(if (string=? (string-join '("foo" "bar") "/") "foo/bar")
    (exit 0))
(exit 1)
