;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2022 Timothy Sample <samplet@ngyro.com>
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

;;; SRFI 1: List Library.  This module provides procedures for
;;; constructing, searching, and manipulating lists.

;;; Code:

(define-module (srfi srfi-1)
  #:re-export (append-reverse
               reverse
               reverse!)
  #:replace (member
             iota
             filter
             srfi-1:member)
  #:export (every
            find
            append-map
            filter-map
            fold
            fold-right
            unfold
            remove
            mes:member
            mes:iota
            srfi-1:iota
            delete-duplicates
            any
            any1
            every
            every1
            list-index
            lset-union
            lset-intersection
            lset-difference
            take-while

            drop
            drop-while
            partition))

(include-from-path "srfi/srfi-1.mes")

(define drop list-tail)

(define (drop-while pred lst)
  (let loop ((lst lst))
    (if (null? lst)
        '()
        (if (pred (car lst))
            (loop (cdr lst))
            lst))))

(define (partition pred lst)
  (let loop ((lst lst) (yeas '()) (nays '()))
    (if (null? lst)
        (values (reverse yeas) (reverse nays))
        (let ((x (car lst)))
          (if (pred x)
              (loop (cdr lst) (cons x yeas) nays)
              (loop (cdr lst) yeas (cons x nays)))))))
