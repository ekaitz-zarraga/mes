;;; GNU MES --- Maxwell Equations of Software
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU MES.
;;;
;;; GNU MES is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU MES is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU MES.  If not, see <http://www.gnu.org/licenses/>.

(define (x x1 x2)
  (define b 1)
  (define b1 1)
  (define b2 1)
  (define (y) b)
  (define (y1) b)
  (define (y2) b)
  (set! b 0)
  (list b (y)))

(core:display "x:")
(core:display x)
(core:display "\n")
(core:display (x 1 2))
(core:display "\n")
