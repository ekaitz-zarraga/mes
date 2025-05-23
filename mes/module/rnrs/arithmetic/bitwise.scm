;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

;;; Code:

(define-module (rnrs arithmetic bitwise)
  #:export (bitwise-arithmetic-shift
            bitwise-arithmetic-shift-left
            bitwise-arithmetic-shift-right))

(define bitwise-arithmetic-shift ash)
(define bitwise-arithmetic-shift-left bitwise-arithmetic-shift)
(define (bitwise-arithmetic-shift-right n count)
  (bitwise-arithmetic-shift n (- count)))
