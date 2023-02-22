;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (srfi srfi-14)
  #:re-export (char-set
               char-set?
               char-set=
               char-set:whitespace
               char-set:digit
               char-set:upper-case
               list->char-set
               string->char-set
               string->char-set!
               char-set-adjoin
               char-set-contains?
               char-set-complement
               char-whitespace?
               char-set-copy
               char-upcase
               char-downcase))
