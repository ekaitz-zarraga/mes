;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2018, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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
  (define-macro (include-from-path file)
    (list
     'begin
     (list 'primitive-load
           (if (getenv "srcdest") (string-append (getenv "srcdest") file)
               file))))))

(include-from-path "scaffold/boot/data/i.scm")

(core:display "from-i:")
(core:display from-i)
(core:display "\n")

(core:display "from-i-macro")
(core:display (from-i-macro))
(core:display "\n")
