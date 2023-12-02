;;; GNU Mes --- Maxwell Equations of Software
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

(define-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-132)
  #:export (scandir))

(define* (scandir name #:optional (select? (lambda _ #t)) (entry<? string<))
  (let ((dir (opendir name)))
    (let loop ((acc '()))
      (cond
       ((readdir dir)
        => (lambda (n)
             (loop (if (select? n)
                       (cons n acc)
                       acc))))
       (else (closedir dir)
             (list-sort entry<? acc))))))
