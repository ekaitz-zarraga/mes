;;; GNU Mes --- Maxwell Equations of Software
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

(define-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:export (make-regexp
            regexp?

            regexp/icase
            regexp/newline
            regexp/basic
            regexp/extended

            regexp-match?
            match:substring
            match:start
            match:end
            match:prefix
            match:suffix
            match:count
            match:string

            string-match
            regexp-exec
            fold-matches
            list-matches

            regexp-quote))

(include "/home/samplet/Code/3rdparty/mes/wip-gash/mes/module/ice-9/pregexp.upstream.scm")


;;; Patterns

(define-record-type <regexp>
  (tree->regexp tree)
  regexp?
  (tree regexp-tree))

(define (make-regexp pat . flags)
  (if (not (null? flags))
      (error "make-regexp: Flags are not supported" flags))
  (tree->regexp (pregexp pat)))

(define regexp/icase (list 'regexp/icase))
(define regexp/newline (list 'regexp/newline))
(define regexp/basic (list 'regexp/basic))
(define regexp/extended (list 'regexp/extended))


;;; Matches

(define-record-type <regexp-match>
  (make-regexp-match source positions)
  regexp-match?
  (source regexp-match-source)
  (positions regexp-match-positions))

(define* (match:substring m #:optional (n 0))
  (let* ((str (regexp-match-source m))
         (pos (list-ref (regexp-match-positions m) n))
         (start (car pos))
         (end (cdr pos)))
    (substring str start end)))

(define* (match:start m #:optional (n 0))
  (let ((pos (list-ref (regexp-match-positions m) n)))
    (car pos)))

(define* (match:end m #:optional (n 0))
  (let ((pos (list-ref (regexp-match-positions m) n)))
    (cdr pos)))

(define (match:prefix m)
  (let* ((str (regexp-match-source m))
         (pos (car (regexp-match-positions m)))
         (start (car pos)))
    (substring str 0 start)))

(define (match:suffix m)
  (let* ((str (regexp-match-source m))
         (pos (car (regexp-match-positions m)))
         (end (cdr pos)))
    (substring str end)))

(define (match:count m)
  (length (regexp-match-positions m)))

(define (match:string m)
  (regexp-match-source m))


;;; Searching

(define* (string-match pattern str #:optional (start 0))
  (let ((positions (pregexp-match-positions pattern str start)))
    (and positions
         (make-regexp-match str positions))))

(define* (regexp-exec rx str #:optional (start 0) (flags 0))
  (if (not (zero? flags))
      (error "regexp-exec: Flags are not supported" flags))
  (if (not (regexp? rx))
      (error "regexp-exec: Not a regular expression" rx))
  (let ((positions (pregexp-match-positions (regexp-tree rx) str start)))
    (and positions
         (make-regexp-match str positions))))

(define* (fold-matches regexp str init proc #:optional (flags 0))
  (let ((rx (if (regexp? regexp)
                regexp
                (make-regexp regexp))))
    (let loop ((k 0) (acc init))
      (if (< k (string-length str))
          (let ((m (regexp-exec rx str k flags)))
            (if m
                (loop (match:end m) (proc m acc))
                acc))
          acc))))

(define* (list-matches regexp str #:optional (flags 0))
  (reverse (fold-matches regexp str '() cons flags)))


;;; Quoting

(define regexp-quote pregexp-quote)
