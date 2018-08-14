;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (mescc preprocess)
  #:use-module (ice-9 optargs)
  #:use-module (system base pmatch)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc lang c99 parser)
  #:use-module (nyacc version)
  #:use-module (mes guile)
  #:export (c99-input->ast))

(format (current-error-port) "*nyacc-version*=~a\n" *nyacc-version*)
;; list of which rules you want progress reported
(define need-progress
  (assoc-ref
   '(("0.85.3" (1 2 3))
     ("0.86.0" (1 2 3)))
   *nyacc-version*))

(define (progress o)
  (when (and o (getenv "NYACC_DEBUG"))
    (display "    :" (current-error-port))
    (display o (current-error-port))
    (display "\n" (current-error-port))))

(define (insert-progress-monitors act-v len-v)
  (let ((n (vector-length act-v)))
    (let loop ((ix 0))
      (when (< ix n)
	(if (memq ix need-progress)
	    (vector-set
	     act-v ix
	     (lambda args
	       (progress (list-ref args (1- (vector-ref len-v ix))))
	       (apply (vector-ref act-v ix) args))))
        (loop (1+ ix))))))

(cond-expand
 (guile
  (insert-progress-monitors (@@ (nyacc lang c99 parser) c99-act-v)
                            (@@ (nyacc lang c99 parser) c99-len-v)))
 (mes
  (insert-progress-monitors c99-act-v c99-len-v)))

(define (logf port string . rest)
  (apply format (cons* port string rest))
  (force-output port)
  #t)

(define (stderr string . rest)
  (apply logf (cons* (current-error-port) string rest)))

(define mes? (pair? (current-module)))

(define* (c99-input->full-ast #:key (prefix "") (defines '()) (includes '()))
  (let ((sys-include (if (equal? prefix "") "include" (string-append prefix "/share/include"))))
    (parse-c99
     #:inc-dirs (append includes (cons* sys-include "include" "lib" (or (and=> (getenv "C_INCLUDE_PATH") (cut string-split <> #\:)) '())))
     #:cpp-defs `(
                  "NULL=0"
                  "__linux__=1"
                  "POSIX=0"
                  "_POSIX_SOURCE=0"
                  "__STDC__=1"
                  "__MESC__=1"
                  ,(if mes? "__MESC_MES__=1" "__MESC_MES__=0")
                  ,@defines)
     #:mode 'code)))

(define* (c99-input->ast #:key (prefix "") (defines '()) (includes '()))
  (stderr "parsing: input\n")
  ((compose ast-strip-const ast-strip-comment) (c99-input->full-ast #:prefix prefix #:defines defines #:includes includes)))

(define (ast-strip-comment o)
  (pmatch o
    ((comment . ,comment) #f)
    (((comment . ,comment) . ,t) (filter-map ast-strip-comment t))
    (((comment . ,comment) . ,cdr) cdr)
    ((,car . (comment . ,comment)) car)
    ((,h . ,t) (if (list? o) (filter-map ast-strip-comment o)
                   (cons (ast-strip-comment h) (ast-strip-comment t))))
    (_  o)))

(define (ast-strip-const o)
  (pmatch o
    ((type-qual ,qual) (if (equal? qual "const") #f o))
    ((pointer (type-qual-list (type-qual ,qual)) . ,rest)
     (if (equal? qual "const") `(pointer ,@rest) o))
    ((decl-spec-list (type-qual ,qual))
     (if (equal? qual "const") #f
         `(decl-spec-list (type-qual ,qual))))
    ((decl-spec-list (type-qual ,qual) . ,rest)
     (if (equal? qual "const") `(decl-spec-list ,@rest)
         `(decl-spec-list (type-qual ,qual) ,@(map ast-strip-const rest))))
    ((decl-spec-list (type-qual-list (type-qual ,qual)) . ,rest)
     (if (equal? qual "const") `(decl-spec-list ,@rest)
         `(decl-spec-list (type-qual-list (type-qual ,qual)) ,@(map ast-strip-const rest))))
    ((,h . ,t) (if (list? o) (filter-map ast-strip-const o)
                   (cons (ast-strip-const h) (ast-strip-const t))))
    (_  o)))
