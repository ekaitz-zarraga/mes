;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2017,2018,2019,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; srfi-9.scm - records, based on struct.

;;; Code:

;; FIXME: a second use-modules of srfi-9 gives STACK_FULL
;; (define-module (srfi srfi-9)
;;   #:export (define-record-type
;;             make-record-type
;;             record-type?
;;             struct-vtable
;;             record-type-name
;;             record-type-descriptor
;;             record-type-fields
;;             record-predicate
;;             record?
;;             record-constructor
;;             record-accessor
;;             record-modifier))

(define-module (srfi srfi-9))

(define-macro (define-record-type name constructor+field-names predicate . fields)
  (let ((type (make-record-type name (map car fields))))
   `(begin
      (define ,name ,type)
      (define ,(car constructor+field-names) ,(record-constructor type (cdr constructor+field-names)))
      (define ,predicate ,(record-predicate type))
      (define-record-accessors ,type ,@fields))))

(define (make-record-type type fields . printer)
  (let ((printer (if (pair? printer) (car printer))))
    (make-struct '<record-type> (cons type (list fields)) printer)))

(define (record-type? o)
  (eq? (record-type-descriptor o) '<record-type>))

(define (struct-vtable o)
  (record-type-descriptor o))

(define (record-type-name o)
  (struct-ref o 2))

(define (record-type-descriptor o)
  (struct-ref o 0))

(define (record-type-fields o)
  (struct-ref o 3))

(define (record-predicate type)
  (lambda (o)
    (and (record? o)
         (eq? (record-type-descriptor o) type))))

(define (record? o)
  (and (struct? o)
       (record-type? (struct-vtable o))))

(define (record-constructor type . field-names)
  (let* ((fields (record-type-fields type))
         (name (record-type-name type))
         (field-names (if (null? field-names) fields
                          (car field-names))))
    (lambda (. o)
      (if (not (= (length o) (length field-names))) (error "wrong number of arguments for record-constructor")
          (let ((rest (make-list (- (length fields) (length field-names)))))
            (make-struct type (cons name (append o rest)) record-printer))))))

(define record-printer *unspecified*)   ; TODO
(define (record-printer o)
  (display "#<")
  (display (record-type-name o))
  (let* ((vtable (struct-vtable o))
         (fields (record-type-fields vtable)))
    (for-each (lambda (field)
                (display " ")
                (display field)
                (display ": ")
                (display ((record-accessor vtable field) o)))
              fields))
  (display ">"))

(define-macro (define-record-accessors type . fields)
  `(begin
     ,@(map (lambda (field)
              `(define-record-accessor ,type ,field))
            fields)))

(define-macro (define-record-accessor type field)
  `(begin
     (define ,(cadr field) ,(record-accessor type (car field)))
     (if ,(pair? (cddr field))
         (define ,(if (pair? (cddr field)) (caddr field)) ,(record-modifier type (car field))))))

(define (record-accessor type field)
  (let ((i (record-field-index type field)))
    (lambda (o . field?)
      (if (and #f (not (eq? (record-type-descriptor o) type))) (error "record accessor: record expected" type field o)
          (if (pair? field?) field
              (struct-ref o i))))))

(define (record-modifier type field)
  (let ((i (record-field-index type field)))
    (lambda (o v)
      (if (not (eq? (record-type-descriptor o) type)) (error "record modifier: record expected" type field o)
          (struct-set! o i v)))))

(define (record-field-index type field)
  (+ 3 (or (lst-index (record-type-fields type) field)
           (error "no such field" type field))))

(define (lst-index lst o)
  (let loop ((lst lst) (i 0))
    (and (pair? lst)
         (if (eq? o (car lst)) i
             (loop (cdr lst) (1+ i))))))

;; (define-record-type <employee>
;;   (make-employee name age salary)
;;   employee?
;;   (name employe-name)
;;   (age employee-age set-employee-age!)
;;   (salary employee-salary))

;; (display <employee>)
;; (newline)

;; (display make-employee)
;; (newline)
;; (display "employee-age ")
;; (display employee-age)
;; (newline)

;; (display "set-employee-age! ")
;; (display set-employee-age!)
;; (newline)

;; (define janneke (make-employee "janneke" 49 42))
;; (display janneke)
;; (newline)

;; (display (employee-age janneke))
;; (newline)

;; (display (set-employee-age! janneke 33))
;; (newline)
;; (display (employee-age janneke))
;; (newline)
