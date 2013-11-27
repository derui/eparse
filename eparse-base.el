;;; -*- coding: utf-8; lexical-binding : t -*-
;;; eparse-base.el --- A base library for eparse.el and eparse-lexer.el
;; 
;; Filename: eparse-base.el
;; Description: A base library for eparse.el and eparse-lexer.el
;; Author: derui
;; Maintainer: derui
;; Copyright (C) 2013, derui, all rights reserved.
;; Created: Nov 13 2013
;; Version: 0.0.0
;; Keywords: parser library
;; Compatibility: 24.3 <=
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; eparse provide some functions to parse and combinator to concatnate
;; functions and construct for it.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cl-lib)

;; Definitions for functions are support to parse and lex.
(defun eplib:successp (v)
  "Check the value if it equal a type of success"
  (and v
       (listp v)
       (eq 'success (car v))))

(defun eplib:failp (v)
  "Check the value if it equal a type of failure"
  (and v
       (listp v)
       (eq 'fail (car v))))

(defun eplib:success-val (v)
  (when (eplib:successp v) (cadr v)))

(defun eplib:fail-val (v)
  (when (eplib:failp v) (cdr v)))

(defun eplib:pull-source (ret)
  "Pull source in a value with `eplib:success'."
  (when (eplib:successp ret)
    (nth 2 ret)))

(defun eplib:success (source val)
  "Return a value which is result of some funciton and consumed source"
  (list 'success val source))

(defun eplib:fail (val)
  "Return fail tag and empty source informations.
If you use it that is returned this funciton, any lexer with fail."
  (cons 'fail val))

(defun eplib:make-pos ()
  "Get initialized source position which is point of the document while parsing."
  '(0 0 0))

(defun eplib:posp (pos)
  (and pos
       (listp pos)
       (= 3 (length pos))
       (numberp (car pos))
       (numberp (cadr pos))
       (numberp (nth 2 pos))))

(defun eplib:pos-position (pos)
  "Get current position at the document. This value is absolute position of it, and it is zero origin."
  (cond ((eplib:posp pos)
         (nth 0 pos))
        (t
         -1)))

(defun eplib:pos-col (pos)
  "Get current column in the given position at the document.
If given argument is not valid position or illegal column, return -1 as error"
  (cond ((eplib:posp pos)
         (nth 1 pos))
        (t
         -1)))

(defun eplib:pos-line (pos)
  "Get current line in the given position at the document.
If given argument is not valid position or illegal line, return -1 as error"
  (cond ((eplib:posp pos)
         (nth 2 pos))
        (t -1)))

(defun eplib:update-pos (pos newer &optional info)
  "Return new position with newer column and line which are updating explicitly as
associate list. Original position is not updating with destructive."
  (if (and pos newer (numberp newer))
      (let* ((col (nth 1 pos))
             (line (nth 2 pos))
             (new-col (and (numberp (cdr (assoc 'col info)))
                           (cdr (assoc 'col info))))
             (new-line (and (numberp (cdr (assoc 'line info)))
                            (cdr (assoc 'line info)))))
        (list newer
              (if new-col new-col col)
              (if new-line new-line line)))
    pos))

(defun eplib:make-source (text)
  "Make and return source structure with initial position and text"
  (cons (eplib:make-pos) text))

(defun eplib:sourcep (source)
  (and source
       (consp source)
       (eplib:posp (car source))
       (stringp (cdr source))))

(defun eplib:source-pos (source)
  "Get position that is current reading point from text from argument."
  (and (eplib:sourcep source)
       (car source)))

(defun eplib:source-text (source)
  "Get text from argument"
  (and (eplib:sourcep source)
       (cdr source)))

(defun eplib:update-source (source pos)
  "Update position of source and return newer it."
  (when (eplib:sourcep source) (eplib:posp pos)
        (cons pos (eplib:source-text source))))

(defun eplib:read-from-source (source readsize)
  "Read text start from current position of the source to readsize.
Note, this function no any effect to `source'. You have to use `eplib:pos-update' and
`eplib:replace-source-pos' if you want to read to read continuous."
  (when (eplib:sourcep source)
    (if (and readsize
             (numberp readsize)
             (<= 0 readsize))
        (let* ((pos (eplib:source-pos source))
               (abs-pos (eplib:pos-position pos)))
          (substring (eplib:source-text source) abs-pos (+ abs-pos readsize)))
      "")))

(defun eplib:either (pred next-func)
  "Return new function to execute given function if pred is failed or successful.
Executing function which is one of function in arguments is took `source' structure,
and expecting it return structure made by `eplib:success' or `eplib:fail'
If execute function returned with eplib:fail, it return a list equal returning from `eplib:fail'."
  (cond ((and (eplib:successp pred)
              (functionp next-func))
         next-func)
        ((and (eplib:failp pred)
              #'(lambda () pred))
         (t
          (throw 'illegal-either-type "Given object is not some either type"))
         ))
  )

(defmacro eplib:define-lexer (name argname &rest body)
  "Define lexer with some features. This macro wrap some function to lexer that is
execute in expanded macro. The body given this macro have to use eplib:fail and eplib:success form that are
used to fail or success lex. In the body can use a variable `source' that initialized by eplib:make-source to lex.
The body must be ended with fail or success with source or data.

This macro provide some utility functions follows to make DSL to define lexer.

<<- size : Read string from source with size, this is alias for eplib:read-from-source.
forward-pos pos : Update position forwarding in the source.
success result : this is alias for eplib:success.
fail result : this is alias for eplib:fail.
"
  (let (ret)
    `(defun ,(intern (concat "eplib:lexer:" (symbol-name name))) (,argname)
       (cl-flet ((<<- (size)
                      (let* ((pos (eplib:source-pos ,argname))
                             (abs-pos (eplib:pos-position pos)))
                        (eplib:read-from-source ,argname size)))
                 (forward-pos (size)
                              (let* ((pos (eplib:source-pos ,argname))
                                     (abs-pos (eplib:pos-position pos)))
                                (setq ,argname
                                      (eplib:update-source ,argname
                                                           (eplib:update-pos pos (+ abs-pos size))))))
                 (success (result) (eplib:success ,argname result))
                 (fail (result) (eplib:fail result))
                 )
         (setq ret ,@body)
         (if (or (eplib:failp ret) (eplib:successp ret))
             ret
           (error "What lexer return success or fail have to behave given body"))
         ))))

(defmacro eplib:lex (&rest body)
  "Make new lexer with some lexers are defined by `eplib:define-lexer' macro.
This macro is the DSL to define it, then some functions already defined and provide in only this macro.
That are follows.

<- lexer  : Execute some lexer with previous source.
"
  `(lambda (source)
     (let ((source (eplib:success source "")))
       (cl-flet ((<- (lexer)
                     (cl-letf ((lexer (intern (concat "eplib:lexer:" (symbol-name lexer)))))
                       (cond ((eplib:successp source)
                              (cl-letf ((f (eplib:either source lexer)))
                                (let ((ret (funcall f (eplib:pull-source source))))
                                  (cond ((eplib:successp ret)
                                         (setq source ret)
                                         (eplib:success-val ret))
                                        ((eplib:failp ret)
                                         (setq source ret)
                                         (eplib:fail-val ret))))))
                             ((eplib:failp source)
                              (eplib:fail-val source))))
                     )
                 (success (result) (eplib:success source result))
                 (fail (result) (eplib:fail result))
                 )
         (let ((ret ,@body))
           (cond ((eplib:successp ret)
                  (eplib:success-val ret))
                 ((eplib:failp ret)
                  (eplib:fail-val ret)))))
       )))

(defun eplib:bind (fst snd)
  "Return new function what combine `fst' to `snd'.
Notice, arguments of this function has to be able to give only one argument, because
the result of `fst' executed take to argument of `snd' without change.
If without function or other give this, return id function that is defined `identity'"
  (if (and (functionp fst) (functionp snd))
      #'(lambda (arg)
          (funcall snd (funcall fst arg)))
    'identity))

(provide 'eparse-base)
