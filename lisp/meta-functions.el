;;; meta-functions.el --- Define meta-functions to act differently depending on major mode -*- lexical-binding: t; -*-

;; Version: 2.0
;; Author: Ihor Radchenko <yantar92@gmail.com>
;; Created: 10 March 2018
;; Package-Requires: ((dash "2.0"))

;;; Code:
(require 'seq)
(require 'dash)
(require 'cl-lib)


(defun meta-functions--process-args-1 (args &optional recursive)
  "Return normalized arglist.
Add ((sub-body-symbol condition-func|mode-symbol
number-elements-in-args) ...) in front of ARGS."
  (pcase args
    (`(:mode ,(and (pred symbolp) s) . ,_)
     (cons
      (list (make-symbol (concat "%" (symbol-name s)))
	    `(quote ,s)
	    2)
      (meta-functions--process-args-1 (cddr args) 'recursive)))
    (`(:cond :symbol ,(and (pred symbolp) s) ,(and (pred #'functionp) func) . ,_)
     (cons
      (list (make-symbol (concat "%" (symbol-name s)))
	    `(function ,func)
	    4)
      (meta-functions--process-args-1 (cddddr args) 'recursive)))
    (`(:cond :symbol . ,_)
     (error ":cond :symbol must be followed by symbol and function."))
    (`(:cond ,(and (pred functionp) func) . ,_)
     (cons
      (list (when (symbolp func) (make-symbol (concat "%" (symbol-name func))))
	    `(function ,func)
	    2)
      (meta-functions--process-args-1 (cddr args) 'recursive)))
    (`(:cond ,form . ,_)
     (cons
      (list (make-symbol (concat "%lambda" (symbol-name (cl-gensym))))
	    `(function (lambda () ,form))
	    2)
      (meta-functions--process-args-1 (cddr args) 'recursive)))
    (_
     (unless recursive
       (list (list
	      '%t
	      `(function t)
	      0))))))

(defun meta-functions--process-args (name args)
  "Process arguments of `meta-defun'.
The return value is a list (arglist docstring ((sub-body-symbol
condition-func|mode-symbol body) ...))."
  (when (memq ':override-nil args) (setq args (delq ':override-nil args)))
  (let* ((arglist (and (listp (car args)) (pop args)))
	 (docstring (and (stringp (car args)) (pop args)))
	 conditions)
    (while args
      (add-to-list 'conditions
		   (cl-loop for (symbol subcondition num-remove) in (meta-functions--process-args-1 args)
			    with condition = nil
			    with sumsymbol = nil
			    do (when symbol
				 (if sumsymbol
				     (setq sumsymbol
					   (make-symbol (concat (symbol-name sumsymbol)
								"+"
								(symbol-name symbol))))
				   (setq sumsymbol
					 (make-symbol (concat (symbol-name name)
							      (symbol-name symbol))))))
			    do (dotimes (_ num-remove) (pop args))
			    do (push subcondition condition)
			    finally return (cons sumsymbol
						 (nreverse (push (pcase (cl-loop for el = (pop args)
										 collect el
										 until (or (seq-empty-p args) (meta-functions--process-args-1 args 'recursive)))
								   (`(,(and (pred functionp) func)) `(function ,func))
								   (`(,form) `(lambda () (interactive) ,form)))
								 condition))))
		   'append))
    (list arglist docstring conditions)))


(defun meta-functions--generate-docstring (name default-docstring conditions)
  "Generate a docstring for a function define using meta-defun."
  (->>
   (concat "[Meta function.]\n"
	   (when default-docstring default-docstring)
	   "\n"
	   "By default, run `"
	   (format "%s" (cadr (alist-get (make-symbol (concat (symbol-name name) "%t")) conditions nil nil (lambda (a b) (string= (symbol-name a) (symbol-name b))))))
	   "'\n"
	   (mapconcat (lambda (condition)
			(let* ((rcondition (seq-reverse (cdr condition)))
			       (body (car rcondition))
			       (subconditions (seq-reverse (cdr rcondition))))
			  (unless (equal subconditions '(#'t))
			    (concat "When "
				    (mapconcat (lambda (el)
						 (pcase el
						   ('#'t nil)
						   (`(function ,_)
						    (concat "`" (format "%s" el) "' returns 't"))
						   (_
						    (concat "major mode is `" (format "%s" (eval el)) "'"))))
					       subconditions
					       " and ")
				    ", run `"
				    (format "%s" body)
				    "'"))))
		      conditions
		      "\n"))
   (replace-regexp-in-string "#'" "")))

(cl-defmacro meta-defun (name &rest args)
  "Define a meta-function NAME or update its definition."
  (declare (indent defun))
  `(pcase-let ((`(,arglist ,default-docstring ,conditions) (meta-functions--process-args ',name ',args))
	       (old-conditions (function-get ',name 'meta-functions-cond-plist))
	       (old-default-docstring (function-get ',name 'meta-functions-default-docstring)))
     (unless (memq :override-nil ',args)
       (mapc (lambda (el)
	       (setf (alist-get (car el) old-conditions nil nil (lambda (a b) (string= (symbol-name a) (symbol-name b)))) (cdr el)))
	     (reverse conditions))
       (setq conditions old-conditions)
       (unless default-docstring (setq default-docstring old-default-docstring)))
     (mapc (lambda (el)
	     (let ((main-name  ',name)
		   (sub-name (car el))
		   (body (car (reverse el))))
	       (when sub-name
		 (eval `(defun ,sub-name ()
			  (concat "Sub-definition in meta-function `" (symbol-name ',main-name) "'.")
			  ,body)))))
	   conditions)
     (let ((docstring (meta-functions--generate-docstring ',name default-docstring conditions)))
       (eval `(defun ,',name ,arglist
		,docstring
		(interactive)
		(seq-some (lambda (condition)
			    (let* ((rcondition (seq-reverse (cdr condition)))
				   (body (car rcondition))
				   (subconditions (seq-reverse (cdr rcondition))))
			      (when (seq-reduce (lambda (prev el)
						  (and prev
						       (pcase el
							 ('#'t t)
							 (`(function ,_) (eval `(funcall-interactively ,el)))
							 (_ (eq major-mode (eval el))))))
						subconditions
						t)
				(eval `(call-interactively ,body))
				t)))
			  ',conditions))))
     (function-put ',name 'meta-functions-cond-plist conditions)
     (function-put ',name 'meta-functions-default-docstring default-docstring)))

(provide 'meta-functions)

;;; meta-functions.el ends here
