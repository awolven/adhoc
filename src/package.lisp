;; Copyright Andrew K. Wolven 2008, 20021
;; This file is released under the GNU General Public License v3
;; See LICENSE.txt in the main directory for details


(in-package :cl-user)

(cl:defpackage :adhoc
  (:use :cl
	:closer-mop)

  (:shadow :cl
	   #:variable
	   #:the
	   #:defpackage)

  (:shadowing-import-from :closer-mop
			  #:defmethod
			  #:standard-generic-function
			  #:defgeneric
			  #:standard-method
			  #:standard-class)

  #+SBCL
  (:shadowing-import-from :sb-mop
			  #:funcallable-standard-class)

  (:export #:defpackage
	   #:adhoc-class
	   #:defobject
	   #:send
	   #:self
	   #:part
	   #:value
	   #:the
	   #:the-part
	   #:size
	   #:list-elements
	   #:superior
	   #:defobject-amendment
	   #:object
	   #:adhoc-mixin
	   #:slot-variable
	   #:with-variables
	   #:parametric-variable
	   #:component-variable
	   #:get-object-children
	   #:root
	   #:inferior
	   #:component-definition
	   #:aggregate
	   #:indices
	   #:index
	   #:children
	   #:root-path
	   #:without-dependency-capture))

(defmacro adhoc:defpackage (name &rest options)
  (let ((options
	 `((:use :adhoc :closer-mop :cl)

	   (:shadowing-import-from :closer-mop
				   #:defmethod
				   #:standard-generic-function
				   #:defgeneric
				   #:standard-method
				   #:standard-class)
	   
	   (:shadowing-import-from :adhoc
				   #:defpackage
				   #:variable
				   #:the)

	   ,@options)))
    `(cl:defpackage ,name ,@options)))
	   
