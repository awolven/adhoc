;; Copyright Andrew K. Wolven 2008, 20021
;; This file is released under the GNU General Public License v3
;; See LICENSE.txt in the main directory for details

(in-package :adhoc)

(defvar self nil)

(define-symbol-macro +slot-unbound+ #+sbcl sb-pcl::+slot-unbound+ #+ccl (ccl::%slot-unbound-marker)
				    #+allegro `excl::..slot-unbound..)

#+CCL
(defmacro standard-instance-access-compat (instance location)
  `(handler-case (ccl::standard-instance-instance-location-access ,instance ,location)
    (error () +slot-unbound+)))

#+sbcl
(defmacro standard-instance-access-compat (instance location)
  `(standard-instance-access ,instance ,location))

#+allegro
(defmacro standard-instance-access-compat (instance location)
  `(mop:standard-instance-access ,instance ,location))

#+ccl
(defmacro named-lambda (name (&rest arglist) &body body)
  `(ccl::nlambda ,name (,@arglist)
     ,@body))

#+sbcl
(defmacro named-lambda (name (&rest arglist) &body body)
  `(sb-int::named-lambda ,name (,@arglist)
     ,@body))

#+allegro
(defmacro named-lambda (name (&rest arglist) &body body)
  `(excl::named-function ,name (lambda (,@arglist)
				 ,@body)))

(defparameter +slotd-class-slot-name+ #+ccl 'ccl::CLASS #+sbcl 'sb-pcl::%class #+allegro 'excl::class)

(defmacro send (object &rest messages)
  (if (null messages)
      object
      (let ((message (car messages)))
	(if (consp message)
	    
	    (if (eq (car message) 'eval)
		
		(let ((form (cadr message))
		      (form-sym (gensym "MSG")))
		  
		  `(let ((,form-sym ,form))
		     (if (consp ,form-sym)

			 (send
			  
			  (apply (slot-value ,object (car ,form-sym))
				 (cdr ,form-sym))
			  
			  ,@(cdr messages))
			 
			 (send
			  
			  (slot-value ,object ,form-sym)
			  
			  ,@(cdr messages)))))
		
		`(send
		  
		  (funcall (slot-value ,object ',(car message))
			   ,@(cdr message))
		  
		  ,@(cdr messages)))
	    
	    `(send

	      (slot-value ,object ',(car messages))

	      ,@(cdr messages))))))

(defmacro the (&rest messages)
  `(send self ,@messages))

(defclass virtual-slot-definition-mixin ()
  ())

(defmethod slot-definition-allocation ((slotd virtual-slot-definition-mixin))
  :none)

(defmethod shared-initialize :after ((instance virtual-slot-definition-mixin) slot-names &rest initargs
				     &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (values))

(defclass direct-virtual-slot-definition-mixin (virtual-slot-definition-mixin)
  ((getter :initarg :getter)
   (setter :initarg :setter)))

(defclass effective-virtual-slot-definition-mixin (virtual-slot-definition-mixin)
  ((getter)
   (setter)))

(defclass direct-ordinary-virtual-slot-definition (direct-virtual-slot-definition-mixin
						   standard-direct-slot-definition)
  ())

(defclass effective-ordinary-virtual-slot-definition (effective-virtual-slot-definition-mixin
						      standard-effective-slot-definition)
  ())

(defun compute-getter-emfun (dslotds)
  (when dslotds
    (let ((next-emfun (compute-getter-emfun (cdr dslotds)))
	  (this-emfun (slot-value (car dslotds) 'getter)))
      #'(lambda (object)
	  (funcall this-emfun next-emfun object)))))

(defun compute-setter-emfun (dslotds)
  (when dslotds
    (let ((next-emfun (compute-setter-emfun (cdr dslotds)))
	  (this-emfun (slot-value (car dslotds) 'setter)))
      #'(lambda (object value)
	  (funcall this-emfun next-emfun object value)))))

(defclass basic-attribute-definition-mixin () ())

(defclass eager-attribute-definition-mixin () ())

(defclass direct-basic-attribute-definition-mixin
    (basic-attribute-definition-mixin)
  ())



(defclass effective-basic-attribute-definition-mixin
    (basic-attribute-definition-mixin)
  ((shadow :accessor %shadow)))



(defclass settable-slot-definition-mixin ()
  ((noticers :accessor noticers :initform nil)))

(defclass direct-settable-slot-definition-mixin (settable-slot-definition-mixin)
  ())

(defclass effective-settable-slot-definition-mixin (settable-slot-definition-mixin)
  ())

(defclass non-settable-slot-definition-mixin () ())

(defclass direct-non-settable-slot-definition-mixin (non-settable-slot-definition-mixin)
  ())

(defclass effective-non-settable-slot-definition-mixin (non-settable-slot-definition-mixin)
  ())

(defclass input-definition-mixin ()
  ())

(defclass visual-mixin () ())

;; this is for adhoc class serialization
(defmethod slot-class-keyword ((slotd input-definition-mixin))
  :inputs)

(defmethod slot-class-keyword ((dslotd standard-direct-slot-definition))
  :slots)

(defclass direct-input-definition-mixin (input-definition-mixin
					 direct-basic-attribute-definition-mixin)
  ())



(defclass effective-input-definition-mixin (input-definition-mixin
					    effective-basic-attribute-definition-mixin)
  ((status :accessor input-status :initform nil)))
  

(defclass attribute-function-mixin ()
  ((attribute-function :accessor attribute-function)))

(defclass ordinary-input-definition-mixin (input-definition-mixin)
  ())

;; attribute-body is for adhoc-class serialization
(defclass direct-attribute-function-mixin (attribute-function-mixin
					   direct-basic-attribute-definition-mixin)
  ((attribute-body :accessor attribute-body)))

(defclass effective-attribute-function-mixin (attribute-function-mixin
					      effective-basic-attribute-definition-mixin)
  ())

(defclass defaulting-attribute-mixin () ())

(defclass defaulting-ordinary-input-definition-mixin (defaulting-attribute-mixin
						      ordinary-input-definition-mixin)
  ())

(defclass direct-ordinary-input-definition-mixin (direct-input-definition-mixin) ())

(defmethod attribute-function ((dslotd direct-ordinary-input-definition-mixin))
  nil)

(defclass effective-ordinary-input-definition-mixin (effective-input-definition-mixin) ())


(defclass direct-ordinary-input-definition (direct-settable-slot-definition-mixin
					    direct-ordinary-input-definition-mixin
					    standard-direct-slot-definition)
  ())

(defclass direct-visual-ordinary-input-definition (visual-mixin
						   direct-ordinary-input-definition)
  ())


;; this method is for adhoc-class serialization
(defmethod defining-expression ((slotd direct-ordinary-input-definition))
  (let ((slot-definition-name (slot-definition-name slotd)))
    (if (find slot-definition-name
	      (direct-descending-attributes
	       (slot-value slotd +slotd-class-slot-name+)))
	(list slot-definition-name :descending))
    (if (noticers slotd)
	(list* slot-definition-name (mapcar #'(lambda (noticer)
						(list* :noticer (second noticer)))
					    (noticers slotd)))
	slot-definition-name)))

(defclass effective-ordinary-input-definition (effective-settable-slot-definition-mixin
					       effective-ordinary-input-definition-mixin
					       standard-effective-slot-definition)
  ())

(defclass effective-visual-ordinary-input-definition (visual-mixin
						      effective-ordinary-input-definition)
  ())

(defclass direct-defaulting-ordinary-input-definition (defaulting-ordinary-input-definition-mixin
						       direct-settable-slot-definition-mixin
						       direct-ordinary-input-definition-mixin
						       standard-direct-slot-definition)
  ())

(defclass direct-visual-defaulting-ordinary-input-definition
    (visual-mixin
     direct-defaulting-ordinary-input-definition)
  ())

;; this method is for adhoc-class serialization
(defmethod defining-expression ((slotd direct-defaulting-ordinary-input-definition))
  (let ((slot-definition-name (slot-definition-name slotd)))
    (list* slot-definition-name
	   (append (list* :defaulting
			  (when (find slot-definition-name
				      (direct-descending-attributes
				       (slot-value slotd +slotd-class-slot-name+)))
			    (list :descending)))
		   (when (noticers slotd)
		     (mapcar #'(lambda (noticer)
				 (list* :noticer (second noticer)))
			     (noticers slotd)))))))

(defclass effective-defaulting-ordinary-input-definition
    (defaulting-ordinary-input-definition-mixin
     effective-settable-slot-definition-mixin
     effective-ordinary-input-definition-mixin
     standard-effective-slot-definition)
  ())

(defclass effective-visual-defaulting-ordinary-input-definition
    (visual-mixin
     effective-defaulting-ordinary-input-definition)
  ())

(defclass optional-input-definition-mixin (attribute-function-mixin) ())

(defclass defaulting-optional-input-definition-mixin (defaulting-attribute-mixin
						      optional-input-definition-mixin)
  ())

(defclass eager-optional-input-definition-mixin (eager-attribute-definition-mixin
						 optional-input-definition-mixin)
  ())


(defclass direct-optional-input-definition (direct-settable-slot-definition-mixin
					    direct-input-definition-mixin
					    direct-attribute-function-mixin
					    optional-input-definition-mixin
					    standard-direct-slot-definition)
  ())

(defclass direct-visual-optional-input-definition (visual-mixin
						   direct-optional-input-definition)
  ())

(defclass direct-eager-optional-input-definition (direct-settable-slot-definition-mixin
						  direct-input-definition-mixin
						  direct-attribute-function-mixin
						  eager-optional-input-definition-mixin
						  standard-direct-slot-definition)
  ())

(defclass direct-visual-eager-optional-input-definition (visual-mixin
							 direct-eager-optional-input-definition)
  ())

;; serialization
(defmethod defining-expression ((slotd direct-optional-input-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(body (attribute-body slotd)))
    (append (list slot-definition-name)
	    (when (noticers slotd)
	      (mapcar #'(lambda (noticer)
			  (list* :noticer (second noticer)))
		      (noticers slotd)))
	    (if (find slot-definition-name
		      (direct-descending-attributes
		       (slot-value slotd +slotd-class-slot-name+)))
		(list* :descending body)
		body))))

(defclass effective-optional-input-definition (optional-input-definition-mixin
					       effective-settable-slot-definition-mixin
					       effective-input-definition-mixin
					       effective-attribute-function-mixin
					       standard-effective-slot-definition)
  ())

(defclass effective-visual-optional-input-definition (visual-mixin
						      effective-optional-input-definition)
  ())

(defclass effective-eager-optional-input-definition (eager-optional-input-definition-mixin
						     effective-settable-slot-definition-mixin
						     effective-input-definition-mixin
						     effective-attribute-function-mixin
						     standard-effective-slot-definition)
  ())

(defclass effective-visual-eager-optional-input-definition (visual-mixin
							    effective-eager-optional-input-definition)
  ())

(defclass direct-defaulting-optional-input-definition (defaulting-optional-input-definition-mixin
						       direct-settable-slot-definition-mixin
						       direct-input-definition-mixin
						       direct-attribute-function-mixin
						       standard-direct-slot-definition)
  ())

(defclass direct-visual-defaulting-optional-input-definition (visual-mixin
							      direct-defaulting-optional-input-definition)
  ())


;; serialization
(defmethod defining-expression ((slotd direct-defaulting-optional-input-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(body (attribute-body slotd)))
    (append (list slot-definition-name :defaulting)
	    (when (noticers slotd)
	      (mapcar #'(lambda (noticer)
			  (list* :noticer (second noticer)))
		      (noticers slotd)))
	    (if (find slot-definition-name
		      (direct-descending-attributes
		       (slot-value slotd +slotd-class-slot-name+)))
		(list* :descending body)
		body))))	   

(defclass effective-defaulting-optional-input-definition (effective-settable-slot-definition-mixin
							  defaulting-optional-input-definition-mixin
							  effective-input-definition-mixin
							  effective-attribute-function-mixin
							  standard-effective-slot-definition)
  ())

(defclass effective-visual-defaulting-optional-input-definition
    (visual-mixin
     effective-defaulting-optional-input-definition)
  ())

(defclass parametric-variable-slot-definition-mixin () ())

(defclass direct-parametric-slot-definition (parametric-variable-slot-definition-mixin
					     direct-settable-slot-definition-mixin
					     direct-attribute-function-mixin
					     standard-direct-slot-definition)
  ())

(defclass effective-parametric-slot-definition (parametric-variable-slot-definition-mixin
						effective-settable-slot-definition-mixin
						effective-attribute-function-mixin
						standard-effective-slot-definition)
  ())

;; serialization
(defmethod slot-class-keyword ((slotd basic-attribute-definition-mixin))
  :attributes)

(defclass direct-ordinary-attribute-definition-mixin (direct-attribute-function-mixin)
  ())

(defclass direct-global-matrix-attribute-definition (eager-attribute-definition-mixin
						     direct-ordinary-attribute-definition-mixin
						     standard-direct-slot-definition)
  ())

(defclass direct-eager-ordinary-attribute-definition-mixin (eager-attribute-definition-mixin
							    direct-ordinary-attribute-definition-mixin)
  ())

(defclass direct-ordinary-attribute-definition (direct-ordinary-attribute-definition-mixin
						standard-direct-slot-definition)
  ())

(defclass direct-eager-ordinary-attribute-definition (eager-attribute-definition-mixin
						      direct-ordinary-attribute-definition)
  ())

(defclass effective-ordinary-attribute-definition-mixin (effective-non-settable-slot-definition-mixin
							 effective-attribute-function-mixin)
  ())

(defclass effective-eager-ordinary-attribute-definition-mixin
    (eager-attribute-definition-mixin
     effective-ordinary-attribute-definition-mixin)
  ())

(defclass effective-global-matrix-attribute-definition
    (effective-eager-ordinary-attribute-definition-mixin
     standard-effective-slot-definition)
  ())

(defclass effective-ordinary-attribute-definition (effective-ordinary-attribute-definition-mixin
						   standard-effective-slot-definition)
  ())

(defclass effective-eager-ordinary-attribute-definition (effective-eager-ordinary-attribute-definition-mixin
							 standard-effective-slot-definition)
  
  ())

;; serialization
(defmethod defining-expression ((slotd direct-ordinary-attribute-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(body (attribute-body slotd)))
    (list* slot-definition-name (if (find slot-definition-name
					  (direct-descending-attributes
					   (slot-value slotd +slotd-class-slot-name+)))
				    (list* :descending body)
				    body))))

(defclass direct-modifiable-attribute-definition (direct-settable-slot-definition-mixin
						  direct-attribute-function-mixin
						  standard-direct-slot-definition)
  ())

(defclass direct-visual-modifiable-attribute-definition
    (visual-mixin
     direct-modifiable-attribute-definition)
  ())  

;; serialization
(defmethod defining-expression ((slotd direct-modifiable-attribute-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(body (attribute-body slotd)))
    (append (list slot-definition-name :modifiable)
	    (when (noticers slotd)
	      (mapcar #'(lambda (noticer)
			  (list* :noticer (second noticer)))
		      (noticers slotd)))
	    (if (find slot-definition-name
		      (direct-descending-attributes
		       (slot-value slotd +slotd-class-slot-name+)))
		(list* :descending body)
		body))))

(defclass effective-modifiable-attribute-definition (effective-settable-slot-definition-mixin
						     effective-attribute-function-mixin
						     standard-effective-slot-definition)
  ())

(defclass effective-visual-modifiable-attribute-definition
    (visual-mixin
     effective-modifiable-attribute-definition)
  ())

(defclass uncached-attribute-mixin () ())

(defmethod slot-definition-allocation ((slotd uncached-attribute-mixin))
  :none)

(defclass direct-uncached-attribute-definition (uncached-attribute-mixin
						direct-attribute-function-mixin
						standard-direct-slot-definition)
  ())

;; serialization
(defmethod defining-expression ((slotd direct-uncached-attribute-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(body (attribute-body slotd)))
    (list* slot-definition-name :uncached (if (find slot-definition-name
						    (direct-descending-attributes
						     (slot-value slotd +slotd-class-slot-name+)))
					      (list* :descending body)
					      body))))

(defclass effective-uncached-attribute-definition (effective-non-settable-slot-definition-mixin
						   uncached-attribute-mixin
						   effective-attribute-function-mixin
						   standard-effective-slot-definition)
  ())

(defclass component-definition-mixin ()
  ((provided-inputs)
   (class-spec-function :accessor class-spec-function)
   (provided-inputs-source)
   (hidden? :accessor slot-hidden? :initform nil :initarg :hidden?)))

(defmethod attribute-function ((slotd component-definition-mixin))
  nil)

(defclass aggregate-component-definition-mixin (component-definition-mixin)
  ())

(defclass array-aggregate-component-definition-mixin (aggregate-component-definition-mixin)
  ((size-function :accessor size-function)))

(defclass table-aggregate-component-definition-mixin (aggregate-component-definition-mixin)
  ((indices-function :accessor indices-function)))

;; serialization
(defmethod slot-class-keyword ((slotd component-definition-mixin))
  (if (slot-value slotd 'hidden?)
      :hidden-components
      :components))

;; serialization
(defmethod type-function ((slotd component-definition-mixin) &rest indices)
  (declare (ignore indices))
  (class-spec-function slotd))

;; helper method
(defmethod provided-inputs ((slotd component-definition-mixin) &rest indices)
  (declare (ignore indices))
  (slot-value slotd 'provided-inputs))

;; helper method
(defmethod (setf provided-inputs) (inputs (slotd component-definition-mixin) &rest indices)
  (declare (ignore indices))
  (setf (slot-value slotd 'provided-inputs) inputs))

(defclass direct-component-definition-mixin (direct-non-settable-slot-definition-mixin
					     direct-basic-attribute-definition-mixin)
  ((type-expression :accessor component-type-expression)))

(defclass direct-eager-component-definition-mixin (eager-attribute-definition-mixin
						   direct-component-definition-mixin)
  ())

(defclass effective-component-definition-mixin (effective-non-settable-slot-definition-mixin
						effective-basic-attribute-definition-mixin)
  ())

(defclass effective-eager-component-definition-mixin (eager-attribute-definition-mixin
						      effective-component-definition-mixin)
  ())

(defclass ordinary-component-definition-mixin (component-definition-mixin)
  ())

(defclass direct-ordinary-component-definition (ordinary-component-definition-mixin
						direct-component-definition-mixin
						standard-direct-slot-definition)
  ())

(defclass direct-eager-ordinary-component-definition (ordinary-component-definition-mixin
						      direct-eager-component-definition-mixin
						      standard-direct-slot-definition)
  ())

;; serialization
(defmethod defining-expression ((slotd direct-ordinary-component-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(type-expression (slot-value slotd 'type-expression))
	(provided-inputs (slot-value slotd 'provided-inputs-source)))
    (list* slot-definition-name :type type-expression
	   provided-inputs)))

(defclass effective-ordinary-component-definition (ordinary-component-definition-mixin
						   effective-component-definition-mixin
						   standard-effective-slot-definition)
  ())

(defclass effective-eager-ordinary-component-definition (ordinary-component-definition-mixin
							 effective-eager-component-definition-mixin
							 standard-effective-slot-definition)
  ())

(defclass direct-array-aggregate-component-definition-mixin (array-aggregate-component-definition-mixin
							     direct-component-definition-mixin
							     direct-non-settable-slot-definition-mixin)
  ((size-expression :accessor size-expression)))

(defclass direct-eager-array-aggregate-component-definition-mixin (eager-attribute-definition-mixin
								   direct-array-aggregate-component-definition-mixin)
  ())

(defclass direct-table-aggregate-component-definition-mixin (table-aggregate-component-definition-mixin
							     direct-component-definition-mixin
							     direct-non-settable-slot-definition-mixin)
  ((indices-expression :accessor indices-expression)))

(defclass direct-eager-table-aggregate-component-definition-mixin
    (eager-attribute-definition-mixin
     direct-table-aggregate-component-definition-mixin)
  ())  

(defclass effective-array-aggregate-component-definition-mixin (array-aggregate-component-definition-mixin
								effective-component-definition-mixin
								effective-non-settable-slot-definition-mixin)
  ())

(defclass effective-eager-array-aggregate-component-definition-mixin
    (eager-attribute-definition-mixin
     effective-array-aggregate-component-definition-mixin)
  ())

(defclass effective-table-aggregate-component-definition-mixin (table-aggregate-component-definition-mixin
								effective-component-definition-mixin
								effective-non-settable-slot-definition-mixin)
  ())

(defclass effective-eager-table-aggregate-component-definition-mixin
    (eager-attribute-definition-mixin
     effective-table-aggregate-component-definition-mixin)
  ())

(defclass direct-array-aggregate-component-definition (direct-array-aggregate-component-definition-mixin
						       standard-direct-slot-definition)
  ())

(defclass direct-eager-array-aggregate-component-definition
    (direct-eager-array-aggregate-component-definition-mixin
     standard-direct-slot-definition)
  ())

(defmethod defining-expression ((slotd direct-array-aggregate-component-definition))
  (let ((slot-definition-name (slot-definition-name slotd))
	(type-expression (slot-value slotd 'type-expression))
	(size-expression (slot-value slotd 'size-expression))
	(provided-inputs-expression (slot-value slotd 'provided-inputs-source)))
    (list* slot-definition-name
	   :type type-expression
	   :aggregate (list :size size-expression)
	   provided-inputs-expression)))    

(defclass direct-table-aggregate-component-definition (direct-table-aggregate-component-definition-mixin
						       standard-direct-slot-definition)
  ())

(defclass direct-eager-table-aggregate-component-definition
    (direct-eager-table-aggregate-component-definition-mixin
     standard-direct-slot-definition)
  ())

(defclass effective-array-aggregate-component-definition (effective-array-aggregate-component-definition-mixin
							  standard-effective-slot-definition)
  ())

(defclass effective-eager-array-aggregate-component-definition
    (effective-eager-array-aggregate-component-definition-mixin
     standard-effective-slot-definition)
  ())

(defclass effective-table-aggregate-component-definition (effective-table-aggregate-component-definition-mixin
							  standard-effective-slot-definition)
  ())

(defclass effective-eager-table-aggregate-component-definition
    (effective-eager-table-aggregate-component-definition-mixin
     standard-effective-slot-definition)
  ())

(defclass direct-maintained-slot-definition (direct-settable-slot-definition-mixin
					     basic-attribute-definition-mixin
					     standard-direct-slot-definition)
  ())

(defclass effective-maintained-slot-definition (effective-settable-slot-definition-mixin
						basic-attribute-definition-mixin
						standard-effective-slot-definition)
  ())

(defmacro with-cnm-support (attribute-name attribute-method-function-lambda)
  (let ((next-method-arg (gensym "NEXT-METHOD"))
	(self-arg (gensym "SELF-")))
    `(named-lambda ,attribute-name (,next-method-arg ,self-arg)
       (declare (ignorable ,next-method-arg))
       (flet ((next-method-p ()
		(not (null ,next-method-arg))))
	 (declare (ignorable (function next-method-p)))
	 (flet ((call-next-method ()
		  (if (next-method-p)
		      (funcall ,next-method-arg ,self-arg)
		      (error "No next method for the attribute ~S." ',attribute-name))))
	   (declare (ignorable (function call-next-method)))
	   (funcall ,attribute-method-function-lambda ,self-arg))))))

(defmacro with-setter-cnm-support (attribute-name attribute-method-function-lambda)
  (let ((next-method-arg (gensym "NEXT-METHOD"))
	(self-arg (gensym "SELF-"))
	(value-arg (gensym "VALUE-")))
    `(named-lambda ,attribute-name (,next-method-arg ,self-arg ,value-arg)
       (declare (ignorable ,next-method-arg))
       (flet ((next-method-p ()
		(not (null ,next-method-arg))))
	 (declare (ignorable (function next-method-p)))
	 (flet ((call-next-method ()
		  (if (next-method-p)
		      (funcall ,next-method-arg ,self-arg ,value-arg)
		      (error "No next method for the attribute ~S." ',attribute-name))))
	   (declare (ignorable (function call-next-method)))
	   (funcall ,attribute-method-function-lambda ,self-arg ,value-arg))))))

(defun compute-emfun (dslotds)
  (when dslotds
    (let ((attribute-method-function (attribute-function (car dslotds))))
      (when attribute-method-function
	(let ((next-emfun (compute-emfun (cdr dslotds))))
	  #'(lambda (object)
	      (funcall attribute-method-function next-emfun object)))))))

(defclass adhoc-class (standard-class)
  ((direct-descending-attributes :initform nil :reader direct-descending-attributes :initarg :direct-descending-attributes)
   (effective-descending-attributes :initform nil :accessor effective-descending-attributes)
   (component-eslotds :initform nil :accessor component-eslotds)
   (hidden-component-eslotds :initform nil :accessor hidden-component-eslotds)
   (slot-locations :initform (make-hash-table) :accessor slot-locations)
   (children-dependents :initform nil :accessor children-dependents)))

(defmethod validate-superclass ((c1 adhoc-class) (c2 standard-class))
  t)

(defclass funcallable-adhoc-class (funcallable-standard-class)
  ())

(defmethod validate-superclass ((c1 funcallable-adhoc-class) (c2 funcallable-standard-class))
  t)

(defun finalize-inheritance-lite (adhoc-class)
  #+sbcl(sb-pcl::update-class adhoc-class t)
  #+ccl(let ((ccl::*update-slots-preserve-existing-wrapper* t))
	 (ccl::update-class adhoc-class t))
  ;;#+allegro(excl::update-class-and-subclasses adhoc-class t)
  (adhoc-class-finalize-inheritance-after adhoc-class))

(defun adhoc-class-finalize-inheritance-after (class)
  (let* ((adhoc-classes (remove-if-not #'(lambda (class)
					   (typep class 'adhoc-class))
				       (#-allegro class-precedence-list
					#+allegro compute-class-precedence-list
					class)))
	 (all-descending-attributes (apply #'append (mapcar #'direct-descending-attributes adhoc-classes))))

    (setf (effective-descending-attributes class) (remove-duplicates all-descending-attributes))

    (let ((components ())
	  (hidden ()))
      
      (loop for eslotd in (class-slots class)
	    do
	       (setf (gethash (slot-definition-name eslotd) (slot-locations class))
		     (slot-definition-location eslotd))

	   (when (typep eslotd 'effective-component-definition-mixin)
	     (if (slot-hidden? eslotd)
		 (push eslotd hidden)
		 (push eslotd components)))
	   
	 finally (setf (component-eslotds class) (nreverse components)
		       (hidden-component-eslotds class) (nreverse hidden))))
    (values)))
    

(defmethod finalize-inheritance :after ((class adhoc-class))
  (adhoc-class-finalize-inheritance-after class))



(defmethod direct-slot-definition-class ((class adhoc-class) &rest initargs)
  (case (getf initargs :slot-class)
    (:virtual (load-time-value (find-class 'direct-ordinary-virtual-slot-definition)))
    (:visual-ordinary-input (load-time-value (find-class 'direct-visual-ordinary-input-definition)))
    (:ordinary-input (load-time-value (find-class 'direct-ordinary-input-definition)))
    (:parameter (load-time-value (find-class 'direct-parametric-slot-definition)))
    (:visual-defaulting-ordinary-input
     (load-time-value (find-class 'direct-visual-defaulting-ordinary-input-definition)))
    (:defaulting-ordinary-input (load-time-value (find-class 'direct-defaulting-ordinary-input-definition)))
    (:visual-optional-input (load-time-value (find-class 'direct-visual-optional-input-definition)))
    (:optional-input (load-time-value (find-class 'direct-optional-input-definition)))
    (:visual-defaulting-optional-input
     (load-time-value (find-class 'direct-visual-defaulting-optional-input-definition)))
    (:defaulting-optional-input (load-time-value (find-class 'direct-defaulting-optional-input-definition)))
    (:visual-eager-optional-input
     (load-time-value (find-class 'direct-visual-eager-optional-input-definition)))
    (:eager-optional-input (load-time-value (find-class 'direct-eager-optional-input-definition)))
    (:ordinary-attribute (load-time-value (find-class 'direct-ordinary-attribute-definition)))
    (:visual-modifiable-attribute
     (load-time-value (find-class 'direct-visual-modifiable-attribute-definition)))
    (:modifiable-attribute (load-time-value (find-class 'direct-modifiable-attribute-definition)))
    (:uncached-attribute (load-time-value (find-class 'direct-uncached-attribute-definition)))
    (:global-matrix-attribute (load-time-value (find-class 'direct-global-matrix-attribute-definition)))
    (:eager-ordinary-attribute (load-time-value (find-class 'direct-eager-ordinary-attribute-definition)))
    (:ordinary-component (load-time-value (find-class 'direct-ordinary-component-definition)))
    (:eager-ordinary-component (load-time-value (find-class 'direct-eager-ordinary-component-definition)))
    (:array-aggregate (load-time-value (find-class 'direct-array-aggregate-component-definition)))
    (:eager-array-aggregate (load-time-value (find-class 'direct-eager-array-aggregate-component-definition)))
    (:table-aggregate (load-time-value (find-class 'direct-table-aggregate-component-definition)))
    (:eager-table-aggregate (load-time-value (find-class 'direct-eager-table-aggregate-component-definition)))
    ;;(t (load-time-value (find-class 'direct-maintained-slot-definition)))
    (t (call-next-method))
    ))

(defmethod direct-slot-definition-class ((class funcallable-adhoc-class) &rest initargs)
  (case (getf initargs :slot-class)
    (:ordinary-attribute
     (load-time-value (find-class 'direct-ordinary-attribute-definition)))
    (t (call-next-method))))     

;; I think I have this method to allow slot definition metaobjects to take additional keyword arguments
(defmethod shared-initialize :after ((instance basic-attribute-definition-mixin) slot-names &rest initargs
			      &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (values))

(defmethod effective-slot-definition-class ((class adhoc-class) &rest initargs)
  (let* ((name (getf initargs :name))
	 (dslotd (loop for c in (compute-class-precedence-list class) ;; was (class-precedence-list class)
		    do (let ((dslotd (find name (class-direct-slots c)
					   :key #'slot-definition-name :test #'eq)))
			 (when dslotd (return dslotd))))))
    (typecase dslotd
      (direct-ordinary-virtual-slot-definition
       (load-time-value (find-class 'effective-ordinary-virtual-slot-definition)))
      (direct-visual-ordinary-input-definition
       (load-time-value (find-class 'effective-visual-ordinary-input-definition)))
      (direct-ordinary-input-definition (load-time-value (find-class 'effective-ordinary-input-definition)))
      (direct-parametric-slot-definition (load-time-value (find-class 'effective-parametric-slot-definition)))
      (direct-visual-eager-optional-input-definition
       (load-time-value (find-class 'effective-visual-eager-optional-input-definition)))
      (direct-eager-optional-input-definition (load-time-value (find-class 'effective-eager-optional-input-definition)))
      (direct-visual-defaulting-ordinary-input-definition
       (load-time-value (find-class 'effective-visual-defaulting-ordinary-input-definition)))
      (direct-defaulting-ordinary-input-definition (load-time-value (find-class 'effective-defaulting-ordinary-input-definition)))
      (direct-visual-optional-input-definition
       (load-time-value (find-class 'effective-visual-optional-input-definition)))
      (direct-optional-input-definition (load-time-value (find-class 'effective-optional-input-definition)))
      (direct-visual-defaulting-optional-input-definition
       (load-time-value (find-class 'effective-visual-defaulting-optional-input-definition)))
      (direct-defaulting-optional-input-definition (load-time-value (find-class 'effective-defaulting-optional-input-definition)))
      (direct-global-matrix-attribute-definition (load-time-value (find-class 'effective-global-matrix-attribute-definition)))
      (direct-eager-ordinary-attribute-definition (load-time-value (find-class 'effective-eager-ordinary-attribute-definition)))
      (direct-ordinary-attribute-definition (load-time-value (find-class 'effective-ordinary-attribute-definition)))
      (direct-visual-modifiable-attribute-definition
       (load-time-value (find-class 'effective-visual-modifiable-attribute-definition)))
      (direct-modifiable-attribute-definition (load-time-value (find-class 'effective-modifiable-attribute-definition)))
      (direct-uncached-attribute-definition (load-time-value (find-class 'effective-uncached-attribute-definition)))
      
      (direct-ordinary-component-definition (load-time-value (find-class 'effective-ordinary-component-definition)))
      (direct-eager-ordinary-component-definition (load-time-value (find-class 'effective-eager-ordinary-component-definition)))
      (direct-array-aggregate-component-definition (load-time-value (find-class 'effective-array-aggregate-component-definition)))
      (direct-eager-array-aggregate-component-definition (load-time-value (find-class 'effective-eager-array-aggregate-component-definition)))
      (direct-table-aggregate-component-definition (load-time-value (find-class 'effective-table-aggregate-component-definition)))
      (direct-eager-table-aggregate-component-definition (load-time-value (find-class 'effective-eager-table-aggregate-component-definition)))
      ;;(direct-maintained-slot-definition (load-time-value (find-class 'effective-maintained-slot-definition)))
      (t (call-next-method)))))

(defmethod effective-slot-definition-class ((class funcallable-adhoc-class) &rest initargs)
  (let* ((name (getf initargs :name))
	 (dslotd (loop for c in (compute-class-precedence-list class)
		    do (let ((dslotd (find name (class-direct-slots c)
					   :key #'slot-definition-name :test #'eq)))
			 (when dslotd (return dslotd))))))
    (typecase dslotd
      (direct-ordinary-attribute-definition
       (load-time-value (find-class 'effective-ordinary-attribute-definition)))
      (t (call-next-method)))))

(defmethod upgrade-eslotd ((eslotd effective-virtual-slot-definition-mixin)
			   (dslotd direct-virtual-slot-definition-mixin)
			   &rest dslotds)
  (setf (slot-value eslotd 'getter) (compute-getter-emfun dslotds))
  (setf (slot-value eslotd 'setter) (compute-setter-emfun dslotds))
  eslotd)

(defmethod upgrade-eslotd (eslotd (dslotd standard-direct-slot-definition) &rest dslotds)
  (declare (ignore dslotds))
  eslotd)

(defmethod upgrade-eslotd :around ((eslotd effective-basic-attribute-definition-mixin)
				   (dslotd direct-basic-attribute-definition-mixin)
				   &rest dslotds)
  (declare (ignore dslotds))
  (setf (%shadow eslotd) dslotd)
  (call-next-method)
  eslotd)

(defmethod upgrade-eslotd ((eslotd effective-attribute-function-mixin)
			   (dslotd direct-attribute-function-mixin)
			   &rest dslotds)
  (setf (attribute-function eslotd) (compute-emfun dslotds))
  eslotd)

(defmethod upgrade-eslotd :after ((eslotd effective-settable-slot-definition-mixin)
				  (dslotd direct-settable-slot-definition-mixin)
				  &rest dslotds)
  (declare (ignore dslotds))
  (setf (noticers eslotd) (mapcar #'first (noticers dslotd)))
  eslotd)

(defmethod upgrade-eslotd ((eslotd effective-component-definition-mixin)
			   (dslotd direct-component-definition-mixin)
			   &rest dslotds)
  (declare (ignore dslotds))
  (setf (class-spec-function eslotd) (class-spec-function dslotd)
	(slot-value eslotd 'provided-inputs) (slot-value dslotd 'provided-inputs)
	(slot-value eslotd 'hidden?) (slot-value dslotd 'hidden?))
  eslotd)

(defmethod upgrade-eslotd ((eslotd effective-array-aggregate-component-definition-mixin)
			   (dslotd direct-array-aggregate-component-definition-mixin)
			   &rest dslotds)
  (declare (ignore dslotds))
  (call-next-method)
  (setf (size-function eslotd) (size-function dslotd))
  eslotd)

(defmethod upgrade-eslotd ((eslotd effective-table-aggregate-component-definition-mixin)
			   (dslotd direct-table-aggregate-component-definition-mixin)
			   &rest dslotds)
  (declare (ignore dslotds))
  (call-next-method)
  (setf (indices-function eslotd) (indices-function dslotd))
  eslotd)


(defmethod compute-effective-slot-definition ((class adhoc-class) slot-name dslotds)
  (declare (ignore slot-name))
  (let ((eslotd (call-next-method)))
    (apply #'upgrade-eslotd eslotd (first dslotds) dslotds)))

(defmethod compute-effective-slot-definition ((class funcallable-adhoc-class) slot-name dslotds)
  (declare (ignore slot-name))
  (let ((eslotd (call-next-method)))
   (apply #'upgrade-eslotd eslotd (first dslotds) dslotds)))

;; basic mixin all adhoc-enabled objects have
(defclass adhoc-mixin ()
  ((root :initform nil :initarg :root)
   (superior :reader superior :initform nil :initarg :superior)
   (component-definition :accessor component-definition :initform nil :initarg :component-definition)
   (aggregate :reader aggregate :initform nil :initarg :aggregate)
   (indices :reader component-indices :initform nil :initarg :indices)
   (inittest :initform nil)))

(defclass adhoc-scene-graph::node-mixin (adhoc-mixin)
  ()
  (:metaclass adhoc-class))

(defgeneric adhoc-scene-graph::rm-redraw-node (node)
  (:method (node) (values)))

(defgeneric adhoc-scene-graph::rm-draw-node (node)
  (:method (node) (values)))

(defgeneric adhoc-scene-graph::rm-erase-node (node)
  (:method (node) (values)))

(defgeneric adhoc-scene-graph::rm-retransform-node (node)
  (:method (node) (values)))

(defun initialize-eager-slots (instance)
  ;; the ordinary-input-definitions should have already been setf'd
  ;; by the main normal shared-initialize method
  ;; at top-level 'defaulting' ordinary inputs and defaulting optional inputs
  ;; behave the same as ordinary inputs and optional inputs
  (let* ((class (class-of instance))
	 (eslotds (class-slots class)))
    ;; so now we need to compute the effective-eager-optional-input-definitions
    (loop for eslotd in eslotds
	  when (typep eslotd 'eager-optional-input-definition-mixin)
	    do (ensure-slot-value class instance eslotd)
	       (setq eslotds (remove eslotd eslotds)))
    (loop for eslotd in eslotds
	  when (typep eslotd 'effective-eager-ordinary-attribute-definition-mixin)
	    do (ensure-slot-value class instance eslotd)
	       (setq eslotds (remove eslotd eslotds)))
    (loop for eslotd in eslotds
	  when (typep eslotd 'effective-eager-component-definition-mixin)
	    do (ensure-slot-value class instance eslotd)
	       (setq eslotds (remove eslotd eslotds)))
    (loop for eslotd in eslotds
	  when (typep eslotd 'effective-eager-array-aggregate-component-definition-mixin)
	    do (ensure-slot-value class instance eslotd)
	       (setq eslotds (remove eslotd eslotds)))
    (loop for eslotd in eslotds
	  when (typep eslotd 'effective-eager-table-aggregate-component-definition-mixin)
	    do (ensure-slot-value class instance eslotd)
	       (setq eslotds (remove eslotd eslotds)))
    (values)))
	     


(defmethod initialize-instance :before ((instance adhoc-mixin) &rest initargs
					&key superior &allow-other-keys)
  (declare (ignore initargs))
  (unless superior
    (setf (slot-value instance 'root) instance))
  (values))

(defmethod initialize-instance :around ((instance adhoc-mixin) &rest initargs)
  (declare (ignore initargs))
  (prog1 (call-next-method)
    (initialize-eager-slots instance)
    (setf (slot-value instance 'inittest) t)))

;; slow method for looking up slotd objects on class
(defmethod get-slot-definition (class (slot-name symbol))
  (let ((slotds (class-slots class)))
    (loop for slotd in slotds
	  when (or (eq (slot-definition-name slotd) slot-name))
	    return slotd)))

(defmethod get-slot-definition (class (location integer))
  (let ((slotds (class-slots class)))
    (loop for slotd in slotds
	  when (or (eq (slot-definition-location slotd) location))
	    return slotd)))

(defclass basket ()
  ((value :accessor basket-value :initarg :value)
   (dependents :accessor dependents)
   (status :accessor basket-status)))

(defmethod print-object ((object basket) stream)
  (print-unreadable-object (object stream)
    (princ "BASKET value = " stream)
    (if (slot-boundp object 'value)
	(princ (slot-value object 'value) stream)
	(princ +slot-unbound+ stream))
    (princ " " stream)
    (when (basket-status object)
      (princ (basket-status object) stream))
    object))
	   

;; to have a place where settable-slots can differentiate between cached and setf'd
;;(defclass settable-variable (variable)
;;  ((status :accessor variable-status :initform nil)))

;;(defclass parameter (settable-variable)
  ;;())

#+NIL
(defmethod print-object ((object parameter) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object 'value)
	(princ (variable-value object) stream)
	(princ :unbound stream))
    object))

;; to have an dependency maintenance location for the class of the subpart
#+NIL
(defclass component-variable (variable)
  ((class)
   (indices :initarg :indices)))

(defclass aggregate-mixin (standard-generic-function)
  ((root :initform nil :initarg :root)
   (superior :reader superior :initform nil :initarg :superior)
   (component-definition :reader component-definition :initform nil :initarg :component-definition)
   (value :reader basket-value :initarg :value)
   (dependents :accessor dependents :initform nil))
  (:metaclass funcallable-adhoc-class))

(defmethod initialize-instance :around ((instance aggregate-mixin) &rest initargs)
  (declare (ignore initargs))
  (prog1 (call-next-method)
    (when (typep (component-definition instance) 'eager-attribute-definition-mixin)
      (loop for child in (send instance list-elements)
	    do (initialize-eager-slots child)))))

(defclass array-aggregate-mixin (aggregate-mixin)
  ()
  (:metaclass funcallable-adhoc-class))  

(defclass table-aggregate-mixin (aggregate-mixin)
  ()
  (:metaclass funcallable-adhoc-class))

(defclass array-aggregate (array-aggregate-mixin)
  ()
  (:metaclass funcallable-adhoc-class))

(defclass table-aggregate (table-aggregate-mixin)
  ()
  (:metaclass funcallable-adhoc-class))

#+NIL
(defun variable-p (thing)
  (or (typep thing 'variable)
      (typep thing 'aggregate-mixin)))

#+NIL
(defmethod print-object ((object variable) stream)
  (print-unreadable-object (object stream)
    (princ "DAG VARIABLE " stream)
    (when (slot-boundp object 'slot-name)
      (princ (slot-name object) stream))
    #+NIL(when (slot-boundp object 'value)
      (princ " = " stream)
      (princ (variable-value object) stream))))

(defmethod print-object ((object array-aggregate) stream)
  (print-unreadable-object (object stream)
    (princ "ARRAY-AGGREGATE " stream)
    (princ (slot-definition-name (component-definition object)) stream)
    object))

(defmethod print-object ((object table-aggregate) stream)
  (print-unreadable-object (object stream)
    (princ "TABLE-AGGREGATE " stream)
    (princ (slot-definition-name (component-definition object)) stream)
    object))
    

#+NIL
(defmethod print-object ((object settable-variable) stream)
  (print-unreadable-object (object stream)
    (princ "SETTABLE DAG VARIABLE " stream)
    (when (slot-boundp object 'status)
      (when (slot-value object 'status)
	(princ (slot-value object 'status) stream)
	(princ " " stream)))
    (when (slot-boundp object 'slot-name)
      (princ (slot-name object) stream))
    #+NIL(when (slot-boundp object 'value)
      (princ " = " stream)
      (princ (variable-value object) stream))
    #+NIL(when (dependents object)
      (princ " DEPENDENTS: " stream)
      (princ (dependents object) stream))))

#+NIL
(defmethod print-object ((object component-variable) stream)
  (print-unreadable-object (object stream)
    (princ "COMPONENT VARIABLE " stream)
    (when (slot-boundp object 'slot-name)
      (princ (slot-name object) stream))
    (when (slot-boundp object 'indices)
      (princ " " stream)
      (princ (slot-value object 'indices) stream))
    (when (slot-boundp object 'value)
      (princ " = " stream)
      (princ (variable-value object) stream))))

;; the variable-type generic function computes the type of the variable from the type of the slot
#+NIL
(defmethod variable-type ((slotd t))
  nil)

#+NIL
(defmethod variable-type ((slotd basic-attribute-definition-mixin))
  (load-time-value (find-class 'variable)))

#+NIL
(defmethod variable-type ((slotd effective-settable-slot-definition-mixin))
  (load-time-value (find-class 'settable-variable)))

#+NIL
(defmethod variable-type ((slotd effective-parametric-slot-definition))
  (load-time-value (find-class 'parameter)))

#+NIL
(defmethod variable-type ((slotd component-definition-mixin))
  (load-time-value (find-class 'component-variable)))

#+NIL
(defmethod variable-type ((slotd array-aggregate-component-definition-mixin))
  (load-time-value (find-class 'array-aggregate)))

#+NIL
(defmethod variable-type ((slotd table-aggregate-component-definition-mixin))
  (load-time-value (find-class 'table-aggregate)))

(defun slot-value-internal (instance slot-name)
  (let* ((location (gethash slot-name (slot-locations (class-of instance)))))
    (if (null location)
	(slot-missing (class-of instance) instance slot-name 'slot-value)
	(standard-instance-access-compat instance location))))

(defun (setf slot-value-internal) (value instance slot-name)
  (let* ((location (gethash slot-name (slot-locations (class-of instance)))))
    (if (null location)
	(slot-missing (class-of instance) instance slot-name 'slot-value)
	(setf (standard-instance-access instance location)
	      value))))

(declaim (inline allocate-basket))
(defun allocate-basket ()
  (let ((b (allocate-instance (load-time-value (find-class 'basket)))))
    (declare (type basket b))
    (setf (slot-value b 'dependents) nil)
    (setf (slot-value b 'status) nil)
    b))
  

;; slot basket is like slot-value except it returns the basket instead of the value
;; used in the constraint solver or for debugging purposes

(defmethod slot-basket (instance (slot-name symbol))
  (let* ((class (class-of instance))
	 (location (gethash slot-name (slot-locations class))))
    (if (null location)
	(slot-missing (class-of instance) instance slot-name 'slot-value)
        ;; if it's already there it's fast, else it's slow
	(let ((maybe-basket (standard-instance-access-compat instance location)))
	  (if (eq +slot-unbound+ maybe-basket)
	      (setf (standard-instance-access instance location)
		    (allocate-basket))
	      maybe-basket)))))

#+NIL
(defmethod (setf slot-variable) (variable instance (slot-name symbol))
  (let* ((class (class-of instance))
	 (location (gethash slot-name (slot-locations class))))
    (if (null location)
	(slot-missing (class-of instance) instance slot-name 'slot-value)
	(setf (standard-instance-access instance location) variable))))

(defmethod slot-basket (instance (eslotd basic-attribute-definition-mixin))
  (let* ((location (slot-definition-location eslotd))
	 (maybe-basket (standard-instance-access-compat instance location)))
    (if (eq +slot-unbound+ maybe-basket)
	(setf (standard-instance-access instance location)
	      (allocate-basket))
	maybe-basket)))

;; like with slots but fetches the variable instead of the value.
(defmacro with-variables (slots instance &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance))
       (declare (ignorable ,in))
       (symbol-macrolet
	   ,(mapcar (lambda (slot-entry)
		      (unless (typep slot-entry '(or symbol (cons symbol (cons symbol null))))
			(error "Malformed variable slot entry: ~S" slot-entry))
		      (destructuring-bind (var-name &optional (slot-name var-name))
			  (if (consp slot-entry)
			      slot-entry
			      (list slot-entry))
			`(,var-name (slot-variable ,in ',slot-name))))
	     slots)
	 ,@body))))

(defmethod aggregate-class ((slotd table-aggregate-component-definition-mixin))
  (load-time-value (find-class 'table-aggregate)))

(defmethod aggregate-class ((slotd array-aggregate-component-definition-mixin))
  (load-time-value (find-class 'array-aggregate)))

(defmethod slot-basket (instance (slotd aggregate-component-definition-mixin))
  (let* ((location (slot-definition-location slotd))
	 (maybe-aggregate (standard-instance-access-compat instance location)))
    (if (eq +slot-unbound+ maybe-aggregate)
	(make-instance (aggregate-class slotd)			      
		       :instance instance
		       :location location
		       :root (slot-value instance 'root)
		       :superior instance
		       :component-definition slotd)
	maybe-aggregate)))


(defmethod get-aggregate-member-basket ((aggregate table-aggregate) slot-name indices)
  (if (member indices (send aggregate indices) :test #'equalp)
      (let* ((ht (slot-value aggregate 'value))
	     (maybe-basket (gethash indices ht +slot-unbound+)))
	(if (eq +slot-unbound+ maybe-basket)
	    (let* ((basket (allocate-basket)))
	      (setf (gethash indices ht) basket))
	    maybe-basket))
      (error "Aggregate indices ~S for ~S on ~S not found."
	     indices
	     slot-name
	     (superior aggregate))))
      

(defmethod get-aggregate-member-basket ((aggregate array-aggregate) slot-name indices)
  (handler-bind ((#+SBCL sb-int:invalid-array-index-error
		  #+CCL error
		  #+ALLEGRO error
		  #'(lambda (e)
		      (declare (ignore e))
		      (error "Aggregate indices ~S for ~S on ~S not found."
			     indices
			     slot-name
			     (superior aggregate)))))
    (let* ((array (slot-value aggregate 'value))
	   (maybe-basket (apply #'aref array indices)))
      (if (eq +slot-unbound+ maybe-basket)
	  (setf (apply #'aref array indices)
		(allocate-basket))
	  maybe-basket))))

(defmethod slot-basket ((instance aggregate-mixin) slotd)
  (let* ((location (slot-definition-location slotd))
	 (maybe-basket (funcallable-standard-instance-access instance location)))
    (if (eq +slot-unbound+ maybe-basket)
	(setf (funcallable-standard-instance-access instance location)
	      (allocate-basket))
	maybe-basket)))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-ordinary-virtual-slot-definition))
  (funcall (slot-value slotd 'getter) instance))

(defmethod (setf slot-value-using-class) (value (class adhoc-class) instance
					  (slotd effective-ordinary-virtual-slot-definition))
  (funcall (slot-value slotd 'setter) instance value)
  (values))

(defmethod slot-boundp-using-class ((class adhoc-class) instance
				    (slotd effective-ordinary-virtual-slot-definition))
  t)


  

(defmethod (setf slot-value-using-class) (value (class adhoc-class) instance
					  (slotd effective-settable-slot-definition-mixin))
  (let ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    
    (prog1 (setf (slot-value basket 'value) value)

      (when (and (slot-boundp instance 'inittest)
		 (slot-value instance 'inittest))
	(frob-dependents basket))
      
      (setf (basket-status basket) :set)
    
      #+NOTYET
      (unwind-protect
	   (when (noticers slotd)
	     ;; noticers will not run until inittest is set to non-nil value
	     (when (and (slot-boundp instance 'inittest)
			(slot-value instance 'inittest))
	       (loop for noticer in (noticers slotd)
		     do
			(funcall noticer instance value))))
	(setf (variable-status variable) :set))

      )))

(defmethod (setf slot-value-using-class) (value (class adhoc-class) instance
					  (slotd visual-mixin))

  (let ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    
    (prog1 (setf (slot-value basket 'value) value)

      (when (and (slot-boundp instance 'inittest)
		 (slot-value instance 'inittest))
	(progn
	  (when (typep instance 'adhoc-scene-graph::node-mixin)
	    (adhoc-scene-graph::rm-redraw-node instance))
	  
	  (frob-dependents basket :redraw? t)))
      
      (setf (basket-status basket) :set))))
  

(defmethod (setf slot-value-using-class) (value (class adhoc-class) instance
					  (slotd effective-non-settable-slot-definition-mixin))
  (declare (ignore value instance))
  (error "slot ~S of type ~S is read only" (slot-definition-name slotd) (class-name (class-of slotd))))

(defmethod slot-makunbound-using-class ((class adhoc-class) instance (slotd effective-basic-attribute-definition-mixin))
  (let ((basket (slot-basket instance slotd)))
    (slot-makunbound basket 'value)
    (frob-dependents basket)
    (values)))

(defparameter %dependent% nil)

(defparameter %recompute% nil)

(declaim (inline recompute?))
(defun recompute? (object slotd indices)
  (member (list object slotd indices) %recompute% :test #'equal))

(defmacro with-dependee-advisement ((object slotd &optional indices) &body body)
  `(let* ((%dependent% (list ,object ,slotd ,indices))
	  (%recompute% (remove %dependent% %recompute% :test #'equal)))
     ,@body))

(defmacro capture-direct-dependent (instance basket)
  `(when (and %dependent% (eq (slot-value ,instance 'root) (slot-value (car %dependent%) 'root)))
     ;; only capture the dependent if the dependent and the instance are in the same tree.
     (pushnew %dependent% (dependents ,basket) :test #'equal)))

(defmacro without-dependency-capture (&body body)
  `(let ((%dependent% nil))
     ,@body))

(defmethod slot-value-using-class ((class adhoc-class) instance (slotd effective-basic-attribute-definition-mixin))
  (slot-value (cl:the basket (ensure-slot-value class instance slotd)) 'value))

(defmethod slot-value-using-class ((class funcallable-adhoc-class) instance (slotd effective-basic-attribute-definition-mixin))
  (slot-value (cl:the basket (ensure-slot-value class instance slotd)) 'value))




(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin) (slotd attribute-function-mixin))
  ;; we must capture any dependency coming from any attribute access which,
  ;; in it's expression, somehow landed us here.
  ;; however, we only need to send notification when we are actually evaluating any code, since that is the only time
  ;; that any other attributes will be accessed, not during a cache fetch
  ;; capture-direct-dependent and with-dependee-advisement perform these functions, respectively.
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (setf (slot-value basket 'value)
	    (with-dependee-advisement (instance slotd)
	      (funcall (attribute-function slotd) instance))))
    
    basket))

(defmethod ensure-slot-value ((class funcallable-adhoc-class) (aggregate aggregate-mixin) (slotd attribute-function-mixin))
  ;; we must capture any dependency coming from any attribute access which,
  ;; in it's expression, somehow landed us here.
  ;; however, we only need to send notification when we are actually evaluating any code, since that is the only time
  ;; that any other attributes will be accessed, not during a cache fetch
  ;; capture-direct-dependent and with-dependee-advisement perform these functions, respectively.
  (let* ((basket (slot-basket aggregate slotd)))
    (declare (type basket basket))
    (capture-direct-dependent aggregate basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? aggregate slotd nil))
      (setf (slot-value basket 'value)
	    (with-dependee-advisement (aggregate slotd)
	      (funcall (attribute-function slotd) aggregate))))
    basket))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-uncached-attribute-definition))
  ;; uncached attributes don't capture any dependencies
  ;; nor do they inform dependents of changes
  ;; is this true?
  (funcall (attribute-function slotd) instance))

(defmethod slot-boundp-using-class ((class adhoc-class) instance
				    (slotd effective-uncached-attribute-definition))
  (declare (ignore instance))
  t)

(defmethod shared-initialize :after ((instance direct-attribute-function-mixin) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (attribute-function instance) (getf initargs :function))
  (setf (attribute-body instance) (getf initargs :body))
  (values))

(defmethod shared-initialize :after ((instance direct-settable-slot-definition-mixin) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (noticers instance) (getf initargs :noticers))
  (values))

(defmethod shared-initialize :after ((instance direct-component-definition-mixin) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (component-type-expression instance) (getf initargs :type-expression)
	(class-spec-function instance) (getf initargs :type-function)
	(slot-value instance 'provided-inputs)
	(loop for (initarg plist) on (getf initargs :inputs) by #'cddr
	   append (list initarg (getf plist :function)))
	(slot-value instance 'provided-inputs-source)
	(loop for (initarg plist) on (getf initargs :inputs) by #'cddr
	   append (list initarg (getf plist :expression))))
  (values))

(defmethod shared-initialize :after ((instance direct-array-aggregate-component-definition-mixin) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (size-expression instance) (getf initargs :size-expression)
	(size-function instance) (getf initargs :size-function))
  (values))

(defmethod shared-initialize :after ((instance direct-table-aggregate-component-definition-mixin) slot-names &rest initargs)
  (declare (ignore slot-names))
  (setf (indices-expression instance) (getf initargs :indices-expression)
	(indices-function instance) (getf initargs :indices-function))
  (values))

(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd component-definition-mixin))
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (let ((child (make-instance 
		    (with-dependee-advisement (instance slotd)
		      (funcall (class-spec-function slotd) instance))
		    :root (slot-value instance 'root)
		    :superior instance
		    :component-definition slotd)))
	(setf (slot-value basket 'value) child)))
    basket))




(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd table-aggregate-component-definition-mixin))
  (let* ((aggregate (slot-basket instance slotd)))
    (declare (type table-aggregate aggregate))
    (capture-direct-dependent instance aggregate)
    (when (or (not (slot-boundp aggregate 'value)) (recompute? instance slotd nil))
      (setf (slot-value aggregate 'value) (make-hash-table :test #'equalp)))
    aggregate))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd aggregate-component-definition-mixin))
  (ensure-slot-value class instance slotd))


(defmethod shared-initialize :before ((aggregate aggregate-mixin) slot-names
				      &rest initargs &key instance location &allow-other-keys)
									      
  (declare (ignore slot-names initargs))
  (setf (standard-instance-access instance location) aggregate)
  (set-funcallable-instance-function
   aggregate
   #'(lambda (&rest indices)
       (apply #'aggregate-lookup aggregate indices)))
  (values))  

(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd array-aggregate-component-definition-mixin))
  (let* ((aggregate (slot-basket instance slotd)))
    (declare (type array-aggregate aggregate))
    (capture-direct-dependent instance aggregate)
    (when (or (not (slot-boundp aggregate 'value)) (recompute? instance slotd nil))
      (setf (slot-value aggregate 'value)
	    (make-array
	     (with-dependee-advisement (instance slotd)
	       (funcall (size-function slotd) instance))
	     :initial-element +slot-unbound+)))
    aggregate))
  
(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd input-definition-mixin))
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (setf (slot-value basket 'value)
	    (flet ((unbound ()
		     (slot-unbound class instance (slot-definition-name slotd))))
	      (let ((component-definition (component-definition instance))
		    (initarg (first (slot-definition-initargs slotd))))
		(if component-definition
		    (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								 (slot-value instance 'indices))))
		      (labels ((has-descending? (instance slot-name)
				 (if (member slot-name (slot-value (class-of instance)
								   'effective-descending-attributes)
					     :test #'eq)
				     (slot-value instance slot-name)
				     (if (superior instance)
					 (has-descending? (superior instance) slot-name)
					 (unbound))))
			       (normal-lookup ()
				 (let ((input-function (getf provided-input-function-plist initarg)))
				   (if input-function
				       (with-dependee-advisement (instance slotd)
					 (funcall input-function (superior instance) instance))
				       (has-descending? (superior instance) (slot-definition-name slotd))))))
			(let ((plist-function (getf provided-input-function-plist :@)))
			  (if plist-function
			      (let ((plist (with-dependee-advisement (instance slotd)
					     (funcall plist-function (superior instance) instance))))
				(let ((result (getf plist initarg +slot-unbound+)))
				  (if (eq result +slot-unbound+)
				      (normal-lookup)
				      result)))
			      (normal-lookup)))))
		    (unbound))))))
    basket))

(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd optional-input-definition-mixin))
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (setf (slot-value basket 'value)
	    (flet ((get-toplevel ()
		     (with-dependee-advisement (instance slotd)
		       (funcall (attribute-function slotd) instance))))
	      (let ((component-definition (component-definition instance))
		    (initarg (first (slot-definition-initargs slotd))))
		(if component-definition
		    (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								 (slot-value instance 'indices))))
		      (flet ((normal-lookup ()
			       (let ((input-function (getf provided-input-function-plist initarg)))
				 (if input-function
				     (with-dependee-advisement (instance slotd)
				       (funcall input-function (superior instance) instance))
				     (get-toplevel)))))
			(let ((plist-function (getf provided-input-function-plist :@)))
			  (if plist-function
			      (let ((plist (with-dependee-advisement (instance slotd)
					     (funcall plist-function (superior instance) instance))))
				(let ((result (getf plist initarg +slot-unbound+)))
				  (if (eq result +slot-unbound+)
				      (normal-lookup)
				      result)))
			      (normal-lookup)))))
		    (get-toplevel))))))
    basket))

(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd defaulting-optional-input-definition-mixin))
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (setf (slot-value basket 'value)
	    (flet ((get-toplevel ()
		     (with-dependee-advisement (instance slotd)
		       (funcall (attribute-function slotd) instance))))
	      (let ((component-definition (component-definition instance))
		    (initarg (first (slot-definition-initargs slotd))))
		(if component-definition
		    (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								 (slot-value instance 'indices))))
		      (labels ((normal-lookup ()
				 (let* ((input-function (getf provided-input-function-plist initarg)))
				   (if input-function
				       (with-dependee-advisement (instance slotd)
					 (funcall input-function (superior instance) instance))
				       (get-toplevel))))
			       (answers-message? (instance message)
				 (if (slot-exists-p instance message)
				     instance
				     (when (superior instance)
				       (answers-message? (superior instance) message)))))
			(let* ((slotd-name (slot-definition-name slotd))
			       (ancestor (answers-message? (superior instance) slotd-name)))
			  (if ancestor
			      (slot-value ancestor slotd-name)
			      (let ((plist-function (getf provided-input-function-plist :@)))
				(if plist-function
				    (let ((plist (with-dependee-advisement
						     (instance slotd)
						   (funcall plist-function (superior instance) instance))))
				      (let ((result (getf plist initarg +slot-unbound+)))
					(if (eq result +slot-unbound+)
					    (normal-lookup)
					    result)))
				    (normal-lookup)))))))
		    (get-toplevel))))))
    basket))

(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd defaulting-ordinary-input-definition-mixin))
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (setf (slot-value basket 'value)
	    (let ((component-definition (component-definition instance))
		  (slotd-name (slot-definition-name slotd))
		  (initarg (first (slot-definition-initargs slotd))))
	      (if component-definition
		  (labels ((answer-locally ()
			     (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
									  (slot-value instance 'indices)))
				    (input-function (getf provided-input-function-plist initarg)))
			       (if input-function
				   (with-dependee-advisement (instance slotd)
				     (funcall input-function (superior instance) instance))
				   (slot-unbound class instance slotd-name))))
			   (answers-message? (instance message)
			     (if (slot-exists-p instance message)
				 instance
				 (when (superior instance)
				   (answers-message? (superior instance) message)))))
		    (let ((ancestor (answers-message? (superior instance) slotd-name)))
		      (if ancestor
			  (slot-value ancestor slotd-name)
			  (answer-locally))))
		  (slot-unbound class instance slotd-name)))))
    basket))

(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd effective-maintained-slot-definition))
  (let* ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (slot-unbound class instance (slot-definition-name slotd)))
    basket))

(defun aggregate-lookup (aggregate &rest indices)
  (let ((basket (ensure-aggregate-member aggregate indices)))
    (basket-value basket)))

(defun ensure-aggregate-member (aggregate indices)
  (declare (type aggregate-mixin aggregate))
  (let* ((slotd (component-definition aggregate))
	 (basket (get-aggregate-member-basket aggregate slotd indices)))
    (declare (type basket basket))
    
    (capture-direct-dependent aggregate basket)
    
    (when (or (not (slot-boundp basket 'value)) (recompute? aggregate slotd indices))
      (let ((child (make-instance (with-dependee-advisement
			       (aggregate slotd indices)
			     (apply (type-function slotd)
				    (superior aggregate)
				    indices))
			   :root (slot-value aggregate 'root)
			   :superior (slot-value aggregate 'superior)
			   :component-definition slotd
			   :aggregate aggregate
			   :indices indices)))
	(setf (slot-value basket 'value) child)))
    basket))

(defmethod unbind-aggregate-slot ((aggregate table-aggregate) indices)
  (let ((basket (gethash indices (slot-value aggregate 'value))))
    (slot-makunbound basket 'value)))

(defmethod unbind-aggregate-slot ((aggregate array-aggregate) indices)
  (handler-bind ((#+SBCL sb-int:invalid-array-index-error
		  #+CCL error
		  #+ALLEGRO error
		  #'(lambda (e)
		      (declare (ignore e))
		      nil)))
    (let* ((array (slot-value aggregate 'value))
	   (maybe-basket (handler-bind ((#+SBCL sb-int:invalid-array-index-error
					   #+CCL error
					   #+ALLEGRO error
					   #'(lambda (e)
					       (declare (ignore e))
					       nil)))
			     (apply #'aref array indices))))
      (when maybe-basket
	(slot-makunbound maybe-basket 'value)))))  


(defmethod frob-this-slot (class object (slotd basic-attribute-definition-mixin))
  (let ((basket (slot-basket object slotd)))
    ;; we don't want to erase basket dependents here
    ;; because they may be used by an eager computation later
    (unless (eq (basket-status basket) :set)
      (slot-makunbound basket 'value))))

(defmethod frob-this-slot :after (class object (slotd effective-global-matrix-attribute-definition))
  (setf (slot-value object 'adhoc-scene-graph::retransform?) t)
  (values))

(defmethod frob-this-slot (class object (slotd eager-attribute-definition-mixin))
  (ensure-slot-value class object slotd))

(defmethod frob-aggregate-member-slot ((slotd eager-attribute-definition-mixin) aggregate indices)
  (ensure-aggregate-member aggregate indices))

(defmethod frob-aggregate-member-slot (slotd aggregate indices)
  (unbind-aggregate-slot aggregate indices))
  
(defun frob-dependents-1 (&key (redraw? nil))
  (let ((frobbed-objects ()))
    (loop for dependent in %recompute%
	  do (destructuring-bind (object slotd indices) dependent
	       ;; frobbing a slot either uncaches it or recomputes it
	       ;; depending on whether it's lazy or eager
	       (if indices
		   ;; when indices are present, object is the aggregate
		   ;; so as to avoid an extra slot-value call
		   (let ((aggregate object))
		     (frob-aggregate-member-slot slotd aggregate indices))
			      
		   (let ((class (class-of object)))
		     (frob-this-slot class object slotd)
		     (pushnew object frobbed-objects)))))

    (loop for object in (nreverse frobbed-objects)
	  when (and (typep object 'adhoc-scene-graph::node-mixin)
		    redraw?)
	    do (adhoc-scene-graph::rm-redraw-node object)
	  when (and (typep object 'adhoc-scene-graph::node-mixin)
		    (send object adhoc-scene-graph::retransform?))
	    do (adhoc-scene-graph::rm-retransform-node object)
	       (setf (slot-value object 'adhoc-scene-graph::retransform?) nil)))
  (values))
  

(defun all-dependents (basket)
  (sort
   (remove-duplicates 
    (loop for dependent in (prog1 (dependents basket) (setf (dependents basket) nil))
	  append (list*
		  dependent
		  (destructuring-bind (object slotd indices) dependent
		    (all-dependents
		     (if indices
			 (get-aggregate-member-basket object (slot-definition-name slotd) indices)
			 (slot-basket object slotd)))))))
   #'(lambda (dep1 dep2)
       (let ((slot1 (cadr dep1))
	     (slot2 (cadr dep2)))
	 (if (and (typep slot1 'eager-attribute-definition-mixin)
		  (typep slot2 'eager-attribute-definition-mixin))
	     (if (and (typep slot1 'input-definition-mixin)
		      (typep slot2 'input-definition-mixin))
		 nil
		 (when (typep slot1 'input-definition-mixin)
		   t))
	     (if (typep slot1 'eager-attribute-definition-mixin)
		 t
		 (if (and (typep slot1 'input-definition-mixin)
			  (typep slot2 'input-definition-mixin))
		     nil
		     (when (typep slot1 'input-definition-mixin)
		       t))))))))	  

(defun frob-dependents (basket &key (redraw? nil))
  (let ((%recompute% (all-dependents basket)))

    (frob-dependents-1 :redraw? redraw?)))
      

(defmethod slot-missing ((class adhoc-class) (instance adhoc-mixin) slot-name (op (eql 'slot-value))
			 &optional new-value)
  (declare (ignore new-value))
  (labels ((has-descending? (instance slot-name)
	     (if (member slot-name (slot-value (class-of instance) 'effective-descending-attributes) :test #'eq)
		 (slot-value instance slot-name)
		 (if (superior instance)
		     (has-descending? (superior instance) slot-name)
		     (call-next-method))))
	   (normal-lookup (provided-inputs)
	     (let ((input-function (getf provided-inputs slot-name)))
	       (if input-function ;; pseudo inputs
		   (funcall input-function (superior instance) instance)
		   (has-descending? (superior instance) slot-name)))))

    (let ((component-definition (component-definition instance)))
      (if component-definition
	  (let* ((provided-inputs (apply #'provided-inputs component-definition (slot-value instance 'indices))))
	    (let ((plist-function (getf provided-inputs :@)))
	      (if plist-function
		  (let ((plist (funcall plist-function (superior instance) instance)))
		    (let ((result (getf plist slot-name +slot-unbound+)))
		      (if (eq result +slot-unbound+)
			  (normal-lookup provided-inputs)
			  result)))
		  (normal-lookup provided-inputs))))
	  (call-next-method)))))

(defun root-path2 (self)
  (if (the superior)
      (append
       (root-path2 (the superior))
       (list
	(let ((slotd (the component-definition)))
	  (if (typep slotd 'aggregate-component-definition-mixin)
	      (if (typep self 'aggregate-mixin)
		  (slot-definition-name slotd)
		  (list* (slot-definition-name slotd) (the indices)))
	      (slot-definition-name slotd)))))
      (list (class-name (class-of self)))))

(defun root-path (self)
  (if (the superior)
      (append
       (root-path (the superior))
       (list
	(let ((slotd (the component-definition)))
	  (if (typep slotd 'aggregate-component-definition-mixin)
	      (if (typep self 'aggregate-mixin)
		  (slot-definition-name slotd)
		  (list* (slot-definition-name slotd) (the indices)))
	      (slot-definition-name slotd)))))
      (list 'root)))

(defmacro the-part (&rest messages)
  `(send part ,@messages))


(defmethod get-object-children ((object adhoc-mixin))
  (let* ((class (class-of object)))
    ;; get-object-children can be called at any time and depends on
    ;; the class metaobject being up-to-date
    ;; finalize-inheritance is what normally ensures a class metaobject
    ;; is ready to go for instance access
    ;; but the actual finalize-inheritance will call make-instances-obsolete
    ;; this would then force an update-instance-for-redefined class for slot-value to work
    ;; but adhoc's update-instance-for-redefined-class blows away generative data such as children
    ;; so using finalize-inheritance here would cause the child objects to be recomputed on every
    ;; call to get-object-children.
    ;; so what we need is to finalize the appropriate adhoc-class object stuff, without calling make-instances-obsolete
    (finalize-inheritance-lite class)
    ;; if we entered here from an update-instance-for-redefined-class method, the make-instances-obsolete machinery
    ;; has already been processed, and this function should demand-recompute the children
    (when %dependent%
      (pushnew %dependent% (children-dependents class) :test #'equal))
    (let ((eslotds (component-eslotds class)))
      (loop for eslotd in eslotds
	        append (let ((c (slot-value-using-class class object eslotd)))
		             (if (typep c 'aggregate-mixin)
		                 (send c list-elements)
		                 (list c)))))))

(defmethod get-hidden-children ((object adhoc-mixin))
  (let* ((class (class-of object)))
    (finalize-inheritance-lite class)
    (when %dependent%
      (pushnew %dependent% (children-dependents class) :test #'eq))
    (let ((eslotds (hidden-component-eslotds class)))
      (loop for eslotd in eslotds
	        append (let ((c (slot-value-using-class class object eslotd)))
		             (if (typep c 'aggregate-mixin)
		                 (send c list-elements)
		                 (list c)))))))
		    

(defparameter *descending-attributes* nil)
(defparameter *slots-table* nil)

(defun duplicate-slot-error (class-name name)
  (error "duplicate slot name ~S in ~S." name class-name))

(defun parse-inputs-section (class-name section)
  (loop for input-definition in section
	do (let ((slot (if (consp input-definition) (first input-definition) input-definition)))
	     (if (gethash slot *slots-table*)
		 (duplicate-slot-error class-name slot)
		 (setf (gethash slot *slots-table*) t)))
	collect (cond ((and input-definition
			    (atom input-definition)
			    (symbolp input-definition)
			    (not (keywordp input-definition)))
		       (list 'list
			     :name `',input-definition
			     :initargs `'(,input-definition)
			     :slot-class :ordinary-input))
		      ((and (consp input-definition)
			    (atom (first input-definition))
			    (symbolp (first input-definition))
			    (not (keywordp (first input-definition))))
		       (let ((defaulting nil)
			     (descending nil)
			     (eager nil)
			     (visual nil)
			     (noticers ())
			     (list (copy-list (rest input-definition)))
			     (body nil))
			 (tagbody
			  start
			    (cond ((eq (first list) :visual) (setq visual t) (pop list) (go start))
				  ((eq (first list) :eager) (setq eager t) (pop list) (go start))
				  ((eq (first list) :defaulting) (setq defaulting t) (pop list) (go start))
				  ((eq (first list) :descending) (setq descending t) (pop list) (go start))
				  ((and (consp (first list))
					(eq (first (first list)) :noticer))
				   (push (rest (pop list)) noticers) (go start))
				  ((not (or (eq (first list) :defaulting)
					    (eq (first list) :descending)))
				   (when list
				     (setq body list))
				   (go exit)))
			  exit)
			 (when descending
			   (push (first input-definition) *descending-attributes*))
			 
			 (list* 'list
				:name `',(first input-definition)
				:initargs `'(,(first input-definition))
				(append
				 (if (null body)
				     (if visual
					 (if eager
					     (if defaulting
						 (list :slot-class :visual-eager-defaulting-ordinary-input)
						 (list :slot-class :visual-eager-ordinary-input))
					     (if defaulting
						 (list :slot-class :visual-defaulting-ordinary-input)
						 (list :slot-class :visual-ordinary-input)))
					 (if eager
					     (if defaulting
						 (list :slot-class :defaulting-eager-ordinary-input)
						 (list :slot-class :eager-ordinary-input))
					     (if defaulting
						 (list :slot-class :defaulting-ordinary-input)
						 (list :slot-class :ordinary-input))))
				     (list* :body `',body
					    :function `(with-cnm-support (:input (,(first input-definition) ,class-name))
							 (named-lambda (:input (,(first input-definition) ,class-name))
							     (self)
							   (declare (ignorable self))
							   (declare (type ,class-name self))
							   ,@body))
					    (if visual
						(if eager
						    (if defaulting
							(list :slot-class :visual-defaulting-eager-optional-input)
							(list :slot-class :visual-eager-optional-input))
						    (if defaulting
							(list :slot-class :visual-defaulting-optional-input)
							(list :slot-class :visual-optional-input)))
						(if eager
						    (if defaulting
							(list :slot-class :defaulting-eager-optional-input)
							(list :slot-class :eager-optional-input))
						    (if defaulting
							(list :slot-class :defaulting-optional-input)
							(list :slot-class :optional-input))))))
				 (when noticers
				   (list :noticers
					 (list
					  'list
					  (loop for noticer in (reverse noticers) for i from 0
						append
						(list 'list
						      `(named-lambda (:noticer (,(first input-definition) ,class-name ,i))
							   (self value)
							 (declare (ignorable self value))
							 (declare (type ,class-name self))
							 ,@noticer
							 (values))
						      :source `',noticer)))))))))
		      (t (error "Invalid input specification: ~S" input-definition)))))

(defun parse-attributes-section (class-name section)
  (loop for attribute-definition in section
	do (let ((slot (first attribute-definition)))
	     (if (gethash slot *slots-table*)
		 (duplicate-slot-error class-name slot)
		 (setf (gethash slot *slots-table*) t)))
	collect (cond ((and (consp attribute-definition)
			    (symbolp (first attribute-definition))
			    (not (keywordp (first attribute-definition))))
		       (let ((modifiable nil)
			     (uncached nil)
			     (descending nil)
			     (visual nil)
			     (eager nil)
			     (noticers ())
			     (list (copy-list (rest attribute-definition)))
			     (body nil))

			 (tagbody
			  start
			    (cond ((eq (first list) :visual) (setq visual t) (pop list) (go start))
				  ((eq (first list) :eager) (setq eager t) (pop list) (go start))
				  ((eq (first list) :modifiable) (setq modifiable t) (pop list) (go start))
				  ((eq (first list) :uncached) (setq uncached t) (pop list) (go start))
				  ((eq (first list) :descending) (setq descending t) (pop list) (go start))
				  ((and (consp (first list))
					(eq (first (first list)) :noticer))
				   (push (rest (pop list)) noticers) (go start))
				  ((and list
					(not (or (eq (first list) :modifiable)
						 (eq (first list) :uncached)
						 (eq (first list) :descending))))
				   (setq body list)
				   (go exit))
				  ((null list) (error "attribute definitions, ~S, must have at least one expression."
						      (first attribute-definition))))
			  exit)
			 (when descending
			   (push (first attribute-definition) *descending-attributes*))
			 (cond ((and modifiable uncached) (error "attribute definitions, ~S, cannot be both modifiable and uncached."
								 (first attribute-definition)))
			       ((and noticers (not modifiable))
				(error "only modifiable attributes and inputs cannot have :noticers: ~S"
				       (first attribute-definition)))
			       (modifiable (list* 'list
						  :name `',(first attribute-definition)
						  :slot-class (if visual
								  :visual-modifiable-attribute
								  :modifiable-attribute)
						  :function `(with-cnm-support (:modifiable-attribute (,(first attribute-definition) ,class-name))
							       (named-lambda (:modifiable-attribute (,(first attribute-definition) ,class-name))
								   (self)
								 (declare (ignorable self))
								 (declare (type ,class-name self))
								 ,@body))
						  :body `',body
						  (when noticers
						    (list :noticers
							  (list
							   'list
							   (loop for noticer in (reverse noticers) for i from 0
								 append
								 (list 'list
								       `(named-lambda (:noticer (,(first attribute-definition) ,class-name ,i))
									    (self value)
									  (declare (ignorable self value))
									  (declare (type ,class-name self))
									  ,@noticer
									  (values))
								       :source `',noticer)))))))

			       (uncached (list 'list
					       :name `',(first attribute-definition)
					       :slot-class :uncached-attribute
					       :function `(with-cnm-support (:uncached-attribute (,(first attribute-definition) ,class-name))
							    (named-lambda (:uncached-attribute (,(first attribute-definition) ,class-name))
								(self)
							      (declare (ignorable self))
							      (declare (type ,class-name self))
							      ,@body))
					       :body `',body))
			       (t (list 'list
					:name `',(first attribute-definition)
					:slot-class
					(if (eq (first attribute-definition) 'adhoc-scene-graph::global-matrix)
					    :global-matrix-attribute
					    (if eager
						:eager-ordinary-attribute
						:ordinary-attribute))
					:function `(with-cnm-support (:attribute (,(first attribute-definition) ,class-name))
						     (named-lambda (:attribute (,(first attribute-definition) ,class-name))
							 (self)
						       (declare (ignorable self))
						       (declare (type ,class-name self))
						       ,@body))
					:body `',body)))))
		      (t (error "Invalid attribute specification: ~S" attribute-definition)))))

(defun parse-components-section (class-name section &key (hidden? nil))
  (loop for component-definition in section
	do (let ((slot (first component-definition)))
	     (if (gethash slot *slots-table*)
		 (duplicate-slot-error class-name slot)
		 (setf (gethash slot *slots-table*) t)))
     collect (let ((plist)
		   (type-spec)
		   (eager nil)
		   (name)
		   (aggregate nil)
		   (indices nil)
		   (size nil))
	       (if (not (consp component-definition))
		   (error "Invalid component specification: ~S" component-definition)
		   (if (symbolp (first component-definition))
		       (progn
			 (setq name (first component-definition))
			 (setq aggregate (and (rest component-definition)
					      (getf (rest component-definition) :aggregate)))
			 (when aggregate
			   (if (eq (first aggregate) :size)
			       (setq size (second aggregate))
			       (if (eq (first aggregate) :indices)
				   (setq indices (second aggregate))
				   (error "Unknown aggregate specification: ~S"
					  aggregate)))))
		       (error "Invalid component specification: ~S" component-definition)))
		     
	       (setq plist (copy-list (rest component-definition)))
	       (remf plist :aggregate)
	       (setq type-spec (getf plist :type 'null))
	       (if (and (eq type-spec 'null))
		   (error "component definition ~S has no type expression"
			  (first component-definition))
		   (remf plist :type))

	       (setq eager (getf plist :eager nil))
	       (remf plist :eager)
		   
	       (push name *descending-attributes*)
		     
	       (if aggregate
		   (list* 'list
			  :name `',name
			  (append
			   (if size
			       (if eager
				   (list :slot-class :eager-array-aggregate)
				   (list :slot-class :array-aggregate))
			       (if eager
				   (list :slot-class :eager-table-aggregate)
				   (list :slot-class :table-aggregate)))
			   (when hidden? (list :hidden? t))
			   (list :type-function
				 (let ((value-sym (gensym)))
				   `(named-lambda (:aggregate-component-input :type (,class-name (,name . indices)))
					(self &rest indices)
				      (declare (ignorable self))
				      (declare (ignorable indices))
				      (declare (type ,class-name self))
				      (let ((,value-sym ,type-spec))
					(if (typep ,value-sym 'sequence)
					    (elt ,value-sym (first indices))
					    ,value-sym))))
				    
				 :type-expression `',type-spec
				    
				 :inputs (list* 'list
						(loop for (initarg expression) on plist by #'cddr
						   append (list `',initarg
								(list 'list
								      :function
								      `(named-lambda (:component-input ,initarg (,class-name ((,name part . indices) ?)))
									   (self part)
									 (declare (ignorable self part))
									 (declare (type ,class-name self))
									 ,@(when (and (constantp type-spec)
										      (eq (first type-spec) 'quote)
										      (find-class (second type-spec) nil))
									     `((declare (type ,(second type-spec) part))))
									 ,expression)
								      :expression `',expression)))))
			   (when size
			     (list :size-function
				   `(named-lambda (:aggregate-size (,class-name (,name)))
					(self)
				      (declare (ignorable self))
				      (declare (type ,class-name self))
				      ,size)
				   :size-expression `',size))
			   
			   (when indices
			     (list :indices-function
				   `(named-lambda (:aggregate-indices (,class-name (,name)))
					(self)
				      (declare (ignorable self))
				      (declare (type ,class-name self))
				      ,indices)
				   :indices-expression `',indices))))
		   (list 'list
			 :name `',name
			 :slot-class (if eager :eager-ordinary-component :ordinary-component)
			 :hidden? hidden?
			 :type-function `(named-lambda (:component-input :type (,class-name (,name ?)))
					     (self)
					   (declare (ignorable self))
					   (declare (type ,class-name self))
					   ,type-spec)
			 :type-expression `',type-spec
			       
			 :inputs (list* 'list
					(loop for (initarg expression) on plist by #'cddr
					   append (list `',initarg
							(list 'list
							      :function
							      `(named-lambda (:component-input ,initarg (,class-name (,(first component-definition) ?)))
								   (self part)
								 (declare (ignorable self part))
								 (declare (type ,class-name self))
								 ;; declare type of part if type is constant and also defined
								 ,@(when (and (constantp type-spec)
									      (eq (first type-spec) 'quote)
									      (find-class (second type-spec) nil))
								     `((declare (type ,(second type-spec) part))))
								 ,expression)
							      :expression `',expression)))))))))

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;; from closette
(defun canonicalize-direct-slot (class-name spec)
  (let ((s (if (symbolp spec) spec (car spec))))
    (if (gethash s *slots-table*)
	(duplicate-slot-error 'unknown s)
	(setf (gethash s *slots-table*) t)))
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
            (initfunction nil)
            (initform nil)
            (initargs ())
            (readers ())
            (writers ())
	    (getter ())
	    (setter ())
            (other-options ()))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform
             (setq initfunction
                   `(function (lambda () ,(cadr olist))))
             (setq initform `',(cadr olist)))
            (:initarg 
             (push-on-end (cadr olist) initargs))
            (:reader 
             (push-on-end (cadr olist) readers))
            (:writer 
             (push-on-end (cadr olist) writers))
	    (:getter
	     (setq getter `(with-cnm-support (:getter ,name (,class-name))
			     (#+sbcl sb-int::named-lambda
			      #+ccl ccl::named-lambda
			      #+allegro named-lambda
			      (:getter ,name (,class-name))
				     (self)
				     (declare (type ,class-name self))
				     ,@(cadr olist)))))
	    (:setter
	     (setq setter `(with-setter-cnm-support (:setter ,name (,class-name))
			     (#+sbcl sb-int::named-lambda
			      #+ccl ccl::named-lambda
			      #+allegro named-lambda
			      (:setter ,name (,class-name))
				     (self value)
				     (declare (type ,class-name self))
				     (declare (ignorable value))
				     ,@(cadr olist)))))
            (:accessor
             (push-on-end (cadr olist) readers)
             (push-on-end `(setf ,(cadr olist)) writers))
            (otherwise 
             (push-on-end `',(car olist) other-options)
             (push-on-end `',(cadr olist) other-options))))
	
	(when (and (eq (getf (cdr spec) :slot-class) :virtual)
		   (null getter))
	  (setq getter `(with-cnm-support (:getter ,name (,class-name))
			  (#+sbcl sb-int::named-lambda
			   #+ccl ccl::named-lambda
			   #+allegro named-lambda
			   (:getter ,name (,class-name))
				     (self)
				     (declare (type ,class-name self))
				     (call-next-method)))))
	
	(when (and (eq (getf (cdr spec) :slot-class) :virtual)
		   (null setter))
	  (setq setter `(with-setter-cnm-support (:setter ,name (,class-name))
			  (#+sbcl sb-int::named-lambda
			   #+ccl ccl::named-lambda
			   #+allegro named-lambda
			   (:setter ,name (,class-name))
				     (self value)
				     (declare (type ,class-name self))
				     (declare (ignorable value))
				     (call-next-method)))))
        `(list
          :name ',name
          ,@(when initfunction
              `(:initform ,initform
                :initfunction ,initfunction))
          ,@(when initargs `(:initargs ',initargs))
          ,@(when readers `(:readers ',readers))
          ,@(when writers `(:writers ',writers))
	  ,@(when getter `(:getter ,getter))
	  ,@(when setter `(:setter ,setter))
          ,@other-options))))


(defun parse-slots-section (class-name section)
  (mapcar #'(lambda (slot)
	      (canonicalize-direct-slot class-name slot)) section))

(defmethod defining-expression ((dslotd standard-direct-slot-definition))
  (let* ((class (slot-value dslotd +slotd-class-slot-name+))
	 (name (slot-definition-name dslotd))
	 (descending (find name (direct-descending-attributes class))))
    (append (list name)
	    (when descending
	      (list :descending t))
	    (apply #'append
		   (when (slot-definition-readers dslotd)
		     (mapcar #'(lambda (reader)
				 (list :reader reader))
			     (slot-definition-readers dslotd)))
		   (when (slot-definition-writers dslotd)
		     (mapcar #'(lambda (writer)
				 (list :writer writer))
			     (slot-definition-readers dslotd)))
		   (when (slot-definition-initargs dslotd)
		     (mapcar #'(lambda (initarg)
				 (list :initarg initarg))
			     (slot-definition-initargs dslotd)))
		   (when (slot-definition-initform dslotd)
		     (list (list :initform (slot-definition-initform dslotd))))
		   (when (slot-definition-allocation dslotd)
		     (list (list :allocation (slot-definition-allocation dslotd))))))))



;; forward declare object for defobject macro
(defclass object ()
  ()
  (:metaclass adhoc-class))


(defun defobject-expansion (name supers slots metaclass direct-default-initargs)
  (progn
    ;; forward declare type before compiling attribute bodies.
    (unless (find-class name nil)
      (ensure-class name :metaclass (or metaclass 'adhoc-class)))
    `(let ((old (find-class ',name nil)))
       (prog1 (ensure-class ',name
			    :metaclass ',metaclass
			    :direct-default-initargs (list ,@(loop for (initarg expression) on direct-default-initargs by #'cddr
								   collect #-allegro
								   `(list ',initarg ',expression
									  (lambda ()
									    ,expression))
								   #+allegro
								   `(list ',initarg
									  (lambda ()
									    ,expression)
									  ',expression)))
			    :direct-superclasses '(,@supers object)
			    :direct-slots (list ,@slots)
			    :direct-descending-attributes ',*descending-attributes*)
	 (when old
	   (make-instances-obsolete old))))))


(defmethod update-instance-for-redefined-class :after
    ((instance adhoc-mixin) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore initargs))
  (labels ((unbind-dependent (object new-slotd old-slotd)
	     (let* ((location (slot-definition-location new-slotd))
		    (maybe-basket (cond ((typep (class-of object) 'funcallable-standard-class)
					 (funcallable-standard-instance-access object location))
					(t (standard-instance-access-compat object location)))))
	       (unless (eq maybe-basket +slot-unbound+)
		 ;; preserve values of :set settable slots which are still settable-slots.
		 (unless (and (typep new-slotd 'settable-slot-definition-mixin)
			      (eq :set (basket-status maybe-basket)))
		   (cond ((typep (class-of object) 'funcallable-standard-class)
			  (setf (funcallable-standard-instance-access object location) +slot-unbound+))
			 (t (setf (standard-instance-access-compat object location) +slot-unbound+)))
		   (let ((dependents (dependents maybe-basket)))
		     (when (and (slot-boundp maybe-basket 'value)
				(typep old-slotd 'component-definition-mixin))
		       (scan-erase-object (slot-value maybe-basket 'value)))
		     (setf (dependents maybe-basket) nil)
		     (loop for dependent in dependents
			   do (destructuring-bind (object old-slotd indices) dependent
				(let* ((slot-name (slot-definition-name old-slotd))
				       (new-slotd (get-slot-definition
						   (class-of object) slot-name)))
				  (when new-slotd
				    (if indices
					(unbind-aggregate-member object indices)
					(unbind-dependent object new-slotd old-slotd)))))))))))
	   
	   (unbind-aggregate-member (aggregate indices)
	     (assert (typep aggregate 'aggregate-mixin))
	     ;; process the member slot:
	     (cond ((typep aggregate 'table-aggregate)
		    (when (slot-boundp aggregate 'value)
		      (let ((table (slot-value aggregate 'value)))
			(let ((member-basket (gethash indices table)))
			  (when member-basket
			    (unless (eq member-basket +slot-unbound+)
			      (when (slot-boundp member-basket 'value)
				(scan-erase-object (slot-value member-basket 'value)))
			      ;;(slot-makunbound member-basket 'value)
			      (remhash indices table)
			      (let ((dependents (dependents member-basket)))
				(setf (dependents member-basket) nil)
				(loop for dependent in dependents
				      do (destructuring-bind (object old-slotd indices) dependent
					   (let* ((slot-name (slot-definition-name old-slotd))
						  (new-slotd (get-slot-definition
							      (class-of object) slot-name)))
					     (when new-slotd
					       (if indices
						   (unbind-aggregate-member object indices)
						   (unbind-dependent object new-slotd old-slotd)))))))))))))
		   ((typep aggregate 'array-aggregate)
		    (when (slot-boundp aggregate 'value)
		      (let ((array (slot-value aggregate 'value)))
			(let ((member-basket (apply #'aref array indices)))
			  (unless (eq member-basket +slot-unbound+)
			    (slot-makunbound member-basket 'value)
			    (let ((dependents (dependents member-basket)))
			      (setf (dependents member-basket) nil)
			      (loop for dependent in dependents
				    do (destructuring-bind (object old-slotd indices) dependent
					 (let* ((slot-name (slot-definition-name old-slotd))
						(new-slotd (get-slot-definition
							    (class-of object) slot-name)))
					   (when new-slotd
					     (if indices
						 (unbind-aggregate-member object indices)
						 (unbind-dependent object new-slotd old-slotd))))))))))))))

	   (process-discarded-slots ()
	     (loop for (slot-name value) on property-list by #'cddr
		   when (or (typep value 'basket) (typep value 'aggregate-mixin))
		     do (let ((dependents (dependents value)))
			  
			  (when (slot-boundp value 'value)
			    (let ((thing (slot-value value 'value)))
			      (when (typep thing 'adhoc-scene-graph::node-mixin)
				(scan-erase-object thing))))
			  ;; not necessary here to setf (dependents value) to nil,
			  ;; since value will be GC'd
			  (loop for dependent in dependents
				do (destructuring-bind (object old-slotd indices) dependent
				     (let* ((slot-name (slot-definition-name old-slotd))
					    (new-slotd (get-slot-definition (class-of object) slot-name)))
				       (when new-slotd
					 (if indices
					     (unbind-aggregate-member object indices)
					     (unbind-dependent object new-slotd old-slotd)))))))))

	   (process-children-dependents ()
	     (let* ((class (class-of instance))
		    (dependents (children-dependents class)))
	       (loop for dependent in dependents
		     do (destructuring-bind (object old-slotd indices) dependent
			  (if indices
			      (let ((aggregate object))
				(when (eq (superior aggregate) instance)
				  (setf (children-dependents class)
					(remove (list object old-slotd indices)
						(children-dependents class)
						:test #'equal))
				  (unbind-aggregate-member aggregate indices)))
			      (when (eq object instance)
				(setf (children-dependents class)
				      (remove (list object old-slotd nil)
					      (children-dependents class)
					      :test #'equal))
				(let* ((slot-name (slot-definition-name old-slotd))
				       (new-slotd (get-slot-definition (class-of object) slot-name)))
				  (when new-slotd
				    (unbind-dependent object new-slotd old-slotd)))))))))

	   (process-remaining-dependencies ()
	     (let ((slotv (#+SBCL sb-pcl::std-instance-slots
			   #+ALLEGRO excl:std-instance-slots
			   #+CCL ccl::instance-slots
			   #-(OR SBCL ALLEGRO CCL) standard-instance-slots
			   instance))
		   #+CCL
		   (size (ccl::uvsize slotv))
		   (slotds (remove-if-not #'(lambda (slotd)
					      ;; we don't deal with allocation :class or allocation :none
					      (eq :instance (slot-definition-allocation slotd)))
					  (class-slots (class-of instance)))))

	       #+(OR ALLEGRO SBCL)
	       (loop for maybe-basket across slotv
		     for i from 0
		     for slotd in slotds
		     unless (= i (slot-definition-location slotd))
		       do (error "slotd location does not match slotv index ~S ~S"
				 (slot-definition-location slotd) i)
		     do (unless (eq maybe-basket +slot-unbound+)
			  (when (or (typep maybe-basket 'basket) (typep maybe-basket 'aggregate-mixin))
			    ;; preserve set settable slots set but process their dependents:
			    (unless (and (typep slotd 'settable-slot-definition-mixin)
					 (eq :set (basket-status maybe-basket)))
			      (setf (svref slotv i) +slot-unbound+))
			    
			    (let ((dependents (dependents maybe-basket)))
			      (loop for dependent in dependents
				    do (destructuring-bind (object old-slotd indices) dependent
					 (let* ((slot-name (slot-definition-name old-slotd))
						(new-slotd (get-slot-definition (class-of object) slot-name)))
					   (when new-slotd
					     (if indices
						 (unbind-aggregate-member object indices)
						 (unbind-dependent object new-slotd old-slotd))))))))))
	       #+CCL
	       (loop for i from 1 below size
		     for slotd in slotds
		     unless (= i (slot-definition-location slotd))
		       do (error "slotd location does not match slotv index ~S ~S"
				 (slot-definition-location slotd) i)
		     do (let ((maybe-basket (ccl::%svref slotv i)))
			  (unless (eq maybe-basket +slot-unbound+)
			    (when (or (typep maybe-basket 'basket) (typep maybe-basket 'aggregate-mixin))
			      ;; preserve set settable slots set but process their dependents:
			      (unless (and (typep slotd 'settable-slot-definition-mixin)
					   (eq :set (basket-status maybe-basket)))
				(setf (ccl::%svref slotv i) +slot-unbound+)
				
				(let ((dependents (dependents maybe-basket)))
				  (loop for dependent in dependents
					do (destructuring-bind (object old-slotd indices) dependent
					     (let* ((slot-name (slot-definition-name old-slotd))
						    (new-slotd (get-slot-definition (class-of object) slot-name)))
					       (when new-slotd
						 (if indices
						     (unbind-aggregate-member object indices)
						     (unbind-dependent
						      object new-slotd old-slotd)))))))))))))))
    
    ;; first, process the dependencies of the discarded slots:
    (process-discarded-slots)


    (scan-erase-object instance)
    ;; next, process the children-dependencies of the class
    ;; if we find ourself in these depedencies, remove the
    ;; dependency from children-dependents and process the dependency
    (process-children-dependents)
		   
    ;; next, any remaining dependencies in the slot-vector are processed
    ;; leaving the instance in an almost virgin state (except set settable slots)
    (process-remaining-dependencies)	     
    
    ;; finally, evaluate the eager slots in case the instance needs to be redrawn
    (initialize-eager-slots instance)

    (adhoc-scene-graph::rm-maybe-draw-node-recursively (send instance root))

    ;; return the instance.
    instance))

(defun scan-erase-object (instance &optional done-list)
  ;; erases a tree of objects ungeneratively without disturbing anything else.
  (when (typep instance 'adhoc-scene-graph::node-mixin)
    (adhoc-scene-graph::rm-erase-node instance)
    (let ((slotv (#+SBCL sb-pcl::std-instance-slots
		  #+ALLEGRO excl:std-instance-slots
		  #+CCL ccl::instance-slots
		  #-(OR SBCL ALLEGRO CCL) standard-instance-slots
		  instance))
	  #+CCL
	  (size (ccl::uvsize slotv))
	  (slotds (remove-if-not #'(lambda (slotd)
				     ;; we don't deal with allocation :class or allocation :none
				     (eq :instance (slot-definition-allocation slotd)))
				 (class-slots (class-of instance)))))

      #+(OR ALLEGRO SBCL)
      (loop for maybe-basket across slotv
	    for i from 0
	    for slotd in slotds
	    unless (= i (slot-definition-location slotd))
	      do (error "slotd location does not match slotv index ~S ~S"
			(slot-definition-location slotd) i)
	    do (unless (or (eq (slot-definition-name slotd) 'root)
			   (eq (slot-definition-name slotd) 'superior))
		 (unless (eq maybe-basket +slot-unbound+)
		   (if (typep maybe-basket 'basket)
		       (when (slot-boundp maybe-basket 'value)
			 (let ((value (slot-value maybe-basket 'value)))
			   (unless (eq value instance)
			     (unless (member value done-list)
			       (scan-erase-object value (cons value done-list))))))
		       (if (typep maybe-basket 'table-aggregate-mixin)
			   (when (slot-boundp maybe-basket 'value)
			     (let ((table (slot-value maybe-basket 'value)))
			       (maphash #'(lambda (k v)
					    (declare (ignore k))
					    (when (typep v 'basket)
					      (when (slot-boundp v 'value)
						(let ((value (slot-value v 'value)))
						  (unless (eq value instance)
						    (unless (member value done-list)
						      (scan-erase-object value (cons value done-list))))))))
					table)))
			   (when (typep maybe-basket 'array-aggregate-mixin)
			     (when (slot-boundp maybe-basket 'value)
			       (let ((array (slot-value maybe-basket 'value)))
				 (labels ((do-rank (dims &rest indices)
					    (when dims
					      (let ((dim (car dims)))
						(loop for i from 0 below dim
						      append (if (null (cdr dims))
								 (let ((maybe-basket
									 (apply #'aref array (cons i indices))))
								   (when (slot-boundp maybe-basket 'value)
								     (let ((value
									     (slot-value maybe-basket 'value)))
								       (unless (eq value instance)
									 (unless (member value done-list)
									   (scan-erase-object
									    value
									    (cons value done-list)))))))
								 (apply #'do-rank (cdr dims) i indices)))))))
				   (let* ((dims (array-dimensions array)))
				     (do-rank (reverse dims)))))))))))))))
  



    

			    


(defmacro defobject (name supers &body body)
  (let ((*descending-attributes* ()))
    (multiple-value-bind (slots metaclass direct-default-initargs) (parse-defobject-body name body)
      (defobject-expansion name supers slots (or metaclass 'adhoc-class) direct-default-initargs))))

(ensure-class 'null-object
	      :metaclass 'adhoc-class
	      :direct-superclasses '(adhoc-mixin))	      

(defmethod null-object-p (self)
  nil)

(defmethod null-object-p ((self null-object))
  t)



;; define object and its slots for real (without using defobject)
(ensure-class 'object
	      :metaclass 'adhoc-class
	      :direct-superclasses '(adhoc-mixin)
	      :direct-slots
              (list
	       (list :name 'index
		     :slot-class :ordinary-attribute
		     :function
		     (lambda (next-emfun self)
		       (declare (ignore next-emfun))
		       (funcall
			(named-lambda (:attribute
				       (root-path object))
			    (self)
			  (declare (type object self))
			  (let ((indices (the indices)))
			    (when (null (cdr indices))
			      (car indices))))
			self))
		     :body
		     '((let ((indices (the indices)))
			 (when (null (cdr indices))
			   (car indices)))))
	       (list :name 'children
		     :slot-class :ordinary-attribute
		     :function
		     (lambda (next-emfun self)
		       (declare (ignore next-emfun))
		       (funcall
			(named-lambda (:attribute
				       (children object))
			    (self)
			  (declare (type object self))
			  (remove-if #'null-object-p
				     (get-object-children self)))
			self))
		     :body '((get-object-children self)))
	       (list :name 'root-path
		     :slot-class :ordinary-attribute
		     :function
		     (lambda (next-emfun self)
		       (declare (ignore next-emfun))
		       (funcall
			(named-lambda (:attribute
				       (root-path object))
			    (self)
			  (declare (type object self))
			  (root-path self))
			self))
		     :body
		     '((root-path self))))
	      :direct-descending-attributes 'nil)

;; upgrade array-aggregate to include adhoc style messages:
(ensure-class 'array-aggregate
	      :metaclass 'funcallable-adhoc-class
	      :direct-superclasses '(array-aggregate-mixin)
	      :direct-slots
	      (list
	       (list :name 'size
		     :slot-class :ordinary-attribute
		     :function
		     (named-lambda (:attribute (size array-aggregate))
			 (next-emfun aggregate)
		       (declare (ignore next-emfun))
		       (declare (type array-aggregate aggregate))
		       (funcall (size-function (component-definition aggregate)) (superior aggregate))))
	       (list :name 'list-elements
		     :slot-class :ordinary-attribute
		     :function
		     (named-lambda (:attribute
				    (list-elements array-aggregate))
			 (next-emfun aggregate)
		       (declare (ignore next-emfun))
		       (declare (type array-aggregate aggregate))
		       (let ((instance (superior aggregate)))
			 (ensure-slot-value (class-of instance) instance (component-definition aggregate)))
		       (labels ((list-rank (dims &rest indices)
				  (when dims
				    (let ((dim (car dims)))
				      (loop for i from 0 below dim
					    append (if (null (cdr dims))
						       (list (apply aggregate (cons i indices)))
						       (apply #'list-rank (cdr dims) i indices)))))))
			 (let* ((%size (send aggregate size))
                                (dims (if (consp %size) %size (list %size))
                                      #+NO(array-dimensions (slot-value aggregate 'value))))
			   (list-rank (reverse dims)))))
		     :body '((labels ((list-rank (dims &rest indices)
					(when dims
					  (let ((dim (car dims)))
					    (loop for i from 0 below dim
					          append (if (null (cdr dims))
							     (list (apply aggregate (cons i indices)))
							     (apply #'list-rank (cdr dims) i indices)))))))
			       (let ((dims (array-dimensions (slot-value aggregate 'value))))
				 (list-rank (reverse dims))))))))

(ensure-class 'table-aggregate
	      :metaclass 'funcallable-adhoc-class
	      :direct-superclasses '(table-aggregate-mixin)
	      :direct-slots
	      (list
	       (list :name 'indices
		     :slot-class :ordinary-attribute
		     :function
		     (named-lambda (:attribute (indices table-aggregate))
			 (next-emfun aggregate)
		       (declare (ignore next-emfun))
		       (declare (type table-aggregate aggregate))
		       (funcall (indices-function (component-definition aggregate)) (superior aggregate))))
	       (list :name 'list-elements
		     :slot-class :ordinary-attribute
		     :function
		     (named-lambda (:attribute
				    (list-elements table-aggregate))
			 (next-emfun aggregate)
		       (declare (ignore next-emfun))
		       (declare (type table-aggregate aggregate))
		       (let ((instance (superior aggregate)))
			 (ensure-slot-value (class-of instance) instance (component-definition aggregate)))
		       (loop for indices in (send aggregate indices)
			     collect (apply #'aggregate-lookup aggregate indices)))
		     :body '((loop for indices in (send aggregate indices)
				   collect (apply #'aggregate-lookup aggregate indices))))))

(defmethod emit-defobject-body ((class adhoc-class) &optional add remove rename)
  (let* ((result ())
	 (sub (list :head))
	 (addz (loop for (kwd value) on add by #'cddr
		     append (mapcar #'first value)))
	 (dslotds (remove-if #'(lambda (slotd)
				 (or (member (slot-definition-name slotd) remove)
				     (member (slot-definition-name slotd) addz)))
			     (class-direct-slots class))))
    (loop for dslotd in dslotds
       for next in (append (rest dslotds) (list nil))
       with first? = t
       do (when first?
	    (push (slot-class-keyword dslotd) result)
	    (setq first? nil))

	  (let* ((defining-expression (defining-expression dslotd))
		 (rename-cell (assoc (if (symbolp defining-expression)
					 defining-expression
					 (first defining-expression))
				     rename)))

	   (when rename-cell
	     (setq defining-expression (if (listp defining-expression)
					   (list* (second rename-cell)
						  (cdr defining-expression))
					   (second rename-cell))))
	    
	   (push defining-expression (cdr sub)))

	 (if next
	     (unless (eq (slot-class-keyword dslotd)
			 (slot-class-keyword next))
	       (push (nreverse (cdr sub)) result)
	       (setq sub (list :head))
	       (push (slot-class-keyword next) result))
	     (push (nreverse (cdr sub)) result))
       finally (return-from emit-defobject-body
		 (append (nreverse result) add)))))

(defun serialize-adhoc-class (class &optional add remove rename)
  (append (list 'defobject)
	  (list (class-name class))
	  (list
	   (remove 'object
		   (remove 'adhoc-mixin (mapcar #'class-name
						(class-direct-superclasses class)))))
	  (unless (eq class (find-class 'adhoc-class))
	    (list :metaclass (class-name (class-of class))))
	  (emit-defobject-body class add remove rename)))
	 
(defmacro defobject-amendment (name &key (add nil) (remove nil) (rename nil))
  (let ((class (find-class name)))
    (serialize-adhoc-class class add remove rename)))

(defun add-runtime-component (&key class-name slot-name type-expression provided-inputs indices)
  (declare (ignore indices))
  (funcall (compile nil `(lambda ()
			   (defobject-amendment ,class-name
			     :add
			     (:components
			      ((,slot-name :type ,type-expression
					   ,@provided-inputs))))))))

(defun compute-relative-path (source destination)
  (compute-relative-path-1 (send source root-path)
			   (send destination root-path)))

(defun compute-relative-path-1 (src dest)
  (if (and (null src) (null dest))
      nil
      (if (eq (car src) (car dest))
	  (compute-relative-path-1 (cdr src) (cdr dest))
	  (append (make-list (length src) :initial-element 'superior) dest))))

;; parameters





(defmethod ensure-slot-value ((class adhoc-class) (instance adhoc-mixin)
			      (slotd effective-parametric-slot-definition))
  (let ((basket (slot-basket instance slotd)))
    (declare (type basket basket))
    (capture-direct-dependent instance basket)
    (when (or (not (slot-boundp basket 'value)) (recompute? instance slotd nil))
      (setf (slot-value basket 'value)
	    (flet ((get-toplevel ()
		     (if (attribute-function slotd)
			 (coerce (funcall (attribute-function slotd) instance) 'double-float)
			 0.0d0)))
	      (let ((component-definition (component-definition instance))
		    (initarg (first (slot-definition-initargs slotd))))
		(if component-definition
		    (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								 (slot-value instance 'indices))))
		      (flet ((normal-lookup ()
			       (let ((input-function (getf provided-input-function-plist initarg)))
				 (if input-function
				     (coerce (funcall input-function (superior instance) instance) 'double-float)
				     (get-toplevel)))))
			(let ((plist-function (getf provided-input-function-plist :@)))
			  (if plist-function
			      (let ((plist (funcall plist-function (superior instance) instance)))
				(let ((result (getf plist initarg +slot-unbound+)))
				  (if (eq result +slot-unbound+)
				      (normal-lookup)
				      (coerce result 'double-float))))
			      (normal-lookup)))))
		    (get-toplevel))))))
    basket))

(defun parse-parameters-section (class-name section)
  (loop for parameter-definition in section
	do (let ((slot (if (consp parameter-definition) (first parameter-definition) parameter-definition)))
	     (if (gethash slot *slots-table*)
		 (duplicate-slot-error class-name slot)
		 (setf (gethash slot *slots-table*) t)))
	collect (cond ((and (consp parameter-definition)
			    (symbolp (first parameter-definition))
			    (not (keywordp (first parameter-definition))))
		       (let* ((list (copy-list (rest parameter-definition)))
			      (descending nil)
			      (body nil))
			 (tagbody
			  start
			    (cond ((eq (first list) :descending) (setq descending t) (pop list) (go start))
				  ((and list (not (eq (first list) :descending)))
				   (setq body list)
				   (go exit))
				  ((null list) (go exit)))
			  exit)
			 (when descending (push (first parameter-definition) *descending-attributes*))
			 (list* 'list :name
				`',(first parameter-definition)
				:initargs `'(,(first parameter-definition))
				:slot-class :parameter
				(when body
				  (list :function
					`(with-cnm-support (:parameter (,(first parameter-definition) ,class-name))
					   
					   (#+SBCL sb-int:named-lambda
					    #+ccl ccl::named-lambda
					    #+allegro named-lambda
					    (:parameter (,(first parameter-definition) ,class-name))
					       (self)
					     (declare (ignorable self))
					     (declare (type ,class-name self))
					     ,@body))
					:body `',body)))))
		      ((and (symbolp parameter-definition)
			    (not (keywordp parameter-definition)))
		       (list 'list :name `',parameter-definition
				   :initargs `'(,parameter-definition)
				   :slot-class :parameter))
			 
		      (t (error "Invalid parameter specification: ~S" parameter-definition)))))

(defun parse-defobject-body (class-name body)
  (let ((*slots-table* (make-hash-table)))
    (let* ((metaclass nil)
	   (direct-default-initargs nil)
	   (all-slots
	     (append
	      (loop for (keyword section) on body by #'cddr
		    append (ecase keyword
			     (:default-initargs (progn (setf direct-default-initargs
							     (append direct-default-initargs section))
						       nil))
			     (:metaclass (if (and (symbolp section) (not (keywordp section)))
					     (progn (setq metaclass section)
						    nil)
					     (error "invalid metaclass: ~S" section)))
			     (:slots (parse-slots-section class-name section))
			     (:inputs (parse-inputs-section class-name section))
			     (:attributes (parse-attributes-section class-name section))
			     (:components (parse-components-section class-name section))
			     (:hidden-components (parse-components-section class-name section :hidden? t))
			     (:parameters (parse-parameters-section class-name section)))))))
      (values all-slots metaclass direct-default-initargs))))
