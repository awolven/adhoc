;; Copyright Andrew K. Wolven 2008, 20021
;; This file is released under the GNU General Public License v3
;; See LICENSE.txt in the main directory for details

(in-package :adhoc)

(defvar self nil)

(define-symbol-macro +slot-unbound+ #+sbcl sb-pcl::+slot-unbound+ #+ccl (ccl::%slot-unbound-marker))

#+CCL
(defmacro standard-instance-access-compat (instance location)
  `(handler-case (ccl::standard-instance-instance-location-access ,instance ,location)
    (error () +slot-unbound+)))

#+sbcl
(defmacro standard-instance-access-compat (instance location)
  `(standard-instance-access ,instance ,location))

#+ccl
(defmacro named-lambda (name (&rest arglist) &body body)
  `(ccl::nlambda ,name (,@arglist)
     ,@body))

#+sbcl
(defmacro named-lambda (name (&rest arglist) &body body)
  `(sb-int::named-lambda ,name (,@arglist)
     ,@body))

(defparameter +slotd-class-slot-name+ #+ccl 'ccl::CLASS #+sbcl 'sb-pcl::%class)

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

(defclass basic-attribute-definition-mixin () ())

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

;; attribute-body is for adhoc-class serialization
(defclass direct-attribute-function-mixin (attribute-function-mixin
					   direct-basic-attribute-definition-mixin)
  ((attribute-body :accessor attribute-body)))

(defclass effective-attribute-function-mixin (attribute-function-mixin
					      effective-basic-attribute-definition-mixin)
  ())

(defclass defaulting-attribute-mixin () ())

(defclass direct-ordinary-input-definition-mixin (direct-input-definition-mixin) ())

(defmethod attribute-function ((dslotd direct-ordinary-input-definition-mixin))
  nil)

(defclass effective-ordinary-input-definition-mixin (effective-input-definition-mixin) ())

(defclass direct-ordinary-input-definition (direct-settable-slot-definition-mixin
					    direct-ordinary-input-definition-mixin
					    standard-direct-slot-definition)
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

(defclass direct-defaulting-ordinary-input-definition (defaulting-attribute-mixin
						       direct-settable-slot-definition-mixin
						       direct-ordinary-input-definition-mixin
						       standard-direct-slot-definition)
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
    (defaulting-attribute-mixin
     effective-settable-slot-definition-mixin
     effective-ordinary-input-definition-mixin
     standard-effective-slot-definition)
  ())

(defclass optional-input-definition-mixin (attribute-function-mixin) ())

(defclass direct-optional-input-definition (direct-settable-slot-definition-mixin
					    direct-input-definition-mixin
					    direct-attribute-function-mixin
					    optional-input-definition-mixin
					    standard-direct-slot-definition)
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

(defclass direct-defaulting-optional-input-definition (defaulting-attribute-mixin
						       optional-input-definition-mixin
						       direct-settable-slot-definition-mixin
						       direct-input-definition-mixin
						       direct-attribute-function-mixin
						       standard-direct-slot-definition)
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
							  effective-input-definition-mixin
							  defaulting-attribute-mixin
							  effective-attribute-function-mixin
							  standard-effective-slot-definition)
  ())

;; serialization
(defmethod slot-class-keyword ((slotd basic-attribute-definition-mixin))
  :attributes)

(defclass direct-ordinary-attribute-definition (direct-attribute-function-mixin
						standard-direct-slot-definition)
  ())

(defclass effective-ordinary-attribute-definition (effective-non-settable-slot-definition-mixin
						   effective-attribute-function-mixin
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

(defclass uncached-attribute-mixin () ())

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

(defclass effective-component-definition-mixin (effective-non-settable-slot-definition-mixin
						effective-basic-attribute-definition-mixin)
  ())

(defclass ordinary-component-definition-mixin (component-definition-mixin)
  ())

(defclass direct-ordinary-component-definition (ordinary-component-definition-mixin
						direct-component-definition-mixin
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

(defclass direct-array-aggregate-component-definition-mixin (array-aggregate-component-definition-mixin
							     direct-component-definition-mixin
							     direct-non-settable-slot-definition-mixin)
  ((size-expression :accessor size-expression)))

(defclass direct-table-aggregate-component-definition-mixin (table-aggregate-component-definition-mixin
							     direct-component-definition-mixin
							     direct-non-settable-slot-definition-mixin)
  ((indices-expression :accessor indices-expression)))

(defclass effective-array-aggregate-component-definition-mixin (array-aggregate-component-definition-mixin
								effective-component-definition-mixin
								effective-non-settable-slot-definition-mixin)
  ())

(defclass effective-table-aggregate-component-definition-mixin (table-aggregate-component-definition-mixin
								effective-component-definition-mixin
								effective-non-settable-slot-definition-mixin)
  ())

(defclass direct-array-aggregate-component-definition (direct-array-aggregate-component-definition-mixin
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

(defclass effective-array-aggregate-component-definition (effective-array-aggregate-component-definition-mixin
							  standard-effective-slot-definition)
  ())

(defclass effective-table-aggregate-component-definition (effective-table-aggregate-component-definition-mixin
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

(defclass funcallable-adhoc-class (funcallable-standard-class)
  ())

(defmethod validate-superclass ((c1 funcallable-adhoc-class) (c2 funcallable-standard-class))
  t)

(defun finalize-inheritance-lite (adhoc-class)
  #+sbcl(sb-pcl::update-class adhoc-class t)
  #+ccl(let ((ccl::*update-slots-preserve-existing-wrapper* t))
	 (ccl::update-class adhoc-class t))
  (adhoc-class-finalize-inheritance-after adhoc-class))

(defun adhoc-class-finalize-inheritance-after (class)
  (let* ((adhoc-classes (remove-if-not #'(lambda (class)
					   (typep class 'adhoc-class))
				       (class-precedence-list class)))
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

(defmethod validate-superclass ((c1 adhoc-class) (c2 standard-class))
  t)

(defmethod direct-slot-definition-class ((class adhoc-class) &rest initargs)
  (case (getf initargs :slot-class)
    (:ordinary-input (load-time-value (find-class 'direct-ordinary-input-definition)))
    (:defaulting-ordinary-input (load-time-value (find-class 'direct-defaulting-ordinary-input-definition)))
    (:optional-input (load-time-value (find-class 'direct-optional-input-definition)))
    (:defaulting-optional-input (load-time-value (find-class 'direct-defaulting-optional-input-definition)))
    (:ordinary-attribute (load-time-value (find-class 'direct-ordinary-attribute-definition)))
    (:modifiable-attribute (load-time-value (find-class 'direct-modifiable-attribute-definition)))
    (:uncached-attribute (load-time-value (find-class 'direct-uncached-attribute-definition)))
    (:ordinary-component (load-time-value (find-class 'direct-ordinary-component-definition)))
    (:array-aggregate (load-time-value (find-class 'direct-array-aggregate-component-definition)))
    (:table-aggregate (load-time-value (find-class 'direct-table-aggregate-component-definition)))
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
	 (dslotd (loop for c in (class-precedence-list class)
		    do (let ((dslotd (find name (class-direct-slots c)
					   :key #'slot-definition-name :test #'eq)))
			 (when dslotd (return dslotd))))))
    (typecase dslotd
      (direct-ordinary-input-definition (load-time-value (find-class 'effective-ordinary-input-definition)))
      (direct-defaulting-ordinary-input-definition (load-time-value (find-class 'effective-defaulting-ordinary-input-definition)))
      (direct-optional-input-definition (load-time-value (find-class 'effective-optional-input-definition)))
      (direct-defaulting-optional-input-definition (load-time-value (find-class 'effective-defaulting-optional-input-definition)))
      (direct-ordinary-attribute-definition (load-time-value (find-class 'effective-ordinary-attribute-definition)))
      (direct-modifiable-attribute-definition (load-time-value (find-class 'effective-modifiable-attribute-definition)))
      (direct-uncached-attribute-definition (load-time-value (find-class 'effective-uncached-attribute-definition)))
      (direct-ordinary-component-definition (load-time-value (find-class 'effective-ordinary-component-definition)))
      (direct-array-aggregate-component-definition (load-time-value (find-class 'effective-array-aggregate-component-definition)))
      (direct-table-aggregate-component-definition (load-time-value (find-class 'effective-table-aggregate-component-definition)))
      ;;(direct-maintained-slot-definition (load-time-value (find-class 'effective-maintained-slot-definition)))
      (t (call-next-method)))))

(defmethod effective-slot-definition-class ((class funcallable-adhoc-class) &rest initargs)
  (let* ((name (getf initargs :name))
	 (dslotd (loop for c in (class-precedence-list class)
		    do (let ((dslotd (find name (class-direct-slots c)
					   :key #'slot-definition-name :test #'eq)))
			 (when dslotd (return dslotd))))))
    (typecase dslotd
      (direct-ordinary-attribute-definition
       (load-time-value (find-class 'effective-ordinary-attribute-definition)))
      (t (call-next-method)))))

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

(defmethod shared-initialize :after ((instance adhoc-mixin) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (unless (superior instance)
    (setf (slot-value instance 'root) instance))
  (values))

(defmethod shared-initialize :around ((instance adhoc-mixin) slot-names &rest initargs)
  (declare (ignore initargs slot-names))
  (prog1 (call-next-method)
    (setf (slot-value instance 'inittest) t)))

(defgeneric aggregate-lookup (aggregate &rest indices))


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

(defclass variable ()
  ((value :accessor variable-value :initarg :value)
   (dependents :accessor dependents :initform nil)
   (root-path :reader variable-root-path :initarg :root-path)
   (slot-name :reader slot-name :initarg :slot-name)
   (instance :accessor variable-instance :initarg :instance)))

;; to have a place where settable-slots can differentiate between cached and setf'd
(defclass settable-variable (variable)
  ((status :accessor variable-status :initform nil)))

;; to have an dependency maintenance location for the class of the subpart
(defclass component-variable (variable)
  ((class)
   (indices :initarg :indices)))

(defclass aggregate-mixin (standard-generic-function)
  ((root :initform nil :initarg :root)
   (superior :reader superior :initform nil :initarg :superior)
   (component-definition :reader component-definition :initform nil :initarg :component-definition)
   (value :reader variable-value :initarg :value)
   (dependents :accessor dependents :initform nil)
   (root-path :reader variable-root-path :initarg :root-path)
   (slot-name :reader slot-name :initarg :slot-name)
   (instance :accessor instance :initarg :instance))
  (:metaclass funcallable-adhoc-class))  

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

(defun variable-p (thing)
  (or (typep thing 'variable)
      (typep thing 'aggregate-mixin)))

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
    #+NOTYET
    (when (slot-boundp object 'slot-name)
      (princ (slot-name object) stream))))

(defmethod print-object ((object table-aggregate) stream)
  (print-unreadable-object (object stream)
    (princ "TABLE-AGGREGATE " stream)
    #+NOTYET
    (when (slot-boundp object 'slot-name)
      (princ (slot-name object) stream))))

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
(defmethod variable-type ((slotd t))
  nil)

(defmethod variable-type ((slotd basic-attribute-definition-mixin))
  (load-time-value (find-class 'variable)))

(defmethod variable-type ((slotd effective-settable-slot-definition-mixin))
  (load-time-value (find-class 'settable-variable)))

(defmethod variable-type ((slotd component-definition-mixin))
  (load-time-value (find-class 'component-variable)))

(defmethod variable-type ((slotd array-aggregate-component-definition-mixin))
  (load-time-value (find-class 'array-aggregate)))

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

;; slot variable is like slot-value except it returns the variable instead of the value
;; used in the constraint solver or for debugging purposes

(defmethod slot-variable (instance (slot-name symbol))
  (let* ((class (class-of instance))
	 (location (gethash slot-name (slot-locations class))))
    (if (null location)
	(slot-missing (class-of instance) instance slot-name 'slot-value)
        ;; if it's already there it's fast, else it's slow
	(let ((maybe-variable (standard-instance-access-compat instance location)))
	  (if (eq +slot-unbound+ maybe-variable)
	      (let ((slotd (get-slot-definition class location)))
		(setf (standard-instance-access instance location)
		      (make-instance (variable-type slotd)
				     :root-path (root-path2 instance) ;; this will be a slowdown
				     :slot-name (slot-definition-name slotd)
                                     :instance instance)))
	      maybe-variable)))))

(defmethod (setf slot-variable) (variable instance (slot-name symbol))
  (let* ((class (class-of instance))
	 (location (gethash slot-name (slot-locations class))))
    (if (null location)
	(slot-missing (class-of instance) instance slot-name 'slot-value)
	(setf (standard-instance-access instance location) variable))))

(defmethod slot-variable (instance (eslotd basic-attribute-definition-mixin))
  (get-variable instance eslotd))

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

(defun get-variable (instance slotd &optional (root nil root-present-p)
				      (superior nil superior-present-p)
				      (component-definition nil component-definition-present-p))
  (let* ((location (slot-definition-location slotd))
	 (maybe-variable (standard-instance-access-compat instance location)))
    (if (eq +slot-unbound+ maybe-variable)
	(let* ((variable-type (variable-type slotd))
	       (variable (apply #'make-instance variable-type
			        :root-path (root-path2 instance) ;; this will be a slowdown
			        :slot-name (slot-definition-name slotd)
                                :instance instance
			        (append (when root-present-p
				          (list :root root))
				        (when superior-present-p
				          (list :superior superior))
				        (when component-definition-present-p
				          (list :component-definition component-definition))))))
	  (setf (standard-instance-access instance location) variable))
	maybe-variable)))

(defun get-variable-fi (owner instance slotd)
  (let* ((location (slot-definition-location slotd))
	 (maybe-variable (funcallable-standard-instance-access instance location)))
    (if (eq +slot-unbound+ maybe-variable)
	(let* ((variable-type (variable-type slotd))
	       (variable (make-instance variable-type
					:root-path (root-path2 instance) ;; this will be a slowdown
					:slot-name (slot-definition-name slotd)
                    :instance owner)))
	  (setf (funcallable-standard-instance-access instance location) variable))
	maybe-variable)))


(defmethod (setf slot-value-using-class) (value (class adhoc-class) instance
					  (slotd effective-settable-slot-definition-mixin))
  (let ((variable (get-variable instance slotd)))
    (prog1
	(setf (slot-value variable 'value) value)
      
      (unbind-dependent-variables variable :force t)
      (unwind-protect
	   (when (noticers slotd)
	     ;; noticers will not run until inittest is set to non-nil value
	     (when (and (slot-boundp instance 'inittest)
			(slot-value instance 'inittest))
	       (loop for noticer in (noticers slotd)
		     do
			(funcall noticer instance value))))
	(setf (variable-status variable) :set)))))

(defmethod (setf slot-value-using-class) (value (class adhoc-class) instance
					  (slotd effective-non-settable-slot-definition-mixin))
  (declare (ignore value instance))
  (error "slot ~S of type ~S is read only" (slot-definition-name slotd) (class-name (class-of slotd))))

(defmethod slot-makunbound-using-class ((class adhoc-class) instance (slotd effective-basic-attribute-definition-mixin))
  (let ((variable (get-variable instance slotd)))
    (slot-makunbound variable 'value)
    (unbind-dependent-variables variable :force nil)
    (values)))

(defparameter *dependent* nil)

(defmacro with-dependent-notification ((variable) &body body)
  `(let ((*dependent* ,variable))
     ,@body))

(declaim (inline capture-direct-dependency))
(defun capture-direct-dependency (variable)
  (when *dependent*
    (pushnew *dependent* (dependents variable) :test #'eq)))

(defmacro without-dependency-capture (&body body)
  `(let ((*dependent* nil))
     ,@body))

(defmethod slot-value-using-class ((class adhoc-class) instance (slotd effective-attribute-function-mixin))
  ;; we must capture any dependency coming from any attribute access which,
  ;; in it's expression, somehow landed us here.
  ;; however, we only need to send notification when we are actually evaluating any code, since that is the only time
  ;; that any other attributes will be accessed, not during a cache fetch
  ;; capture-direct-dependency and with-dependent-notification perform these functions, respectively.
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(setf (slot-value variable 'value)
	      (with-dependent-notification (variable)
		(funcall (attribute-function slotd) instance))))))

(defmethod slot-value-using-class ((class funcallable-adhoc-class) instance (slotd effective-attribute-function-mixin))
  ;; we must capture any dependency coming from any attribute access which,
  ;; in it's expression, somehow landed us here.
  ;; however, we only need to send notification when we are actually evaluating any code, since that is the only time
  ;; that any other attributes will be accessed, not during a cache fetch
  ;; capture-direct-dependency and with-dependent-notification perform these functions, respectively.
  (let* ((variable (get-variable-fi (slot-value instance 'superior) instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(setf (slot-value variable 'value)
	      (with-dependent-notification (variable)
		(funcall (attribute-function slotd) instance))))))

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

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-component-definition-mixin))
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'class)
	(if (slot-boundp variable 'value)
	    (slot-value variable 'value)
	    (setf (slot-value variable 'value)
		  (make-instance (slot-value variable 'class)
				 :root (slot-value instance 'root)
				 :superior instance
				 :component-definition slotd)))
	(progn
	  (setf (slot-value variable 'class)
		(with-dependent-notification (variable)
		  (let ((type (funcall (class-spec-function slotd) instance)))
		    (if (symbolp type)
			(find-class type)
			type))))
	  (setf (slot-value variable 'value)
		(make-instance (slot-value variable 'class)
			       :root (slot-value instance 'root)
			       :superior instance
			       :component-definition slotd))))))


(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd aggregate-component-definition-mixin))
  (let* ((variable (get-variable instance slotd (slot-value instance 'root) instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(progn
	  (setf (slot-value variable 'value)
		(with-dependent-notification (variable)
		  (if (typep slotd 'table-aggregate-component-definition-mixin)
		      (make-hash-table :test #'equalp)
		      (let ((size (funcall (size-function slotd) instance)))
			(make-array size :initial-element nil)))))
	  (set-funcallable-instance-function
	   variable
	   #'(lambda (&rest indices)
	       (apply #'aggregate-lookup variable indices)))))
    variable))
  
(defmethod slot-value-using-class ((class adhoc-class) instance
				                   (slotd effective-ordinary-input-definition))
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	    (slot-value variable 'value)
	    (setf (slot-value variable 'value)
	          (flet ((unbound ()
		               (slot-unbound class instance (slot-definition-name slotd))))
		        (let ((component-definition (component-definition instance))
		              (initarg (first (slot-definition-initargs slotd))))
		          (if component-definition
		              (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								                                   (slot-value instance 'indices))))
			            (flet ((normal-lookup ()
				                 (let ((input-function (getf provided-input-function-plist initarg)))
				                   (if input-function
				                       (with-dependent-notification (variable)
					                     (funcall input-function (superior instance) instance))
				                       (unbound)))))
			              (let ((plist-function (getf provided-input-function-plist :@)))
			                (if plist-function
				                (let ((plist (with-dependent-notification (variable)
					                           (funcall plist-function (superior instance) instance))))
				                  (let ((result (getf plist initarg +slot-unbound+)))
				                    (if (eq result +slot-unbound+)
					                    (normal-lookup)
					                    result)))
				                (normal-lookup)))))
		              (unbound))))))))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-optional-input-definition))
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(setf (slot-value variable 'value)
	      (flet ((get-toplevel ()
		       (with-dependent-notification (variable)
			 (funcall (attribute-function slotd) instance))))
		(let ((component-definition (component-definition instance))
		      (initarg (first (slot-definition-initargs slotd))))
		  (if component-definition
		      (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								   (slot-value instance 'indices))))
			(flet ((normal-lookup ()
				 (let ((input-function (getf provided-input-function-plist initarg)))
				   (if input-function
				       (with-dependent-notification (variable)
					 (funcall input-function (superior instance) instance))
				       (get-toplevel)))))
			  (let ((plist-function (getf provided-input-function-plist :@)))
			    (if plist-function
				(let ((plist (with-dependent-notification (variable)
					       (funcall plist-function (superior instance) instance))))
				  (let ((result (getf plist initarg +slot-unbound+)))
				    (if (eq result +slot-unbound+)
					(normal-lookup)
					result)))
				(normal-lookup)))))
		      (get-toplevel))))))))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-defaulting-optional-input-definition))
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(setf (slot-value variable 'value)
	      (flet ((get-toplevel ()
		       (with-dependent-notification (variable)
			 (funcall (attribute-function slotd) instance))))
		(let ((component-definition (component-definition instance))
		      (initarg (first (slot-definition-initargs slotd))))
		  (if component-definition
		      (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								   (slot-value instance 'indices))))
			(labels ((normal-lookup ()
				   (let* ((input-function (getf provided-input-function-plist initarg)))
				     (if input-function
					 (with-dependent-notification (variable)
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
				      (let ((plist (with-dependent-notification (variable)
						     (funcall plist-function (superior instance) instance))))
					(let ((result (getf plist initarg +slot-unbound+)))
					  (if (eq result +slot-unbound+)
					      (normal-lookup)
					      result)))
				      (normal-lookup)))))))
		      (get-toplevel))))))))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-defaulting-ordinary-input-definition))
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(let ((component-definition (component-definition instance))
	      (slotd-name (slot-definition-name slotd))
	      (initarg (first (slot-definition-initargs slotd))))
	  (if component-definition
	      (labels ((answer-locally ()
			 (let* ((provided-input-function-plist (apply #'provided-inputs component-definition
								      (slot-value instance 'indices)))
				(input-function (getf provided-input-function-plist initarg)))
			   (if input-function
			       (with-dependent-notification (variable)
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
	      (slot-unbound class instance slotd-name))))))

(defmethod slot-value-using-class ((class adhoc-class) instance
				   (slotd effective-maintained-slot-definition))
  (let* ((variable (get-variable instance slotd)))
    (capture-direct-dependency variable)
    (if (slot-boundp variable 'value)
	(slot-value variable 'value)
	(slot-unbound class instance (slot-definition-name slotd)))))

(defmethod aggregate-lookup ((aggregate table-aggregate) &rest indices)
  (let* ((table (slot-value aggregate 'value))
	     (element (gethash indices table nil)))
    (if element
	    (progn (capture-direct-dependency aggregate) element)
	
	    (if (member indices (with-dependent-notification (aggregate)
			                  (send aggregate indices))
		            :test #'equalp)
	        (progn (capture-direct-dependency aggregate)
		           (setf (gethash indices table)
			             (make-instance (with-dependent-notification (aggregate)
					                      (apply (type-function (component-definition aggregate))
						                         (superior aggregate)
						                         indices))
					                    :root (slot-value aggregate 'root)
					                    :superior (slot-value aggregate 'superior)
					                    :component-definition (component-definition aggregate)
					                    :aggregate aggregate
					                    :indices indices)))
	        (error "~S is not an active index" indices)))))

(defmethod aggregate-lookup ((aggregate array-aggregate) &rest indices)
  (let* ((array (slot-value aggregate 'value))
	     (element (handler-bind ((#+SBCL sb-int:invalid-array-index-error
				                  #+CCL error
				                  #'(lambda (e)
				                      (declare (ignore e))
				                      (error "Aggregate indices ~S for ~S on ~S not found."
					                         indices
					                         (slot-value aggregate 'slot-name)
					                         (superior aggregate)))))
		            (apply #'aref array indices))))

    (if element
	    (progn
	      (capture-direct-dependency aggregate)
	      element)
	    (flet ((new-instance ()
		         (with-dependent-notification (aggregate)
                   ;; to capture the dependency of the child object in any slot size-function calls
		           (send aggregate size)
		           (make-instance (apply (type-function (component-definition aggregate))
					                     (superior aggregate)
					                     indices)
				                  :root (slot-value aggregate 'root)
				                  :superior (slot-value aggregate 'superior)
				                  :component-definition (component-definition aggregate)
				                  :aggregate aggregate
				                  :indices indices))))
	      (capture-direct-dependency aggregate)
	      (setf (apply #'aref array indices)
		        (new-instance))))))

(defmethod unbind-this-variable ((variable variable))
  (slot-makunbound variable 'value)
  (values))

(defmethod unbind-this-variable ((variable settable-variable))
  (if (eq (variable-status variable) :set)
      (values)
      (call-next-method)))

(defmethod unbind-this-variable ((variable component-variable))
  (call-next-method)
  (slot-makunbound variable 'class)
  (values))

(defmethod unbind-dependent-variables (variable &key force)
  (declare (ignore force))
  (loop while (dependents variable)
	do
	   (let ((dependent-variable (pop (dependents variable))))
	     (unwind-protect (unbind-dependent-variables dependent-variable)
	       (unbind-this-variable dependent-variable)))))

(defmethod unbind-dependent-variables ((variable settable-variable) &key (force nil))
  (if (eq (variable-status variable) :set)
      (if force
	  (call-next-method)
	  (values))
      (call-next-method)))

(defmethod unbind-dependent-variables ((variable array-aggregate-mixin) &key force)
  (declare (ignore force))
  (unwind-protect (when (slot-boundp variable 'value)
		    (let ((value (slot-value variable 'value)))
		      (loop for element across value
			    when element
			      do (unbind-instance-variables element))))
    (call-next-method))
  (values))

(defmethod unbind-dependent-variables ((variable table-aggregate-mixin) &key force)
  (declare (ignore force))
  (when (slot-boundp variable 'value)
    (let ((value (slot-value variable 'value)))
      (unwind-protect (maphash #'(lambda (k v)
				   (declare (ignore k))
				   (unbind-instance-variables v))
			       value)
	(clrhash value))))
  (call-next-method)
  (values))

(defmethod unbind-this-variable ((variable aggregate-mixin))
  (slot-makunbound variable 'value)
  (values))

(defun unbind-instance-variables (instance)
  (let* ((class (class-of instance))
	 (eslotds (class-slots class)))
    (loop for eslotd in eslotds
          when (and (typep eslotd 'basic-attribute-definition-mixin)
		    (let ((allocation (slot-definition-allocation eslotd)))
		      (and allocation (not (eq allocation :none)))))
       do (let ((maybe-variable (standard-instance-access-compat instance (slot-definition-location eslotd))))
	    (unless (eq +slot-unbound+ maybe-variable)
	      (when (variable-p maybe-variable)
		(unwind-protect (unbind-dependent-variables maybe-variable)
		  (unbind-this-variable maybe-variable))))))
    (values)))
  
      

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
		   (has-descending? instance slot-name)))))

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
    (when *dependent*
      (pushnew *dependent* (children-dependents class) :test #'eq))
    (let ((eslotds (component-eslotds class)))
      (loop for eslotd in eslotds
	        append (let ((c (slot-value-using-class class object eslotd)))
		             (if (typep c 'aggregate-mixin)
		                 (send c list-elements)
		                 (list c)))))))

(defmethod get-hidden-children ((object adhoc-mixin))
  (let* ((class (class-of object)))
    (finalize-inheritance-lite class)
    (when *dependent*
      (pushnew *dependent* (children-dependents class) :test #'eq))
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
			     (noticers ())
			     (list (copy-list (rest input-definition)))
			     (body nil))
			 (tagbody
			  start
			    (cond ((eq (first list) :defaulting) (setq defaulting t) (pop list) (go start))
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
				     (if defaulting
					 (list :slot-class :defaulting-ordinary-input)
					 (list :slot-class :ordinary-input))
				     (list* :body `',body
					    :function `(with-cnm-support (:input (,(first input-definition) ,class-name))
							 (named-lambda (:input (,(first input-definition) ,class-name))
							     (self)
							   (declare (ignorable self))
							   (declare (type ,class-name self))
							   ,@body))
					    (if defaulting
						(list :slot-class :defaulting-optional-input)
						(list :slot-class :optional-input))))
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
			  (noticers ())
			  (list (copy-list (rest attribute-definition)))
			  (body nil))

		      (tagbody
		       start
			 (cond ((eq (first list) :modifiable) (setq modifiable t) (pop list) (go start))
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
					       :slot-class :modifiable-attribute
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
					    :allocation :none
					    :function `(with-cnm-support (:uncached-attribute (,(first attribute-definition) ,class-name))
							 (named-lambda (:uncached-attribute (,(first attribute-definition) ,class-name))
							     (self)
							   (declare (ignorable self))
							   (declare (type ,class-name self))
							   ,@body))
					    :body `',body))
			    (t (list 'list
				      :name `',(first attribute-definition)
				      :slot-class :ordinary-attribute
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
		   
	       (push name *descending-attributes*)
		     
	       (if aggregate
		   (list* 'list
			  :name `',name
			  (append
			   (if size
			       (list :slot-class :array-aggregate)
			       (list :slot-class :table-aggregate))
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
			 :slot-class :ordinary-component
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
(defun canonicalize-direct-slot (spec)
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
            (:accessor
             (push-on-end (cadr olist) readers)
             (push-on-end `(setf ,(cadr olist)) writers))
            (otherwise 
             (push-on-end `',(car olist) other-options)
             (push-on-end `',(cadr olist) other-options))))
        `(list
          :name ',name
          ,@(when initfunction
              `(:initform ,initform
                :initfunction ,initfunction))
          ,@(when initargs `(:initargs ',initargs))
          ,@(when readers `(:readers ',readers))
          ,@(when writers `(:writers ',writers))
          ,@other-options))))


(defun parse-slots-section (class-name section)
  (declare (ignore class-name))
  (mapcar #'canonicalize-direct-slot section))

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



(defun parse-defobject-body (class-name body)
  (let ((*slots-table* (make-hash-table)))
    (let* ((metaclass nil)
	   (all-slots
	     (append
	      (loop for (keyword section) on body by #'cddr
		    append (ecase keyword
			     (:metaclass (if (and (symbolp section) (not (keywordp section)))
					     (progn (setq metaclass section)
						    nil)
					     (error "invalid metaclass: ~S" section)))
			     (:slots (parse-slots-section class-name section))
			     (:inputs (parse-inputs-section class-name section))
			     (:attributes (parse-attributes-section class-name section))
			     (:components (parse-components-section class-name section))
			     (:hidden-components (parse-components-section class-name section :hidden? t)))))))
      (values all-slots metaclass))))

;; forward declare object for defobject macro
(defclass object ()
  ()
  (:metaclass adhoc-class))


(defun defobject-expansion (name supers slots metaclass)
  (progn
    ;; forward declare type before compiling attribute bodies.
    (ensure-class name :metaclass (or metaclass 'adhoc-class))
    `(let ((old (find-class ',name nil)))
       (prog1 (ensure-class ',name
			    :metaclass ',metaclass
			    :direct-superclasses '(,@supers object)
			    :direct-slots (list ,@slots)
			    :direct-descending-attributes ',*descending-attributes*)
	 (when old
	   (make-instances-obsolete old))))))

(defmethod make-instances-obsolete :before ((class adhoc-class))
  ;; special cases
  ;; 'children' is a special meta-slot, there may be more such slots later
  (loop while (children-dependents class)
     do (let ((variable (pop (children-dependents class))))
	  (unbind-this-variable variable)
	  (unbind-dependent-variables variable))))

(defmethod update-instance-for-different-class :before ((previous object) (current object) &rest initargs)
  (declare (ignore initargs))

  (let* ((class (class-of current))
	 (slotds (class-slots class))
	 (new-location nil))

    (loop for new-slotd in (subseq slotds 5)
	  when (and (eq (slot-definition-allocation new-slotd) :instance)
		    (setq new-location (slot-definition-location new-slotd)))
	    ;; clear these slots, more specific methods will have to fill them
	    do (setf (standard-instance-access current new-location) +slot-unbound+)))
#+NIL
  (let* ((class (class-of previous))
	 (slotds (class-slots class))
	 (old-location nil))

    (loop for old-slotd in slotds
       when (and (typep old-slotd 'basic-attribute-definition-mixin)
		 (eq (slot-definition-allocation old-slotd) :instance)
		 (setq old-location (slot-definition-location old-slotd)))
       do
	  (let ((maybe-variable (standard-instance-access-compat previous old-location)))
	   (when (typep maybe-variable 'variable)
	     (unbind-this-variable maybe-variable)
	     (unbind-dependent-variables maybe-variable)))))
#+NIL
  (let* ((class (class-of previous))
	 (slotds (class-slots class))
	 (old-location nil))

    (loop for old-slotd in slotds
       when (and (typep old-slotd 'basic-attribute-definition-mixin)
		 (eq (slot-definition-allocation old-slotd) :instance)
		 (setq old-location (slot-definition-location old-slotd)))
       do (let ((new-slotd (get-slot-definition (class-of current) (slot-definition-name old-slotd))))
	    (when (typep new-slotd 'basic-attribute-definition-mixin)
	      (if (typep new-slotd 'effective-settable-slot-definition-mixin)
		  (let ((transferred-value (standard-instance-access-compat current (slot-definition-location new-slotd))))
		    (unless (typep transferred-value 'variable)
		      ;; primary method may have naively placed value in standard instance location:
		      (setf (slot-value (setf (standard-instance-access current (slot-definition-location new-slotd))
					      (make-instance (variable-type new-slotd)
							     :root-path (root-path2 current)
							     :slot-name (slot-definition-name new-slotd)
                                 :instance current))
					'value)
			    transferred-value)))
		  (setf (standard-instance-access current (slot-definition-location new-slotd)) +slot-unbound+))))))
  (values))

(defmethod update-instance-for-redefined-class
    ((instance object) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots initargs))

  ;; todo: need to set the slots which are not generative
  ;; with the old value of the slot as if shared-initialize were doing it
  ;; but we do not actually want shared-initialize to run
  ;; or possibly an opaque shared-initialize could be implemented
  ;; for adhoc 'object' and then this could be a :before method

  ;; first, unbind dependents for any slot we're losing:
  (loop for value in property-list by #'cddr
	when (variable-p value)
	  do (unwind-protect (unbind-this-variable value)
	       (unbind-dependent-variables value)))

  #+sbcl
  ;; for all variables in the slot vector, except set settable variables which are still going to be in setttable slots
  ;; blow away generative data
  (let ((slotv (sb-pcl::std-instance-slots instance))
	(slotds (remove-if-not #'(lambda (slotd)
				   (eq :instance (slot-definition-allocation slotd)))
			       (class-slots (class-of instance)))))
			       
    (loop for maybe-variable across slotv
	  for i from 0
	  for slotd in slotds
	  unless (= i (slot-definition-location slotd))
	    do (error "slotd location does not match slotv index.")
	  do (unless (eq maybe-variable +slot-unbound+)
	       (when (variable-p maybe-variable)
		 ;; preserve values of :set settable slots which are still settable-slots.
		 (unless (and (typep slotd 'settable-slot-definition-mixin)
			      (typep maybe-variable 'settable-variable)
			      (eq :set (variable-status maybe-variable)))
		   (unwind-protect
			(unwind-protect (unbind-this-variable maybe-variable)
			  (unbind-dependent-variables maybe-variable))
		     (setf (svref slotv i) +slot-unbound+)))))))

  #+ccl 
  (let* ((slotv (ccl::instance-slots instance))
	 (size (ccl::uvsize slotv))
	 (slotds (remove-if-not #'(lambda (slotd)
				    (eq :instance (slot-definition-allocation slotd)))
				(class-slots (class-of instance)))))
    (loop for i from 1 below size
	  for slotd in slotds
	  unless (= i (slot-definition-location slotd))
	    do (error "slotd location does not match slotv index ~S ~S" (slot-definition-location slotd) i)
	  do (let ((maybe-variable (ccl::%svref slotv i)))
	       (unless (eq maybe-variable (ccl::%slot-unbound-marker))
		 (when (variable-p maybe-variable)
		   ;; preserve values of :set settable slots which are still settable-slots.
		   (unless (and (typep slotd 'settable-slot-definition-mixin)
				(typep maybe-variable 'settable-variable)
				(eq :set (variable-status maybe-variable)))
		     (unwind-protect
			  (unwind-protect (unbind-this-variable maybe-variable)
			    (unbind-dependent-variables maybe-variable))
		       (setf (ccl::%svref slotv i) (ccl::%slot-unbound-marker)))))))))
  (values))


(defmacro defobject (name supers &body body)
  (let ((*descending-attributes* ()))
    (multiple-value-bind (slots metaclass) (parse-defobject-body name body)
      (defobject-expansion name supers slots (or metaclass 'adhoc-class)))))

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
	 ;;(break "~s" result)
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

