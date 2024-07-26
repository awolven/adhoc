;; Copyright Andrew K. Wolven 2008, 20021
;; This file is released under the GNU General Public License v3
;; See LICENSE.txt in the main directory for details

(in-package :adhoc-tests)

(5am:test smokecheck-1
  ()
  (5am:is (typep (defobject a0 ()) 'adhoc-class)))

(5am:test smokecheck-2
  ()
  (5am:is (typep (defobject a1 (a0)) 'adhoc-class)))

(5am:test inputs-1
  ()
  #+sbcl (defobject a1 (a0))
  (defobject a0 () :inputs (a))
  #+(or CCL ALLEGRO)(defobject a1 (a0))
  (let ((a1 (make-instance 'a1 'a 1)))
    (5am:is (eq (send a1 a) 1))))

(5am:test inputs-2
  ()
  #+sbcl (defobject a1 (a0))
  (defobject a0 () :inputs ((a 0)))
  #+(or CCL ALLEGRO)(defobject a1 (a0))
  (let ((a1 (make-instance 'a1)))
    (5am:is (eq (send a1 a) 0))))

#-ccl ;; ccl does not seem to support forward referenced classes
(5am:test inputs-1.5
  ()
  (let ((fwrc (intern (symbol-name (gensym)) *package*))
	(cn (intern (symbol-name (gensym)) *package*)))
    (loop while (find-class fwrc nil)
       do (setq fwrc (intern (symbol-name (gensym)) *package*)))
    (loop while (find-class cn nil)
       do (setq cn (intern (symbol-name (gensym)) *package*)))
    (eval `(defobject ,cn (,fwrc))) ;; first encountered class def with forward referenced class
    (eval `(defobject ,fwrc () :inputs (a)))
    (eval `(let ((self (make-instance ',cn 'a 1)))
	     (5am:is (eq (the a) 1))))))
	 
(5am:test inputs-unbound-slot-1
  ()
  (defobject a1 (a0))
  (defobject a0 () :inputs (a))
  (let ((a1 (make-instance 'a1)))
    (5am:is
     (handler-case (send a1 a)
       (unbound-slot () t)
       (error () nil)))))

(5am:test inputs-unbound-slot-2
  ()
  (defobject a1 (a0))
  (defobject a0 () :inputs ((a :descending)))
  (let ((a1 (make-instance 'a1)))
    (5am:is
     (handler-case (send a1 a)
       (unbound-slot () t)
       (error () nil)))))

(5am:test inputs-unbound-slot-3
  ()
  (defobject a1 (a0))
  (defobject a0 () :inputs ((a :defaulting)))
  (let ((a1 (make-instance 'a1)))
    (5am:is
     (handler-case (send a1 a)
       (unbound-slot () t)
       (error () nil)))))

(5am:test inputs-unbound-slot-4
  ()
  (defobject a1 (a0))
  (defobject a0 () :inputs ((a :defaulting :descending)))
  (let ((a1 (make-instance 'a1)))
    (5am:is
     (handler-case (send a1 a)
       (unbound-slot () t)
       (error () nil)))))

(5am:test inputs-3
  ()
  (defobject a1 (a0))
  (defobject a0 () :inputs ((a :defaulting :descending 2)))

  (let ((a1 (make-instance 'a1)))
    (5am:is (eq 2 (send a1 a)))))

(5am:test inputs-4
  ()
  (defobject a1 (a0))
  (defobject a0 () :inputs ((a :defaulting nil :descending)))

  (let ((a1 (make-instance 'a1)))
    (5am:is (eq :descending (send a1 a)))))

(defun settable-slot-status-1 ()
  (defobject a1 ()
    :inputs
    ((a 10)
     (b (1+ (the a))))

    :attributes
    ((c (1+ (the b)))))

  (let ((self (make-instance 'a1)))
    ;; normal dependency maintenance
    (and (eq (the c) 12)
	 (progn
	   (setf (the a) 20)
	   (eq (the c) 22)))))

(5am:test settable-slot-status-1
  ()
  (5am:is (settable-slot-status-1)))

(defun settable-slot-status-2 ()
  (defobject a1 ()
    :inputs
    ((a 10)
     (b (1+ (the a))))

    :attributes
    ((c (1+ (the b)))))

  (let ((self (make-instance 'a1)))
    ;; b is :set, c is now 1
    ;; a is :set, b stays :set, c stays 1
    (and (eq (the c) 12)
	 (progn
	   (setf (the b) 0)
	   (eq (the c) 1))
	 (progn
	   (setf (the a) 20)
	   (eq (the c) 1)))))

(5am:test settable-slot-status-2
  ()
  (5am:is (settable-slot-status-2)))

(defun settable-slot-status-3 ()
  (defobject a1 ()
    :inputs
    ((a 10))

    :attributes
    ((b :modifiable (1+ (the a))))

    :attributes
    ((c (1+ (the b)))))

  (let ((self (make-instance 'a1)))
    ;; same as settable-slot-status-2 but with modifiable attribute
    ;; instead of input
    (and (eq (the c) 12)
	 (progn
	   (setf (the b) 0)
	   (eq (the c) 1))
	 (progn
	   (setf (the a) 20)
	   (eq (the c) 1)))))

(5am:test settable-slot-status-3
  ()
  (5am:is (settable-slot-status-3)))

(defun settable-slot-status-4 ()
  (defobject a1 ()
    :inputs
    ((a 10))

    :attributes
    ((b :modifiable (1+ (the a))))

    :attributes
    ((c (1+ (the b)))))

  (let ((self (make-instance 'a1)))
    ;; same basic test as settable-slot-status-1 but with modifiable attribute
    ;; in the middle instead of input and two set calls
    (and (eq (the c) 12)
	 (progn
	   (setf (the a) 0)
	   (eq (the c) 2))
	 (progn
	   (setf (the a) 20)
	   (eq (the c) 22)))))

(5am:test settable-slot-status-4
  ()
  (5am:is (settable-slot-status-4)))

(defun settable-slot-status-5 ()
  (defobject a1 ()
    :inputs
    ((z 10)
     (a (1+ (the z))))

    :attributes
    ((b (1+ (the a)))
     (c (1+ (the b)))))

  (let ((self (make-instance 'a1)))
    (and (eq (the c) 13)
	 (progn (setf (the a) 0)
		(eq (the c) 2))
	 (progn (setf (the z) 20)
		(eq (the c) 2)))))

(5am:test settable-slot-status-5
  ()
  (5am:is (settable-slot-status-5)))

(defun settable-slot-status-6 ()
  (defobject a1 ()
    :inputs
    ((z 10)
     (a (1+ (the z))))
    
    :attributes
    ((b (1+ (the a)))
     (c (1+ (the b)))))

  (let ((self (make-instance 'a1)))
    (and (eq (the c) 13)
	 (let ((self (make-instance 'a1 'a 0)))
	   (and (eq (the c) 2)
		(progn (setf (the z) 100)
		       (eq (the c) 2)))))))

(5am:test settable-slot-status-6
  ()
  (5am:is (settable-slot-status-6)))

(defun with-slots-test-1 ()
  (defobject a1 ()
    :attributes
    ((a 1) (b (1+ (the a))) (c (1+ (the b))) (d (1+ (the c)))))

  (let ((object (make-instance 'a1)))
    (with-slots (d) object
      (eq d 4))))

(5am:test with-slots-test-1
  ()
  (5am:is (with-slots-test-1)))

(5am:test inheritance-redfinition-1
  ()
  (defobject foo-bar-0 () :inputs ((a 0)))
  (defobject baz-blah-1 (foo-bar-0))
  (defobject foo-bar-0 () :inputs ((a 2)))
  (let ((bb1 (make-instance 'baz-blah-1)))
    (5am:is (eq 2 (send bb1 a)))))

(5am:test :defaulting-1
  (defobject aaa ()
    :inputs
    ((a :defaulting 4)))

  (defobject bbb ()
    :components
    ((c :type 'aaa)))

  (defobject ccc ()
    :components
    ((cb :type 'bbb)))

  (defobject ddd ()
    :components
    ((cc :type 'ccc))

    :inputs
    ((a 0)))

  (let* ((ddd (make-instance 'ddd))
	 (cc (send ddd cc))
	 (cb (send cc cb))
	 (c (send cb c)))
    (5am:is (eq 0 (send c a)))))

(5am:test :defaulting-2
  (let ((aaa (make-instance 'aaa)))
    (5am:is (eq 4 (send aaa a)))))

(5am:test descending-attribute-test-1
  ()

  #+(or CCL ALLEGRO) (defobject baz ()
    :components
    ((b2 :type 'bar)))
  
  (defobject baz2 (baz)
    
    :attributes
    ((b3 :descending 9)))
  
  #+SBCL (defobject baz ()
    :components
    ((b2 :type 'bar)))

  (defobject bar ()
    :components
    ((f1 :type 'foo)))

  (defobject foo ())
  
  (5am:is (eq 9 (send (make-instance 'baz2) b2 f1 b3))))

(5am:test unsettable-1
  ()
  (defobject foo () :inputs (one))
  (defobject bar (foo) :attributes ((one 1)))
  (let ((result (ignore-errors (make-instance 'bar 'one 2))))
    (5am:is (null result))))

;; tests needed:
;; unbind-dependent-variable tests
;; pseudo-input tests
;; descendant attribute tests
;; inheritance tests
;; - make sure attributes are shadowed properly
;; - make sure all initargs work for inputs but not for attributes
;; components tests:
;; psuedo-inputs don't work if parent attribute is an attribute
;; todo: pass "root" down through the component tree
;; todo: part/whole-defaulting possibly through the use of descendant attributes

(5am:test dependency-maintenance-1
  ()
  (defobject a0 () :inputs ((a 0)))
  (defobject a1 (a0) :attributes ((b (1+ (the a)))))
  (let ((a1 (make-instance 'a1)))
    (5am:is (and (eq (send a1 b) 1)
		 (progn (setf (slot-value a1 'a) 1)
			(eq (send a1 b) 2))))))

(defun pseudo-input-test-1 ()
  
  (defobject a0 () :inputs ((a 0)))
  (defobject a1 (a0))
  (defobject b0 ()
    :components
    ((a1 :type 'a1
	 a 2
	 b 3)))
  (let ((b0 (make-instance 'b0)))
    (and (eq 2 (send b0 a1 a))
	 (eq 3 (send b0 a1 b)))))

(5am:test pseudo-input-test-1
  ()
  (5am:is (pseudo-input-test-1)))

(defun pseudo-input-test-2 ()
  (defobject a0 ()
    :attributes
    ((a 12)))
  (defobject a1 ()
    :components
    ((an-a0 :type 'a0
	    a 13)))
  (let ((self (make-instance 'a1)))
    (eq 12 (the an-a0 a))))

(5am:test psuedo-input-test-2
  ()
  (5am:is (pseudo-input-test-2)))
	  

(5am:test provided-inputs-1
  ()
  (defobject a0 () :inputs ((a 0)))
  (defobject a1 ()
    :components
    ((a-zero :type 'a0
	     a (list self part))))

  (let ((a1 (make-instance 'a1)))
    (5am:is (equalp (list a1 (send a1 a-zero)) (send a1 a-zero a)))))

(5am:test part-whole-defaulting-1
  ()
  (defobject a2 () :attributes ((assy (the assembly))))
  (defobject assembly ())
  (defobject a1 ()
    :components
    ((assembly :type 'assembly
	       b 99)
     (a-two :type 'a2)))
  (let ((a1 (make-instance 'a1)))
    (5am:is (eq 99 (send a1 a-two assembly b)))))

(defun spliced-provided-inputs-test-1 ()
  (defobject a0 ())
  (defobject a1 ()
    :attributes
    ((part-inputs (list 'a 1 'b 2 'c 3)))

    :components
    ((an-a0 :type 'a0
	    :@ (the part-inputs))))

  (let ((self (make-instance 'a1)))
    (eq 3 (the an-a0 c))))

(5am:test spliced-provided-inputs-test-1
  ()
  (5am:is (spliced-provided-inputs-test-1)))

(defun spliced-provided-inputs-test-2 ()
  (defobject a0 ()
    :inputs
    ((c 2)))
  
  (defobject a1 ()
    :attributes
    ((part-inputs (list 'a 1 'b 2 'c 3)))

    :components
    ((an-a0 :type 'a0
	    :@ (the part-inputs))))

  (let ((self (make-instance 'a1)))
    (eq 3 (the an-a0 c))))

(5am:test spliced-provided-inputs-test-2
  ()
  (5am:is (spliced-provided-inputs-test-2)))

(defun spliced-provided-inputs-test-3 ()
  (defobject a0 ()
    :attributes
    ((c 2)))
  
  (defobject a1 ()
    :attributes
    ((part-inputs (list 'a 1 'b 2 'c 3)))

    :components
    ((an-a0 :type 'a0
	    :@ (the part-inputs))))

  (let ((self (make-instance 'a1)))
    (handler-case (eq 2 (the an-a0 c))
      (error () nil))))

(5am:test spliced-provided-inputs-test-3
  ()
  (5am:is (spliced-provided-inputs-test-3)))

(defun spliced-provided-inputs-test-4 ()
  (defobject a0 ()
    :inputs
    (c))
  
  (defobject a1 ()
    :attributes
    ((part-inputs (list 'a 1 'b 2 'c 3)))

    :components
    ((an-a0 :type 'a0
	    :@ (the part-inputs))))

  (let ((self (make-instance 'a1)))
    (handler-case (eq 3 (the an-a0 c))
      (error () nil))))

(5am:test spliced-provided-inputs-test-4
  ()
  (5am:is (spliced-provided-inputs-test-4)))

(defun defaulting-3 ()
  (defobject bottom ()
    :inputs
    ((foo :defaulting nil)))
  (defobject top ()
    :inputs
    ((foo :bar))
    :components
    ((bottom :type 'bottom)))
  (let ((self (make-instance 'top)))
    (eq :bar (the bottom foo))))

(5am:test defaulting-3
  ()
  (5am:is (defaulting-3)))

(defun defaulting-4 ()
  (defobject bottom ()
    :inputs
    ((foo :defaulting)))
  (defobject top ()
    :inputs
    ((foo :bar))
    :components
    ((bottom :type 'bottom)))
  (let ((self (make-instance 'top)))
    (eq :bar (the bottom foo))))

(5am:test defaulting-4
  ()
  (5am:is (defaulting-4)))

#-ALLEGRO
(defun standard-class-superclass-test-1 ()
  (defclass foo-std-class ()
    ((b :initform 8)))
  (defobject bar ())
  (defobject baz (foo-std-class bar))
  (let ((instance (make-instance 'baz)))
    (eq 8 (send instance b))))

#-ALLEGRO
(5am:test standard-class-superclass-test-1
  ()
  (5am:is (standard-class-superclass-test-1)))

(defgeneric foo-b (self))

(defun defobject-slot-test-1 ()
  (defobject foo ()
    :slots
    ((b :initform 9 :reader foo-b :initarg :b)))
  (let ((self (make-instance 'foo :b 7)))
    (eq (foo-b self) 7)))

(5am:test defobject-slot-test-1
  ()
  (5am:is (defobject-slot-test-1)))

#-ALLEGRO
(defun changing-slot-structure-1 ()
  (defobject a ())
  (defobject b ()
    :components
    ((a1 :type 'a)))
  (let* ((ab (make-instance 'b)))
    (send ab a1)
    (defclass c () ((c1)))
    (defobject b (c)
      :components
      ((a1 :type 'a)))
    (typep (first (get-object-children ab)) 'a)))

(defobject child1 ()
  :inputs (chil))

(defobject base ()
  :inputs (my-object)

  :attributes
  ((my-list (send (the my-object) children)))

  :components
  ((child :type 'child1
	  :aggregate (:size (length (the my-list)))
	  chil (nth (first (the-part indices)) (the my-list)))))

(defun parser-test-1 ()
  (defobject a ()
    :slots
    (a1 (a2 :initarg :a2 :accessor a2 :initform 'a2)))

  (let ((a (make-instance 'a))) 
    (handler-case (send a a1)
      (unbound-slot () t)
      (error () nil))))

(5am:test parser-test-1
  ()
  (5am:is (parser-test-1)))

(defobject simple-foo ())

(defobject aggregate-test-1 ()
  :components
  ((sfs :type 'simple-foo
	:aggregate (:size 10))))

(defobject aggregate-test-2 ()
  :inputs
  ((indices-list '((1) (2) (3))))
  
  :components
  ((sfs :type 'simple-foo
	:aggregate (:indices (the indices-list)))))

(defobject aggregate-test-3 ()
  :inputs
  ((aggregate-size 10))
  
  :components
  ((sfs :type 'simple-foo
	:aggregate (:size (the aggregate-size)))))

(defobject hidden-components-test-1 ()
  :hidden-components
  ((sf :type 'simple-foo)
   (sf3s :type 'simple-foo
	 :aggregate (:size 3)))

  :components
  ((c1 :type 'child1)
   (c2s :type 'child1
	:aggregate (:size 2))))

(defun hidden-components-test-1 ()
  (let ((self (make-instance 'hidden-components-test-1)))
    (let ((hidden-children (adhoc::get-hidden-children self))
	  (ordinary-children (the children)))
      (and hidden-children (every #'(lambda (obj)
				      (typep obj 'simple-foo))
				  hidden-children)
	   (eq (length hidden-children) 4)
	   ordinary-children (every #'(lambda (obj)
				      (typep obj 'child1))
				    ordinary-children)
	   (eq (length ordinary-children) 3)))))

(5am:test hidden-components-test-1 ()
  (5am:is (hidden-components-test-1)))
      
(defobject class-0 ()
  :inputs
  ((one 1))

  :attributes
  ((two 2))

  :components
  ((three :type 'class-0
	  one 7)))

(defobject class-1 ()
  :inputs
  ((three 8))

  :attributes
  ((one 9))

  :components
  ((two :type 'class-0
	one 10)))

(defun change-class-test-1 ()
  (let ((inst-a (make-instance 'class-0)))
    (change-class inst-a 'class-1)
    (send inst-a one)
    (send inst-a two one)
    (send inst-a three)
    t))

(5am:test change-class-test-1 ()
  (5am:is (change-class-test-1)))

(defobject class-2 ()
  :inputs
  ((one 1))

  :attributes
  ((two 2))

  :components
  ((three :type 'class-0
	  one 11))

  :attributes
  ((four :modifiable 4)))

(defun change-class-test-2 ()
  (let ((inst (make-instance 'class-0)))
    (change-class inst 'class-2)
    (send inst one)
    (send inst two)
    (send inst three)
    (send inst four)
    t))

(5am:test change-class-test-2 ()
  (5am:is (change-class-test-2)))

(defun change-class-test-3 ()
  (let ((inst (make-instance 'class-2)))
    (change-class inst 'class-0)
    (send inst one)
    (send inst two)
    (send inst three)
    t))

(5am:test change-class-test-3 ()
  (5am:is (change-class-test-3)))

(defobject class-3 ()
  :inputs
  ((one 1))

  :attributes
  ((two :uncached 2))

  :components
  ((three :type 'class-0
	  one 11)))

(defun change-class-test-4 ()
  (let ((self (make-instance 'class-3)))
    (change-class self 'class-0)
    (the one)
    (the two)
    (the three)
    t))

(5am:test change-class-test-4 ()
  (5am:is (change-class-test-4)))

(defun change-class-test-5 ()
  (let ((self (make-instance 'class-0)))
    (change-class self 'class-3)
    (the one)
    (the two)
    (the three)
    t))

(5am:test change-class-test-5 ()
  (5am:is (change-class-test-5)))
  
(defobject odd-class ())

(defobject even-class ())

(defobject numbers ()
  :components
  ((numbers :aggregate (:size 10)
            :type (let ((index (first indices)))
                    (if (oddp index)
                        'odd-class
                        'even-class)))))
