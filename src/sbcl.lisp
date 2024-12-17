(in-package "SB-PCL")

(defmethod initialize-instance :before ((slotd slot-definition)
                                        &key (name nil namep)
                                          (initform nil initformp)
                                          (initfunction nil initfunp)
                                          (type nil typep)
                                          (allocation nil allocationp)
                                          (initargs nil initargsp)
                                          (documentation nil docp))
  (declare (ignore initform initfunction type))
  (unless namep
    (error 'slotd-initialization-error :initarg :name :kind :missing))
  (unless (symbolp name)
    (error 'slotd-initialization-type-error :initarg :name :datum name :expected-type 'symbol))
  #+NIL
  (when (and (constantp name)
             ;; KLUDGE: names of structure slots are weird, and their
             ;; weird behaviour gets grandfathered in this way.  (The
             ;; negative constraint is hard to express in normal
             ;; CLOS method terms).
             (not (typep slotd 'structure-slot-definition)))
    (error 'slotd-initialization-error :initarg :name :kind :constant :value name))
  (when (and initformp (not initfunp))
    (error 'slotd-initialization-error :initarg :initfunction :kind :missing))
  (when (and initfunp (not initformp))
    (error 'slotd-initialization-error :initarg :initform :kind :missing))
  (when (and typep (not t))
    ;; FIXME: do something.  Need SYNTACTICALLY-VALID-TYPE-SPECIFIER-P
    )
  (when (and allocationp (not (symbolp allocation)))
    (error 'slotd-initialization-type-error :initarg :allocation :datum allocation :expected-type 'symbol))
  (when initargsp
    (unless (typep initargs 'list)
      (error 'slotd-initialization-type-error :initarg :initarg :datum initargs :expected-type 'list))
    (do ((is initargs (cdr is)))
        ((atom is)
         (unless (null is)
           (error 'slotd-initialization-type-error :initarg :initarg :datum initargs :expected-type '(satisfies proper-list-p))))
      (unless (symbolp (car is))
        (error 'slotd-initialization-type-error :initarg :initarg :datum is :expected-type '(or null (cons symbol))))))
  (when docp
    (unless (typep documentation '(or null string))
      (error 'slotd-initialization-type-error :initarg :documentation :datum documentation :expected-type '(or null string)))))
