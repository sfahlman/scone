;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Code for building large (though mostly meaningless) KB's for performance
;;; profiling and testing.
;;;
;;; Author: Scott E. Fahlman
;;; ***************************************************************************

;;; Note: This file should be loaded into a running Scone that nodes-pealready
;;; has the "core" KB loaded.

(defun test-parent-isa-tree (layers fanout
			     &optional
			     (parent (new-type "test elements" *thing*)))
  "Builds a simple tree of type-nodes for performance testing. The
   tree is rooted under a new type-node {test elements}. The LAYERS
   argument says how many layers in the tree, and the FANOUT
   argument says how many nodes to add under each node in the previous
   layer.  These are all connected with only parent wires."
  ;; Build the next layer of type-nodes under the PARENT.
  (dotimes (n fanout)
    (let ((node
	   ;; Create one new node under PARENT, making up an appropriate name.
	   (new-type (make-element-iname
		      :value (format nil "~A-~D" (internal-name parent) n))
		     parent)))
      ;; And recurse to create the next layer under this.
      (unless (= 1 layers)
	(test-parent-isa-tree (- layers 1)
			      fanout
			      node)))))

(defun test-isa-link-tree (layers fanout
			   &optional
			   (parent (new-type "test elements" *thing*)))
  "Builds a tree of type-nodes for performance testing.  The LAYERS
   argument says how many layers in the tree, and the FANOUT
   argument says how many nodes to add under(load-kc  each node in the previous
   layer. In this version, all the parent-wires go to {thing}, and we
   use IS-A links to connect the elements we have created."
  ;; Build the next layer of type-nodes under the PARENT.
  (dotimes (n fanout)
    (let ((node
	   ;; Create one new node under PARENT, making up an appropriate name.
	   (new-type (make-element-iname
		      :value (format nil "~A-~D" (internal-name parent) n))
		     *thing*)))
      (new-is-a node parent)
      ;; And recurse to create the next layer under this.
      (unless (= 1 layers)
	(test-isa-link-tree (- layers 1)
			    fanout
			    node)))))

(defun test-people (ntypes npeople-per-type)
  "This builds up a number NTYPES of {person} subtypes, and then a
   number NPEOPLE-PER-TYPE of individual people.  For each type, we
   create a default {business phone number}, which should be
   inherited, and for each individual we create a random {nickname},
   which is an arbitrary string.  This can be made more complex if
   desired."
  ;; Right now the compiler chokes on curly braces, so we want to get
  ;; the pointer to the node representing {person} into a variable.
  (let ((person-node (lookup-element "person")))
    ;; Create as many person subtypes as NTYPES specifies.
    (dotimes (nt ntypes)
      (let ((this-person-type
	     ;; The name will just be {person type-37} or something like that.
	     (new-type
	      (make-element-iname
	       :value (format nil "person type-~D" nt))
	      ;; And each of these is a subtype of {person}.
	      person-node)))
	;; For each person-type, make up a default business phone
	;; number -- just a string.
	(x-is-the-y-of-z
	 (new-string (format nil "business phone-~D" nt))
	 (lookup-element "business phone number")
	 this-person-type)
	;; Now create the individuals in this type.
	(dotimes (np npeople-per-type)
	  ;; A NIL iname argument says just make up a name.
	  (let ((this-person
		 (new-indv nil this-person-type)))
	    ;; And give that person a nickname.
	    (x-is-the-y-of-z
	     (new-string (format nil "Nick-~D"
				 (random 100)))
	     (lookup-element "nickname")
	     this-person)))))))

