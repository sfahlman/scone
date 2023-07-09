;;; Performance benchmark test for rule firing in the setting of family tree structure.
;;; To use this, load this kb WITHOUT loading core.lisp, then use the
;;; test-parents function to test performance.

(new-type {person} {thing})
(new-complete-split-subtypes {person} '({male} {female}))
(new-type-role {parent} {person} {person} :n 2)
(new-indv-role {mother} {person} {parent})
(new-is-a {mother} {female})
(new-indv-role {father} {person} {parent})
(new-is-a {father} {male})
(new-type-role {child} {person} {person} :may-have t)
(new-indv-role {daughter} {person} {child} :may-have t)
(new-is-a {daughter} {female})
(new-indv-role {son} {person} {child} :may-have t)
(new-is-a {son} {male})
(new-indv-role {spouse} {person} {person} :may-have t)
(new-indv-role {wife} {person} {spouse} :may-have t)
(new-is-a {wife} {female})
(new-indv-role {husband} {person} {spouse} :may-have t)
(new-is-a {husband} {male})
(new-type-role {sibling} {person} {person} :may-have t)
(new-type-role {sister} {person} {sibling} :may-have t)
(new-is-a {sister} {female})
(new-type-role {brother} {person} {sibling} :may-have t)
(new-is-a {brother} {male})
(new-type-role {grandparent} {person} {person} :n 4)
(new-type-role {grandfather} {person} {grandparent} :n 2)
(new-is-a {grandfather} {male})
(new-type-role {grandmother} {person} {grandparent} :n 2)
(new-is-a {grandmother} {female})
(new-type-role {grandchild} {person} {person} :may-have t)
(new-type-role {grandson} {person} {grandchild} :may-have t)
(new-is-a {grandson} {male})
(new-type-role {granddaughter} {person} {grandchild} :may-have t)
(new-is-a {granddaughter} {female})
(new-type-role {uncle} {person} {male} :may-have t)
(new-type-role {aunt} {person} {female} :may-have t)
(new-type-role {cousin} {person} {person} :may-have t)
(new-type-role {nephew} {person} {male} :may-have t)
(new-type-role {niece} {person} {female} :may-have t)

;; Define rules about family relationships

(new-if-added-rule (a b)
    ((a {parent} b))
  (x-is-a-y-of-z b {child} a))

(new-if-added-rule (a b)
    ((a {child} b))
  (x-is-a-y-of-z b {parent} a))

(new-if-added-rule ((a :superior {female}) b)
    ((a {parent} b))
  (x-is-the-y-of-z a {mother} b))

(new-if-added-rule ((a :superior {male}) b)
    ((a {parent} b))
  (x-is-the-y-of-z a {father} b))

(new-if-added-rule ((a :superior {female}) b)
    ((a {child} b))
  (x-is-a-y-of-z a {daughter} b))

(new-if-added-rule ((a :superior {male}) b)
    ((a {child} b))
  (x-is-a-y-of-z a {son} b))

(new-if-added-rule (a b)
    ((a {sibling} b))
  (x-is-a-y-of-z b {sibling} a))

(new-if-added-rule ((a :superior {female}) b)
    ((a {sibling} b))
  (x-is-a-y-of-z a {sister} b))

(new-if-added-rule ((a :superior {male}) b)
    ((a {sibling} b))
  (x-is-a-y-of-z a {brother} b))

(new-if-added-rule (a b c)
    ((a {sibling} b)
     (b {sibling} c))
  (x-is-a-y-of-z a {sibling} c))

(new-if-added-rule (a b c)
    ((a {parent} b)
     (b {parent} c))
  (x-is-a-y-of-z a {grandparent} c))

(new-if-added-rule (a b)
    ((a {grandparent} b))
  (x-is-a-y-of-z b {grandchild} a))

(new-if-added-rule (a b)
    ((a {grandchild} b))
  (x-is-a-y-of-z b {grandparent} a))

(new-if-added-rule ((a :superior {male}) b)
    ((a {grandparent} b))
  (x-is-a-y-of-z a {grandfather} b))

(new-if-added-rule ((a :superior {female}) b)
    ((a {grandparent} b))
  (x-is-a-y-of-z a {grandmother} b))

(new-if-added-rule ((a :superior {male}) b)
    ((a {grandchild} b))
  (x-is-a-y-of-z a {grandson} b))

(new-if-added-rule ((a :superior {female}) b)
    ((a {grandchild} b))
  (x-is-a-y-of-z a {granddaughter} b))

(defun test-parents (npeople)
  "Create npeople {person} nodes, and make each one a {male} and the {parent} of the previous node.
   The rules defined above will add the inferred knowledge like father, son, grandfather, etc."
  (new-indv {person 0} {male})
  (dotimes (number (1- npeople))
    (commentary "~D" number)
    (let ((node
            (new-indv (make-element-iname :value (format nil "person ~D"  (1+ number))) {person})))
      (x-is-a-y-of-z node {parent} (lookup-element (format nil "person ~D" number)))
      (new-is-a node {male}))))

