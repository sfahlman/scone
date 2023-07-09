;;; Showcase of production rule system.

;;; Resolve conflicts in case core kb is loaded.
(if (lookup-element {person})
    (remove-element {phone number})
    (progn
      (new-type {person} {thing})
      (new-split-subtypes {person} '({male person} {female person}))
      (new-type-role {parent} {person} {person})
      (new-type-role {grandparent} {person} {parent})
      (new-indv-role {mother} {person} {parent})
      (new-is-a {mother} {female person})))

(new-indv-role {phone number} {person} {string})

;;; Functions for arithmetic on Scone numbers.

(defun scone-plus (e1 e2)
  (new-number (+ (node-value e1) (node-value e2))))

(defun scone-minus (e1 e2)
  (new-number (- (node-value e1) (node-value e2))))

;;; Dummy function to look up a phone number for a person.
(defun lookup-phone-number (person)
  {"123-456-7890"})

;;; Define some elements related to food and cooking.

(new-type {food} {thing})
(new-type {dish} {food})
(new-type-role {ingredient} {dish} {food})
(new-type {spicy dish} {dish})

(new-type {ham} {food})
(new-type {pineapple} {food})
(new-type {chili pepper} {food})
(new-type {pizza} {dish})
(new-type-role {topping} {pizza} {ingredient})
(new-type {hawaiian pizza} {pizza})

(new-type {cooking method} {thing})
(new-type {searing} {cooking method})
(new-relation {cooked by} :a-inst-of {dish} :b-inst-of {cooking method})
(new-type {steak} {dish})
(new-indv-role {exterior} {steak} {part})
(new-indv-role {interior} {steak} {part})

(new-type {color} {thing})
(new-type {colored thing} {thing})
(new-type-role {color of} {colored thing} {color})
(new-is-a {exterior} {colored thing})
(new-is-a {interior} {colored thing})
(new-split-subtypes {color} '({brown} {pink}))

;;; Define a new container type.
;;; We want containers to have the property that the amount filled plus
;;; the amount empty is equal to the capacity of the container.
(new-type {container} {thing})
(new-indv-role {capacity} {container} {number})
(new-indv-role {amount filled} {container} {number})
(new-indv-role {amount empty} {container} {number})

;;; Test both if-added (eager) and if-needed (lazy) rules with different container types.
(new-split-subtypes {container} '({eager container} {lazy container}))

;;; The parent of a parent is a grandparent.
(new-if-added-rule (a b c)
                   ((a {parent} b)
                    (b {parent} c))
  (x-is-a-y-of-z a {grandparent} c))

;;; A female parent is a mother.
(new-if-added-rule ((a :superior {female person})
                    b)
                   ((a {parent} b))
  (x-is-the-y-of-z a {mother} b))

;;; A dish with chili pepper is spicy.
(new-if-added-rule (a
                    (b :superior {chili pepper}))
                   ((b {ingredient} a))
  (new-is-a a {spicy dish}))

;; A Hawaiian pizza is a pizza with pineapple and ham as toppings.
(new-if-added-rule ((a :superior {pizza})
                    (b :superior {pineapple})
                    (c :superior {ham}))
                   ((b {topping} a)
                    (c {topping} a))
  (new-is-a a {hawaiian pizza}))

;;; If steak is seared, the exterior will be brown and the interior will be pink.
(new-if-added-rule ((a :superior {steak})
                    (b :superior {searing}))
                   ((a {cooked by} b))
  ;; Can have multiple actions.
  (new-eq (the-x-role-of-y {color of} (the-x-role-of-y {exterior} a)) {brown})
  (new-eq (the-x-role-of-y {color of} (the-x-role-of-y {interior} a)) {pink}))

;;; If the amount filled and amount empty of an eager container are known,
;;; set its capacity to be equal to the sum of the two values.
;;; The :PROPER T keyword in the rule variables indicates that only
;;; proper Scone elements (in this case actual Scone numbers) should
;;; be substituted into this rule.
(new-if-added-rule ((a :proper t)
                    (b :proper t)
                    (c :superior {eager container}))
                   ((a {amount filled} c)
                    (b {amount empty} c))
  (x-is-the-y-of-z (scone-plus a b) {capacity} c))

;;; Similar rules for setting amount empty and amount filled.
(new-if-added-rule ((a :proper t) (b :proper t) (c :superior {eager container}))
                   ((a {amount filled} c)
                    (b {capacity} c))
  (x-is-the-y-of-z (scone-minus b a) {amount empty} c))

(new-if-added-rule ((a :proper t) (b :proper t) (c :superior {eager container}))
                   ((a {amount empty} c)
                    (b {capacity} c))
  (x-is-the-y-of-z (scone-minus b a) {amount filled} c))

;;; If the amount filled and amount empty of a lazy container are known
;;; and the capacity is requested, compute it by adding the two values.
;;; The :proper t keyword in the rule variables indicate that only
;;; proper Scone elements (in this case actual Scone numbers) should
;;; be substituted into this rule.
(new-if-needed-rule ((a :proper t)
                     (b :proper t)
                     (c :superior {lazy container}))
                    ((a {amount filled} c)
                     (b {amount empty} c))
                    ((scone-plus a b) {capacity} c))

;;; Similar rules for requesting amount empty and amount filled.
(new-if-needed-rule ((a :proper t)
                     (b :proper t)
                     (c :superior {lazy container}))
                    ((a {amount filled} c)
                     (b {capacity} c))
                    ((scone-minus b a) {amount empty} c))
(new-if-needed-rule ((a :proper t)
                     (b :proper t)
                     (c :superior {lazy container}))
                    ((a {amount empty} c)
                     (b {capacity} c))
                    ((scone-minus b a) {amount filled} c))

;;; Can provide a way to lookup the phone number of a person if
;;; it is not explicitly in the knowledge base.
(new-if-needed-rule (person)
                    nil
                    ((lookup-phone-number person) {phone number} person))

;;; Testing if-added rules. Rule system is triggered on calls to
;;; NEW-IS-A, NEW-EQ, X-IS-A-Y-OF-Z, X-IS-THE-Y-OF-Z, and NEW-STATEMENT.

(new-indv {bob} {person})
(new-indv {mary} {person})
(new-indv {sue} {person})

(x-is-a-y-of-z {mary} {parent} {bob})
(x-is-a-y-of-z {sue} {parent} {mary})
(new-is-a {mary} {female person})
(new-is-a {sue} {female person})
(assert (simple-is-x-a-y? {sue} (the-x-role-of-y {grandparent} {bob})))
(assert (simple-is-x-eq-y? {mary} (the-x-role-of-y {mother} {bob})))
(assert (simple-is-x-eq-y? {sue} (the-x-role-of-y {mother} {mary})))

(new-type {jalapeno poppers} {dish})
(new-type {jalapeno} {chili pepper})
(x-is-a-y-of-z {jalapeno} {ingredient} {jalapeno poppers})
(assert (simple-is-x-a-y? {jalapeno poppers} {spicy dish}))

(new-type {mystery pizza} {pizza})
(new-type {canadian ham} {ham})
(x-is-a-y-of-z {canadian ham} {topping} {mystery pizza})
(x-is-a-y-of-z {pineapple} {topping} {mystery pizza})
(assert (simple-is-x-a-y? {mystery pizza} {hawaiian pizza}))

(new-indv {my steak} {steak})
(new-statement {my steak} {cooked by} {searing})
(assert (simple-is-x-eq-y?
         {brown}
         (the-x-role-of-y {color of} (the-x-role-of-y {exterior} {my steak}))))
(assert (simple-is-x-eq-y?
         {pink}
         (the-x-role-of-y {color of} (the-x-role-of-y {interior} {my steak}))))

(new-type {my eager container} {eager container})
;;; Create role node without value to make sure rule engine
;;; handles missing (non-proper) values correctly.
(the-x-of-y {amount empty} {my eager container})
(x-is-the-y-of-z {4} {amount filled} {my eager container})
(x-is-the-y-of-z {6} {amount empty} {my eager container})
(assert (simple-is-x-eq-y? {10} (the-x-of-y {capacity} {my eager container})))

;;; Testing if-needed rules. Rule system is triggered on calls to THE-X-OF-Y.

(new-indv {my lazy container} {lazy container})
(x-is-the-y-of-z {4} {amount filled} {my lazy container})
(x-is-the-y-of-z {6} {amount empty} {my lazy container})
(assert (simple-is-x-eq-y? {10} (the-x-of-y {capacity} {my lazy container})))

(new-indv {jeffrey} {person})
(assert (simple-is-x-eq-y? {"123-456-7890"} (the-x-of-y {phone number} {jeffrey})))
