;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Upper Ontology concepts for Scone.
;;;
;;; Author & Maintainer: Scott E. Fahlman
;;; ***************************************************************************
;;; Copyright (C) 2003-2014, Carnegie Mellon University.
;;;
;;; The Scone software is made available to the public under the
;;; Apache 2.0 open source license.  A copy of this license is
;;; distributed with the software.  The license can also be found at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0.
;;;
;;; Unless required by applicable law or agreed to in writing,
;;; software distributed under the License is distributed on an "AS
;;; IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
;;; express or implied.  See the License for the specific language
;;; governing permissions and limitations under the License.
;;;
;;; Development since February 2013 has been supported in part by the
;;; U.S. Office of Naval Research under award number N000141310224.
;;;
;;; Development of Scone from January, 2012, through August 2013 was
;;; supported in part by the Intelligence Advanced Research Projects
;;; Activity (IARPA) via Department of Defense US Army Research
;;; Laboratory contract number W911NF-12-C-0020.
;;;
;;; Development of Scone from 2003 through 2008 was supported in part
;;; by the Defense Advanced Research Projects Agency (DARPA) under
;;; contract numbers NBCHD030010 and FA8750-07-D-0185.
;;;
;;; Additional support for Scone development has been provided by
;;; research grants from Cisco Systems Inc. and from Google Inc.
;;;
;;; The U.S. Government is authorized to reproduce and distribute
;;; reprints for Governmental purposes notwithstanding any copyright
;;; annotation thereon.
;;;
;;; Disclaimer: The views and conclusions contained herein are those
;;; of the authors and should not be interpreted as necessarily
;;; representing the official policies or endorsements, either
;;; expressed or implied, of IARPA, DoD/ARL, ONR, DARPA, the
;;; U.S. Government, or any of our other sponsors.
;;; ***************************************************************************
(in-namespace "common")
(in-context {universal})

;;; ------------------------------------------------------------------------
;;; MATHEMATICAL STUFF: SETS, ETC.

;;; Define a sequence.  Could be finite, unbounded, or cyclic.
(new-type {sequence} {set})

;; Each sequence member has an index.
(new-indv-role {sequence member index}
	       (the-x-role-of-y {member} {sequence})
	       {integer})

;; Each sequence member has a successor, which itself is a
;; sequence member.
(new-indv-role {successor}
	       (the-x-role-of-y {member} {sequence})
	       (the-x-role-of-y {member} {sequence})
	       :english '(:role :inverse-role "predecessor"))

;;; A finite sequence has a first and last member.
(new-type {finite sequence} {sequence})

(new-indv-role {finite sequence length}
	       {finite sequence}
	       {integer}
	       :english '(:role "length"))

;; A sequence has a first and last member.
(new-indv-role {first member}
	       {finite sequence}
	       (the-x-role-of-y {member} {finite sequence})
	       :english '(:role "first element"))

(new-indv-role {last member}
	       {finite sequence}
	       (the-x-role-of-y {member} {finite sequence})
	       :english '(:role "last element"))

#| %%% These don't work until we fix a bug in negation.

;; The last member of a finite set has no successor.
(x-has-no-y {last member} {successor})

;; The first member of a finite set has no predecessor.
(x-has-no-y (first member} {predecessor})
|#

;;; A cyclic sequence is something like the days of the week.  Each
;; element has a successor, but there are no first or last elements
;; because the elements form a loop.

(new-type {cyclic sequence} {sequence})

;; A cyclic sequence has a period that is an integer.
(new-indv-role {cyclic sequence period}
	       {cyclic sequence}
	       {integer}
	       :english '(:role "period" "cycle length" "length"))

(defun define-cycle (sequence member-parent list)
  "Define a new cyclic SEQUENCE.  Takes a list of lists.  Each sublist
  defines a subtype under MEMBER-PARENT.  Each sublist has a name for the
  member element, followed by any number of alternative names.  In
  addition to defining these subtypes and adding the properties, we
  connect them in a loop via the {has-successor} relation."
  (setq member-parent (lookup-element-test member-parent))
  (let* ((seq (new-indv sequence {cyclic sequence}))
	 (elist nil)
	 (i 1))
    (dolist (x list)
      (let* ((e (new-type
		 (make-element-iname
		  :value (car x))
		 member-parent
		 :english (cddr x))))
	(x-is-a-y-of-z e {member} seq)
	(x-is-the-y-of-z (new-integer i) {sequence member index} e)
	(push e elist)
	(incf i)))
    ;; Elements are now on elist in reverse order.  Make the successor
    ;; relations.
    (do ((el elist (cdr el)))
	((null (cdr el)))
      (the-x-of-y-is-a-z {successor} (cadr el) (car el)))
    ;; Make the last one.
    (the-x-of-y-is-a-z {successor} (car elist) (car (last elist)))
    seq))


;;;; -------------------------------------------------------------------------
;;;; ROOT KNOWLEDGE OF PHYSICAL OBJECTS AND MATERIALS

(in-context {general})

(new-complete-split-subtypes
 {tangible}
 '(({naturally occurring} :adj "natural")
   ({man-made} :adj "artificial")))

(new-type {physical object} {tangible}
	  :english '("object"))

(new-intersection-type {natural object}
		       '({physical object} {naturally occurring})
		       :english '("naturally occurring object"))

(new-intersection-type {man-made object}
		       '({physical object} {man-made})
		       :english '("artificial object"))

(new-complete-split-subtypes
 {physical object}
 '(({animate} :adj "active" "living" "alive")
   ({inanimate} :adj "inert" "dead")))

(new-is-a {animate} {natural object})

(new-complete-split-subtypes
 {animate}
 '({animal} {plant})
 :english "living thing")

(new-complete-split-subtypes
 {animate}
 '(({mature} :adj "full-grown")
   ({immature} :adj "juvenile")))

(new-type {person} {animal})

;;; Stuff and Materials

;;; We use {stuff} to include pseudo-subtances like time and information.
(new-type {stuff} {thing})

;;; We use material for physical substances like concrete and water.
(new-intersection-type {material} '({stuff} {tangible})
		       :english '("substance" "physical substance"))

(new-intersection-type {natural material}
		       '({material} {naturally occurring})
		       :english '("naturally occurring material"
				  "natural substance"
				  "naturally occurring substance"))

(new-intersection-type {man-made material}
		       '({material} {man-made})
		       :english '("artificial material"
				  "man-made substance"
				  "artificial substance"))

;;;; -------------------------------------------------------------------------
;;;; CONDITIONS AND QUALITIES

;;; A condition is some state of being, of the universe or some entity,
;;; that may be present in some context.

(new-type {condition} {thing}
	  :english '("state of being" "situation" "status"))

;;; There are many types of condition.  Here are a few.

(new-split-subtypes
 {condition}
 '(({economic condition} "economic status")
   ({health condition} "health" "health status" "medical status")
   ({happiness condition} "happiness")
   ({weather condition} "weather")
   ({social condition} "social status")
   {state-of-repair condition}))

;;; Sometimes we lump together the social and economic, or have
;;; a condition that spans both.
(new-type {socioeconomic condition} {condition}
	  :english "socioeconomic status")
(new-is-a {social condition} {socioeconomic condition})
(new-is-a {economic condition} {socioeconomic condition})

;;; The condition subject is the thing we are describing as being in
;;; a given condition.  For example, for a {health condition} this will
;;; be some living thing.

(new-indv-role {condition subject} {condition} {thing} :may-have t)

(the-x-of-y-is-a-z {condition subject}
		   {health condition}
		   {animate})

(the-x-of-y-is-a-z {condition subject}
		   {happiness condition}
		   {animal})

(the-x-of-y-is-a-z {condition subject}
		   {state-of-repair condition}
		   {man-made object})

(new-type {set of people} {set})
(the-x-of-y-is-a-z {member} {set of people} {person})

(the-x-of-y-is-a-z {condition subject}
		   {economic condition}
		   {set of people})

;;; Good and bad are of course subjective, subject to gradation as well
;;; as point of view.  But we use these as adjectives and need a way to
;;; represent them.

(new-split-subtypes {thing}
		    '(({good thing}
		       :no-iname :adj "good" "wonderful" "excellent" "great")
		      ({bad thing}
		       :no-iname :adj "bad" "poor" "terrible" "horrible"
		       "dismal")))

(new-intersection-type {good condition}
		       '({good thing} {condition}))
(new-intersection-type {bad condition}
			'({bad thing} {condition}))

(new-intersection-type {good economic condition}
		       '({economic condition} {good thing}))
(new-intersection-type {bad economic condition}
		       '({economic condition} {bad thing}))
(new-intersection-type {good health condition}
		       '({health condition} {good thing}))
(new-intersection-type {bad health condition}
		       '({health condition} {bad thing}))

(new-intersection-type {good happiness condition}
		       '({happiness condition} {good thing}))
(new-intersection-type {bad happiness condition}
		       '({happiness condition} {bad thing}))

(new-intersection-type {good weather condition}
		       '({weather condition} {good thing}))
(new-intersection-type {bad weather condition}
		       '({weather condition} {bad thing}))

(new-intersection-type {good state-of-repair condition}
		       '({state-of-repair condition} {good thing}))
(new-intersection-type {bad state-of-repair condition}
		       '({state-of-repair condition} {bad thing}))

(new-intersection-type {good social condition}
		       '({social condition} {good thing}))
(new-intersection-type {bad social condition}
		       '({social condition} {bad thing}))

;;; An attribute is some aspect of a type of entity, such as {happiness}
;;; or {density}.  It may be permanent or variable -- if the latter, it
;;; may have different values at different times or in different contexts.

;;; Usually an attribute has an owner -- the thing exhibiting this
;;; quality -- but we usually speak of the attribute as a role of the
;;; owner, rather than vice-versa.  But we often need to speak of
;;; attributes as entities in their own right.

(new-type {attribute} {thing} :english '("quality" "property"))

;;; The attribute owner is the thing that owns the attribute.
(new-indv-role {attribute owner} {attribute} {thing})

(new-split-subtypes
 {attribute}
 '({physical attribute}
   {abstract attribute}))

(new-intersection-type
 {good attribute}
 '({attribute} {good thing}))

(new-intersection-type
 {bad attribute}
 '({attribute} {bad thing}))






