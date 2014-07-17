;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Scone Knowledge Representation System
;;; Bootstrap KB definitions.
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

;;; NOTE: LOAD THIS FILE BEFORE ANY OTHER KB FILES, AND BEFORE TRYING
;;; TO RUN ANYTHING ELSE IN SCONE.

;;; This file contains the initial bootstrap structures for the Scone
;;; Knowledge Base.  This includes concepts such as {thing} and {set} that
;;; are essential to the proper working of the Scone engine.

;;; Some of the elements contained in this file are stored in global
;;; variables such as *thing* and *set* so that engine functions can easily
;;; refer to them.  These variables are declared in the engine file.

;;; This file necessarily contains some forward references.
(setq *defer-unknown-connections* t)

;;; Suppress error checking for this file.
(setq *no-kb-error-checking* t)

;;; Set up initial namespace.
(in-namespace "common")

;;; Assign context and activation markers for this Scone process.
(context-marker-setup)

;;; Must put this first so that deferred connection is fixed later.
(setq *has-link* {has link})

;;; This is the top node of the type hierarchy.  Everything is a {thing}.
;;; Note that the parent of {thing} is {thing} -- top of the tree.
(setq *thing*
      (new-type {thing} {thing}
		:context {universal}))

;;; When we try to make a connection to an element that does not yet
;;; exist (the iname has no referent), we create it as a type-node
;;; with parent {undefined thing}.  This connects it into the IS-A
;;; hierachy, but still allows us to identify elements that were never
;;; formally defined.  If later we try to define this node with a
;;; specified parent, that parent replaces {undefined thing}.
(setq *undefined-thing*
      (new-type {undefined thing} {thing}
		:context {universal}))

;;; Evil hack for bootstrapping the roots of the IS-A and context
;;; trees.  The next few new elements get the element-iname
;;; {universal} as their context, and this will be resolved later.
(setq *context* {universal})

;;; Create {tangible} and {intangible}.  Split them later.
(new-type {tangible} {thing}
	  :context {universal}
	  :english :adj)

(new-type {intangible} {thing}
	  :context {universal}
	  :english :adj)

;;; The universal context is always on. Statements in this context are
;;; always active, and entities in this context always exist.
(setq *universal*
      (new-indv {universal} {intangible}
		:english :adj-noun))

;;; This is the default context, representing actual reality.
(setq *general*
      (new-indv {general} {universal}
		:english :adj-noun))

;;; ---------------------------------------------------------------------------
;;; Create parent nodes for the built-in Scone element-types.

;;; The following definitions are in the {universal} context.
(in-context {universal})

;;; Parent node for relations.
(setq *relation*
      (new-type {relation} {intangible}))


;;; Numbers types.

(setq *number* (new-type {number} {intangible}))

(new-type {exact number} {number})
(new-type {inexact number} {number})

(setq *integer*
      (new-type {integer} {exact number}
		:english '("whole number" "counting number")))

(setq *ratio*
      (new-type {ratio} {exact number}
		:english '("fraction")))

(setq *float*
      (new-type {floating-point number} {inexact number}
		:english '("float"
			   :adj-noun
			   "real")))

;;; Strings.
(setq *string*
      (new-type {string} {intangible}))

;;; Functions.
(setq *function*
      (new-type {function} {intangible}))

;;; Lisp Defstructs.
(setq *struct*
      (new-type {struct} {intangible}))

;;; Built-in link types.
(setq *link* 
      (new-type {link} {relation}))

(setq *is-a-link*
      (new-type {is-a link} {link}))

(setq *is-not-a-link*
      (new-type {is-not-a link} {link}))

(setq *eq-link*
      (new-type {eq link} {link}))

(setq *not-eq-link*
      (new-type {not-eq link} {link}))

(setq *has-link*
      (new-type {has link} {link}))

(setq *has-no-link*
      (new-type {has-no link} {link}))

(setq *cancel-link*
      (new-type {cancel link} {link}))

(setq *split*
      (new-type {split link} {link}))

(setq *complete-split*
      (new-type {complete split link} {split link}))


;;; Now we can create some splits among types created earlier.

(new-split '({tangible} {intangible}))

(new-complete-split {number}
  '({exact number} {inexact number}))

(new-split '({integer} {ratio}))

;;; ---------------------------------------------------------------------------
;;; Core mathematical concepts.

;;; Define sets.
(setq *set*
      (new-type {set} {intangible}
		:english '("group"
			   "collection"
			   "bunch")))

;;; Useful constants.
(setq *zero* {0})
(setq *one* {1})
(setq *two* {2})

(process-deferred-connections)

;;; Set the context to {general}.
(in-context {general})

;;; Now we can add some role nodes.

;;; Almost any THING can have a set of parts.  The interpetation of PART
;;; may vary a bit between classes like TYPEWRITER, TEXAS, and THURSDAY,
;;; but all of these fall under this PART role.
(new-type-role {part} {thing} {thing}
	       :may-have t)

;;; Every set has a set of MEMBERs.
(new-type-role {member} {set} {thing})

;;; Every set has a cardinality, which is an integer.
(setq *cardinality*
      (new-indv-role {cardinality} {set} {integer}
		     :english '("number of members")))

;;; Define empty and non-empty sets.
(new-complete-split-subtypes
 {set}
 '({empty set}
   {non-empty set})
 :context {universal})

(setq *empty-set* (lookup-element {empty set}))

(setq *non-empty-set* (lookup-element {non-empty set}))


;;; ---------------------------------------------------------------------------
;;; Finally...

;;; Note the last element created by this file.
(setq *last-bootstrap-element* *last-element*)

