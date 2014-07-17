;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Generic knowledge about people and their aspects for the Scone's
;;; core knowledge base.
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

;;; General knowledge about people that you would want around for
;;; almost any "common sense" application.

;; Note: The {person} type and some other core knowledge about
;; parent/offspring relations are defined in the "simple-biology"
;; file.

;; Some subtypes of person.

(new-intersection-type {child}
		       '({person} {immature}))

(new-intersection-type {adult}
		       '({person} {mature}))

(new-intersection-type {boy}
		       '({child} {male}))

(new-intersection-type {girl}
		       '({child} {female}))

(new-intersection-type {man}
		       '({adult} {male}))

(new-intersection-type {woman}
		       '({adult} {female}))

(new-intersection-type {male person}
		       '({person} {male}))

(new-intersection-type {female person}
		       '({person} {female}))

;; Additional stuff about family relations.  For now, we just
;; list relationships we may want to represent, and not all the
;; rules about "sister of parent is aunt", etc.

(new-type-role {relative} {person} {person})

(new-type-role {close relative} {person} {relative})
(new-type-role {medium relative} {person} {relative})
(new-type-role {distant relative} {person} {relative})

(new-type-role {blood relative} {person} {relative})
(new-type-role {relative by marriage} {person} {relative})
(new-intersection-type {close blood relative}
		       '({close relative} {blood relative}))
(new-intersection-type {medium blood relative}
		       '({medium relative} {blood relative}))

(new-is-a {parent} {close blood relative})
(new-is-a {offspring} {close blood relative})

(new-type-role {sibling} {person} {close blood relative} :may-have t)
(new-type-role {brother} {person} {sibling} :may-have t)
(new-is-a {brother} {male})
(new-type-role {sister} {person} {sibling} :may-have t)
(new-is-a {sister} {female})
(new-type-role {grandparent} {person} {close blood relative} :n 4)
(new-type-role {grandfather} {person} {grandparent} :n 2)
(new-is-a {grandfather} {man})
(new-type-role {grandmother} {person} {grandparent} :n 2)
(new-is-a {grandmother} {woman})
(new-type-role {grandchild} {person} {close blood relative} :may-have t)
(new-type-role {granddaughter} {person} {grandchild} :may-have t)
(new-is-a {granddaughter} {female})
(new-type-role {grandson} {person} {grandchild} :may-have t)
(new-is-a {grandson} {male})
(new-type-role {cousin} {person} {medium blood relative})
(new-type-role {aunt} {person} {medium blood relative})
(new-is-a {aunt} {female})
(new-type-role {uncle} {person} {medium blood relative})
(new-is-a {uncle} {male})


;; NOTE: The following stuff was initially developed for the RADAR
;; project at CMU, circa 2006, updated and re-formatted to match the
;; current Scone engine.

;; Parts of the person's name.  For now, this is specific to
;; Anglo-American culture.  So we really should put this into a
;; culture-specific context.

(new-indv-role {person full name} {person} {string}
	       :english '(:role "name" "full name"))

(new-indv-role {first name} {person}
	       {string}
	       :english '(:role "given name" "Christian name"))

(new-indv-role {last name} {person}
	       {string}
	       :english '(:role "family name" "surname"))

(new-indv-role {middle name} {person}
	       {string} :may-have t)

;; This is for stuff like "Sr.", "Jr.", "III", etc.
(new-indv-role {name suffix} {person}
	       {string} :may-have t
	       :english '(:role "suffix"))

;; This is for honorific titles such as "Professor" or
;; "Emperor".  For now, this is just a string field, but we
;; probably should enumerate the common ones.
(new-indv-role {person title} {person}
	       {string}
	       :may-have t
	       :english '(:role "title" "honorific title"))

;; A person may have any number of alternative names.  We
;; distinguish between nicknames and aliases, though the
;; difference depends on intent and can be fuzzy.

(new-type-role {alternative name} {person}
	       {string} :may-have t)

(new-type-role {nickname} {person}
	       {alternative name} :may-have t)

(new-type-role {alias} {person}
	       {alternative name} :may-have t)

;; A person may have an employer, which can be any legal entity --
;; that is, an organization or a person.
(new-indv-role {employer} {person}
	       {legal entity}
	       :may-have t)

;;; Addresses, home and business.

;;; Add functions to parse an address and relate to geographic
;;; regions or entities.  For now it's just an uninterpreted string.

(new-type-role {person address} {person} {street address}
	       :english '(:role "address" "street address"
				"mailing address" "physical mail address"
				"postal address"))

(new-indv-role {home address} {person} {person address})
(new-indv-role {business address} {person} {person address})

;; Phone numbers.
(new-type-role {person phone number} {person}
	       {phone number}
	       :english '("phone number" "telephone number"))
(new-indv-role {primary phone number} {person}
	       {person phone number})
(new-indv-role {home phone number} {person}
	       {person phone number}
	       :may-have t)
(new-indv-role {cell phone number} {person}
	       {person phone number}
	       :may-have t
	       :english '("mobile phonbe number"))
(new-indv-role {business phone number} {person}
	       {person phone number}
	       :may-have t
	       :english '("office phone number"))
(new-indv-role {fax phone number} {person}
	       {person phone number} :may-have t
	       :english '("fax number"))

;;; Email address
(new-indv-role {person email address} {person}
	       {email address}
	       :english '(:role "Email address" "E-mail address"
				"Email" "E-mail"))

;;; Personal web page.
(new-indv-role {personal web page} {person}
	       {url}
	       :english '(:role "url" "web page" "personal web page"
			  "home page"))

;;; Bio is some kind of text object, proibably a file or string.
(new-indv-role {biographical note} {person}
	       {text object}  :may-have t
	       :english '(:role "bio" "biography" "biographical note"))

;;; A person may have one or more pictures.
(new-type-role {person picture} {person}
	       {image file}  :may-have t
	       :english '(:role "picture" "photo" "mug shot"))

;;; Contact information is a collection of other roles.
(new-type-role {contact information} {person}
	       {string})

(new-is-a {person phone number} {contact information})
(new-is-a {person email address} {contact information})
(new-is-a {personal web page} {contact information})
(new-is-a {person address} {contact information})

;;; ------------------------------------------------------------------------
;;; Inter-Personal Relations

(new-relation {friend of}
	      :a-inst-of {person}
	      :b-inst-of {person}
	      :symmetric t)

(new-relation {co-worker of}
	      :a-inst-of {person}
	      :b-inst-of {person}
	      :symmetric t)

(new-relation {family member of}
	      :a-inst-of {person}
	      :b-inst-of {person}
	      :symmetric t)

;;; This may be either formal or informal status.
(new-relation {higher status than}
	      :a-inst-of {person}
	      :b-inst-of {person}
	      :transitive t
	      :english '(:relation
			 :inverse-relation "lower status than"))

;;; This is meant to be a formal ranking according to the rules of some
;;; hierarchical organization, such as the military or a university.  It
;;; implies higher status.

(new-relation {higher rank than}
	      :parent {higher status than}
	      :a-inst-of {person}
	      :b-inst-of {person}
	      :transitive t)

(new-relation {reports to}
	      :parent {higher rank than}
	      :a-inst-of {person}
	      :b-inst-of {person})

;;; ------------------------------------------------------------------------
;;; Subtypes of Person

;; Some types of people you find around a university.  (This is not
;; really core stuff, since it is only useful for certain specialized
;; applications, but we keep it here for now as an illustration of complex ranks
;; and positions within an organization.)

(new-type {university person} {person}
	  :english '(:adj-noun "academic"))

(new-split-subtypes
 {university person}
 '(({faculty member} :noun :adj "faculty")
   ({post-doc} "post-doctoral fellow" "post-doctoral research associate")
   {visitor} 
   {student}
   ({staff member} :noun :adj "staff")))

;;; This type may overlap with faculty or staff.
(new-type {university administrator} {university person})

;;; Types of staff.
(new-split-subtypes {staff member}
		    '({technical staff member}
		      {administrative staff member}))

(new-split-subtypes 
 {technical staff member}
 '(({staff programmer} "research programmer" "programmer")
   {technical writer}
   {data wrangler}
   {user studies assistant}))

(new-split-subtypes
 {administrative staff member}
 '({departmental administrator}
   ({secretary} "executive assistant" "assistant")
   {student coordinator}
   {business manager}))

(new-split-subtypes
 {student}
 '(({undergraduate student}  :adj-noun "undergradaute" "undergrad")
   ({graduate student} "grad student")
   {special student}))

;;; Hierarchy of professitude.

(new-split-subtypes
 {faculty member}
 '(({professor-rank} :adj)
   ({associate-professor-rank} :adj)
   ({assistant-professor-rank} :adj)))

(new-statement {professor-rank}
	       {higher rank than}
	       {associate-professor-rank})

(new-statement {associate-professor-rank}
	       {higher rank than}
	       {assistant-professor-rank})

(new-statement {assistant-professor-rank}
	       {higher rank than}
	       {post-doc})

(new-statement {post-doc}
	       {higher rank than}
	       {graduate student})

(new-statement {graduate student}
	       {higher rank than}
	       {undergraduate student})

;;; NOTE:  Some of the below is CMU-specific.  Leave it that way for now.
;;; Ultimately this should be in a CMU context.

(new-split-subtypes {faculty member}
		    '({tenure-track faculty member}
		      {research faculty member}
		      {teaching faculty member}
		      {systems faculty member}))

(english {tenure-track faculty member}
	 :adj "tenure-track")
(english {research faculty member}
	 :adj "research" "research-track" :noun "research scientist")
(english {teaching faculty member}
	 :adj "teaching" "teaching-track" :noun "lecturer" )
(english {systems faculty member}
	 :adj "systems" "systems-track" :noun "systems scientist")

(new-intersection-type
 {professor}
 '({tenure-track faculty member} {professor-rank})
 :english '("full professor"))

(new-intersection-type
 {associate professor}
 '({tenure-track faculty member} {associate-professor-rank}))

(new-intersection-type
 {assistant professor}
 '({tenure-track faculty member} {assistant-professor-rank}))

(new-intersection-type
 {research professor}
 '({research faculty member} {professor-rank})
 :english '("resarch full professor" "full research professor"))

(new-intersection-type
 {research associate professor}
 '({research faculty member} {associate-professor-rank})
 :english '("associate research professor"))

(new-intersection-type
 {research scientist}
 '({research faculty member} {assistant-professor-rank}))

(new-intersection-type
 {teaching professor}
 '({teaching faculty member} {professor-rank})
 :english '("teaching full professor" "full teaching professor"))

(new-intersection-type
 {teaching associate professor}
 '({teaching faculty member} {associate-professor-rank})
 :english '("associate teaching professor"))

(new-intersection-type
 {lecturer}
 '({teaching faculty member} {assistant-professor-rank}))

(new-intersection-type
 {principal systems scientist}
 '({systems faculty member} {professor-rank}))

(new-intersection-type
 {senior systems scientist}
 '({systems faculty member} {associate-professor-rank}))

(new-intersection-type
 {systems scientist}
 '({systems faculty member} {assistant-professor-rank}))

;;; End of RADAR-derived ontology.
