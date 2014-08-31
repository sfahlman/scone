;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Minimal intuitive knowledge of biology and medicine for the
;;; Scone's core knowledge base.
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
;;; *************************************************************
;;; NOTE: This is meant to be the stuff that you will usually want around for
;;; any "common sense" application.  It knows about animals, plants, gender,
;;; sickness, etc.  For more specialized scientific/medical applictions you
;;; would load a much more detailed file on top of this one.

;;; Sex and Gender

(new-complete-split-subtypes
 {animate}
 '(({sexually reproducing} :adj)
   ({asexually reproducing} :adj)))

(new-complete-split-subtypes
 {sexually reproducing}
 '(({male} :adj-noun)
   ({female} :adj-noun)))

(new-type-role {parent} {sexually reproducing}
	       {sexually reproducing}
	       :n 2)

(new-indv-role {mother} {sexually reproducing}
	       {parent})

(new-is-a {mother} {female})

(new-indv-role {father} {sexually reproducing}
	       {parent})

(new-is-a {father} {male})

(new-type-role {offspring} {sexually reproducing}
	       {sexually reproducing}
	       :may-have t)

(new-type-role {son} {sexually reproducing}
	       {offspring}
	       :may-have t)

(new-is-a {son} {male})

(new-type-role {daughter} {sexually reproducing}
	       {offspring}
	       :may-have t)

(new-is-a {daughter} {female})


;;; Habitat

(new-complete-split-subtypes
 {animal}
 '(({flying} :adj)
   ({non-flying} :adj )))

(new-complete-split-subtypes
 {animal}
 '(({water-dwelling} :adj "aquatic")
   ({land-dwelling} :adj "terrestrial")
   ({amphibious} :adj)))

(new-complete-split-subtypes
 {animal}
 '(({air-breathing} :adj)
   ({water-breathing} :adj)))

(new-is-a {land-dwelling} {air-breathing})
(new-is-a {water-dwelling} {water-breathing})


;;; Taxonomy.

(new-complete-split-subtypes {animal}
			     '({vertebrate} {invertebrate}))

(new-is-a {vertebrate} {sexually reproducing})

(new-complete-split-subtypes {vertebrate}
			     '({fish} {amphibian} {reptile} {bird} {mammal}))

(new-is-a {fish} {water-dwelling})
(new-is-a {amphibian} {amphibious})
(new-is-a {reptile} {land-dwelling})
(new-is-a {bird} {land-dwelling})
(new-is-a {mammal} {land-dwelling})

(new-type {water-dwelling mammal} {mammal})
(new-is-not-a {water-dwelling mammal} {land-dwelling})
(new-is-a {water-dwelling mammal} {water-dwelling})
(new-is-not-a {water-dwelling mammal} {water-breathing})
(new-is-a {water-dwelling mammal} {air-breathing})

(new-is-a {fish} {non-flying})
(new-is-a {amphibian} {non-flying})
(new-is-a {reptile} {non-flying})
(new-is-a {bird} {flying})
(new-is-a {mammal} {non-flying})

;;; {person} defined earlier.
(new-is-a {person} {mammal})

;;; Define a few animal types, just as an example.
(let ((mammal-split
       (new-split-subtypes
	{mammal}
	'({elephant} {lion} {tiger} {bat} {monkey}
	  {pig} {horse} {cow} {donkey} {dog} {cat} {whale}))))
  (add-to-split mammal-split {person}))

(new-is-not-a {bat} {non-flying})
(new-is-a {bat} {flying})

;; Got to say explicitly that whale is not land-dwelling.  That's implied by
;; {water-dwelling mammal}, but we lose the race because of the direct parent
;; link to {mammal}.
(new-is-not-a {whale} {land-dwelling})
(new-is-a {whale} {water-dwelling mammal})

(new-split-subtypes {bird}
		    '({canary} {eagle} {crow} {penguin}))

(new-is-not-a {penguin} {flying})
(new-is-a {penguin} {non-flying})

(new-split-subtypes {fish}
		    '({shark} {tuna} {perch} {goldfish} {flying-fish}))

(new-is-not-a {flying-fish} {non-flying})
(new-is-a {flying-fish} {flying})

;;; Diseases

(new-type {disease} {intangible})

(new-complete-split-subtypes
 {disease}
 '(({infectious} :adj)
   ({non-infectious} :adj)))

(new-indv-role {pathogen} {infectious}
	       {animate})

;;; Define a few diseases.
(new-members {infectious}
	     '({influenza} {smallpox} {anthrax}))

(new-members {non-infectious}
	     '({scurvy} {cystic fibrosis} {melanoma}))

(new-complete-split-subtypes
 {animate}
 '(({sick} :adj "ill" "diseased")
   ({healthy} :adj)))

(new-is-a {sick} {bad health condition})
(new-is-a {healthy} {good health condition})

(new-indv-role {affliction} {sick} {disease}
	       :english '(:role "malady"))
