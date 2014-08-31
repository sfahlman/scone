;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Minimal intuitive physics and chemistry knowledge for Scone's core
;;; knowledge base.
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
;;; NOTE: This is very minimal for now.

;;; Every physical object has a mass.
(new-indv-role {object mass} {physical object}
	       {mass measure}
	       :english '(:role "mass" "weight"))

;;; Every physical object also has some dimensions.
(new-indv-role {object height} {physical object}
	       {length measure}
	       :english '(:role "height"))

(new-indv-role {object length} {physical object}
	       {length measure}
	       :english '(:role "length"))

(new-indv-role {object width} {physical object}
	       {length measure}
	       :english '(:role "width" "breadth"))

(new-indv-role {object volume} {physical object}
	       {volume measure}
	       :english '(:role "volume"))

(new-indv-role {object area} {physical object}
	       {area measure}
	       :english '(:role "area" "footprint"))

;;; More about materials.

(new-split-subtypes
 {material}
 '(({solid} :adj-noun)
   ({liquid} :adj-noun)
   ({gas} "vapor" :adj "gaseous" "vaporized")
   ({plasma})))

;;; A favorite material.
(new-type {water} {material})
(new-intersection-type {ice} '({water} {solid}) :english "water ice")
(new-intersection-type {liquid water} '({water} {liquid}) :english "water")
(new-intersection-type {steam} '({water} {gas}) :english "water vapor")









