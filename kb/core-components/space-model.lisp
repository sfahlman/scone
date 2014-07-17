;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Model of space and spatial relations for Scone's core knowledge base.
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
;;; NOTE: This is very minimal for now.  Ideally, we need to integrate
;;; this with a module that reasons about 2D and 3D maps and images.
;;; I'm pretty sure that human spatial reasoning is not all symbolic.
;;; But that's a big task -- probably a Ph.D. thesis for some really
;;; smart student.

;;; "Place" is used in a few ways: geographical area, point in 3-space, etc.

(new-type {place} {thing}
	  :english '("place" "location" "position"))

;;; The concept "place" includes both abstract spaces, like "Canada" and
;;; tangible objects such as "my house".

(new-intersection-type {tangible place}
		       '({tangible} {place}))
(new-intersection-type {intangible place}
		       '({intangible} {place}))

;;; A geographical-area is a region on earth.
;;; We'll worry about other planets later.

(new-type {geographical area} {intangible place}
	  :english '("area" "location" "geographical area"))

(new-split-subtypes {geographical area}
		    '(({land region} "land")
		      ({water region} "water")
		      ({air region} "air")
		      ({space region} "space")
		      {mixed-type region}))

;;; Define some relations over geographical areas.

(new-relation {area near}
	      :a-inst-of {geographical area}
	      :b-inst-of {geographical area}
	      :symmetric t
	      :english '(:relation "near"))

(new-relation {area adjacent to}
	      :a-inst-of {geographical area}
	      :b-inst-of {geographical area}
	      :symmetric t
	      :english '(:relation "adjacent to"))

(new-relation {area contains}
	      :a-inst-of {geographical area}
	      :b-inst-of {geographical area}
	      :english '(:relation "contains"))

(new-relation {same area as}
	      :a-inst-of {geographical area}
	      :b-inst-of {geographical area}
	      :symmetric t)

(new-relation {greater area than}
	      :a-inst-of {geographical area}
	      :b-inst-of {geographical area}
	      :english '(:relation "larger than"
			 "greater area than"))

(new-relation {area more populous than}
	      :a-inst-of {geographical area}
	      :b-inst-of {geographical area}
	      :english '(:relation "larger than"
			 "more populous than"))

(new-relation {located at}
	      :a-inst-of {physical object}
	      :b-inst-of {place})
