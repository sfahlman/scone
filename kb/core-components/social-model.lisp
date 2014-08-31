;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; General knowledge about human organizations, social relationships, and
;;; governance for Scone's core knowledge base.
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
;;; General knowledge about human organizations and social
;;; relationships that you would want around for almost any "common
;;; sense" application.

;;; An organization is a set of people.
(new-type {organization} {set})
(the-x-of-y-is-a-z {member} {organization} {person})


;;; A "legal entity" is an organization or a person able to act as a
;;; person under the law.
(new-union-type {legal entity}
		{thing}
		'({organization} {person})
		:english '("legal person"))

;;; A few kinds of organization.
(new-split-subtypes {organization}
		    '({school}
		      {company}
		      {cartel}
		      ({political party} "party")
		      {government agency}
		      {club}
		      {family}
		      {criminal organziation}
		      {military unit}))

;;; A few common kinds of school.
(new-split-subtypes {school}
		    '({university}
		      {community college}
		      {high school}
		      {middle school}
		      {grade school}
		      {kindergarten}
		      {pre-school}))

;;; A few universities.
(new-members
 {university}
 '(({Carnegie Mellon} "CMU" "Carnegie Mellon University")
   ({MIT} "Massachusetts Institute of Technology")
   ({Stanford} "Stanford University")
   ({Berkeley} "University of California at Berkeley" "Cal")
   ({Harvard} "Harvard University")
   ({Yale} "Yale University")
   ({Princeton} "Princeton University")
   ({Johns Hopkins} "Johns Hopkins University" "JHU")
   ({University of Maryland} "Maryland")
   ({Oxford} "Oxford University" "University of Oxford")
   ({Cambridge} "Cambridge University" "University of Cambridge")))

;;; Organizations have leaders and usually one principal leader.
(new-type-role {organization leader} {organization} {person}
	       :english '("leader"))
(new-indv-role {organization principal leader} {organization} {person}
	       :may-have t
	       :english '("principal leader" "leader"))
(new-is-a {organization principal leader} {organization leader})

