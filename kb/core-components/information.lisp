;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Generic knowledge about information and files of various types for
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
;;; ***************************************************************************

;;; NOTE: The information below about file-types, message headers, and
;;; other forms of communication was originally developed for the CMU
;;; RADAR project.  This was a DARPA_funded project, roughly from 2003
;;; through 2007, whose goal was to develop a software personal
;;; assistant that could help with organizing Email, scheduling
;;; appointments and events, etc.  The KB has been updated to match
;;; the current Scone engine and to reflect evolution in message
;;; technologies -- for example, it is no longer true that we can
;;; assume that EVERYONE has a home phone or a home phone number.

;;; Information objects

;;; An information object is some blob of information itself, not the
;;; physical manifestation.

(new-type {information object} {thing})

(new-split '({information object} {physical object}))

;;; Define some (disjoint) types of information object.
(new-split-subtypes {information object}
		    '({text object}
		      {image object}
		      {audio object}
		      {video object}
		      {executable object}
		      {binary data object}))

;;; A file is more about packaging than content.
(new-type {file} {information object})

;;; Define some file types and relate them to information object types.

(new-intersection-type {text file}
		       '({text object} {file}))

(new-intersection-type {image file}
		       '({image object} {file}))

(new-intersection-type {audio file}
		       '({audio object} {file}))

(new-intersection-type {video file}
		       '({video object} {file}))

(new-intersection-type {executable file}
		       '({executable object} {file}))

(new-intersection-type {binary file}
		       '({binary data object} {file}))

;;; A string is a kind of text object.
(new-is-a {string} {text object})

;;; Some subtypes of string.
(new-split-subtypes
 {string}
 '(({url} "web page" "web address")
   ({Email address} "E-mail address" "internet address" "net address")
   ({date-time expression} "timestamp" "date" "time" "time expression")
   ({street address} "mailing address" "physical mail address" "postal address")
   ({phone number} "telephone number")))

;;; A message is a kind of information object.
(new-type {message} {information object})

;;; Some types of message.
(new-intersection-type {voice message}
		       '({audio object} {message}))

(new-intersection-type {video message}
		       '({video object} {message}))

(new-intersection-type {text message}
		       '({text object} {message}))

(new-split-subtypes {text message}
		    '({electronic message}
		      {hardcopy message}))

(new-split-subtypes 
 {electronic message}
 '(({Email message} "E-mail message" "Email" "E-mail")
   ({instant message} "IM" "chat message")))

;;; A message has one sender and any number of recipients.
(new-indv-role {message sender} {message} {person}
	       :english '(:role "sender"))

(new-type-role {message recipient} {message} {person}
	       :english '(:role "recipient"))

(new-type-role {message To recipient} {message}
	       {message recipient})

(new-type-role {message CC recipient} {message}
	       {message recipient})

(new-type-role {message BCC recipient} {message}
	       {message recipient})

;;; An Email message has a set of fields.
(new-type-role {message field} {message} {information object})
(x-is-a-y-of-z {message field} {part} {message})

;;; The fields of an Email message.
(new-type-role {Email message field} {email message} {string})
(x-is-the-y-of-z {Email message field} {message field} {Email message})

(new-indv-role {Email From field} {Email message} {Email address}
	       :english '(:role "From field" "From address"))

(new-type-role {Email To field} {Email message} {Email address}
	       :english '(:role "To field" "To address"))

(new-type-role {Email CC field} {Email message} {Email address}
	       :may-have t
	       :english '(:role "CC field" "CC address"))

(new-type-role {Email BCC field} {Email message} {Email address}
	       :may-have t
	       :english '(:role "BCC field" "BCC address"))

(new-type-role {Email Reply-To field} {Email message} {Email address}
	       :may-have t
	       :english '(:role "Reply-To field" "Reply-To address"
				"Reply field" "Reply address"))

(new-indv-role {Email Subject field} {Email message} {string}
	       :may-have t
	       :english '(:role "Subject field" "Subject"))

(new-indv-role {Email DATE field} {Email Message}
	       {date-time expression}
	       :english '(:role "timestamp" "date" "send date"))

(new-indv-role {Email Body field} {Email message} {string}
	       :english '(:role "Body" "Body field"))

(new-type-role {Email attachment} {Email message} {file}
	       :may-have t
	       :english '(:role "attachment" "enclosure"))

;;; End of RADAR-derived knowledge.

