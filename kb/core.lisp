;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Generic core knowledge for Scone, used in most Scone applications.
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

;;;; -------------------------------------------------------------------------
;;;; UPPER

;;; Load the KB file containing important upper-ontology structures that
;;; don't need to be in "bootstrap.lisp".  As a rule, the bootstrap file contains
;;; only those structures that Scone engine code has to refer to.

(load-kb "core-components/upper")

;;;; -------------------------------------------------------------------------
;;;; UNITS

;;; Load the KB file defining units, measurable qualities, measures, and
;;; quantities.

(load-kb "core-components/units")

;;;; -------------------------------------------------------------------------
;;;; SPACE MODEL

;;; The minimal space model that you would want for almost all "common sense"
;;; reasoning.

(load-kb "core-components/space-model")

;;;; -------------------------------------------------------------------------
;;;; TIME MODEL

;;; Scone's time model.  This is essential for reasoning about actions
;;; and events, for example.

(load-kb "core-components/time-model")

;;;; -------------------------------------------------------------------------
;;;; SIMPLE PHYSICS & CHEMISTRY

;;; Minimal intuitive physics and chemistry knowledge that you will usually
;;; want around for any "common sense" application.  For example, it knows that
;;; a physical object has mass.  For more specialized scientific/engineering
;;; applications you would load a much more detailed file on top of this one.

(load-kb "core-components/physics")

;;;; -------------------------------------------------------------------------
;;;; SIMPLE BIOLOGY & MEDICINE

;;; This is meant to be the stuff that you will usually want around
;;; for almost any "common sense" application.  It knows about
;;; animals, plants, gender, parent/child relations, sickness, etc.
;;; For more specialized scientific/medical applictions you would load
;;; a much more detailed file on top of this one.

(load-kb "core-components/biology")

;;;; ----------------------------------------------------------------------
;;;; INFORMATION, FILES, AND MESSAGES

;;; General knowledge about information that you would want around for
;;; almost any "common sense" application.  The Person model uses this
;;; heavily.

(load-kb "core-components/information")

;;;; ----------------------------------------------------------------------
;;;; SOCIAL

;;; General knowledge about human organizations and social relationships that you would
;;; want around for almost any "common sense" application.

(load-kb "core-components/social-model")

;;;; ----------------------------------------------------------------------
;;;; ASPECTS OF PERSON

;;; General knowledge about people that you would want around for
;;; almost any "common sense" application.

(load-kb "core-components/person-model")

;;;; ----------------------------------------------------------------------
;;;; EPISODIC REPRESENTATION AND REASONING

;;; General knowledge of events, actions, sequences, and plans.

(load-kb "core-components/episodic")

;;;; ----------------------------------------------------------------------
;;;; GEOPOLITICS

;;; General knowledge of geography, government, and politics.

(load-kb "core-components/geopolitics")

