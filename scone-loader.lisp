;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Scone Knowledge Representation System
;;; Load the Scone engine files and the bootstrap knowledge.
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
;;; U.S. Government, or any of our other sponsors
;;; ***************************************************************************

;;; This file defines the SCONE function that starts up some user-specified
;;; version of SCONE, and sets things up so that we will by default get the
;;; matching KB files.

;;; NOTE: Contains some installation-specific pathnames.  If you move this
;;; directory, be sure to update the pathnames.

(defvar *version*)
(defvar *default-kb-pathname*)

(declaim (ftype (function (string &key (:verbose boolean)))
		load-kb))

(defun scone (&optional (version "scone-git"))
  (setq *version* version)
  (setq *default-kb-pathname* 
    (format nil "/Users/sef/Dropbox/Scone/~A/kb/anonymous.lisp"
	    *version*))
  (load (format nil "/Users/sef/Dropbox/Scone/~A/engine"
		*version*))
  ;; If we're using a Scone engine that creates a separate scone package,
  ;; get into that package.
  (when (find-package :scone)
    (in-package :scone)
    (set (intern "*DEFAULT-KB-PATHNAME*") cl-user::*default-kb-pathname*))
  (funcall (intern "LOAD-KB") "bootstrap")
  (values))

(format t "~2%;;; Call (scone \"some-version-name\") to start Scone.~%")

