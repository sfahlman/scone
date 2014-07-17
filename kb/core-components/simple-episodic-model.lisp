;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Core knowledge about actions, events, sequences of actions, and plan for
;;; Scone's core knowledge base.
;;;
;;; Constributors: E. Cinar Sahin
;;;                Maria Santofimia Romero
;;;                Laleh Roosta Pour
;;;                Scott E. Fahlman
;;; Current Maintainer: Scott E. Fahlman
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
;;; Core model for events, actions, sequences of events and actions, plans,
;;; goals, etc. Depends heavily on the time model and Scone's multi-context
;;; mechanism.  These structures are meant to support applications that describe
;;; events and actions, that try to infer goals from obeservable events, and
;;; eventually a very flexible "recipe-driven" planner.

;;; This file contains core stuff that you would want around for
;;; almost any "common sense" application.  For any more specialized
;;; area of planning, this would be augmented with a lot of additional
;;; episodic knowledge that is domain-specific.

;;; Roots of the episodic system: actions, events, time.

;;; Event
(setq *event*
      (new-type {event} {thing}
		:english '("happening" "occurrence")))

;;; Every event has a {before context} and an {after context}.
(new-indv-role {before context} {event} {intangible})
(new-indv-role {after context} {event} {intangible})

;;; The AFTER CONTEXT starts out as a clone of the BEFORE CONTEXT.
;;; Then we can make changes.
(new-is-a {after context} {before context})

;;; Often events have a location.
(new-indv-role {event location} {event} {place}
	       :english '("location" "venue" "place")
	       :may-have t)

;;; An event has a time interval, which in turn has a start-time,
;;; end-time, and duration.
(new-indv-role {event time} {event} {time interval})

;;; Any thing may be listed as the cause of an event.
(new-relation {causes}
	      :a-inst-of {thing}
	      :b-inst-of {event})

;;; Events in general have subevents.
(new-type-role {subevent} {event} {event})

;;; Anything that we think of as capable of acting on its own.
(setq *potential-agent*
      (new-type {potential agent} {thing}))

(new-intersection-type {automaton} '({man-made object}
				     {potential agent}))

(new-is-a {animate} {potential agent})
(new-is-a {organization} {potential agent})


(new-complete-split {potential agent}
		    '({animate} {organization} {automaton}))

;;; A {legal person} can own things, make contracts, etc.  Exclude,
;;; for now, automata and non-human animals.
(new-union-type {legal person}
		{potential agent}
		'({person} {organization}))

;;; ---------------------------------------------------------------------------
;;; Actions

;;; Philosophers may disagree but for Scone an {action} is just an
;;; event with an {agent} slot.

(setq *action*
      (new-type {action} {event}))

(setq *action-agent*
      (new-indv-role {action agent} {action} {potential agent}
       :english '(:no-iname :role "agent" "doer" "actor" "subject")))

(new-statement {action agent} {causes} {action})

;;; An action may have a principal object and a recipient,
;;; corresponding more or less to the (English) grammatical roles of
;;; direct and indirect object.

(setq *action-object*
      (new-indv-role {action object} {action} {thing}
		     :may-have t
		     :english '(:no-iname :role "object")))

(setq *action-recipient*
      (new-indv-role {action recipient} {action} {potential agent}
		     :may-have t
		     :english '(:no-iname :role "recipient")))

;;; Functions to make it easy to define new action types.

;;; Somewhat analogous to NEW-RELATION and NEW-STATEMENT, but instead of
;;; special element-types, these structures use standard description frames.

(defun new-action-type
    (iname
     &key
     (parent *action*)
     agent-type
     object-type
     recipient-type
     english)  
  "Create a new action-type structure with the specified INAME.
   Optionally provide a PARENT more specific than {action} and specify
   the types of the agent, object, and recipient (like the indirect
   object in English).

   If the action-type already exists, don't create a new one, but do
   add the type-restrictions for these roles."
  ;; Make it possible to pass in single items as the English argument.
  (unless (listp english) (setq english (list english)))
  ;; See if a node of this iname already exists.
  (if (lookup-element iname)
      ;; This node already exists.  See if it is an action.
      (if (eq :yes (is-x-a-y? iname *action*))
	  ;; Yes, add the role restrictions.
	  (progn
	    (setq iname (lookup-element iname))
	    ;; Add the role restrictions, if any.
	    (when agent-type
	      (the-x-of-y-is-a-z *action-agent* iname agent-type))
	    (when object-type
	      (the-x-of-y-is-a-z *action-object* iname object-type))
	    (when recipient-type
	      (the-x-of-y-is-a-z *action-recipient* iname recipient-type))
	    iname)
	  ;; No, complain and do nothing else.
	  (commentary
	   "~&New action type ~S already exists and is not an action.  Ignoring.~%"
	   iname))
      ;; No existing node with this name.  Create one.
      (let ((e (new-type iname parent
			 :english english)))
	;; Add the role restrictions, if any.
	(when agent-type
	  (the-x-of-y-is-a-z *action-agent* e agent-type))
	(when object-type
	  (the-x-of-y-is-a-z *action-object* e object-type))
	(when recipient-type
	  (the-x-of-y-is-a-z *action-recipient* e recipient-type))
	e)))

(defun new-action (agent action-type &optional object recipient)
  "Create an action instance of the specified ACTION-TYPE, filling in
   the AGENT and RECIPIENT roles if supplied.  Return the node
   representing the action itself."
  (let ((e (new-indv nil action-type)))
      (when agent
	(x-is-the-y-of-z agent *action-agent* e))
      (when object
	(x-is-the-y-of-z object *action-object* e))
      (when recipient
	(x-is-the-y-of-z recipient *action-recipient* e))
      e))
   

;;; ---------------------------------------------------------------------------
;;; Processes and activities

;;; A process is really a sort of event (or sequence of events), which may be
;;; of indeterminate duration and which may be repetitive.  Figure out how to
;;; express that properly, but for now we just need a place-holder.
(new-type {process} {event})

;;; And I think an activity is to an action as a process is to an event.
(new-type {activity} {action})

