;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Engine for the Scone Knowledge Representation System
;;;
;;; Author & Maintainer: Scott E. Fahlman
;;; ***************************************************************************
;;; Copyright (C) 2003-2021, Carnegie Mellon University.
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
;;; Development Feb 2013 - Dec 2015 was supported in part by the
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
;;;
;;; ***************************************************************************
;;; GENERAL STYLE NOTE: We want the runtime marker-scanning operations
;;; to be as fast as possible and to be non-consing.  So that code is
;;; optimized for performance first and for clarity second.  Code that
;;; builds or modifies the KB, or that does higher-level setup for
;;; marker ops, can be slower, so I have tried to optimize that for
;;; clarity and ease of maintenance.
;;; ***************************************************************************

;;; TABLE OF CONTENTS:

;;; Global Variables, Constants, and Macros
;;;    Variables That Control KB Input and Loading
;;;    Variables That Control Printing and Naming
;;;    Variables That Control Scans
;;;    Element-Related Variables
;;;    Variables Linking to Essential Scone Elements
;;;    Variables and Macros Related to Marker Bits
;;;       Define the Range of Legal Markers
;;;       Marker Pairs
;;;       Marker Predicates
;;;       Marker Allocation
;;;       Context and Activation Markers
;;;       Marker Implementation
;;;    Definitions Related to Flag Bits
;;;    Variables Related to Element Names
;;; Scone Elements
;;;    Definition of the SCONE Element Structure
;;;    Low-Level Element Operations
;;;    Element Properties
;;; Marker Operations
;;;    Low-Level Marker Operations
;;;    User-Level Marker Operations
;;; Accessing, Connecting, and Disconnecting Wires
;;; Definition of the SCONE Element Types
;;;    Indv Nodes
;;;    Primitive Nodes
;;;    Type Nodes
;;;    Map Nodes
;;;    Is-A Links
;;;    EQ Links
;;;    Has Links
;;;    Cancel Links
;;;    Splits
;;;    Relations
;;;    Statements
;;;    Creating Roles
;;;    Printing Elements
;;;    Adding Sets of Subtypes and Instances
;;;    Defined Types
;;;    Functions to Maintain a Well-Formed Network
;;; Marker Scans
;;; Is-A Hierarchy Scans
;;;    Uppermost/Lowermost Scans
;;;    Boolean Operations Over Marked Elements
;;;    Marker Scans For Roles, Relations, and Statements
;;;    Context Activation
;;; Queries and Predicates
;;; Mark, List, and Show Functions
;;;    Basic List and Show Machinery
;;;    Mark, List, Show Operations on the Is-A Hierarchy
;;;    Miscellaneous Show Functions
;;; Operations on Roles, Relations, and Statements
;;;    Access Functions
;;;    Predicates
;;;    Adding New Roles or Fillers
;;;    List and Show Functions
;;; Cardinality Functions
;;; Internal Element Names and Namespace
;;; External Names for Elements
;;;    Basic Machinery for English Names
;;;    List and Show Functions on External Names
;;; Loading KB Files
;;; KB Checkpointing and Persistence
;;; Removing Elements from the KB

;;; ***************************************************************************
;;;     HERE BEGINS THE ACTUAL CODE
;;; ***************************************************************************

;;; Define forms for section headings and other comments that will affect
;;; auto-generated documentation.

(defun section (&rest rest) (declare (ignore rest)))
(defun subsection (&rest rest) (declare (ignore rest)))
(defun subsubsection (&rest rest) (declare (ignore rest)))
(defun to-do (&rest rest) (declare (ignore rest)))

;;; ***************************************************************************
(section "Global Variables, Constants, and Macros"

   "This section contains global variables, constants, and macros used
   by the Scone engine.")

;;; The operations beyond this point must be as fast as possible.
(declaim (optimize (speed 3) (space 0) (safety 0)))

;;; ========================================================================
(subsection "Variables That Control KB Input and Loading")

;;; Default pathname for various load-store operations.
;;; NOTE: Customize this for your own site.
(defvar *default-kb-pathname*
  (pathname "/afs/cs.cmu.edu/project/scone/current/kb/anonymous")
  "Default location for text-format KB files.")

(defvar *load-kb-stream* nil
  "Stream used for loading the current KB file.")

(defvar *loading-kb-file* nil
  "If reading a KB file, this wil be set.  Certain interactive dialogues
   are suppressed, etc.")

(defvar *loaded-files* nil
  "A list of files loaded in the current KB.  The head of the list is
   the file loaded most recently.  Files go on this list when they
   have been loaded completely.")

(defvar *last-loaded-elements* nil
  "Every time we load a file, we push the value of the last loaded element
   onto this list.  The first element of the list is therefore the last
   element we loaded.  Any elements created after this in the element chain
   must be newly created.  The structure of this list parallels that of
   *LOADED-FILES*: for each file name in one list, we have its last element
   in the other list.")

(defvar *currently-loading-files* nil
  "A list of the files currently being loaded. One file may load another,
   and so on, so there many be a number of files on this list.  The first
   file on the list is the most recently started (i.e. the load that is
   most deeply nested), while the last file is generally the one for which
   the user called LOAD-KB.")

(defvar *last-bootstrap-element* nil
  "Last element created by the BOOTSTRAP.LISP file.")

(defvar *verbose-loading* nil
  "If set, loading files will produce printout showing progress.
   Normally set by the :verbose keyword arg in various load
   functions.")

(defvar *disambiguate-policy* :ask
  "This global var controls what happens when an ambiguous element
   name is encountered during input.  :ERROR says to signal an error.
   :ASK says to ask the user. :FIRST says just return the first
   meaning.")

(defvar *defer-unknown-connections* nil
  "Normally when we see a reference to an unknown element, an error is
   signalled.  If this variable is set, save the known connection on
   the *deferred-connections* list and try to fix it later.  This
   allows for forward or circular references when these are
   necessary.")

(defvar *deferred-connections* nil
  "When loading a KB file, we may find references to elements not yet
   loaded or defined.  When we do, push a triplet onto this list and try to
   create the connection later.")
;;; ========================================================================
(subsection "Variables That Control Load-Time Inference")

(defvar *no-kb-error-checking* nil
  "If T, suppress most checking when creating KB elements.  Use this only
   when loading a file that you've loaded successfully before.")

(defvar *check-defined-types* t
  "If T, check whether newly defined elements fit the definition of
   some defined type.  If NIL, don't bother.")

(defvar *create-undefined-elements* t
  "If non-nil, when we encounter a reference to a node that is not yet
   defined, we create a new type node with parent {thing} to represent
   that element.  If nil, signal an error in this case.  Note that
   *DEFER-UNKNOWN-CONNECTIONS* takes precedence.")

(defvar *deduce-owner-type-from-role* t
  "If NIL, we signal an error if we call X-IS-THE-Y-OF-Z (or a variant)
   and Z does not have or inherit a Y role.  But if this switch is on,
   we try to create an is-a link from Z to the owner of Y.  If successful,
   we create and return the link.  If not, we return NIL, and the operation
   will be aborted.")

(defvar *comment-on-element-creation* t
  "When Scone creates a new element not specificially defined by the
   user, usually because the element is referred to before it is
   defined, comment on this via COMMENTARY.")

(defvar *comment-on-defined-types* nil
  "When Scone detects that some element satisfies the definition of a
   defined type and adds the element to that type, comment on this via
   COMMENTARY.")

(defvar *comment-on-rule-check* nil
  "When Scone checks if rules are satisfied, comment on this via
   COMMENTARY.")

(defvar *comment-on-redundant-links* nil
  "When Scone detects that a new IS-A or EQ link is redundant,
   comment on this via COMMENTARY.
   NOTE: remove when merging with actual redundant link code.")

;;; ========================================================================
(subsection "Variables That Control Printing and Naming")

(defvar *print-namespace-in-elements* :maybe
  "If T, always include the namespace, followed by a colon, in element
   inames.  This ensures that what we print can be read back in without
   any namespace ambiguity.  If NIL, do not include the namespace.  This
   is better for human readabilty.  If :MAYBE, print the namespace if it
   differs from the current *NAMESPACE*.  This is usually safe.")

(defvar *generate-long-element-names* nil
  "If T, generate long names for certain element types.  For example,
   a statement link will have an internal name that is an English
   description of arguments and the relation.  This is good for
   development, but significantly increases the storage requirement for
   large knowledge bases.")

(defvar *print-keylist* t
  "If T, dumping an element pattern includes the list of keyword/value
   pairs as part of the pattern.  That is necessary if we want to read
   that pattern back in, but set this to NIL for some human-readable
   dumps.")

(defvar *show-ontology-counter* 0
  "Counter used only by SHOW-ONTOLOGY.")
(declaim (type integer *show-ontology-counter*))

(defvar *show-ontology-indent* 3
  "An integer, the number of spaces to indent each level in
   SHOW-ONTOLOGY.")
(declaim (type integer *show-ontology-counter*))

(defvar *show-stream* *standard-output*
  "Stream to which we print the output of SHOW functions.  If NIL,
   don't print these at all.")
(declaim (type stream *show-stream*))

(defvar *commentary-stream* *standard-output*
  "Stream to which we print commentary about Scone's actions.  For example,
   if Scone makes a deduction and adds something to the KB as a result of 
   that, we note that here.  If NIL, don't print this stuff at all.")
(declaim (type stream *commentary-stream*))

(defun commentary (&rest args)
  "Just like FORMAT but with no STREAM arg.  Printing goes to the
   *COMMENTARY-STREAM*, if any.  Each call to COMMENTARY is put on its
   own line, so the caller doesn't ahve to do that."
  (when *commentary-stream*
    (format *commentary-stream* "~&")
    (apply #'format (cons *commentary-stream* args))
    (terpri *commentary-stream*)))

;;; ========================================================================
(subsection "Variables Related to Element Names")

(defvar *namespace* nil
  "The structure representing the current default namespace.")

(defvar *namespaces* (make-hash-table :test #'equal)
  "A hash table containing an entry for each namespace currently loaded in
   the KB.  Associates a string (the name of the namespace) with a
   namespace structure.")

(defun list-namespaces ()
  "Return a list of all the Scone namespaces that are currently loaded."
  (let ((namespace-list nil))
    (maphash #'(lambda (key entry)
		 (declare (ignore entry))
		 (push key namespace-list))
	     *namespaces*)
    namespace-list))

(defvar *number-hashtable*
  (make-hash-table :test #'eql)
  "A hashtable associating numbers (integer, ratio, or float) with existing
   elements.")

(defvar *string-hashtable*
  (Make-hash-table :test #'equal)
  "A hashtable associating strings with existing string-nodes.  Lookup is
   case-sensitive")

(defvar *function-hashtable*
  (make-hash-table :test #'eq)
  "A hashtable associating Common Lisp functions with existing
   function-nodes.  This works only for named functions, as in #'foo.  It
   does not work for lambda expressions, even if they are equal.")

(defvar *struct-hashtable*
  (make-hash-table :test #'eq)
  "A hashtable associating Common Lisp desfstruct objects with existing
   primitive elements.")

(defvar *iname-counter* 0
  "Counter used by GEN-INAME to create unique internal names for elements.
   Increment this counter each time it is used.")
(declaim (type fixnum *iname-counter*))

(defvar *iname-prefix* 0
  "Increment this to start a whole new series of unique internal inames.")
(declaim (type fixnum *iname-prefix*))  

;;; NOTE: English names are distinct from internal names.  See the
;;; commentary at the start of the English Names section.

(defvar *english-dictionary*
  (make-hash-table :test #'equal)
  "Hash table associating English words and phrases (strings in Lisp) with
   Scone elements.  A given string may be associated with many elements, so
   the hash-table value is a list if it exists at all.  Each element in the
   list is a pair (element . syntax-tag).")

(defvar *legal-syntax-tags*
  '(:noun :adj :adj-noun :role :inverse-role :relation :inverse-relation
	  :verb :adverb)
  "Each external name has an associated syntax-tag.  The tags currently
   defined are stored in this constant.")

;;; ========================================================================
(subsection "Variables That Control Scans")

(defvar *ignore-context* nil
  "If T, marker-propagations pay no attention to whether or not the nodes and links
   are in an active context.  This variable is used for upscanning while
   activating a context, and may also be useful finding whether some entity
   exists in ANY context.")

(defvar *one-step* nil
  "If T, marker propagations go only one level in the desired
   direction.  Used in functions such as MARK-PARENTS, where you don't
   want the full transitive closure.")

(defvar *default-recursion-allowance* 2
  "In functions such as MARK-REL, we must individually activate and
   explore each description instance in which the source node plays a
   role, and this process may recurse.  This is the default allowance
   for recursion depth that we will explore.")


;;; ========================================================================
(subsection "Element-Related Variables")

(defvar *n-elements* 0
  "The number of elements currently in use in the KB.")
(declaim (type fixnum *n-elements*))

;;; There is a chain of pointers running through all the elements in the
;;; KB.  This starts at *FIRST-ELEMENT*.  Each element has a NEXT-ELEMENT
;;; slot pointing to the next element in the chain.  We keep hold of
;;; *LAST-ELEMENT* so that we can extend the chain at the end.

(defvar *first-element* nil
  "First element in the chain of all elements in the KB.")

(defvar *last-element* nil
  "Last element in the chain of all elements in the KB.")

;;; ========================================================================
(subsection "Production System Variables")

(defvar *rules* nil
  "List of active rules to check.")

(defvar *satisfied-rules* nil
  "List of rule that are satisfied and will be fired when rule checking
   is complete.")

;;; ========================================================================
(subsection "Variables Linking to Essential Scone Elements")

;;; These variables hold a few special KB elements referred to by
;;; the engine code.  These are created in the BOOTSTRAP file and the
;;; elements, once created, are saved into these variables.

;;; We can't just refer to these elements by their curly-brace names
;;; because the curly braces may confuse the compiler.

;;; Nodes of general importance.

(defvar *thing* nil)
(defvar *undefined-thing*)
(defvar *universal* nil)
(defvar *general* nil)
(defvar *set* nil)
(defvar *empty-set* nil)
(defvar *non-empty-set* nil)
(defvar *cardinality* nil)
(defvar *zero* nil)
(defvar *one* nil)
(defvar *two* nil)
(defvar *event* nil)
(defvar *action* nil)
(defvar *potential-agent* nil)
(defvar *action-agent* nil)
(defvar *action-object* nil)
(defvar *action-recipient* nil)

;;; Parent nodes for various built-in Scone element types.

(defvar *number* nil)
(defvar *integer* nil)
(defvar *ratio* nil)
(defvar *float* nil)
(defvar *string* nil)
(defvar *function* nil)
(defvar *struct* nil)
(defvar *relation* nil)
(defvar *link* nil)
(defvar *is-a-link* nil) 
(defvar *is-not-a-link* nil)
(defvar *eq-link* nil)
(defvar *not-eq-link* nil)
(defvar *cancel-link* nil)
(defvar *has-link* nil)
(defvar *has-no-link* nil)
(defvar *split* nil)
(defvar *complete-split* nil)

(defvar *statement-link* nil)
(defvar *not-statement-link* nil)

;;; ========================================================================
(section "Variables and Macros Related to Marker Bits")

;;; These macros are hacked for maximum spped in Common Lisp.  These
;;; have lots of declarations to ensure that array access and other
;;; ops will be coded inline.

;;; The Scone system supports a fixed number of markers, designated by
;;; an integers greater than or equal to zero and less than N-MARKERS.

;;; ---------------------------------------------------------------------
(subsubsection "Define the Range of Legal Markers")

;;; Sense how many bits we have to work with in a fixnum in the Lisp
;;; system we are using.  Use that as the maximum number of marker
;;; bits.  This varies from Lisp to Lisp, and of course will be large
;;; in a 64-bit Common Lisp.

(eval-when (:execute :load-toplevel :compile-toplevel)

  (defconstant max-marker-bits (- (logcount most-positive-fixnum) 1)
    "This is the maximum number of marker bits the Lisp implementation
   can support.")

  (defconstant desired-marker-bits 24
    "This is the maximum number of marker bits we want, if the Lisp
   implementation allows this many. The space allocated for each Scone
   element grows by one pointer-word for each marker, so you may want
   to reduce this if the core image is too large.")


  (defconstant n-markers
    (min max-marker-bits
	 desired-marker-bits)
    "The maximum number of marker bits allowed in this Scone engine.
     This number gets compiled in, so it cannot be changed
     dynamically.")

  (defconstant n-marker-pairs (floor n-markers 2)
    "The maximum number of marker pairs.")

  (defconstant all-markers-mask (- (expt 2 n-markers) 1)
    "Bit mask that is 1 for each legal marker.  Must be a fixnum.")

  (declaim (type fixnum max-marker-bits desired-marker-bits n-markers
		 n-marker-pairs all-markers-mask))

  )  ;; End EVAL-WHEN.

(defmacro legal-marker? (m)
  `(typep ,m '(integer 0 (,n-markers))))

;;; Marker to marker-bit conversion is a simple table lookup.

(eval-when (:execute :load-toplevel :compile-toplevel)

  (defvar *marker-bits*
    (make-array n-markers))

  (proclaim `(type (simple-vector ,n-markers)
		   *marker-bits*))

  (dotimes (i n-markers)
    (setf (aref *marker-bits* i)
	  (ash 1 i)))

  )  ;; End EVAL-WHEN.

(defmacro marker-bit (m)
  "Get the marker bit associated with marker M."
  `(the fixnum (aref *marker-bits* ,m)))

(defmacro check-legal-marker (m)
  "In interactive routines, check if M is a legal marker.  If not, signal
   an error."
  `(unless (legal-marker? ,m)
     (error "~S is not a legal marker." ,m)))

;;; ---------------------------------------------------------------------
(subsubsection "Marker Pairs")

;;; Markers are allocated in pairs.  For each marker we allocate, there is
;;; a corresponding cancel-marker.  So we divide the space of markers in
;;; half, rounding down.  Indices in the lower half designate markers (or
;;; marker pairs) available to the user, while indices in the upper half
;;; designate the corresponding cancel-markers.

(defmacro get-cancel-marker (m)
  "Get the cancel-marker corresponding to marker M."
  `(the fixnum (+ (the fixnum ,m)
		  (the fixnum n-marker-pairs))))

(defmacro legal-marker-pair? (m)
  `(typep ,m '(integer 0 (,n-marker-pairs))))

(defmacro check-legal-marker-pair (m)
  "In interactive routines, check if M is a legal user marker.  If not,
   signal an error."
  `(unless (legal-marker-pair? ,m)
     (error "~S is not a legal user marker." ,m)))

;;; ---------------------------------------------------------------------
(subsubsection "Marker Predicates")

;; These are fast internal macros that do no checking. Not intended for
;; use at user-level.

(defmacro fast-marker-on? (e m)
  "Check whether element E has marker M turned on."
  `(/= 0 (the fixnum (logand (the fixnum (bits ,e))
			     (the fixnum (marker-bit ,m))))))

(defmacro fast-marker-off? (e m)
  "Check whether element E has marker M turned off."
  `(= 0 (the fixnum (logand (the fixnum (bits ,e))
			    (the fixnum (marker-bit ,m))))))

(defmacro all-bits-on? (bits mask)
  "Predicate tests whether all the one-bits in MASK are on in BITS.
   Assume that both BITS and MASK are positive fixnums with fewer than
   N-MARKERS potentially on."
  `(= 0 (the fixnum
	  (logand (the fixnum ,mask)
		  (the (unsigned-byte ,n-markers)
		    (logxor (the fixnum ,bits)
			    (the fixnum all-markers-mask)))))))

(defmacro any-bits-on? (bits mask)
  "Predicate tests whether any of the bits in MASK are on in fixnum BITS."
  `(/= 0 (the fixnum (logand (the fixnum ,bits)
			     (the fixnum ,mask)))))

(defmacro all-bits-off? (bits mask)
  "Predicate tests whether all the bits in MASK are off in fixnum BITS."
  `(= 0 (the fixnum (logand (the fixnum ,bits)
			    (the fixnum ,mask)))))

;;; ---------------------------------------------------------------------
(subsubsection "Marker Allocation")

;;; NOTE: Don't just grab a random marker and try to use it.  Call
;;; GET-MARKER and use whatever marker it gives you.  Keep it forever
;;; or use FREE-MARKER to return it when you're done.  Better yet, use
;;; WITH-MARKERS.  If you use random markers without telling the
;;; allocation machinery, you'll confuse things and may end up with
;;; some very subtle bugs.

;;; An array with a boolean value for each general marker pair.  T means
;;; the marker is available for allocation.  NIL means the marker is in
;;; use.
(defvar *available-markers*
  (make-array n-marker-pairs :initial-element t))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (proclaim `(type (simple-array t (,n-marker-pairs))
		   *available-markers*)))

(defvar *n-available-markers* n-marker-pairs
  "The number of markers currently available for allocation.")
(declaim (type fixnum *n-available-markers*))

(defvar *locked-markers*
  (make-array n-marker-pairs :initial-element nil)
  "A vector with an entry for each marker.  If the entry is T, this
   marker is locked and cannot be released by FREE-MARKER.")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (proclaim `(type (simple-array t (,n-marker-pairs))
		   *locked-markers*)))

(defun lock-marker (m)
  "A locked marker will not be released by FREE-MARKER."
  (setf (aref *locked-markers* m) t))

(defun unlock-marker (m)
  "Release the lock on marker M."
  (setf (aref *locked-markers* m) nil))

(defun locked-marker? (m)
  "Predicate to determine if marker M is locked."
  (svref *locked-markers* m))

(defmacro available-marker? (m)
  "Predicate to check wether marker M is currently available."
  `(and (legal-marker-pair? ,m)
    (aref *available-markers* ,m)))

(defun get-marker ()
  "Allocate an available marker, returning the index.  If no available
   markers remain, return NIL."
  (when (> *n-available-markers* 0)
    ;; Scan all the markers to find an available one.
    (dotimes (i n-marker-pairs)
      (when (available-marker? i)
	;; Marker I is the winner.
	(setf (svref *available-markers* i) nil)
	(decf *n-available-markers*)
	(return-from get-marker i)))))

(defun free-marker (m)
  "Return Marker M to the pool of available markers and clear the marker.
   If the marker is already available or is locked, do nothing."
  (check-legal-marker-pair m)
  (unless (or (available-marker? m) (locked-marker? m))
    (clear-marker-pair m)
    (setf (aref *available-markers* m) t)
    (incf *n-available-markers*)))

(defun free-markers ()
  "Return all markers to the free pool and clear them."
  (dotimes (i n-marker-pairs)
    (free-marker i)))

(defmacro with-markers ((&rest marker-vars)
			body-form
			&optional
			(fail-form
			 '(commentary "Insufficient markers available.")))
  "MARKER-VARS is a list of variables that are locally bound, each getting
   the index of a freshly allocated marker.  We then execute the the BODY-FORM,
   returning whatever value or values the last form returns.  But
   before leaving this form, we de-allocate and clear the markers.
   If there are not enough markers available to fill all the MARKER-VARS,
   we don't allocate any new markers.  Instead we executre the FAIL-FORM
   and return what it returns."
  (let ((n-markers-needed (length marker-vars))
	(bind-list nil)
	(free-list)
	(declare-list))
    ;; Prepare a let-list, a list of free-marker forms, and a
    ;; declare list.
    (dolist (mv marker-vars)
      (push `(,mv (get-marker)) bind-list)
      (push `(free-marker ,mv) free-list)
      (push mv declare-list))
    ;; Allocate the markers in the expected order.
    (setq bind-list (nreverse bind-list))
    (setq declare-list (nreverse declare-list))
    ;; Now construct the macro-expansion.  If we have enough markers,
    ;; bind each variable to a freshly-allocated marker, then execute
    ;; the body-form. Free the markers on the way out, even if the
    ;; user throws.
    `(if (>= *n-available-markers* ,n-markers-needed)
      (let ,bind-list
	(declare (fixnum ,@declare-list))
	(unwind-protect
	     ,body-form
	  (progn ,@free-list)))
      ,fail-form)))

;;; ---------------------------------------------------------------------
(subsubsection "Context and Activation Markers")

;;; The variable *CONTEXT* holds the currently-active context node.  The
;;; *CONTEXT-MARKER* is placed on this node and upscanned whenever we change
;;; *CONTEXT*.

;;; The activation marker is placed on all nodes and links in the active
;;; context by following chains of context-wires.  This use of an activation
;;; marker makes IN-CONTEXT slower, but makes the various scans faster.

;;; *CONTEXT-MARKER* is initially set to -1 to indicate that no marker has
;;; yet been assigned.

(defvar *context* nil
  "The element representing the current context.")

;;; *CONTEXT-MARKER* marks the current context and all its superiors.

(defvar *context-marker* -1
  "Marker permanently assigned to mark the current *CONTEXT* node and
   its superiors.")
(declaim (type fixnum *context-marker*))

(defvar *context-mask* 0
  "Bit mask for *CONTEXT-MARKER*.")
(declaim (type fixnum *context-mask*))

(defvar *context-cancel-marker* 0
  "Cancellation marker corresponding to *CONTEXT-MARKER*.")
(declaim (type fixnum *context-cancel-marker*))

(defvar *context-cancel-mask* 0
  "Bit mask for *CONTEXT-CANCEL-MARKER*.")
(declaim (type fixnum *context-cancel-mask*))

;;; ACTIVATION-MARKER marks the set of elements in the current context.

(defvar *activation-marker* 0
  "Marker permanently assigned to mark every element active or
   present in the current context.")
(declaim (type fixnum *activation-marker*))

(defvar *activation-mask* 0
  "Bit mask for *ACTIVATION-MARKER*.")
(declaim (type fixnum *activation-mask*))

;;; NOTE: I'm not sure if we need a cancel-marker for activation, or
;;; if we can always use *context-cancel-marker*.  Create it just in
;;; case we need it, since this marker would be wasted anyway.

(defvar *activation-cancel-marker* 0
  "Cancellation marker corresponding to *ACTIVATION-MARKER*.")
(declaim (type fixnum *activation-cancel-marker*))

(defvar *activation-cancel-mask* 0
  "Bit mask for *CONTEXT-CANCEL-MARKER*.")
(declaim (type fixnum *activation-cancel-mask*))

(defun context-marker-setup ()
  "Set up the context and activation markers.  These normally are
   kept constant for the lifetime of a Scone process."
  (if (eql *context-marker* -1)
      (progn
	(setq *context-marker* (get-marker))
	(setq *activation-marker* (get-marker))
	(commentary
	 "Assigning ~D as context marker, ~D as activation marker."
	 *context-marker*
	 *activation-marker*)
	(setq *context-mask* (marker-bit *context-marker*))
	(setq *activation-mask* (marker-bit *activation-marker*))
	(setq *context-cancel-marker* (get-cancel-marker *context-marker*))
	(setq *activation-cancel-marker* (get-cancel-marker *activation-marker*))
	(setq *context-cancel-mask* (marker-bit *context-cancel-marker*))
	(setq *activation-cancel-mask* (marker-bit *activation-cancel-marker*))
	(lock-marker *context-marker*)
	(lock-marker *activation-marker*))
      (commentary "Ignoring attempt to re-initialize context marker.")))

;;; ---------------------------------------------------------------------
(subsubsection "Marker Implementation")

;;; For each marker, there is a doubly-linked list running through all the
;;; elements.

;;; NOTE: This is a tradeoff of space for time.  Many scans want to do some
;;; operation for "all elements with marker N".  We want to avoid scanning
;;; ALL elements, so we need a list for each marker.  But we also want to
;;; avoid allocating list cells that we will have to GC later.  This means
;;; that every element has space pre-allocated for all these chains.  Store
;;; the start and end of the chain here, along with the count of marked
;;; elements.

(defvar *first-marked-element*
  (make-array n-markers :initial-element nil)
  "A vector with an entry for each marker.  This is the first element in
   the chain of elements having that mark.")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (proclaim `(type (simple-vector ,n-markers) *first-marked-element*)))

(defvar *last-marked-element*
  (make-array n-markers :initial-element nil)
  "A vector with an entry for each marker.  This is the last element in the
   chain of elements having that mark.")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (proclaim `(type (simple-vector ,n-markers) *last-marked-element*)))

(defvar *n-marked-elements*
  (make-array n-markers :element-type 'fixnum :initial-element 0))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (proclaim `(type (simple-array fixnum (,n-markers)) *n-marked-elements*)))

(defmacro fast-marker-count (m)
  "Return the number of elements marked with marker M."
  `(aref (the (simple-array fixnum (,n-markers)) 
	      *n-marked-elements*)
	 ,m))

(defmacro inc-marker-count (m)
  "Increment the number of elements marked with marker M."
  `(incf (the fixnum (aref (the (simple-array fixnum (,n-markers)) 
				*n-marked-elements*)
			   ,m))))

(defmacro dec-marker-count (m)
  "Decrement the number of elements marked with marker M."
  `(decf (the fixnum (aref (the (simple-array fixnum (,n-markers)) 
				*n-marked-elements*)
			   ,m))))

(defmacro zero-marker-count (m)
  "Set to zero the number of elements marked with marker M."
  `(setf (aref (the (simple-array fixnum (,n-markers)) 
		    *n-marked-elements*)
	       ,m)
	 0))

(defmacro do-marked ((var
		      m
		      &optional
		      after)
		     &body body)
  "This macro iterates over the set of elements with marker M.  Each
   element in turn is bound to VAR and then the body is executed, returning
   NIL.  If AFTER is present and non-nil, it should evaluate into an
   element marked with M.  Only iterate over elements after this one in the
   chain of M-marked elements.  NOTE: It is OK to mark new elements with M
   in the body of this loop, but don't unmark any elements."
  `(do ((,var
	 ,(if after
	      `(if ,after
		   (svref (next-marked-element ,after) ,m)
		   (svref *first-marked-element* ,m))
	      `(svref *first-marked-element* ,m))
	 (svref (next-marked-element ,var) ,m)))
       ((null ,var))
     ,@body))

;;; ========================================================================
(subsection "Definitions Related to Flag Bits")

;;; A flag is like a permanent marker-bit on an element.  Each
;;; flag-bit is assigned to some system-defined purpose -- these are
;;; not meant to be user-defined.

;;; There is a word full of flag-bits in each element, but we do not
;;; maintain a linked list of elements with a given flag.  This is a
;;; difference between markers and flags.

;;; One important use of flag bits is to record the type of each
;;; element in a way that can quickly be tested by boolean-mask
;;; operations.

;;; IMPLEMENTATION NOTE: The current version of Scone was developed
;;; for a 32-bit implementation of Common Lisp, so flags and markers
;;; are kept in two separate words at the start of each element.  Now
;;; that we have 64-bit machines running 64-bit Common Lisp
;;; impelementnations we can speed up the system by keeping all these
;;; bits in a single marker/flag word, and doing Boolean checks over
;;; both at the same time.  That's on the to-do list, but it's tricky
;;; to build this in such a way that the system runs either on 32-bit
;;; or 64-bit implementations.  So we may either burn our bridges and
;;; go to 64-bit always, or maintain two distinct Scone versions.

(defmacro new-flags (start &rest names)
  "Allocate a number of flags in order, starting with START.  We cannot
   have more flags than MAX-MARKER-BITS."
  (declare (list names))
  (let ((list nil))
    (do ((n start (1+ n))
	 (l names (cdr l)))
	((null l)
	 (cons 'progn (nreverse list)))
      (declare (fixnum n))
      (when (> n max-marker-bits)
	(error "Cannot define more than ~D flags in this Lisp."
	       n-markers))
      (push `(defconstant ,(car l)
	      (ash 1 (the (unsigned-byte ,n-markers) ,n)))
	    list))))

;;; Simple flag macros and functions.

(defmacro fast-flag-on? (e f)
  "Check whether element E has flag F turned on."
  `(/= 0 (logand (flags ,e) ,f)))

(defmacro fast-flag-off? (e f)
  "Check whether element E has flag F turned off."
  `(= 0 (logand (flags ,e) ,f)))

;;; Define flags for the basic element types and modifiers.

;;; NOTE: The worst-case Lisp that we know about and care about
;;; allows 22 flag bits, so try to stay under that limit.

(new-flags
 0
 
 ;; Basic families.
 node-flag	        ; An indv or type node.
 link-flag		; Any kind of link.
 split-flag		; A split-element, maybe complete.
 relation-flag		; A relation-element.

;; Basic modifiers.
 kill-flag)              ; Renders the element inoperative.
 
;; Node flags
(new-flags
 5
 indv-flag		; Any kind of individual node.
 type-flag		; A type-node.
 map-flag 	        ; A MAP-node, may be either INDV or TYPE.

 role-flag              ; Node was defined as a role node.
 primitive-flag		; An indv representing a Lisp object.
 proper-flag		; A proper individual, not generic.
 defined-flag)		; This node has a definition.
 
 ;; Subtypes and modfiers of a LINK or RELATION.
(new-flags
 5
 is-a-flag		; An IS-A link.
 eq-flag		; An EQ link.
 has-flag               ; A HAS-link.
 cancel-flag		; A CANCEL link.
 statement-flag		; A link instantiating a user-defined relation.

 not-flag		; Set on a link to indicate negation.
 true-flag		; Set on a link tht is not negated.
 up-flag		; Any link that markers cross A to B in an upscan.
 h-flag			; A relation or statement.
 mod-flag               ; On an IS-A link, creates a derived
			; IS-A link.
 
 transitive-flag	; A transitive relation.
 symmetric-flag)	; A smmetric relation.
 
 ;; Subtypes and modifiers of a SPLIT.
(new-flags
 5
 complete-split-flag)	; A complete split.


;;; The UP-MASK designates any link that propagates markers A-to-B in an UPSCAN.
(defconstant up-mask (logior link-flag true-flag up-flag))

;;; The CANCEL-UP-MASK designates any link that propagates cancel markers
;;; A-to-B in an UPSCAN.
(defconstant cancel-up-mask (logior link-flag not-flag up-flag))

;;; The H-MASK designates a STATEMENT or RELATION -- basically, any link
;;; type that propagates markers from the A, B, or C node to the
;;; corresponding node of the parent -- an H-shaped inheritance pattern.
(defconstant h-mask h-flag)


;;; ***************************************************************************
(section "Scone Elements")
;;; ***************************************************************************

;;; An element is the basic unit of knowledge in the Scone system.  An
;;; element is either a NODE, representing some individual, type, or
;;; entity, a LINK, representing the statement of a relation between two or
;;; more entities.  But a link has a built-in "handle node" representing
;;; the statement itself, and a node may have some "wires" that represent
;;; certain special relations with other nodes.  So every element has both
;;; a set of outgoing wires (PARENT, CONTEXT, A, B, and C) plus connection
;;; points for any number of incoming wires.

;;; NOTE: Speed in the inner loops is very important.  The natural thing,
;;; would be to implement ELEMENT as a CLOS class and all of the various
;;; element types as sub-classes.  In principle, the CLOS machinery in
;;; Common Lisp should be able to implement that very efficiently.
;;; However, CLOS implementations vary a lot and are inefficient in some
;;; Common Lisp implementations.  So for now we simply use use the same
;;; structure (a Common Lisp DEFSTRUCT) for every element, and use the flag
;;; bits to distinguish the element's type.

;;; NOTE: We could reorganize this to use separate subtypes of element
;;; for nodes and links, saving some space for the simpler node
;;; elements.  But the savings is small and this might actually slow
;;; down some of the scans.  Sometime I should run an experiment to
;;; explore the time/space tradeoff here, but a single element type
;;; will do for now.

;;; ========================================================================
(subsection "Definition of the SCONE Element Structure")

;;; This is the heart of the monster.

(defstruct (element
	    (:constructor internal-make-element)
	    (:conc-name nil)
	    (:print-function print-element))
  ;; Each element has an integer whose bits indicate the current markers on
  ;; that element, and another integer holding the flag bits.
  (bits 0 :type fixnum)
  (flags 0 :type fixnum)
  ;; Outging connections (known as "wires").
  (parent-wire nil)
  (context-wire nil)
  (a-wire nil)
  (b-wire nil)
  (c-wire nil)
  (split-wires nil :type list)
  ;; Markers can move across wires in either direction, so we need to keep
  ;; back-pointers.  These are the wires of each type that are attached to
  ;; this element.
  (incoming-parent-wires nil :type list)
  (incoming-context-wires nil :type list)
  (incoming-a-wires nil :type list)
  (incoming-b-wires nil :type list)
  (incoming-c-wires nil :type list)
  (incoming-split-wires nil :type list)
  ;; There is a chain of pointers running through all the elements so that
  ;; you can scan them all.
  (next-kb-element nil)
  ;; A vector with an entry for each marker. The marker's entry in this
  ;; vector points to the next marked element in the chain.
  (next-marked-element (make-array n-markers :initial-element nil))
  ;; A vector with an entry for each marker.  This points to previous
  ;; marked element in the chain.
  (prev-marked-element (make-array n-markers :initial-element nil))
  ;; If this element is a defined type, the definition lives here.
  ;; The definition is either a list or a predicate function or two
  ;; args: the candidate member and a marker that has already been
  ;; upscanned from that member.
  (definition nil)
  ;; If we create a new inferior of this class, check each of the elements
  ;; on this list to see if the new inferior fits its definition. 
  (defined-subtypes nil)
  ;; The INTERNAL-NAME and NAMESPACE together form an unambiguous,
  ;; permanent, and globally unique identifier for a SCONE element.
  (internal-name nil)
  (namespace nil)
  ;; A property list for various propeties and annotations that some
  ;; elements have, but not all, and that are not critical to system
  ;; performance.
  (properties nil))

;;; This is the element-iname structure.  It is described in the
;;; "INTERNAL NAMES AND NAMESPACES" section, but we put the definition
;;; here for proper compilation.

(defstruct (element-iname
	     (:print-function print-element-iname))
  "Each curly-brace element reference encountered by the reader turns
   into one of these structures before being converted into an actual
   element."
  ;; Type of element to create. Defaults to :ELEMENT, meaning a
  ;; typical Scone element.  Could also be :STRING, :NUMBER, etc.
  (type :element)
  ;; A string that is the name of the element, or the value for a
  ;; primitive element -- one that represents a Lisp object like a
  ;; number or string.
  (value))

;;; This fixes up DESCRIBE so that you can say (describe {Clyde}) instead
;;; of (describe (lookup-element {Clyde})).

(defmethod describe-object ((obj element-iname) stream)
  (declare (stream stream))
  (let ((x (lookup-element obj)))
    (if x
	(describe x stream)
	(format stream "~S is an element-iname structure.~%" obj))))

(defun make-element (flags iname parent context a b c 
			   english english-default hashtable definition
			   properties)
  "Common code creates and returns an element, after doing all the
   necessary bookkeeping.  This is called by the element-creator for
   each specific Scone element-type.  A unique INAME must be supplied.
   If HASHTABLE is supplied, register the name in this hashtable
   instead of the default namespace.  The ENGLISH and ENGLISH-DEFAULT
   arguments control english names for the new element."
  ;; Prevent duplicate or ill-formed inames.  Skip this check for primitive
  ;; elements with their own hashtable.
  (unless hashtable
    ;; Allow lazy users to supply a string as the iname instead of an
    ;; element-name in curly braces.  But it still must be unique in
    ;; this namespace.
    (when (typep iname 'string)
      (setq iname (make-element-iname :value iname)))
    (if (typep iname 'element-iname)
	(when (lookup-element iname)
	  (error "The iname ~S is already in use." iname))
	(error
	 "~S must be an element-iname structure or a string."
	 iname)))
  ;; Create and set up the element.
  (let ((e (internal-make-element :flags flags)))
    ;; Set and register the iname.
    (cond (hashtable
	   ;; If :HASHTABLE was supplied, this type of element has its own
	   ;; hastable, not a true namespace.
	   (setf (gethash iname hashtable) e)
	   (setf (internal-name e) iname)
	   (setf (namespace e) hashtable))
	  (t
	   ;; Register in specified namespace or default to current one.
	   (register-internal-name iname e)))
    ;; Tie new element into the chain of all elements.
    (if *last-element*
	(setf (next-kb-element *last-element*) e))
    (setq *last-element* e)      
    (if (null *first-element*)
	(setq *first-element* e))
    ;; Tick the element counter.
    (incf (the fixnum *n-elements*))
    ;; Process all the outoging wire connections.
    (when parent (connect-wire :parent e parent t))
    (when context (connect-wire :context e context t))
    (when a (connect-wire :a e a t))
    (when b (connect-wire :b e b t))
    (when c (connect-wire :c e c t))
    ;; Process English names, if any.
    (unless (listp english)
      (setq english (list english)))
    (cond ((or (null english-default)
	       (eq english-default :no-iname))
	   ;; No default, just pass the :ENGLISH arg, if any, to ENGLISH.
	   (when english
	     (english-internal e english)))
	  ((and english (member (car english) *legal-syntax-tags* :test #'eq))
	   (english-internal e (cons (car english)
				     (cons :iname
					   (cdr english)))))
	  (t (english-internal e (cons english-default
				       (cons :iname english)))))
    ;; If there is a definition, record it and make sure the defined-flag
    ;; is set.
    (when definition
      (set-definition e definition))
    ;;; If there is a :PLIST argument, that becomes the property list.
    (when properties
      (set-properties e properties))
    e))

;;; ========================================================================
(subsection "Low-Level Element Operations")

(defun set-flag (e flag)
  "Set the specified FLAG (a bit mask) in element E."
  (declare (fixnum flag))
  (prog1
      (setf (flags e)
	    (logior (flags e) flag))))

(defun clear-flag (e flag)
  "Clear the specified FLAG (a bit mask) in element E."
  (declare (fixnum flag))
  (prog1
      (setf (flags e)
	    (logand (flags e) (lognot flag)))))

(defun set-definition (e definition)
  "Add DEFINITION to element E, setting the defined-flag of E."
  (setq e (lookup-element-test e))
  (setf (definition e) definition)
  (set-flag e defined-flag))

(defun set-properties (e plist)
  "Set the property list of element E to PLIST."
  (setq e (lookup-element-test e))
  (setf (properties e) plist))

(defmacro do-elements ((var &optional (after nil)) &body body)
  "This macro iterates over the set of all elements.  Each element in turn
   is bound to VAR and then the body is executed.  Otherwise return NIL.
   If AFTER is supplied, it is an element.  Execute the body only for
   elements created after AFTER."
  `(do ((,var
	 ,(if after
	      `(if ,after
		   (next-kb-element ,after)
		   *first-element*)
	      `*first-element*)
	 (next-kb-element ,var)))
       ((null ,var))
     ,@body))

(defun next-element (e)
  "Given an element, get the next one in the chain."
  (setq e (lookup-element-test e))
  (next-kb-element e))

(defun previous-element (e)
  "Given an element, get the previous element in the chain.  NOTE: this is
   inefficient, since we don't keep back-pointers in this chain.  Used in
   infrequent tasks such as removing an element from the KB."
  (setq e (lookup-element-test e))
  (do ((x *first-element* y)
       (y nil))
      ((null x) nil)
    (setq y (next-kb-element x))
    (when (eq y e) (return x))))

;;; These are user-level functions for accessing the various other
;;; elements connected to element E.  They check to make sure that E
;;; is indeed an element.  Using low-level access functions such as
;;; (a-wire e) is faster, but can lead to fatal and mysterious errors
;;; if E is not an element.

(defun a-element (e)
  "Given an element E, return the element connected to the A-wire of
   E, or NIL if that wire is not connected."
  (setq e (lookup-element-test e))
  (a-wire e))

(defun b-element (e)
  "Given an element E, return the element connected to the A-wire of
   E, or NIL if that wire is not connected."
  (setq e (lookup-element-test e))
  (b-wire e))

(defun c-element (e)
  "Given an element E, return the element connected to the A-wire of
   E, or NIL if that wire is not connected."
  (setq e (lookup-element-test e))
  (c-wire e))

(defun parent-element (e)
  "Given an element E, return the element connected to the A-wire of
   E, or NIL if that wire is not connected."
  (setq e (lookup-element-test e))
  (parent-wire e))

(defun context-element (e)
  "Given an element E, return the element connected to the A-wire of
   E, or NIL if that wire is not connected."
  (setq e (lookup-element-test e))
  (context-wire e))

(defun split-elements (e)
  "Given an element E, return the list of elements connected to
   the split wires of E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (split-wires e))

(defun incoming-a-elements (e)
  "Given an element E, return a list of all elements whose
   a-wires are connected to E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (incoming-a-wires e))

(defun incoming-b-elements (e)
  "Given an element E, return a list of all elements whose
   b-wires are connected to E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (incoming-b-wires e))

(defun incoming-c-elements (e)
  "Given an element E, return a list of all elements whose
   c-wires are connected to E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (incoming-c-wires e))

(defun incoming-parent-elements (e)
  "Given an element E, return a list of all elements whose
   parent-wires are connected to E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (incoming-parent-wires e))

(defun incoming-context-elements (e)
  "Given an element E, return a list of all elements whose
   context-wires are connected to E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (incoming-context-wires e))

(defun incoming-split-elements (e)
  "Given an element E, return a list of all elements whose
   split-wires are connected to E, or NIL if there are none."
  (setq e (lookup-element-test e))
  (incoming-split-wires e))


;;; ========================================================================
(subsection "Element Properties")

;;; NOTE: Use a property rather than a permanently allocated element slot
;;; if the property is not present in all elements and if the property is
;;; not used in operations that must run at maximum speed.

(defmacro get-element-property (e property)
  "Get the specified PROPERTY of element E, or NIL if this property is
   not present."
  `(getf (properties ,e) ,property))

(defun set-element-property (e property &optional (value t))
  "Set the specified PROPERTY of element E to the designated VALUE,
   which defaults to T."
  (prog1
      (setf (getf (properties e) property) value)))

(defun clear-element-property (e property)
  "Remove the specified PROPERTY of element E.  If E doesn't have this property,
   do nothing."
  (remf (properties e) property))

(defun push-element-property (e property value)
  "The PROPERTY of E should be a list.  Push VALUE onto this list.  Create
   the property if it doesn't already exist."
  (push value (getf (properties e) property)))

;;; ***************************************************************************
(section "Accessing, Connecting, and Disconnecting Wires")
;;; ***************************************************************************

(defun wire (wire-name e)
  "Find the element attached to the wire specified by WHICH-WIRE, one
   of :A, :B, :C, :PARENT, :CONTEXT, or :SPLIT.  For :SPLIT wires,
   return a list."
  (setq e (lookup-element-test e))
  (ecase wire-name
    (:a (a-wire e))
    (:b (b-wire e))
    (:c (c-wire e))
    (:parent (parent-wire e))
    (:context (context-wire e))
    (:split (split-wires e))))

(defun incoming-wires (wire-name e)
  "Find the list of incoming elements via the specified wire, one of :A,
   :B, :C, :PARENT, :CONTEXT, or :SPLIT."
  (setq e (lookup-element-test e))
  (ecase wire-name
    (:a (incoming-a-wires e))
    (:b (incoming-b-wires e))
    (:c (incoming-c-wires e))
    (:parent (incoming-parent-wires e))
    (:context (incoming-context-wires e))
    (:split (incoming-split-wires e))))

(defun connect-wire (wire-name from to &optional (may-defer nil))
  "Connect the wire specified by WIRE-NAME (:A, :PARENT, etc.) of
   element FROM to element TO.  If FROM is already connected
   somewhere, disconnect it first.  If connecting a context-wire to an
   active TO node, activate the FROM node as well."
  (setq from (lookup-element-test from))
  (setq to (lookup-element-or-defer to))
  ;; If the element is not yet defined, but *CREATE-UNDEFINED-ELEMENTS*
  ;; is T, create the element on the fly as a type-node of type
  ;; {undefined thing}.
  (when (and *create-undefined-elements*
	     (not *defer-unknown-connections*)
	     (typep to '(or element-iname string)))
    (when *comment-on-element-creation*
      (commentary
       "~S is unknown.  Defining it as a type under {undefined thing}."
       to))
    (setq to (new-type to *undefined-thing*)))
  (cond ((typep to 'element)
	 ;; At this point, TO is an element, so build the connection.
	 (when (and (wire wire-name from)
		    (not (eq wire-name :split)))
	   (disconnect-wire wire-name from))
	 (ecase wire-name
	   (:a (setf (a-wire from) to)
	       (push from (incoming-a-wires to)))
	   (:b (setf (b-wire from) to)
	       (push from (incoming-b-wires to)))
	   (:c (setf (c-wire from) to)
	       (push from (incoming-c-wires to)))
	   (:parent (setf (parent-wire from) to)
		    (push from (incoming-parent-wires to)))
	   (:context (setf (context-wire from) to)
		     (push from (incoming-context-wires to))
		     ;; Activate FROM element if appropriate.
		     (when (fast-marker-on? to *activation-marker*)
		       (fast-mark from *activation-marker*)))
	   (:split (push to (split-wires from))
		   (push from (incoming-split-wires to)))))
	;; The TO argument is not an existing connection.  Defer it if allowed
	;; to, or signal an error.
	((and may-defer (typep to '(or element-iname string)))
	 (defer-connection wire-name from to))
	(t (error "~S is not an element." to))))

(defun disconnect-wire (wire-name from)
  "Disconnect the wire specified by WIRE-NAME (:A, :PARENT, etc.)  of
   element FROM from wherever it is currently connected to.  If it is
   not connected, do nothing."
  (setq from (lookup-element-test from))
  (let ((to (wire wire-name from)))
    (when to
      (ecase wire-name
	(:a (setf (a-wire from) nil)
	    (setf (incoming-a-wires to)
		  (delete from (incoming-a-wires to))))
	(:b (setf (b-wire from) nil)
	    (setf (incoming-b-wires to)
		  (delete from (incoming-b-wires to))))
	(:c (setf (c-wire from) nil)
	    (setf (incoming-c-wires to)
		  (delete from (incoming-c-wires to))))
	(:parent (setf (parent-wire from) nil)
		 (setf (incoming-parent-wires to)
		       (delete from (incoming-parent-wires to))))
	(:context (setf (context-wire from) nil)
		  (setf (incoming-context-wires to)
			(delete from (incoming-context-wires to))))))))

;;; We need a separate function for disconnecting one split wire, since we
;;; need to specify the TO argument.

(defun disconnect-split-wire (from to)
  "Disconnect the SPLIT-wire between element FROM and element TO.  If they
   are not connected, do nothing."
  (declare (type element from to))
  (setq from (lookup-element-test from))
  (setq to (lookup-element-test to))
  (setf (split-wires from)
	(delete to (split-wires from)))
  (setf (incoming-split-wires to)
	(delete from (incoming-split-wires to))))

(defun disconnect-split-wires (from)
  "Disconnect all the SPLIT-wires between element FROM and other elements."
  (setq from (lookup-element-test from))
  (dolist (to (split-wires from))
    (disconnect-split-wire from to)))

(defun convert-parent-wire-to-link (e &key (context *context*))
  "The parent-wire of element E is disconnected and replaced by an
   equivalent is-a link, which can then be cancelled or modified in some
   situations.  If the user does not supply a :context for the is-a link,
   use the current *context*."
  (setq e (lookup-element-test e))
  (let* ((old-parent (parent-wire e)))
    (when old-parent
      (disconnect-wire :parent e)
      (new-is-a e old-parent :context context))))


;;; ***************************************************************************
(section "Definition of the SCONE Element Types")
;;; ***************************************************************************

;;; Some predicates for basic types.  The macro versions are meant to
;;; be fast and internal, so they assume you've already got an
;;; element-object as the argument.  Each has a user-level version
;;; that is slower but safer.

(defmacro fast-node? (e)
  `(fast-flag-on? ,e node-flag))

(defun node? (e)
  "Predicate to determine whether E is a Scone node."
  (and (setq e (lookup-element e))
       (fast-node? e)))

(defmacro fast-link? (e)
  `(fast-flag-on? ,e link-flag))

(defun link? (e)
  "Predicate to determine whether E is a Scone link."
  (and (setq e (lookup-element e))
       (fast-link? e)))

(defmacro fast-h-link? (e)
  "An H-link is a relation or statement."
  `(fast-flag-on? ,e h-flag))

(defmacro fast-defined-type? (e)
  "Predicate to detect if the defined-flag is on."
  `(fast-flag-on? ,e defined-flag))

(defun defined-type? (e)
  "Predicate to determine whether E is a defined Scone element."
  (and (setq e (lookup-element e))
       (fast-defined-type? e)))

(defmacro fast-killed? (e)
  "Predicate to test the KILL-FLAG, which renders the element
   inoperative."
  `(fast-flag-on? ,e kill-flag))

(defun killed? (e)
  "Predicate to determine whether E is a killed (inoperative) Scone
   element."
  (and (setq e (lookup-element e))
       (fast-killed? e)))

;;; ========================================================================
(subsection "Indv Nodes")

;;; An INDV-NODE represents an individual entity.

(defconstant indv-mask (logior node-flag indv-flag))

(defmacro fast-indv-node? (e)
  `(all-bits-on? (flags ,e) indv-mask))

(defun indv-node? (e)
  "Predicate to determine whether E is an INDV node."
  (setq e (lookup-element e))
  (and e (fast-indv-node? e)))

;;; A PROPER-INDV-NODE represents a specific first-class entity such as
;;; "Clyde" or "Afghanistan".

(defconstant proper-indv-mask (logior indv-mask proper-flag))

(defmacro fast-proper-indv-node? (e)
  `(all-bits-on? (flags ,e) proper-indv-mask))

(defun proper-indv-node? (e)
  "Predicate to determine whether E is a Proper INDV node."
  (setq e (lookup-element e))
  (and e (fast-proper-indv-node? e)))

;;; A GENERIC-INDV-NODE is an INDV-NODE that does not have the PROPER-FLAG
;;; on.  A generic node is usually defined.

(defmacro fast-generic-indv-node? (e)
  `(let ((f (flags ,e)))
     (and (all-bits-on? f indv-mask)
	  (all-bits-off? f proper-flag))))

(defun generic-indv-node? (e)
  "Predicate to determine whether E is a Generic INDV node."
  (setq e (lookup-element e))
  (and e (fast-generic-indv-node? e)))

;;; An individual or type node may have a definition: "the man who shot
;;; Kennedy" or "three-legged blue elephants".  For these, we put some form
;;; into the element's DEFINITION slot and we set the DEFINED-FLAG.

(defconstant defined-indv-mask (logior indv-mask defined-flag))

(defmacro fast-defined-indv-node? (e)
  `(all-bits-on? (flags ,e) defined-indv-mask))

(defun defined-indv-node? (e)
  "Predicate to determine whether E is a Defined INDV node."
  (setq e (lookup-element e))
  (and e (fast-defined-indv-node? e)))

(defun new-indv (iname parent
		       &key
		       (context *context*)
		       (proper t)
		       may-have
		       definition
		       english
		       english-default)
  "Make and return a new INDV-NODE with the specified internal name
   and PARENT.  If :PROPER is supplied and NIL, this is a generic
   node, otherwise assume it is a proper individual.  If :DEFINITION
   is non-null, set the defined flag and stick the value in the
   element's definition field.  Also add a HAS-LINK from the :CONTEXT
   to the new node, unless :MAY-HAVE is T.  If the caller specifies no
   :ENGLISH argument, assume :INAME."
  (setq parent (lookup-element-or-defer parent))
  ;; If there's no iname supplied, make one up based on the parent.
  (unless iname
    ;; Made-up inames don't become English names.
    (push :no-iname english)    
    (setq iname
	  (if (typep parent 'element)
	      (gen-iname (internal-name parent))
	      (gen-iname "Indv"))))
  ;; If there already is an element with name INAME whose parent is
  ;; {undefined thing}, we just replace the parent with the parent
  ;; specified here.  If the parent is anything else, this redefinition
  ;; is an error, which will be caught in MAKE-ELEMENT.
  (let ((this-element (lookup-element iname)))
    (when (and this-element
	       (eq (parent-wire this-element) *undefined-thing*)
	       (not (incompatible? this-element parent)))
      ;; The element exists, but its parent is {undefined thing}. 
      (disconnect-wire :parent this-element)
      (connect-wire :parent this-element parent)
      (convert-type-to-indv this-element)
      (return-from new-indv this-element)))
  ;; This is the normal case of an iname we haven't used before.
  ;; A defined class is never proper.
  (let* ((e (make-element (if (and proper (not definition))
			      proper-indv-mask
			      indv-mask)
			  iname parent context nil nil nil
			  english (or english-default :noun) nil
			  definition nil)))
    (when (and context (not may-have))
      (new-has context e :n *one*))
    ;; See whether E is a member of any defined classes.
    (check-defined-type-memberships e)
    e))

;;; NEW-CONTEXT is just a convenience function.  It creates an INDV-NODE,
;;; not a special type.

(defun new-context (iname parents)
  "A convenience function for creating a new context node as a
   sub-context of one or more pre-existing context-nodes.  PARENTS may
   either be a single parent node or a list of parents."
  (let ((first-parent nil)
	(others nil)
	(new-context nil))
    (cond ((listp parents)
	   (setq first-parent (lookup-element-test (car parents)))
	   (setq others (mapcar #'lookup-element-test (cdr parents))))
	  (t (setq first-parent (lookup-element-test parents))))
    ;; Create the new context node.  Its context is NIL.
    (setq new-context
	  (new-indv iname first-parent :context nil))
    (dolist (p others)
      (new-is-a new-context p))
    new-context))

;;; ========================================================================
(subsection "Primitive Nodes")

;;; A PRIMITIVE-NODE is a sub-type of INDV-NODE that represents a Lisp
;;; object of some kind: number, string, function, etc.  The lisp object
;;; itself is stored as the element's internal name.

(defconstant primitive-mask (logior node-flag indv-flag
				 primitive-flag proper-flag))

(defmacro fast-primitive-node? (e)
  `(all-bits-on? (flags ,e) primitive-mask))

(defun primitive-node? (e)
  "Predicate to determine whether E is a primitive Scone node -- that is,
   a node representing a specific Lisp object."
  (setq e (lookup-element e))
  (and e (fast-primitive-node? e)))

(defun new-primitive-node (value parent hashtable)
  "Common code to make a primitive node of some type."
  ;;; If the number element already exists.  Just return it.
  (or (gethash value hashtable)
      ;;; Make and return a new number element.
      (let ((e (make-element
		primitive-mask
		value parent *universal* nil nil nil nil nil
		hashtable nil nil)))
	;; See whether E is a member of any defined classes.
	(check-defined-type-memberships e)
	;; Return the new element.
	e)))

(defun node-value (e)
  "A primitive node in Scone is one representing a Lisp object,
   such as an integer, string, or structure.  Given an element E,
   if E is a primitive element, extract the Lisp object it
   represents, which is stored in the iname.  If it is not
   a primitive object, just return NIL."
  (and (primitive-node? e)
       (iname e)))

(defmacro fast-number-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'number)))

(defun number-node? (e)
  "Predicate to determine whether E is a node representing a Lisp
   number."
  (setq e (lookup-element e))
  (and e (fast-number-node? e)))

(defmacro fast-integer-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'integer)))

(defun integer-node? (e)
  "Predicate to determine whether E is a node representing a Lisp
   integer."
  (setq e (lookup-element e))
  (and e (fast-integer-node? e)))

(defmacro fast-ratio-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'ratio)))

(defun ratio-node? (e)
  "Predicate to determine whether E is node representing a Lisp
   ratio."
  (setq e (lookup-element e))
  (and e (fast-ratio-node? e)))

(defmacro fast-float-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'float)))

(defun float-node? (e)
  "Predicate to determine whether E is a node representing a Lisp
   floating-point number."
  (setq e (lookup-element e))
  (and e (fast-float-node? e)))

(defun new-number (value &key (parent nil))
  "Represents any type of number present in Common Lisp: integer, float,
   ratio, etc.  If a number node with the specified VALUE already exists,
   return it.  Else, create and return the new function node.  Optionally
   provide a :PARENT element.  If no parent is provided, an appropriate one
   will be chosen."
  ;; Accept a number in curly braces as well as a raw number.
  (when (typep value 'element-iname)
    (setq value (element-iname-value value)))
  (check-type value number)
  (unless parent
    (setq parent 
	  (typecase value
	    (integer *integer*)
	    (float *float*)
	    (ratio *ratio*)
	    (t *number*))))
  (new-primitive-node value parent *number-hashtable*))

(defun new-integer (value &key (parent *integer*))
  "VALUE must be a Common Lisp integer.  Create and return a Scone element 
   representing that ratio."
  ;; Accept a number in curly braces as well as a raw number.
  (when (typep value 'element-iname)
    (setq value (element-iname-value value)))
  (check-type value integer)
  (new-primitive-node value parent *number-hashtable*))

(defun new-ratio (value &key (parent *ratio*))
  "VALUE must be a Common Lisp ratio.  Create and return a Scone element 
   representing that ratio."
  ;; Accept a number in curly braces as well as a raw number.
  (when (typep value 'element-iname)
    (setq value (element-iname-value value)))
  (check-type value ratio)
  (new-primitive-node value parent *number-hashtable*))

(defun new-float (value &key (parent *float*))
  "VALUE must be a Common Lisp floating-point number.  Create and return a
   Scone element representing that number."
  ;; Accept a number in curly braces as well as a raw number.
  (when (typep value 'element-iname)
    (setq value (element-iname-value value)))
  (check-type value float)
  (new-primitive-node value parent *number-hashtable*))

(defmacro fast-string-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'string)))

(defun string-node? (e)
  "Predicate to determine whether E is a node representing a Lisp
   string."
  (setq e (lookup-element e))
  (and e (fast-string-node? e)))

(defun new-string (value
		     &key
		     (parent *string*))
  "Represents a string in Common Lisp.  If a string node with the specified
   VALUE already exists, return it.  Else, create and return the new number
   node.  Optionally provide :PARENT element."
  ;; Accept a string (within double quotes) in curly braces as well as
  ;; a raw string.
  (when (and (typep value 'element-iname)
	     (eq (element-iname-type value) :string))
    (setq value (element-iname-value value)))
  (check-type value string)
  (new-primitive-node value parent *string-hashtable*))

(defmacro fast-function-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'function)))

(defun function-node? (e)
  "Predicate to determine whether E is a node representing a Lisp
   function."
  (setq e (lookup-element e))
  (and e (fast-function-node? e)))

(defun new-function (value
		     &key
		     (parent *function*))
  "Represents a function in Common Lisp.  If a function node with the
   specified VALUE already exists, return it.  Else, create and return the
   new function node.  Optionally provide :PARENT element."
  ;; Accept a function in curly braces as well as a raw number.
  (when (typep value 'element-iname)
    (setq value (element-iname-value value)))
  (check-type value function)
  (new-primitive-node value parent *function-hashtable*))

(defmacro fn-call (fn &rest r)
  `(let ((fn1 ,fn))
     (if (function-node? fn1)
	 (funcall (internal-name fn1) ,@r)
	 (funcall fn1 ,@r))))

(defmacro fast-struct-node? (e)
  `(and (fast-primitive-node? ,e)
	(typep (internal-name ,e) 'structure-object)))

(defun struct-node? (e)
  "Predicate to determine whether E is a node representing a Lisp
   structure."
  (setq e (lookup-element e))
  (and e (fast-struct-node? e)))

(defun new-struct (value
		     &key
		     (parent *struct*))
  "Represents a DEFSTRUCT object in Common Lisp.  If a struct node with the
   specified VALUE already exists, return it.  Else, create and return the
   new struct node.  Optionally provide :PARENT element."
  (check-type value structure-object)
  (new-primitive-node value parent *struct-hashtable*))


;;; ========================================================================
(subsection "Type Nodes")

;;; A TYPE-NODE represents the typical member of some class, such as
;;; ELEPHANT.  Each type node is in principle paired with a set-node
;;; representing the set of these elements.  However, we only create the
;;; set-node when we have something to say about it.  Until then, it is
;;; only virtually present.

;;; The type node and its set node point to one another via the :SET-NODE
;;; and :TYPE-NODE properties of the element, since these conncetions are
;;; relatively rare and are not followed during time-critical inner loops.

(defconstant type-mask (logior node-flag type-flag))

(defmacro fast-type-node? (e)
  `(all-bits-on? (flags ,e) type-mask))

(defun type-node? (e)
  "Predicate to determine whether E is a TYPE node."
  (setq e (lookup-element e))
  (and e (fast-type-node? e)))

(defun new-type (iname parent
		       &key
		       (context *context*)
		       definition
		       may-have
		       n
		       english
		       english-default)
  "Make and return a new TYPE-NODE with the specified internal name.
   If :DEFINITION is non-null, set the defined flag and stick the
   value in the element's definition field.  Also connect the
   parent-wire to the specified PARENT.  Create a {HAS} statement from
   the :CONTEXT to the new type-node unless :MAY-HAVE is T.  If the :N
   argument is supplied, this says that there are N members of this
   class in the specified context.  If the caller specifies no
   :ENGLISH argument, assume :INAME."
  (setq parent (lookup-element-or-defer parent))
  ;; If there's no iname supplied, make one up based on the parent.
  (unless iname
    (push :no-iname english)
    (setq iname
	  (if (typep parent 'element)
	      (gen-iname (internal-name parent))
	      (gen-iname "Type"))))
  ;; If there already is an element with name INAME whose parent is
  ;; {undefined thing}, we just replace the parent with the parent
  ;; specified here.  If the parent is anything else, this redefinition
  ;; is an error, which will be caught in MAKE-ELEMENT.
  (let ((this-element (lookup-element iname)))
    (when (and this-element
	       (eq (parent-wire this-element) *undefined-thing*)
	       (not (incompatible? this-element parent)))
      ;; The element exists, but its parent is {undefined thing}. 
      (disconnect-wire :parent this-element)
      (connect-wire :parent this-element parent)
      (return-from new-type this-element)))
  ;; This is the normal case of an iname we haven't used before.
  (let* ((e (make-element type-mask iname parent context nil nil nil
			  english (or english-default :noun) nil
			  definition nil)))
    (when (and context (not may-have))
      (new-has context e :n n))
    ;; See whether E is a member of any defined classes.
    (check-defined-type-memberships e)
    ;; Return the new element.
    e))

;;; Function to convert a type-node to an indv-node.
(defun convert-type-to-indv (e)
  "If element E is a type-node, convert it to an indv-node."
  (setq e (lookup-element e))
  (when (and e (type-node? e))
    (clear-flag e type-flag)
    (set-flag e indv-flag)
    e))

;;; Set-Node machinery

;;; A type node may have have an associated set-node, which represents the
;;; set itself, rather than the typical member of the set.

(defun get-set-node (e &optional (parent *set*))
  "Return the set-node associated with element E, creating it as an indv-node
   if necessary.  The parent of this node is assumed to be {set}, but a different
   parent can be supplied as an optional second arguement."
  (setq e (lookup-element-test e))
  (unless (fast-type-node? e) (error "~S is not a type-node." e))
  (let ((sn (get-element-property e :set-node)))
    (or sn
	(let* ((iname
		(gen-iname (format nil "SET-NODE ~A"
				   (internal-name e)))))
	  (setq sn (new-indv iname parent))
	  (set-element-property e :set-node sn)
	  (set-element-property sn :type-node e)
	  sn))))

(defun get-type-node (e)
  "Given a node E, find the type-node for which E is the set-node.
   If E is not a set-node, return NIL."
  (setq e (lookup-element-test e))
  (get-element-property e :type-node))

(defun remove-set-node (e)
  "Given an element E, sever the connection to its set-node if it has one.
   Else, do nothing and return NIL."
  (let ((sn (get-element-property e :set-node)))
    (when sn
      (clear-element-property e :set-node)
      (clear-element-property sn :type-node)
      sn)))

;;; ========================================================================
(subsection "Map Nodes")


;;; A MAP-NODE, by definition, represents the ROLE (parent wire) of
;;; OWNER (context wire).  Of course, you can only create this if
;;; OWNER inherits that ROLE.  Often you then EQ this mapped role node
;;; to some existing proper node.  It doesn't make sense to cancel or modify this
;;; MAP-NODE -- instead you sould cancel the HAS-LINK that states that it exists.

;;; The map of a type-role node is a type-map; the map of an indv-role node is an
;;; indv-map.

(defconstant map-mask (logior node-flag map-flag))
(defconstant indv-map-mask (logior map-mask indv-flag))
(defconstant type-map-mask (logior map-mask type-flag))

(defmacro fast-map-node? (e) `(all-bits-on? (flags ,e) map-mask))

(defun map-node? (e)
  "Predicate to determine whether E is a MAP node."
  (setq e (lookup-element e))
  (and e (fast-map-node? e)))

(defmacro fast-indv-map-node? (e)
  `(all-bits-on? (flags ,e) indv-map-mask))

(defun indv-map-node? (e)
  "Predicate to determine whether E is an INDV-MAP node."
  (setq e (lookup-element e))
  (and e (fast-indv-map-node? e)))

(defmacro fast-type-map-node? (e)
  `(all-bits-on? (flags ,e) type-map-mask))

(defun type-map-node? (e)
  "Predicate to determine whether E is a TYPE-MAP node."
  (setq e (lookup-element e))
  (and e (fast-type-map-node? e)))

(to-do
 "Add a check to see if OWNER has a ROLE.
  In X-IS-THE-Y-OF-Z, check for x and z being in defined type.")

(defun new-map (role
		owner
		&key
		iname
		english
		no-supplement)
  "Make and return a new Map node representing the ROLE of the OWNER
   argument.  The role and owner must already exist -- no deferrals
   allowed here.  Optionally provide an :INAME and :ENGLISH name.  If
   no :INAME is supplied, we try to make up a suitable one.  Unless
   the caller specifies :NO-SUPPLEMENT, we add derived links as
   needed."
  (setq role (lookup-element-test role))
  (setq owner (lookup-element-test owner))
  ;; If no iname supplied, try to make one up.
  (let ((string1 (internal-name role))
	(string2 (internal-name owner)))
    (when (and (null iname) string1 string2)
      (setq iname (gen-iname
		   (format nil "~A of ~A" string1 string2)))))
  ;; Now create and return the map-node.
  (let ((new-map-node
	 (make-element (if (fast-type-node? role) type-map-mask indv-map-mask)
		       iname role owner nil nil nil
		       english :noun nil nil nil)))
    ;; Create derived links if necessary.
    (unless no-supplement
      (add-derived-links-map new-map-node))
    new-map-node))
		 
;;; ========================================================================
(subsection "Is-A Links")

;;; IS-A-LINK Indicates that A "is a" B.
;;; A can be any node.  B generally will be a type node.

(defconstant is-a-mask
  (logior link-flag true-flag is-a-flag up-flag))

(defconstant is-not-a-mask
  (logior link-flag not-flag is-a-flag up-flag))

(defmacro fast-is-a-link? (e)
  `(all-bits-on? (flags ,e) is-a-mask))

(defun is-a-link? (e)
  "Predicate to determine whether E is an IS-A link."
  (setq e (lookup-element e))
  (and e (fast-is-a-link? e)))

;;; A DERIVED-IS-A-LINK is added to the network to represent the
;;; explicit IS-A relation created by certain MAP configurations.

(defconstant derived-is-a-mask
  (logior is-a-mask mod-flag))

(defmacro fast-derived-is-a-link? (e)
  `(all-bits-on? (flags ,e) derived-is-a-mask))

(defun derived-is-a-link? (e)
  "Predicate to determine whether E is an IS-NOT-A link."
  (setq e (lookup-element e))
  (and e (fast-derived-is-a-link? e)))

(defun new-is-a (a b
		   &key
		   negate
		   dummy
		   derived
		   iname
		   (parent *is-a-link*)
		   (context *context*)
		   english
		   no-supplement)
  "Make and return a new IS-A-LINK with the specified elements on the A and
   B wires.  Optionally provide :PARENT and :CONTEXT."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  ;; Check if A is already a B or cannot be a B. TODO: Remove when merging with actual
  ;; redundant link code.
  (unless (or *no-kb-error-checking* negate dummy)
    (case (is-x-a-y? a b)
      (:yes
       (when *comment-on-redundant-links*
         (commentary "Not adding redundant is-a between ~S and ~S." a b))
       (return-from new-is-a nil))
      (:no (error "~S cannot be a ~S." a b))))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A is-a ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Is-A"))))
  (let ((flags (if negate is-not-a-mask is-a-mask))
	(link nil))
    (declare (fixnum flags))
    (when dummy (setq flags (logior flags kill-flag)))
    (when derived (setq flags (logior flags mod-flag)))
    (setq link
	  (make-element flags
			iname parent context a b
			nil english nil nil nil nil))
    ;; Create derived links if necessary.
    (unless no-supplement
      (add-derived-links-is-a link))
    ;; If A is now a member of some defined class, create an IS-A link
    ;; to that class.
    (check-defined-type-memberships a)
    ;; Check if any rules are now satisfied.
    (check-rule-x-is-a-y a b)
    link))

;;; An IS-NOT-A-LINK Indicates an exception: A is NOT a B, and any
;;; assertion that it is a B should be questioned.

(defmacro fast-is-not-a-link? (e)
  `(all-bits-on? (flags ,e) is-not-a-mask))

(defun is-not-a-link? (e)
  "Predicate to determine whether E is an IS-NOT-A link."
  (setq e (lookup-element e))
  (and e (fast-is-not-a-link? e)))

(defun new-is-not-a (a b
		       &key
		       iname
		       dummy
		       (parent *is-not-a-link*)
		       (context *context*)
		       english)
  "Make and return a new IS-NOT-A-LINK with the specified elements on the A
   and B wires.  Optionally provide :PARENT and :CONTEXT.  This is
   equivalent to NEW-IS-A with the :NEGATE flag."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A is-not-a ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Is-Not-A"))))
  (new-is-a a b
	    :negate t
	    :dummy dummy
	    :iname iname
	    :parent parent
	    :context context
	    :english english))

;;; ========================================================================
(subsection "EQ Links")

;;; EQ-LINK Indicates that elements A and B represent the same entity.

(defconstant eq-mask
  (logior link-flag true-flag eq-flag up-flag))

(defconstant not-eq-mask
  (logior link-flag not-flag eq-flag up-flag))

(defmacro fast-eq-link? (e)
  `(all-bits-on? (flags ,e) eq-mask))

(defun eq-link? (e)
  "Predicate to determine whether E is an EQ link."
  (setq e (lookup-element e))
  (and e (fast-eq-link? e)))

(defun new-eq (a b
		 &key
		 negate
		 dummy
		 iname
		 (parent *eq-link*)
		 (context *context*)
		 english
		 no-supplement)
  "Make and return a new EQ-LINK with the specified elements on the A and B
   wires.  Optionally provide :PARENT and :CONTEXT."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  ;; Check if A is already eq to B or cannot be eq to B. TODO: Remove when merging with actual
  ;; redundant link code.
  (unless (or *no-kb-error-checking* negate dummy)
    (case (is-x-eq-y? a b)
      (:yes
       (when *comment-on-redundant-links*
         (commentary "Not adding redundant eq between ~S and ~S." a b))
       (return-from new-eq nil))
      (:no (error "~S cannot be eq to ~S." a b))))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A eq ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Eq"))))
  (let ((flags (if negate not-eq-mask eq-mask)))
    (declare (fixnum flags))
    (when dummy (setq flags (logior flags kill-flag)))
    (let ((link (make-element flags
                              iname parent context a b nil
                              english nil nil nil nil)))
      ;; Create derived links if necessary.
      (unless no-supplement
        (add-derived-links-eq link))
      ;; See whether A is now a member of a defined class.
      (check-defined-type-memberships a)
      ;; It's not necessary to check B unless :negate is on.
      (when negate (check-defined-type-memberships a))
      ;; Check if any rules are now satisfied.
      (check-rule-x-is-a-y a b)
      link)))


;;; NOT-EQ-LINK Indicates that elements A and B do NOT represent the same
;;; entity.

(defmacro fast-not-eq-link? (e)
  `(all-bits-on? (flags ,e) not-eq-mask))

(defun not-eq-link? (e)
  "Predicate to determine whether E is a NOT-EQ link."
  (setq e (lookup-element e))
  (and e (fast-not-eq-link? e)))

(defun new-not-eq (a b
		     &key
		     iname
		     dummy
		     (parent *not-eq-link*)
		     (context *context*)
		     english)
  "Make and return a new NOT-EQ-LINK with the specified elements on the A
   and B wires.  Optionally provide :CONTEXT."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A not eq ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Not-Eq"))))
  (new-eq a b
	  :negate t
	  :dummy dummy
	  :iname iname
	  :parent parent
	  :context context
	  :english english))

;;; ========================================================================
(subsection "Has Links")

;;; A HAS-LINK says that every instance of A has a B.  The C wire is the
;;; number of Bs that A has.

(defconstant has-mask
  (logior link-flag true-flag has-flag))

(defconstant has-no-mask
  (logior link-flag not-flag has-flag))

(defmacro fast-has-link? (e)
  `(all-bits-on? (flags ,e) has-mask))

(defun has-link? (e)
  "Predicate to determine whether E is a HAS link."
  (setq e (lookup-element e))
  (and e (fast-has-link? e)))

(defun new-has (a b
		  &key
		  n
		  negate
		  dummy
		  iname
		  (parent *has-link*)
		  (context *context*)
		  english)
  "Make and return a new HAS-LINK with the specified elements on the A and
   B wires.  Optionally provide :PARENT and :CONTEXT.  If :N is specified,
   that is the cardinality, stored ont eh C-wire."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  (when n (setq n (lookup-element-or-defer n)))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A has ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Has"))))
  (let ((flags (if negate has-no-mask has-mask)))
    (declare (fixnum flags))
    (when dummy (setq flags (logior flags kill-flag)))
    (make-element flags
		  iname parent context a b n english nil nil nil nil)))

;;; A HAS-NO-LINK indicates that A has no B.

(defmacro fast-has-no-link? (e)
  `(all-bits-on? (flags ,e) has-no-mask))

(defun has-no-link? (e)
  "Predicate to determine whether E is a HAS-NO link."
  (setq e (lookup-element e))
  (and e (fast-has-no-link? e)))

(defun new-has-no (a b
		     &key
		     iname
		     dummy
		     (parent *has-no-link*)
		     (context *context*)
		     english)
  "Make and return a new HAS-NO-LINK with the specified elements on the A
   and B wires.  Optionally provide :PARENT and :CONTEXT.  This is
   equivalent to NEW-HAS with the :NEGATE flag."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A has-no ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Has-No"))))
  (new-has a b
	   :negate t
	   :dummy dummy
	   :iname iname
	   :parent parent
	   :context context
	   :english english))


;;; ========================================================================
(subsection "Cancel Links")

;;; A CANCEL-LINK says that in description A, statement B (which would
;;; otherwise be true) is inoperative.  Do not use this to alter the
;;; membership of A in some class, for example by cancelling an IS-A or EQ
;;; link.  Use an IS-NOT-A link for that.

(defconstant cancel-mask (logior link-flag not-flag cancel-flag up-flag))

(defmacro fast-cancel-link? (e)
  `(all-bits-on? (flags ,e) cancel-mask))

(defun cancel-link? (e)
  "Predicate to determine whether E is a CANCEL link."
  (setq e (lookup-element e))
  (and e (fast-cancel-link? e)))

(defun new-cancel (a b
		     &key
		     iname
		     dummy
		     (parent *cancel-link*)
		     (context *context*)
		     english)
  "Make and return a new CANCEL-LINK with the specified elements on the A
   and B wires.  Optionally provide :CONTEXT element."
  (setq a (lookup-element-or-defer a))
  (setq b (lookup-element-or-defer b))
  ;; Try to create a human-readable iname.
  (unless iname
    (setq iname
	  (if (and *generate-long-element-names*
		   (typep a 'element)
		   (typep b 'element))
	      (gen-iname (format nil "~A cancels ~A"
				 (internal-name a)
				 (internal-name b)))
	      (gen-iname "Cancel"))))
  (let ((flags cancel-mask))
    (declare (fixnum flags))
    (when dummy (setq flags (logior flags kill-flag)))
    (make-element flags
		  iname parent context a b nil
		  english nil nil nil nil)))

;;; ========================================================================
(subsection "Splits")

;;; A SPLIT is like a link or statement, with a context and a handle node.
;;; But instead of A and B wires it has any number of split-wires.  The
;;; meaning is that the elements (usually type nodes) connected by this
;;; split are disjoint and non-overlapping.

;;; So, for example, "bird", "reptile", and "mammal" form a split.  The
;;; engine automatically and efficiently detects any attempt to violate a
;;; split.  If you create an individual that belongs to more than one of
;;; these classes, the engine will detect this and complain, unless that
;;; individual cancels the split.

(defconstant split-mask split-flag)

(defmacro fast-split? (e)
  `(all-bits-on? (flags ,e) split-mask))

(defun split? (e)
  "Predicate to determine whether E is a SPLIT element."
  (setq e (lookup-element e))
  (and e (fast-split? e)))

(defun new-split (members
		  &key
		  iname
		  dummy
		  (parent *split*)
		  (context *context*)
		  english)
  "Make and return a new SPLIT-NODE with the specified members.
   Optionally provide :INAME and :CONTEXT."
  ;; Test whether MEMBERS already have elements in common, violating
  ;; the proposed split.  If so, signal an error."
  (let ((violations (list-split-violations members)))
    (when violations
      (error "Proposed new split already has overlapping members: ~S~%"
	     violations)))
  ;; Try to make a meaningful iname, mentioning the first two members
  ;; in the split.
  (unless iname
    (if *generate-long-element-names*
	(let* ((m1 (car members))
	       (m2 (cadr members))
	       (s1 nil)
	       (s2 nil))
	  (cond ((typep m1 'element)
		 (setq s1 (internal-name m1)))
		((typep m1 'element-iname)
		 (setq s1 (element-iname-value m1))))
	  (cond ((null m1))
		((typep m2 'element)
		 (setq s2 (internal-name m2)))
		((typep m2 'element-iname)
		 (setq s2 (element-iname-value m2))))
	  (setq iname
		(if m2
		    (gen-iname (format nil "split ~A ~A" s1 s2))
		    (gen-iname "Split"))))
	(setq iname (gen-iname "Split"))))
  (let ((s (make-element
	    (if dummy
		(logior split-mask kill-flag)
		split-mask)
	    iname parent context
	    nil nil nil english nil nil nil nil)))
    (dolist (m members)
      (setq m (lookup-element-test m))
      (connect-wire :split s m))
    s))

(defun add-to-split (split new-item)
  "Add NEW-ITEM to the set of items declared disjoint by SPLIT."
  (setq split (lookup-element-test split))
  (unless (split? split)
    (error "~S is not a SPLIT." split))
  (setq new-item (lookup-element-test new-item))
  (connect-wire :split split new-item)
  split)

;;; A COMPLETE-SPLIT is a subtype of split.  We note the superclass of the
;;; split as a property in the COMPLETE-SPLIT element.  The COMPLETE-SPLIT
;;; completely partitiions this type.
(defconstant complete-split-mask (logior split-flag complete-split-flag))

(defmacro fast-complete-split? (e)
  `(all-bits-on? (flags ,e) complete-split-mask))

(defun complete-split? (e)
  "Predicate to determine whether E is a COMPLETE-SPLIT element."
  (setq e (lookup-element e))
  (and e (fast-complete-split? e)))

(defun new-complete-split (supertype members
				     &key
				     iname
				     dummy
				     (parent *complete-split*)
				     (context *context*)
				     english)
  "Make and return a new COMPLETE-SPLIT-NODE with the specified members,
   all under the specified SUPERTYPE.  Optionally provide :INAME and
   :CONTEXT."
  (setq supertype (lookup-element-or-defer supertype))
  ;; Test whether MEMBERS already have elements in common, violating
  ;; the proposed split.  If so, signal an error."
  (let ((violations (list-split-violations members)))
    (when violations
      (error "Proposed new complete split already has overlapping members: ~S~%"
	     violations)))
  ;; Try to make a meaningful iname, mentioning the first two members
  ;; in the split.
  (unless iname
    (if *generate-long-element-names*
	(let* ((m1 (car members))
	       (m2 (cadr members))
	       (s1 nil)
	       (s2 nil))
	  (cond ((typep m1 'element)
		 (setq s1 (internal-name m1)))
		((typep m1 'element-iname)
		 (setq s1 (element-iname-value m1))))
	  (cond ((null m1))
		((typep m2 'element)
		 (setq s2 (internal-name m2)))
		((typep m2 'element-iname)
		 (setq s2 (element-iname-value m2))))
	  (setq iname
		(if m2
		    (gen-iname (format nil "csplit ~A ~A" s1 s2))
		    (gen-iname "C-Split"))))
	(setq iname (gen-iname "C-Split"))))
  (let ((s (make-element
	    (if dummy
		(logior complete-split-mask kill-flag)
		complete-split-mask)
	    iname parent context
	    nil nil nil english nil nil nil nil)))
    (set-element-property s :split-supertype supertype)
    (push-element-property supertype :complete-split-subtypes s)
    (dolist (m members)
      (setq m (lookup-element-test m))
      (connect-wire :split s m))
    s))




  ;;; ========================================================================
(subsection "Relations")

;;; A RELATION element creates a new relation that we can instantiate with
;;; a STATEMENT link.

;;; The RELATION element is similar in structure to a link, since it has A,
;;; B, and C wires, but in function it is more like a type-node.  That is,
;;; the RELATION element itself does not make any statement -- it just
;;; serves to define what its instances (statements) mean.  A RELATION may
;;; be a sub-type (specialization) of other relations.

;;; At the end of the A, B, and C wires are defined generic nodes that
;;; serve as prototypes whose properties are inherited by the A, B, and C
;;; elements of instances (statements) under this relation.  The A node
;;; corresponds (more or less) to the domain of traditional binary
;;; relations, while B corresponds to the range.  The optional C node is a
;;; third element so that we can represent trinary relations such as "the
;;; distance from A to B is C".

(defconstant relation-mask (logior relation-flag h-flag))

(defmacro fast-relation? (e)
  `(all-bits-on? (flags ,e) relation-mask))

(defun relation? (e)
  "Predicate to determine whether E is a RELATION element."
  (setq e (lookup-element e))
  (and e (fast-relation? e)))

(defun new-relation (iname &key
			   a
			   a-inst-of
			   a-type-of
			   b
			   b-inst-of
			   b-type-of
			   c
			   c-inst-of
			   c-type-of
			   (parent *relation*)
			   symmetric
			   transitive
			   english
			   inverse)
  "Define a new RELATION element that can be instantiated by a
   STATEMENT link.  The INAME is the name of the relation from A to B.
   This relation may have another relation as a parent, or a
   type-node.  Normally the caller supplies elements as :A, :B, and :C
   arguments.  However, for the common case where you want to create a
   new generic node with a specified parent, you can instead supply
   :A-INST-OF or :A-TYPE-OF, and similarly for B and C.  Optionally
   provide a :PARENT element for the relation itself.  Optionally set
   the :SYMMETRIC and :TRANSITIVE properties of the relation.  These
   refer to the relation from A to B.  The :ENGLISH argument, as
   usual, is the name of the relation (or set of names) in the forward
   direction.  The :INVERSE argument is similar in form, but in the
   reverse direction."
  (unless (and iname
	       (typep iname 'element-iname)
	       (null (lookup-element iname)))
    (error "Must supply a unique iname."))
  (let ((iname-string (element-iname-value iname))
	(flags relation-mask))
    (declare (fixnum flags))
    ;; Set up additional flags.
    (when symmetric
      (setq flags (logior flags symmetric-flag)))
    (when transitive
      (setq flags (logior flags transitive-flag)))
    ;; Create the A, B, and C nodes if necessary
    (cond (a (setq a (lookup-element-test a)))
	  (a-inst-of 
	   (setq a (new-indv
		    (gen-iname
		     (format nil "A-role of ~A" iname-string))
		    a-inst-of
		    :proper nil :english :no-iname)))
	  (a-type-of
	   (setq a (new-type
		    (gen-iname
		     (format nil "A-role of ~A" iname-string))
		    a-type-of :english :no-iname))))
    (cond (b (setq b (lookup-element-test b)))
	  (b-inst-of 
	   (setq b (new-indv
		    (gen-iname
		     (format nil "B-role of ~A" iname-string))
		    b-inst-of
		    :proper nil :english :no-iname)))
	  (b-type-of
	   (setq b (new-type
		    (gen-iname
		     (format nil "B-role of ~A" iname-string))
		    b-type-of :english :no-iname))))
    (cond (c (setq c (lookup-element-test c)))
	  (c-inst-of 
	   (setq c (new-indv
		    (gen-iname
		     (format nil "C-role of ~A" iname-string))
		    c-inst-of
		    :proper nil :english :no-iname)))
	  (c-type-of
	   (setq c (new-type
		    (gen-iname
		     (format nil "C-role of ~A" iname-string))
		    c-type-of :english :no-iname))))
    ;; Now create the relation-element itself.
    (make-element flags
		  iname parent *universal* a b c
		  (merge-english-inverse english inverse :inverse-relation)
		  :relation nil nil nil)))

(defmacro fast-symmetric? (e) 
  `(fast-flag-on? ,e symmetric-flag))

(defun symmetric? (e)
  "Predicate to determine whether E is a SYMMETRIC relation or
   statement."
  (setq e (lookup-element e))
  (and e (fast-symmetric? e)))

(defmacro fast-transitive? (e) 
  `(fast-flag-on? ,e transitive-flag))

(defun transitive? (e)
  "Predicate to determine whether E is a TRANSITIVE realtion or
   statement."
  (setq e (lookup-element e))
  (and e (fast-transitive? e)))


;;; ========================================================================
(subsection "Statements")

;;; A STATEMENT represents an instance of a relation defined by the
;;; relation that is its parent.

(defconstant statement-mask
  (logior link-flag true-flag statement-flag h-flag))

(defconstant not-statement-mask
  (logior link-flag not-flag statement-flag))

(defmacro fast-statement? (e)
  `(all-bits-on? (flags ,e) statement-mask))

(defun statement? (e)
  "Predicate to determine whether E is a STATEMENT link."
  (setq e (lookup-element e))
  (and e (fast-statement? e)))

(defun new-statement (a rel b
			&key
			c
			(context *context*)
			negate
			dummy
			iname
			english)
  "State that relation REL holds between A and B in CONTEXT.  REL must
   be a relation or statement node.  Optionally connect a C node.  If
   :NEGATE is present, this is the negation of the statement that
   would otherwise be created.  If :A, :B, or :C is :CREATE, create a
   new node representing that role."
  ;; TODO: Remove when merging with actual redundant link code.
  (when (statement-true? a rel b)
    (when *comment-on-redundant-links*
      (commentary "Not creating redundant statement ~S ~S ~S" a rel b))
    (return-from new-statement nil))
  ;; The relation must already be present.  No deferrals.
  (multiple-value-bind (element tag)
      (lookup-element rel)
    (setq rel element)
    ;; If the REL argument is an english name with an
    ;; :INVERSE-RELATION tag, use the relation element but flip the
    ;; arguments around.
    (when (eq tag :inverse-relation)
      (rotatef a b)))
  (unless (and (typep rel 'element)
	       (or (fast-relation? rel) (fast-statement? rel)))
    (error "~S not an existing relation or statement." rel))
  (let* ((ax (lookup-element-test a))
	 (bx (lookup-element-test b))
	 (cx (and c (lookup-element-test c)))
	 (rel-a (a-wire rel))
	 (rel-b (b-wire rel))
	 (rel-c (c-wire rel)))
    ;; Check the types of the A, B, and C arguments if they are
    ;; present.  Do not perform the check if this statement is
    ;; negated.
    (unless *no-kb-error-checking*
      (when (and ax
		 rel-a
		 (not negate)
		 (incompatible? ax rel-a))
	(error "~S cannot be the A-element of a ~S relation."
	       ax rel))
      (when (and bx
		 rel-b
		 (not negate)
		 (incompatible? bx rel-b))
	(error "~S cannot be the B-element of a ~S relation."
	       bx rel))
      (when (and cx
		 rel-c
		 (not negate)
		 (incompatible? cx rel-c))
	(error "~S cannot be the C-element of a ~S relation."
	       cx rel)))
    ;; Try to create a human-readable iname
    (unless iname
      (setq iname
	    (cond 
	      ;; If we are missing either AX or BX, just mention
	      ;; REL in the iname.
	      ((not (and *generate-long-element-names*
			 (typep ax 'element)
			 (typep bx 'element)))
	       (gen-iname (internal-name rel)))
	      ;; If we have AX, BX, and CX, mention them all.
	      ((typep cx 'element)
	       (gen-iname
		(format nil "~A ~A ~A ~A"
			(internal-name ax)
			(internal-name rel)
			(internal-name bx)
			(internal-name cx))))
	      ;; Just name the thing AX REL BX.
	      (t (gen-iname
		  (format nil "~A ~A ~A"
			  (internal-name ax)
			  (internal-name rel)
			  (internal-name bx)))))))
    ;; If A, B, or C is :CREATE, create a generic node.	 
    (when (eq a :create)
      (if (fast-type-node? rel-a)
	  (setq ax (new-type
		    (gen-iname
		     (format nil "A-role of ~A" iname))
		    *thing* :english :no-iname))
	  (setq ax (new-indv
		    (gen-iname
		     (format nil "A-role of ~A" iname))
		    *thing* :proper nil :english :no-iname))))
    (when (eq b :create)
      (if (fast-type-node? rel-b)
	  (setq bx (new-type
		    (gen-iname
		     (format nil "B-role of ~A" iname))
		    *thing* :english :no-iname))
	  (setq bx (new-indv
		    (gen-iname
		     (format nil "B-role of ~A" iname))
		    *thing* :proper nil :english :no-iname))))
    (when (eq c :create)
      (if (fast-type-node? rel-c)
	  (setq cx (new-type
		    (gen-iname
		     (format nil "C-role of ~A" iname))
		    *thing* :english :no-iname))
	  (setq cx (new-indv
		    (gen-iname
		     (format nil "C-role of ~A" iname))
		    *thing* :proper nil :english :no-iname))))
    (let ((flags (if negate not-statement-mask statement-mask)))
      (declare (fixnum flags))
      (when dummy (setq flags (logior flags kill-flag)))
      (when (fast-symmetric? rel)
	(setq flags (logior flags symmetric-flag)))
      (when (fast-transitive? rel)
	(setq flags (logior flags transitive-flag)))
      (let ((new-link
	     (make-element flags
			   iname rel context ax bx cx
			   english nil nil nil nil)))
	;; See if any of the nodes now fits a defined type.
	(check-defined-type-memberships ax)
	(check-defined-type-memberships bx)
	(when cx (check-defined-type-memberships cx))
        ;; Check if any rules are now satisfied.
        (check-rule-x-y-z ax rel bx)
	new-link))))


;;; A NOT-STATEMENT states that something is *not* true.

(defmacro fast-not-statement? (e)
  `(all-bits-on? (flags ,e) not-statement-mask))

(defun not-statement? (e)
  "Predicate to determine whether E is a NOT-STATEMENT link."
  (setq e (lookup-element e))
  (and e (fast-not-statement? e)))

(defun new-not-statement (a rel b
			    &key
			    c
			    (context *context*)
			    iname
			    dummy
			    english)
  "Make and return a new NOT-STATEMENT with the specified elements on the A
   and B wires.  Optionally provide :CONTEXT."
  ;; The relation must already be present.  No deferrals.
  (multiple-value-bind (element tag)
      (lookup-element rel)
    (setq rel element)
    ;; If the REL argument is an english name with an
    ;; :INVERSE-RELATION tag, use the relation element but flip the
    ;; arguments around.
    (when (eq tag :inverse-relation)
      (rotatef a b)))
  ;; Try to create a human-readable iname.
  (unless iname
    (unless (and (typep rel 'element)
		 (or (fast-relation? rel) (fast-statement? rel)))
      (error "~S not an existing relation or statement." rel))
    (setq a (lookup-element-test a))
    (setq b (lookup-element-test b))
    (when c (setq c (lookup-element-test c)))
    (setq rel (lookup-element-test rel))
    (setq iname
	  (cond 
	    ;; If we are missing either A or B, just mention REL in the
	    ;; iname.
	    ((not (and *generate-long-element-names*
		       (typep a 'element)
		       (typep b 'element)))
	     (gen-iname 	
	      (format nil "Not ~A"
		      (internal-name rel))))
	    ;; If we have A, B, and C, mention them all.
	    ((typep c 'element)
	     (gen-iname
	      (format nil "Not ~A ~A ~A ~A"
		      (internal-name a)
		      (internal-name rel)
		      (internal-name b)
		      (internal-name c))))
	    ;; Just name the thing Not A REL B.
	    (t (gen-iname
		(format nil "Not ~A ~A ~A"
			(internal-name a)
			(internal-name rel)
			(internal-name b)))))))
  (new-statement a rel b :c c :context context
		 :negate t :dummy dummy :iname iname
		 :english english))


;;; ========================================================================
(subsection "Creating Roles")

;;; If an indv-node or type-node is defined as a role, set ROLE-FLAG.
(defconstant indv-role-mask (logior indv-mask role-flag))
(defconstant type-role-mask (logior type-mask role-flag))

(defmacro fast-role-node? (e)
  `(all-bits-on? (flags ,e) role-flag))

(defun role-node? (e)
  "Predicate to determine whether E is a ROLE node."
  (setq e (lookup-element e))
  (and e (fast-role-node? e)))

(defmacro fast-indv-role-node? (e)
  `(all-bits-on? (flags ,e) indv-role-mask))

(defun indv-role-node? (e)
  "Predicate to determine whether E is an INDV-ROLE node."
  (setq e (lookup-element e))
  (and e (fast-indv-role-node? e)))

(defmacro fast-type-role-node? (e)
  `(all-bits-on? (flags ,e) type-role-mask))

(defun type-role-node? (e)
  "Predicate to determine whether E is a TYPE-ROLE node."
  (setq e (lookup-element e))
  (and e (fast-type-role-node? e)))

;;; Adding a role to a description is normally done using one of these
;;; functions.

(defun new-indv-role (iname owner parent
			    &key
			    may-have
			    transitive
			    symmetric
			    english
			    inverse)
  "States that OWNER type-node has an individual role with the
   specified INAME and PARENT.  Normally we also create a HAS-LINK
   indicating that every member of the specified type has one instance
   of this role.  However, if :MAY-HAVE is T, it indicates that the
   role exists for some instances of OWNER and not others, so we do
   not create this statement.  The :SYMMETRIC and :TRANSITIVE flags
   refer to the implicit relation created between the owner and the
   role-node.  The :ENGLISH argument, as usual, is the name of the role
   (or set of names) in the forward direction.  The :INVERSE argument
   is similar in form, but in the reverse direction."
  (let ((role-node
	 (new-indv iname
		   parent
		   :context owner
		   :proper nil
		   :may-have may-have
		   :english
		   (merge-english-inverse english inverse :inverse-role)
		   :english-default :role)))
    (set-flag role-node role-flag)
    (when transitive (set-flag role-node transitive-flag))
    (when symmetric (set-flag role-node symmetric-flag))
    role-node))

(defun new-type-role (iname owner parent
			    &key
			    n
			    may-have
			    transitive
			    symmetric
			    english
			    inverse)
  "States that OWNER type-node has a type-role with the specified
   INAME and PARENT.  Normally we also create an HAS-LINK indicating
   that every member of the specified type has at least one instance
   of this role.  However, if :MAY-HAVE is T, it indicates that the
   role exists for some instances of OWNER and not others, so we do
   not create this statement.  If :N is specified, that is a node
   representing the number of these things that a typical OWNER has.
   The :SYMMETRIC and :TRANSITIVE flags refer to the implicit relation
   created between the owner and the role-node.  The :ENGLISH
   argument, as usual, is the name of the role (or set of names) in
   the forward direction.  The :INVERSE argument is similar in form,
   but in the reverse direction."
  (let ((role-node
	 (new-type iname
		   parent
		   :context owner
		   :may-have may-have
		   :n n
		   :english
		   (merge-english-inverse english inverse :inverse-role)
		   :english-default :role)))
    (set-flag role-node role-flag)
    (when transitive (set-flag role-node transitive-flag))
    (when symmetric (set-flag role-node symmetric-flag))
    role-node))

;;; ========================================================================
(subsection "Printing Elements")

;;; The PRINT-ELEMENT function must be defined after all the element-type
;;; predicates have been defined.

;;; This stuff doesn't have to be fast.
(declaim (optimize (speed 1) (space 1) (safety 1)))

(defun print-element (e stream depth)
  "Print element E in a human-readable form."
  (declare (stream stream)
	   (ignore depth))
  (format stream "~A" (element-name e)))

(defun gen-iname (string)
  "Generate an iname.  STRING is used as the first part of the name,
   followed by a counter to ensure uniqueness.  If STRING has a counter
   embedded in it, remove that first."
  (prog1
      (make-element-iname
       :value
       (format nil "~A ~D-~D"
	       (strip-counters string)
	       *iname-prefix*
	       *iname-counter*))
      (incf *iname-counter*)))

(defun digit-or-dash (x)
  (or (digit-char-p x)
      (char= x #\-)))

(defun strip-counters (string)
  "Given a STRING, strip out any counters that GEN-INAME may have added.
   These are integers, surrounded by parentheses, preceded by a space."
  (do ((p 1 (+ p 1))
       (q nil))
      ((= p (length string)) string)
    (and (char= (elt string p) #\( )
	 (char= (elt string (- p 1)) #\space)
	 (setq q (position #\) string :start p))
	 (> (- q p) 1)
	 (every #'digit-or-dash (subseq string (+ p 1) q))
	 (return
	  (strip-counters
	   (concatenate 'string
			(subseq string 0 (- p 1))
			(subseq string (+ q 1))))))))

;;; The operations beyond this point must be as fast as possible.
(declaim (optimize (speed 3) (space 0) (safety 0)))

;;; ========================================================================
(subsection "Adding Sets of Subtypes and Instances")

(defun new-split-subtypes (parent members
				  &key
				  (iname nil)
				  (context *context*)
				  english)
  "MEMBERS is a list.  Each member is either an ELEMENT-INAME or a list
   whose first element is an ELEMENT-INAME.  For each member create a new
   type-node under PARENT.  These new elements are all part of a new split.
   After processing all members, return the new split.  The :CONTEXT arg
   controls the context of the new type-nodes and the split.  The optional
   :INAME and :ENGLISH arguments give the name of the split element.  If a
   member of the MEMBERS list is a list, its first element is the iname of
   the new typeype node and the rest is passed as the :ENGLISH argument for
   the new type."
  (let ((new-elements nil))
    (dolist (m members)
      ;; Create a type-node for each item in MEMBERS.
      (push
       (new-type (if (consp m) (car m) m)
		 parent
		 :context context
		 :english (if (consp m)
			      (cdr m)
			      :iname))
       new-elements))
    ;; Create and return the split.
    (new-split (nreverse new-elements)
	       :iname iname
	       :context context
	       :english english)))

(defun new-complete-split-subtypes (parent members
					   &key
					   (iname nil)
					   (context *context*)
					   english)
  "Like NEW-SPLIT-SUBTYPES, but creates a complete split of the elements
   under PARENT."
  (let ((new-elements nil))
    (dolist (m members)
      ;; Create a type-node for each item in MEMBERS.
      (push
       (new-type (if (consp m) (car m) m)
		 parent
		 :context context
		 :english (if (consp m)
			      (cdr m)
			      :iname))
       new-elements))
    ;; Create and return the split.
    (new-complete-split parent
			(nreverse new-elements)
			:iname iname
			:context context
			:english english)))

(defun new-members (parent members
			   &key
			   (iname nil)
			   (context *context*)
			   english)
  "MEMBERS is a list.  Each member is either an ELEMENT-INAME or a list
   whose first element is an ELEMENT-INAME.  For each member create a new
   proper indv node under PARENT.  These new elements are all part of a new
   split.  After processing all members, return the new split.  The
   :CONTEXT arg controls the context of the new type-nodes and the split.
   The optional :INAME and :ENGLISH arguments give the name of the split
   element.  If a member of the MEMBERS list is a list, its first element
   is the iname of the new typeype node and the rest is passed as the
   :ENGLISH argument for the new type.  Note: It is normally assumed that
   proper individuals are distinct, so the split is somewhat redundant, but
   here we create it anyway just to be safe."
  (let ((new-elements nil))
    (dolist (m members)
      ;; Create a type-node for each item in MEMBERS.
      (push
       (new-indv (if (consp m) (car m) m)
		 parent
		 :context context
		 :english (if (consp m)
			      (cdr m)
			      :iname))
       new-elements))
    ;; Create and return the split.
    (new-split (nreverse new-elements)
	       :iname iname
	       :context context
	       :english english)))

(defun new-complete-members (parent members
				    &key
				    (iname nil)
				    (context *context*)
				    english)
  "Like NEW-MEMBERS, but we create a complete split, indicating that we
   believe this set of members to be complete."
  (let ((new-elements nil))
    (dolist (m members)
      ;; Create a type-node for each item in MEMBERS.
      (push
       (new-indv (if (consp m) (car m) m)
		 parent
		 :context context
		 :english (if (consp m)
			      (cdr m)
			      :iname))
       new-elements))
    ;; Create and return the split.
    (new-complete-split parent
			(nreverse new-elements)
			:iname iname
			:context context
			:english english)))

;;; ========================================================================
(subsection "Defined Types")

(defun new-defined-type (iname supertype-list predicate
			       &key
			       (context *context*)
			       english)
  "Creates and returns a new defined-type node with the specified
   INAME.  The definition is a predicate in two parts:

   SUPERTYPE-LIST is a list of elements that must all be superiors of
   the candidate element.  There must be at least one element in this
   list, and for efficiency it should be as specific as possible.  A
   single object may be passed in instead of a list.

   PREDICATE, if non-NIL, is a predicate function that must hold.  It
   takes one argument, the candidate-element E.  The predicate should
   not include testing the SUPERTYPE-LIST -- that is done separately."
  (unless supertype-list
    (error "Supertype list for ~S must have at least one element."
	   iname))
  (unless (listp supertype-list)
    (setq supertype-list (list supertype-list)))
  (setq supertype-list (mapcar #'lookup-element-test supertype-list))
  ;; The definition for the new element is a list whose car is the
  ;; predicate function and whose CDR is the supertype list.
  (let* ((dtype (new-type iname (car supertype-list)
			 :definition (cons predicate supertype-list)
			 :context context
			 :english english)))
    (dolist (p supertype-list)
      ;; First item on the list is already the parent.  For the others
      ;; make an IS-A link to the supertype.
      (unless (eq p (car supertype-list))
	(new-is-a dtype p))
      ;; Put this new dtype in the DEFINED-SUBTYPES field of each
      ;; supertype.
      (push dtype (defined-subtypes p)))
    ;; Find any existing elements that fit the new element's definition.
    ;; For each, put it into the defined class with an is-a link.
    (populate-defined-type dtype)
    ;; Return the new defined-type node.
    dtype))

(defun populate-defined-type (e)
  "Given a defined-type node E, find all existing instances (within
   the limits of the various effort allowance parameters) and add each
   one to the defined type with an IS-A link.  Only uppermost members
   have to be added."
  (setq e (lookup-element-test e))
  (let* ((definition (definition e))
	 (predicate (car definition))
	 (supertype-list (cdr definition)))
    ;; Allocate a marker.
    (with-markers (m1)
      (progn
	;; Use M1 to mark the intersection of the required supertypes.
	(mark-intersection supertype-list m1)
	;; Remove E itself from the M1 set.
	(fast-unmark e m1)
	;; For each uppermost M1-marked node that satisfies PREDICATE,
	;; make an IS-A link to the new intersection-type.
	(do-marked (x m1)
	  (when (and (uppermost? x m1)
		     (or (null predicate)
			 (funcall (the function predicate) x)))
	    (when *comment-on-defined-types*
	      (commentary "Adding ~S to defined type ~S." x e))
	    (new-is-a x e))))
      ;;; Not enough markers, error.
      (commentary
       "No markers available, unable to populate defined type ~S." e))))

(defun check-defined-type-memberships (e)
  "See if element E is a member of any defined type.  If so, create
   an IS-A links to that class.  Restart every time we crated a new IS-A."
  ;; Only do this if *CHECK-DEFINED-TYPES* is T.
  (when *check-defined-types*
    (setq e (lookup-element-test e))
    (tagbody
     restart
       (with-markers (m)
	 (progn
	   ;; Mark superior classes with M.
	   (upscan e m)
	   (do-marked (superior m)
	     ;; For each superior, see which defined inferiors it suggests
	     ;; that we check.
	     (dolist (dtype-to-check (defined-subtypes superior))
	       (when (check-defined-type-membership-internal
		      e dtype-to-check m)
		 ;; Every time we add an IS-A, recheck from the top.
		 (go restart)))))
	 (commentary "No markers available, unable to check whether ~S is~
                      a member of any defined type."  e)))))

(defun check-defined-type-membership (candidate dtype)
  "CANDIDATE is an element.  DTYPE is a defined type node.  If
   CANDIDATE fits the definition of DTYPE, create a new IS-A link
   putting it under DTYPE.  Return T if we did this, NIL if the test
   failed."
  (setq candidate (lookup-element-test candidate))
  (setq dtype (lookup-element-test dtype))
  (with-markers (m)
    (progn
      (upscan candidate m)
      (check-defined-type-membership-internal candidate dtype m))
    (commentary "No markers available, cannot check whether ~S~
                 belongs in defined type ~S." candidate dtype)))
  
(defun check-defined-type-membership-internal (candidate dtype m)
  "Internal version of CHECK-DEFINED-TYPE-MEMBERSHIP.  In addition to
   CANDIDATE and DTYPE, takes a marker M that has already been
   upscanned from CANDIDATE.  Doesn't do LOOKUP-ELEMENT on the args."
  ;; Don't even test the definition if CANDIDATE is already a member
  ;; of DTYPE.
  (unless (fast-marker-on? dtype m)
    ;; The CAR of the type's definition is a predicate, the CDR is a
    ;; list of elements that must be superiors of E (and thus marked
    ;; with M).
    (let* ((def (definition dtype))
	   (pred (car def))
	   (superiors (cdr def)))
      ;; Check whether marker M is present on all elements in
      ;; the list of superiors.  If not, punt.
      (dolist (s superiors)
	(unless (fast-marker-on? s m)
	  (return-from check-defined-type-membership-internal nil)))
      ;; Now check the predicate, if any.
      (when (or (null pred)
		(and (typep pred 'function)
		     (funcall pred candidate)))
	;; We have a winner.  Add E to the defined class.  Else, fall
	;; through with NIL.
	(when *comment-on-defined-types*
	  (commentary "Adding ~S to defined type ~S."
		      candidate dtype))
	(let ((*check-defined-types* nil))
	  (new-is-a candidate dtype))
	t))))

(defun new-intersection-type (iname parents
				    &key
				    (context *context*) 
				    english)
  "PARENTS should be a list of elements, usually type-nodes.
   Create and return a new defined-type element, with name INAME,
   defined to be the intersection of all of the supertypes on the
   list."
  (declare (list parents))
  (when (< (length parents) 2)
    (error "Parent list for ~S must have two or more elements." iname))
  (new-defined-type iname parents nil
		    :context context :english english))

(defun new-union-type (iname  parent
			      subtypes
			      &key
			      (context *context*)
			      english)
  "SUBTYPES is a list of type-nodes or individuals.  Creates and returns a
   new type that is, by definition, the union of these subtypes.  The
   must-be-one-of-these constraint is implemented using a complete-split.
   The PARENT argument is the same as for NEW-TYPE."
  (declare (list subtypes))
  (when (< (length subtypes) 2)
    (error "Subtype list for ~S must have two or more elements." iname))
  ;; Make sure all the subtypes are elements.
  (setq subtypes (mapcar #'lookup-element-test subtypes))
  ;; Create the new type-node.
  (let* ((node (new-type iname parent
			 :definition (cons :union subtypes)
			 :context context
			 :english english)))
    ;; Each of the subtypes is a subtype of the new node.
    (dolist (x subtypes)
      (new-is-a x node))
    ;; Create the complete split.
    (new-complete-split node subtypes)
    node))

;;; ========================================================================
(subsection "Functions to Maintain a Well-Formed Network")

;;; PRINCIPLE: Suppose a type-node T has a role-node R.  Every
;;; inferior node of T has a virtual copy of R, but not all of these
;;; R-node will be explicitly represented in the network.  But for
;;; every T1 and T2 that have explicit role-nodes R1 and R2, if T1 is
;;; an inferior of T2, there must be a directed path of MAP-parents or
;;; derived IS-A links from R1 to R2. We add derived is-a
;;; links to preserve that property.

(defun add-derived-links-map (map-node)
  "Call this after adding a new map-node MAP for ROLE and OWNER.  This
   function adds derived is-a links as needed to keep the network
   well-formed."
  (let ((role (parent-wire map-node))
	(owner (context-wire map-node)))
    ;; First check the need for upward connections from MAP-NODE.
    (with-markers (m1 m2 m3)
      (progn 
      ;;; We use M1 and M2 to mark superior map-nodes "the hard way",
      ;;; upscanning from OWNER with M1 and then EQ-scanning M2 from
      ;;; ROLE, gated by M1.
	(upscan owner m1)
	(fast-mark role m2)
	(eq-scan-internal m2 :gate-map m1 nil nil)
	;; Now use M3 to mark superior nodes that are reached by a
	;; simple upscan from MAP-NODE..
	(upscan map-node m3)
	;; Recycle M1.  Use it to mark nodes marked with M2, but not M3.
	(mark-boolean m1 (list m2) (list m3))
	;; Now find the lowermost map-nodes marked with M1.  These get
	;; a derived link from MAP-NODE.
	(do-marked (x m1)
	  (when (and (fast-map-node? x)
		     (lowermost? x m1))
	    (new-is-a map-node x :derived t)))
	;; Now we need to find maps of ROLE attached to owners below OWNER
	;; in the hierarchy.  These must get derived links to
	;; PLAYER.  Clear M1 and downscan from OWNER.
	(downscan owner m1)
	;; EQ-scan M2 from ROLE, gated by M1.
	(clear-marker-pair m2)
	(fast-mark role m2)
	(eq-scan-internal m2 :gate-map m1 nil nil)
	;; Use M3 to mark all M2 map-nodes whose owners are marked with
	;; M1.
	(clear-marker m3)
	(unmark map-node m2)
	(do-marked (x m2)
	  (when (and (fast-map-node? x)
		     (context-wire x)
		     (fast-marker-on? (context-wire x) m1))
	    (fast-mark x m3)))
	;; If there are any M3 nodes, the uppermost ones get a
	;; derived link to MAP-NODE.
	(do-marked (y m3)
	  (when (uppermost? y m3)
	    (new-is-a y map-node :derived t))))
      (commentary
       "Insufficient markers to check whether ~S needs derived links."
       map-node))))
      
;;; ADD-DERIVED-LINKS-IS-A is similar, but is called after a new
;;; IS-A link from A to B is added to the network.

(defun add-derived-links-is-a (link)
  "Call this after adding a new IS-A link LINK.  This function
   adds derived is-a links as needed to keep the network
   well-formed."
  (unless (fast-killed? link)
    (add-derived-links-internal link
				     (a-wire link)
				     (b-wire link)
				     (context-wire link))))

;;; For a new EQ-link, treat it like a pair of IS-A links.

(defun add-derived-links-eq (link)
  "Call this after adding a new EQ link LINK.  This function
   adds derived is-a links as needed to keep the network
   well-formed."
  (unless (fast-killed? link)
    (add-derived-links-internal link
				     (a-wire link)
				     (b-wire link)
				     (context-wire link))
    (add-derived-links-internal link
				     (b-wire link)
				     (a-wire link)
				     (context-wire link))))

(defun add-derived-links-internal (link x-node y-node context)
  "Internal function to add derived is-a links after creating
   a new IS-A or EQ link."
  (with-markers (m1 m2 m3 m4)
    (progn
      ;; Using M1 and M2 temporarily, place M3 on all the new
      ;; superiors of X -- that is, nodes that were not superiors
      ;; before LINK was created.  Temporarily de-activate LINK.
      (fast-unmark link *activation-marker*)
      (upscan y-node m1)
      (upscan x-node m2)
      (mark-boolean m3 (list m1) (list m2))
      ;; Similarly, place M4 on new inferiors of Y.
      (downscan x-node m1)
      (downscan y-node m2)
      (mark-boolean m4 (list m1) (list m2))
      ;; Re-activate LINK.
      (fast-mark link *activation-marker*)
      ;; Now use M1 to mark all map-nodes whose owner is marked with M3.
      (clear-marker-pair m1)
      (do-marked (e m3)
	(dolist (f (incoming-context-wires e))
	  (when (fast-map-node? f) (fast-mark f m1))))
      ;; If there are no M1 nodes, punt.
      (when (= 0 (fast-marker-count m1))
	(return-from add-derived-links-internal nil))
      ;; Put M2 on the lowermost of these nodes marked with M1.
      (mark-lowermost m1 m2)
      ;; Recycle M1 and M3.  As above, put M1 on all the map-nodes whose
      ;; owners are marked with M4 (new inferiors of Y), and put M3 on
      ;; the uppermost of these.
      (clear-marker-pair m1)
      (do-marked (e m4)
	(dolist (f (incoming-context-wires e))
	  (when (fast-map-node? f) (fast-mark f m1))))
      (when (= 0 (fast-marker-count m1))
	(return-from add-derived-links-internal nil))
      (mark-uppermost m1 m3)
      ;; For every pair of one M2 node and one M3 node that directly map
      ;; the same role-node R, we want to create a derived is-a
      ;; link from the M3-node to the M2-node.  So first, look at each
      ;; M2-node and find the role it maps.
      (do-marked (g m2)
	(let ((r (map-node-role g)))
	  (when r
	    ;; Then scan the M3-nodes, looking for nodes that also map
	    ;; R.
	    (do-marked (h m3)
	      (when (eq (map-node-role h) r)
		;; We found one, so create the derived link.
		(new-is-a h g :context context :derived t)))))))
    (commentary
     "Insufficient markers to check whether ~S needs derived links."
     link)))

(defun map-node-role (map-node)
  "Given a MAP-NODE, find and return the role-node that it maps.  Just
   run up the chain of parent-wires until we hit a node that isn't a
   map, and return it.  If the chain somehow peters out, just return
   NIL."
  (do ((next (parent-wire map-node) (parent-wire next)))
      ((not (and next (fast-map-node? next)))
       (if (fast-role-node? next) next nil))))

;;; ========================================================================
(subsection "Production Rules")

(defstruct (rule
            (:constructor internal-make-rule))
  "Structure to hold data about production rules."
  ;; Starts as a list of symbols representing variables in the rule.
  ;; Will have Scone elements substituted in for the symbols to indicate
  ;; that those elements can possibly satisfy the rule.
  (vars nil :type list)
  ;; List of lists (X Y1 Y2 ...) representing X is a Y1 and a Y2 and etc.
  (is-a-preds nil :type list)
  ;; List of three-element lists (X Y Z).
  ;; If Y is a role-node, this represents predicate X is a Y of Z.
  ;; If Y is a relation-node, this represents predicate statement-true X Y Z.
  (x-y-z-preds nil :type list)
  ;; List of variables that must be proper.
  (proper-vars nil :type list)
  ;; Function with VARS as the argument list.
  (action nil)
  (repr nil))

(defun validate-rule-spec (bindings x-y-z-preds)
  (let ((vars nil)
        (proper-vars nil)
        (is-a-preds nil))
    ;; Parse BINDINGS.
    (dolist (x bindings)
      (if (listp x)
          ;; Case when the current binding is a list.
          (cond ((null x)
                 (error "Empty binding in rule."))
                ;; Parse :PROPER and :SUPERIOR keyword arguments.
                (t (destructuring-bind (var &key proper superior) x
                     (unless (symbolp var)
                       (error "Expected rule variable to be a symbol, got ~S." var))
                     (push var vars)
                     (when proper
                       (push var proper-vars))
                     (when superior
                       (if (setq superior (lookup-element superior))
                           (push (list var superior) is-a-preds)
                           (error "Expected superior to be a Scone element, got ~S." superior))))))
          ;; Case when the current binding is just a variable.
          (if (symbolp x)
              (push x vars)
              (error "Expected rule variable to be a symbol, got ~S." x))))
    (unless (listp x-y-z-preds)
      (error "Expected X-Y-Z-PREDS to be a list of predicates, got ~S." x-y-z-preds))
    (setq x-y-z-preds
          (mapcar (lambda (pred)
                    (unless (= (length pred) 3)
                      (error "Expected predicate to have length 3, got ~S" pred))
                    (destructuring-bind (x y z) pred
                      (unless (or (symbolp x) (setq x (lookup-element x)))
                        (error "Expected X in predicate to be a symbol or Scone element, got ~S." x))
                      (unless (or (role-node? y) (relation? y))
                        (error "Expected Y in predicate to be a Scone role or relation, got ~S." y))
                      (unless (or (symbolp z) (setq z (lookup-element z)))
                        (error "Expected Z in predicate to be a symbol or Scone element, got ~S." z))
                      (list x (lookup-element y) z)))
                  x-y-z-preds))
    (list vars proper-vars is-a-preds x-y-z-preds)))

(defmacro new-if-added-rule (bindings x-y-z-preds &body body)
  "Macro to define new rules.
    BINDINGS is a list of variable bindings with optional keywords :PROPER and :SUPERIOR.
   :PROPER T specifies that only proper Scone nodes can be substituted for the variable.
   :SUPERIOR E specifies that only inferiors of E can be substituted for the variable.
   X-Y-Z-PREDS is a list of role or relation predicates. Each predicate must
   be a three-element list (X Y Z), where Y is a role or relation node and
   X and Z are either symbols or Scone elements.
   BODY contains the forms to run if all the predicates are newly satisfied.
   This means when there is some mapping from the variables to Scone elements
   that satisfy all the bindings and X-Y-Z predicates, the elements are bound
   to the variables and BODY is run."
  (destructuring-bind
      (vars proper-vars is-a-preds x-y-z-preds)
      (validate-rule-spec bindings x-y-z-preds)
    (let ((r (gensym))
          (x (gensym)))
      `(let ((,r (internal-make-rule
                  :vars ',vars
                  :proper-vars ',proper-vars
                  :is-a-preds ',is-a-preds
                  :x-y-z-preds ',x-y-z-preds
                  :action (lambda ,vars (declare (ignorable ,@vars)) (progn ,@body))
                  :repr ',body)))
         ;; Push rule to global rules list.
         (push ,r *rules*)
         ;; For each X-Y-Z predicate, attach a trigger for this rule to node Y.
         ;; Each trigger looks like (R X Z) where R is this rule and X and Z are
         ;; variables or Scone elements.
         (dolist (,x ',x-y-z-preds)
           (push-element-property (lookup-element (second ,x))
                                  :rule-triggers
                                  (list ,r (first ,x) (third ,x))))
         ;; For each IS-A predicate (X Y), attach a trigger for this rule to node Y.
         ;; Each trigger looks like (R X) where R is this rule and X is a variable.
         (dolist (,x ',is-a-preds)
           (push-element-property (lookup-element (second ,x))
                                  :rule-triggers
                                  (list ,r (first ,x))))
         ;; Return created rule
         ,r))))

(defmacro new-if-needed-rule (bindings x-y-z-preds x-y-z-action)
  "Macro to define if-needed (lazy) rules.
   BINDINGS is a list of variable bindings with optional keywords :PROPER and :SUPERIOR.
   :PROPER T specifies that only proper Scone nodes can be substituted for the variable.
   :SUPERIOR E specifies that only inferiors of E can be substituted for the variable.
   X-Y-Z-PREDS is a list of role and/or relation predicates.
   X-Y-Z-ACTION is a role or relation action in the form of a list (X Y Z).
   X is an expression that can use variables in BINDINGS and evaluates to
   some Scone element. Y is a role or relation node. Z is one of the variables
   in BINDINGS or a Scone element."
  (destructuring-bind
      (vars proper-vars is-a-preds x-y-z-preds)
      (validate-rule-spec bindings x-y-z-preds)
    (let* ((r (gensym))
           (action-y (second x-y-z-action))
           ;; Determine if Y is an indv-role, type-role, or relation and
           ;; pick an appropriate action function.
           (action-function (cond ((indv-role-node? action-y)
                                   'x-is-the-y-of-z)
                                  ((type-role-node? action-y)
                                   'x-is-a-y-of-z)
                                  ((relation? action-y)
                                   'new-statement)
                                  (t (error "~S is not a role or relation." action-y)))))
      `(let ((,r (internal-make-rule
                  :vars ',vars
                  :is-a-preds ',is-a-preds
                  :x-y-z-preds ',x-y-z-preds
                  :proper-vars ',proper-vars
                  :action (lambda ,vars (,action-function ,@x-y-z-action)))))
         (push ,r *rules*)
         ;; Add a trigger to the role or relation Y.
         ;; Each trigger looks like (R Z) where R is this rule and Z is a variable.
         (push-element-property (lookup-element ,action-y)
                                :lazy-rule-triggers
                                (list ,r (third ',x-y-z-action)))
         ,r))))

(defun substitute-in-rule (e var r)
  "Internal helper function to substitute an element for a variable in rule R.
   Also removes predicates that have no more variables after substituting
   if they are true.
   Returns the new rule, or NIL if substituting makes a predicate false."
  (flet (;; PREDS looks like ((SYM-1 E-1) (SYM-2 E-2) ... (SYM-N E-N))
         ;; If VAR equals SYM-I, SIMPLIFY-IS-A-PREDS checks if E is a subtype of E-I.
         (simplify-is-a-preds (preds)
           ;; Temporary variable to store all predicates not involving VAR.
           (let ((simplified nil))
             ;; Iterate through PREDS, return SIMPLIFIED at the end.
             (dolist (pred preds simplified)
               (if (eq var (first pred))
                   ;; Case when VAR matches SYM.
                   (unless (simple-is-x-a-y? e (second pred))
                     ;; Predicate is not satisfied, return NIL to outer function.
                     (return-from substitute-in-rule))
                   ;; Case when VAR doesn't match SYM, don't throw away PRED.
                   (push pred simplified)))))
         ;; PREDS is a list where each element is of the form (X Y Z).
         ;; X and Z are either symbols or variables, Y is a role node or a relation node.
         ;; SIMPLIFY-X-Y-Z-PREDS checks if all predicates are satisfied after substitution.
         (simplify-x-y-z-preds (preds)
           ;; Temporary variable to store all predicates after substituting and
           ;; removing ones with no more variables.
           (let ((simplified nil))
             ;; Iterate through PREDS, return SIMPLIFIED at the end.
             (dolist (pred preds simplified)
               (let* ((substituted (substitute e var pred))
                      (x (first substituted))
                      (y (second substituted))
                      (z (third substituted)))
                 (cond ((or (symbolp x) (symbolp z))
                        ;; If either X or Z are still variables, don't throw away PRED.
                        (push substituted simplified))
                       ((or
                         ;; This predicate is not satisfied, return NIL to outer function.
                         (and (indv-role-node? y)
                              (not (simple-is-x-eq-y? x (the-x-role-of-y y z))))
                         (and (type-role-node? y)
                              (not (simple-is-x-a-y? x (the-x-role-of-y y z))))
                         (and (relation? y)
                              (not (statement-true? x y z))))
                        (return-from substitute-in-rule nil))))))))
    ;; Return rule with the variable substituted and extraneous predicates removed.
    (internal-make-rule
     :vars (substitute e var (rule-vars r))
     :is-a-preds (simplify-is-a-preds (rule-is-a-preds r))
     :x-y-z-preds (simplify-x-y-z-preds (rule-x-y-z-preds r))
     :proper-vars (rule-proper-vars r)
     :action (rule-action r)
     :repr (rule-repr r))))

(defun check-rule-x-y-z (x y z)
  "Function to check if any rules are newly satisfied after adding
   role statement X is the Y of Z or relation statement X Y Z."
  ;; Do not check rules for derived map nodes since they are created
  ;; implicitly and not from any added knowledge.
  (when (or (map-node? x) (map-node? z))
    (return-from check-rule-x-y-z nil))
  (when *comment-on-rule-check*
    (commentary "Check rule x-y-z ~S ~S ~S." x y z))
  (with-markers (m)
    (progn
      ;; Mark all superiors of Y to check triggers on them.
      (upscan y m)
      (do-marked (superior m)
        (when (or (role-node? superior)
                  (relation? superior))
          (dolist (trigger (get-element-property superior :rule-triggers))
            ;; Each trigger is of the form (R A C).
            ;; R is a rule, A and C are either variables or elements.
            ;; The trigger represents a predicate A Y C, and we want to see if
            ;; X and Z can take the places of A and C.
            (let* ((r (first trigger))
                   (a (second trigger))
                   (c (third trigger))
                   (left (lookup-element a)) ; Will be NIL if A is not an element.
                   (right (lookup-element c))) ; Will be NIL if C is not an element.
              (if left
                  ;; Element is on left, variable is on right.
                  ;; This trigger can apply if X is a subtype of A.
                  (when (simple-is-x-eq-y? x left)
                    ;; See if the rule can be satisfied by substituting Z for C.
                    (check-rule-filler r z c))
                  (if right
                      ;; Variable is on left, element is on right.
                      ;; This trigger can apply if Z is a subtype of C.
                      (when (simple-is-x-eq-y? z right)
                        ;; See if the rule can be satisfied by substituting X for A.
                        (check-rule-filler r x a))
                      ;; Variable is on left and right.
                      ;; See if the rule can be satisfied by substituting X for A and Z for C.
                      (check-rule-filler r x a z c)))))))))
  (when *satisfied-rules*
    (fire-rules)))

(defun check-rule-x-is-a-y (x y)
  "Function to check if any rules are newly satisfied after adding
   an IS-A or EQ link between X and Y."
  ;; Do not check rules for derived map nodes since they are created
  ;; implicitly and not from any added knowledge.
  (when (or (map-node? x) (map-node? y))
    (return-from check-rule-x-is-a-y nil))
  (when *comment-on-rule-check*
    (commentary "Check rule ~S is a ~S." x y))
  (with-markers (m)
    (progn
      ;; Mark all superiors of Y to check triggers on them.
      (upscan y m)
      (do-marked (superior m)
        ;; Don't check role/relation triggers.
        (unless (role-node? superior)
          ;; Each trigger is of the form (R SYM). R is a rule, SYM is a variable.
          (dolist (trigger (get-element-property superior :rule-triggers))
            ;; See if the rule can be satisfied by substituting X for SYM in R.
            (check-rule-filler (first trigger) x (second trigger)))))))
  (when *satisfied-rules*
    (fire-rules)))

(defun check-rule-x-of-y (x y)
  "Function to check if there are any lazy rules that can be used to find
   the X of Y if there is currently no good answer."
  (when *comment-on-rule-check*
    (commentary "Check rule ~S of ~S." x y))
  (with-markers (m)
    (progn
      ;; Mark all superiors of X to check lazy triggers on them.
      (upscan x m)
      (do-marked (superior m)
        ;; Each trigger is of the form (R SYM). R is a rule, SYM is a variable
        ;; representing the parent node Y.
        (dolist (trigger (get-element-property superior :lazy-rule-triggers))
          (check-rule-filler (first trigger) y (second trigger))))))
  (when *satisfied-rules*
    (fire-rules)))

(defun check-rule-filler (r e var &optional e2 var2)
  "Function to check if rule R can be satisfied if E is substituted
   for VAR, and if specified E2 is substituted for VAR2."
  (cond ((null (setq r (substitute-in-rule e var r)))
         ;; If substituting E for VAR in R violates a predicate, return NIL.
         (return-from check-rule-filler nil))
        ((and e2 var2 (null (setq r (substitute-in-rule e2 var2 r))))
         ;; If substituting E2 for VAR2 in R violates a predicate, return NIL.
         (return-from check-rule-filler nil))
        ;; If no predicates are violated after substituting, start recursively
        ;; finding more elements to substitute for variables.
        (t
         (when (null (rule-x-y-z-preds r))
           ;; No more X-Y-Z predicates to satisfy in this rule.
           (if (null (rule-is-a-preds r))
               ;; No more is-a predicates to satisfy in this rule, so rule is true.
               (progn
                 ;; When a rule is satisfied, this COMMENTARY prints out the elements
                 ;; satisfying the rule and the action of the rule.
                 (when *comment-on-rule-check*
                   (commentary "Rule true ~S ~S." (reverse (rule-vars r)) (rule-repr r)))
                 ;; The elements that satisfy rule R are now stored in its VARS field.
                 ;; Add the rule to the list of satisfied rules.
                 (push r *satisfied-rules*)
                 (return-from check-rule-filler t))
               ;; If there are still is-a predicates, then there are variables unconnected
               ;; to other X-Y-Z predicates. Treat this rule as ill-defined and return NIL.
               (return-from check-rule-filler nil)))
         ;; Look at the first X-Y-Z predicate to find elements to substitute in the rule.
         (let* ((pred (car (rule-x-y-z-preds r)))
                (left (lookup-element (first pred))) ; LEFT is either an element or NIL.
                (y (lookup-element-test (second pred))) ; Y is either a role or relation node.
                (right (lookup-element (third pred)))) ; RIGHT is either an element or NIL.
           ;; PRED is of the form (X Y Z). LEFT is the element X if non-NIL, and RIGHT is
           ;; the element Z if non-NIL.
           (cond ((and left
                       (null right))
                  ;; Left element is filled, scan for possible right elements.
                  (with-markers (m1)
                    (progn
                      ;; Create temporary marker M2 for scanning.
                      (with-markers (m2)
                        (progn
                          (cond ((role-node? y)
                                 (mark-role-inverse y left m2))
                                ((relation? y)
                                 (mark-rel y left m2)))
                          ;; Mark most specific elements to search as rule fillers.
                          ;; Only mark proper elements if declared that way in rule.
                          (if (member (third pred) (rule-proper-vars r))
                              (mark-proper m2 m1)
                              (mark-most-specific m2 m1))))
                      (do-marked (z m1)
                        ;; We know role/relation predicate (LEFT Y Z) is true, so try substituting
                        ;; element Z for variable Z in PRED.
                        (check-rule-filler r z (third pred))))))
                 ((and (null left)
                       right)
                  ;; Right element is filled, scan for possible left elements.
                  (with-markers (m1)
                    (progn
                      ;; Create temporary marker M2 for scanning.
                      (with-markers (m2)
                        (progn
                          (cond ((role-node? y)
                                 (mark-role y right m2))
                                ((relation? y)
                                 (mark-rel-inverse y right m2)))
                          ;; Mark most specific elements to search as rule fillers.
                          ;; Only mark proper elements if declared that way in rule.
                          (if (member (first pred) (rule-proper-vars r))
                              (mark-proper m2 m1)
                              (mark-most-specific m2 m1))))
                      (do-marked (z m1)
                        ;; We know role/relation predicate (X Y RIGHT) is true, so try substituting
                        ;; element X for variable X in PRED.
                        (check-rule-filler r z (first pred)))))))))))

(defun fire-rules ()
  (setq *satisfied-rules* (nreverse *satisfied-rules*))
  (loop while *satisfied-rules*
        ;; Fire rules starting from ones that were found satisfied first.
        do (let ((satisfied-rules (nreverse *satisfied-rules*)))
             (setq *satisfied-rules* nil)
             (dolist (r satisfied-rules) 
               (apply (rule-action r) (rule-vars r))))))

;;; ***************************************************************************
(section "Marker Operations and Scans")
;;; ***************************************************************************

;;; These operations must be as fast as possible.
(declaim (optimize (speed 3) (space 0) (safety 0)))

;;; ========================================================================
(subsection "Low-Level Marker Operations"
  "These operations are intended for internal use, and do minimal checking.")


(declaim (inline fast-mark))

(defun fast-mark (e m)
  (declare (fixnum m))
  "Mark element E with marker M.  Do not check arguments."
  (let ((bit (marker-bit m)))
    (declare (fixnum bit))
    ;; If element E is already marked with marker M, do nothing.
    (when (= 0 (logand bit (bits e)))
      (if (null (svref *first-marked-element* m))
	  ;; This is the first element to get marker M, so record E as the
	  ;; first marked element.
	  (setf (svref *first-marked-element* m) e)
	  ;; There arlready are other elements with M, so add E to the chain.
	  (setf (svref (next-marked-element (svref *last-marked-element* m)) m) e))
      ;; Adjust forward and back pointers in E.
      (setf (svref (next-marked-element e) m) nil)
      (setf (svref (prev-marked-element e) m)
	    (svref *last-marked-element* m))
      ;; E is now the last marked element.
      (setf (svref *last-marked-element* m) e)
      ;; Turn on the marker bit in E.
      (setf (bits e)
	    (logior (bits e) bit))
      ;; Update the count of elements marked with M.
      (inc-marker-count m))))

(declaim (inline fast-unmark))

(defun fast-unmark (e m)
  (declare (fixnum m))
  "Remove marker M from element E.  Do not check arguments."
  (let ((bit (marker-bit m)))
    (declare (fixnum bit))
    ;; If element E is not marked with marker M, do nothing.
    (unless (= 0 (logand bit (bits e)))
      ;; Remember the elements before and after E in marker M's chain.
      (let ((prev (svref (prev-marked-element e) m))
            (next (svref (next-marked-element e) m)))
        ;; Clear the chain entries from E.
        (setf (svref (prev-marked-element e) m) nil)
        (setf (svref (next-marked-element e) m) nil)
        (if (null prev)
            ;; Element E was the first element in M's chain.
            ;; Make E's successor first.
            (setf (svref *first-marked-element* m) next)
          ;; Else, update next-pointer in previous element.
          (setf (svref (next-marked-element prev) m) next))
        (if (null next)
            ;; Element E was the last element in M's chain.
            ;; Make E's predecessor the last.
            (setf (svref *last-marked-element* m) prev)
          ;; Else, update the prev-pointer in the next element.
          (setf (svref (prev-marked-element next) m) prev))
        ;; Turn off the marker bit in E.
        (setf (bits e)
              (logand (bits e) (lognot bit)))
        ;; Update the count of elements marked with M.
        (dec-marker-count m)))))

(declaim (inline faster-mark))

(defun faster-mark (e m)
  (declare (fixnum m))
  "Just like FAST-MARK, but we use this when we're sure that E does not already
   hold marker M and that there is at least one element already marked with M.
   This is a common situation and saves a few cycles."
  ;; Add E to the end of the chain.
  (setf (svref (next-marked-element (svref *last-marked-element* m)) m) e)
  ;; Adjust forward and back pointers in E.
  (setf (svref (next-marked-element e) m) nil)
  (setf (svref (prev-marked-element e) m)
        (svref *last-marked-element* m))
  ;; E is now the last marked element.
  (setf (svref *last-marked-element* m) e)
  ;; Turn on the marker bit in E.
  (setf (bits e)
        (logior (bits e) (marker-bit m)))
  ;; Update the count of elements marked with M.
  (inc-marker-count m))

(declaim (inline faster-unmark))

(defun faster-unmark (e m)
  (declare (fixnum m))
  "This is like FAST-UNMARK, but it is used in situations where you are
   sure that element E has mark M.  Saves a few cycles."
  (let ((bit (marker-bit m)))
    (declare (fixnum bit))
    ;; Remember the elements before and after E in marker M's chain.
    (let ((prev (svref (prev-marked-element e) m))
          (next (svref (next-marked-element e) m)))
      ;; Clear the chain entries from E.
      (setf (svref (prev-marked-element e) m) nil)
      (setf (svref (next-marked-element e) m) nil)
      (if (null prev)
          ;; Element E was the first element in M's chain.
          ;; Make E's successor first.
          (setf (svref *first-marked-element* m) next)
        ;; Else, update next-pointer in previous element.
        (setf (svref (next-marked-element prev) m) next))
      (if (null next)
          ;; Element E was the last element in M's chain.
          ;; Make E's predecessor the last.
          (setf (svref *last-marked-element* m) prev)
        ;; Else, update the prev-pointer in the next element.
        (setf (svref (prev-marked-element next) m) prev))
      ;; Turn off the marker bit in E.
      (setf (bits e)
            (logand (bits e) (lognot bit)))
      ;; Update the count of elements marked with M.
      (dec-marker-count m))))

(declaim (inline convert-marker))

(defun convert-marker (m1 m2)
  "For every element marked with M1, mark it with M2 (if it was not marked
   with M2 already) and clear M1 from E.  This could be done with
   MARK-BOOLEAN, but this specialized version is faster."
  (declare (fixnum m1 m2))
  (let* ((bit1 (marker-bit m1))
	 (bit2 (marker-bit m2))
	 (mask1 (lognot bit1))
	 (bits 0)
	 (last-m2 (svref *last-marked-element* m2))
	 (next nil))
    (declare (fixnum bit1 bit2 mask1 bits))
    ;; For each element E in the chain for marker M1...
    (do ((e (svref *first-marked-element* m1) next))
	((null e))
      (setq bits (bits e))
      ;; Mark E with M2, unless it is already marked with M2.
      (when (= 0 (logand bit2 bits))
	(if last-m2
	    ;; There are already elements marked with M2, add E to the
	    ;; chain.
	    (setf (svref (next-marked-element last-m2) m2) e)
	    ;; This is the first element marked with M2.
	    (setf (svref *first-marked-element* m2) e))
	;; Adjust the back-pointer in E.
	(setf (svref (prev-marked-element e) m2) last-m2)
	;; Turn on the M2 marker bit in E.
	(setq bits (logior bits bit2))
	;; Update the count of elements marked with M.
	(inc-marker-count m2)
	;; E is now the last element in the m2 chain.
	(setq last-m2 e))
      ;; Remove the M1 links from E, but remember the next element.
      (setq next (svref (next-marked-element e) m1))
      (setf (svref (next-marked-element e) m1) nil)
      (setf (svref (prev-marked-element e) m1) nil)
      ;; Update the marker bits of E.
      (setf (bits e) (logand bits mask1)))
    ;; No elements are now marked with M1.
    (zero-marker-count m1)
    (setf (svref *first-marked-element* m1) nil)
    (setf (svref *last-marked-element* m1) nil)
    ;; Record the last element marked with M2.
    (setf (svref *last-marked-element* m2) last-m2)
    (fast-marker-count m2)))

(defun clear-marker (m)
  "Clear marker M from all elements."
  (declare (fixnum m))
  (check-legal-marker m)
  (let ((mask (lognot (marker-bit m))))
    (declare (fixnum mask))
    (do ((e (svref *first-marked-element* m)))
	((null e) nil)
      ;; For each element E in the chain for marker M...
      (let ((next (svref (next-marked-element e) m)))
	;; Unlink from the chain.
	(setf (svref (next-marked-element e) m) nil)
	(setf (svref (prev-marked-element e) m) nil)
	;; Clear the marker bit.
	(setf (bits e) (logand (bits e) mask))
	(setf e next))))
  ;; Zero the count and set first and last elements to NIL.
  (zero-marker-count m)
  (setf (svref *first-marked-element* m) nil)
  (setf (svref *last-marked-element* m) nil))

(defun clear-marker-pair (m)
  "Clear marker M and the associated cancel marker, but do not free it."
  (declare (fixnum m))
  (check-legal-marker-pair m)
  (clear-marker m)
  (clear-marker (get-cancel-marker m)))

(defun clear-all-markers ()
  "Clear and free all markers from all elements in the KB."
  (do ((i 0 (1+ i)))
      ((>= i n-markers))
    (declare (fixnum i))
    (clear-marker i)
    (free-markers)))

(defun make-mask (markers)
  "Takes a list of markers.
   Returns the corresponding mask (a fixnum)."
  (let ((mask 0))
    (declare (fixnum mask))
    (dolist (m markers)
      (declare (fixnum m))
      (check-legal-marker m)
      (setq mask (logior mask (marker-bit m))))
    mask))

(defun make-cancel-mask (markers)
  "Takes a list of markers.  Returns the mask corresponding to
   the cacnel marker for each marker in the list."
  (let ((mask 0))
    (declare (fixnum mask))
    (dolist (m markers)
      (declare (fixnum m))
      (check-legal-marker m)
      (setq mask (logior mask (marker-bit (get-cancel-marker m)))))
    mask))

;;; ========================================================================
(subsection "User-Level Marker Operations")

;;; The following marker operations may be called directly by users,
;;; so they check their arguments for safety.

(defun mark (e m)
  "User-level MARK function."
  (check-legal-marker m)
  (setq e (lookup-element-test e))
  (fast-mark e m))

(defun unmark (e m)
  "User-level UNMARK function."
  (check-legal-marker m)
  (setq e (lookup-element-test e))
  (fast-unmark e m))

(defun marker-count (m)
  (check-legal-marker m)
  (fast-marker-count m))

(defun marker-on? (e m)
  "User-level function to determine whether element E has marker M
   turned on."
  (check-legal-marker m)
  (setq e (lookup-element-test e))
  (fast-marker-on? e m))

(defun marker-off? (e m)
  "User-level function to determine whether element E has marker M
   turned off."
  (check-legal-marker m)
  (setq e (lookup-element-test e))
  (fast-marker-off? e m))

;;; ========================================================================
(subsection "Is-A Hierarchy Scans")

;;; Macros for internal use.

(defmacro fast-active-element? (e)
  "Internal macro to determine whether element E is active and not
   killed in the current context.  If *IGNORE-CONTEXT* is T, the
   element is always considered active."
  `(and (fast-flag-off? ,e kill-flag)
    (or *ignore-context*
     (fast-marker-on? ,e *activation-marker*))))

(defmacro fast-usable-element? (e)
  "Internal macro to determine if an element is active, not blocked,
   and not killed.  MARKABLE-MASK and BLOCKING-MASK must be set up and
   lexically visible.  *IGNORE-CONTEXT* is checked in setting up
   MARKABLE-MASK."
  `(and
    (all-bits-on? (bits ,e) markable-mask)
    (all-bits-off? (bits ,e) blocking-mask)))

(defmacro fast-markable-element? (e)
  "Internal macro to determine whether element E can be marked during
   a scan.  Like FAST-USABLE-ELEMENT?, but also checks that the element is
   not already marked with M.  MARKABLE-MASK and BLOCKING-M-MASK must
   be set up and lexically visible. *IGNORE-CONTEXT* is checked in
   setting up MARKABLE-MASK."
  `(and
    (all-bits-on? (bits ,e) markable-mask)
    (all-bits-off? (bits ,e) blocking-m-mask)))

(to-do "I think this H-UPSCAN mechanism can cause crosstalk under
some conditions.  Examine and fix if necessary.")

(defmacro h-upscan (wire)
  "Internal macro to upscan from an A, B, or C node of the H-element Z
   (a relation or statement) to the corresponding node of the
   H-element's parent. WIRE is one of A-WIRE, B-WIRE, or C-WIRE.  If Z
   is an H-element, we want to create the effect of a virtual map-link
   connection from A-node to A-node, etc., with the H-link as the
   owner."
  ;; Check whether Z is relation or statement.
  `(when (and (all-bits-on? z-flags h-mask)
	  ;; If so, process this according to the MODE of the scan.
	  ;; If :CROSS-MAP or :GATE-MAP, mark the corresponding node
	  ;; above.
	  (or (eq mode :cross-map)
	      (eq mode :gate-map)))	  
    ;; OK, we're going to do mark the corresponding node above with M.
    ;; Find the parent of Z...
    (let* ((h-parent (parent-wire z))
	   (target nil))
      (when (and h-parent
		 (fast-h-link? h-parent)
		 (fast-usable-element? h-parent))
	;; Then find the node above E.
	(setq target (,wire h-parent))
	;; And mark that node if possible.
	(when (and target (fast-markable-element? target))
	  (faster-mark target m))))))

(defmacro h-downscan (wire)
  "Internal macro to downscan from an A, B, or C node of the H-element Z
   (a relation or statement) to the corresponding node of the
   H-element's children. WIRE is one of A-WIRE, B-WIRE, or C-WIRE.  If Z
   is an H-element, we want to create the effect of a virtual map-link
   connection from A-node to A-node, etc., with the H-link as the
   owner."
  ;; Check whether Z is relation or statement.
  `(when (and (all-bits-on? z-flags h-mask)
	  ;; If so, process this according to the MODE of the scan.
	  ;; If :CROSS-MAP or :GATE-MAP, mark the corresponding node
	  ;; below.
	  (or (eq mode :cross-map)
	      (eq mode :gate-map)))
    ;; OK, we're going to do mark the corresponding nodes below with M.
    ;; Find the children of Z...
    (dolist (h-child (incoming-parent-wires z))
      (when (and (fast-h-link? h-child)
		 (fast-usable-element? h-child))
	;; Then find the node on the same wire as E.
	(let ((target (,wire h-child)))
	  ;; And mark that node if possible.
	  (when (and target (fast-markable-element? target))
	    (faster-mark target m)))))))


(defmacro scan-loop (&body body)
  "Macro to supply the code that is common among the various scan
   routines.  Assumes that the following variables are bound in the
   caller: M, M1, AFTER, and RESTRICT-M.  This macro interates over
   M-marked elements, binding each to E and executing BODY for each."
  `(progn
     (if m1 (check-legal-marker-pair m1))
     ;; Set up some variables and bit masks for use during the scan.
     (let* (;; The last element marked with M before we begin scanning.
	    (last-m)
	    ;; The bit mask for marker M.
	    (m-mask (marker-bit m))
	    ;; The cancel marker for M.
	    (m-cancel (get-cancel-marker m))
	    ;; The bit mask for M-CANCEL.
	    (m-cancel-mask (marker-bit m-cancel))
	    ;; Masks for the gate marker.  May be 0.
	    (m1-mask (if m1 (marker-bit m1) 0))
	    (m1-cancel-mask 
	     (if m1 (marker-bit (get-cancel-marker m1)) 0))
	    ;; These bits must all be set in an element if we are
	    ;; to mark it during a scan.
	    (markable-mask
	     (logior
	      ;; If *IGNORE-CONTEXT*, don't check for activation.
	      (if *ignore-context* 0 *activation-mask*)
	      ;; Also check for RESTRICT-M, if present.
	      (if restrict-m (marker-bit restrict-m) 0)))
	    ;; If any of these bits are set, the node or link cannot
	    ;; be marked.
	    (blocking-mask
	     (logior *context-cancel-mask*
		     *activation-cancel-mask*
		     m-cancel-mask
		     m1-cancel-mask))
	    ;; Like BLOCKING-MASK, but also has marker M's bit set.
	    (blocking-m-mask
	     (logior blocking-mask m-mask)))
       (declare (fixnum m-mask m-cancel m-cancel-mask
			m1-mask m1-cancel-mask
			markable-mask
			blocking-mask blocking-m-mask))
       ;; Note the last element marked with M before we begin.
       (setq last-m  (svref *last-marked-element* m))
       ;; Scan all the M-marked elements, looking for opportunities to
       ;; propagate M in the desired direction.  Note that the chain of
       ;; elements marked with M may grow ahead of us, but we'll reach the
       ;; end eventually because there are a finite number of elements and
       ;; we never mark an element that is already marked.
       (do-marked (e m after)
	 ,@body
	 ;; If *ONE-STEP* and ELEMENT was the last element marked with M
	 ;; going in, quit now.
	 (when (and *one-step* (eq e last-m))
	   (return (fast-marker-count m)))))
     (fast-marker-count m)))

(defun upscan-internal (m mode m1 after restrict-m)
  "Internal function to perform an upscan, not activating
   descriptions.  This is a non-keyword function, sacrificing some
   clarity for speed.

   M is the marker to upscan.  MODE is one of the following:

   :CROSS-MAP Cross map connections in the direction of the scan.
   :GATE-MAP Cross maps only if marker M1 is present on owner .
   :NO-MAP Do not cross map connections.

   If AFTER is present, it is an element marked with M.  We propagate
   only those M-markers placed after this node was marked.  If M1 is
   not used, it should be NIL.

  RESTRICT-M, if present, is a marker already present on some elements in
  the network.  In this scan we place M only on elements that already
  have RESTRICT-M."
  (declare (fixnum m))
  (scan-loop
    ;; Check incoming A wires for various link types.
    (dolist (z (incoming-a-wires e))
      (let ((z-flags (flags z)))
	(declare (fixnum z-flags))
	;; Check that the link is active and unblocked.
	(when (fast-usable-element? z)
	  (cond
	    ;; Is this link a negation or CANCEL link?
	    ((all-bits-on? z-flags cancel-up-mask)
	     ;; Yes, mark the target node as cancelled.
	     (let ((target (b-wire z)))
	       (when (and target (fast-markable-element? target))
		 (fast-mark target m-cancel))))
	    ;; Is this an IS-A or EQ link?
	    ((all-bits-on? z-flags up-mask)
	     (let ((target (b-wire z)))
	       ;; Is the target unblocked and not yet marked?
	       (when (and target (fast-markable-element? target))
		 ;; Yes, mark the target with marker M.
		 (faster-mark target m))))
	    ;; Handle case if Z is a statement or relation.
	    (t (h-upscan a-wire))))))
    ;; Check and mark the parent element of E.
    (let ((target (parent-wire e)))
      (when (fast-markable-element? target)
	(if (fast-map-node? e)
	    ;; E is a map-node, process according to MODE:
	    (when (or (eq mode :cross-map)
		      ;; If :GATE-MAP, marker M1 must be on the owner.
		      (and (eq mode :gate-map)
			   (context-wire e)
			   (any-bits-on? (bits (context-wire e))
					 m1-mask)))
	      (faster-mark target m))
	    ;; Not a map-node, just mark the parent.
	    (faster-mark target m))))
    ;; Check incoming B wires for EQ and H-links.
    (dolist (z (incoming-b-wires e))
      (let ((z-flags (flags z)))
	(declare (fixnum z-flags))
	;; Check that the link is active, unblocked.
	(when (fast-usable-element? z)
	  (cond
	    ;; Is this an EQ link?
	    ((all-bits-on? z-flags eq-mask)
	     (let ((target (a-wire z)))
	       ;; Is the target unblocked and not yet marked?
	       (when (and target (fast-markable-element? target))
		 ;; Yes, mark the target with M.
		 (faster-mark target m))))
	    ;; Is Z an H-element?
	    (t (h-upscan b-wire))))))
    ;; Check incoming C wires for H-links.
    (dolist (z (incoming-c-wires e))
      (let ((z-flags (flags z)))
	(declare (fixnum z-flags))
	;; Is Z active, unblocked, and an H-link?
	(when (fast-usable-element? z)
	  (h-upscan c-wire))))))

(defun downscan-internal (m mode m1 after restrict-m)
  "Internal function to perform a downscan, not activating
   descriptions.  This is a non-keyword function, sacrificing some
   clarity for speed.

   M is the marker to downscan.  MODE is one of the following:

   :CROSS-MAP Cross map links in the direction of the scan.  :GATE-MAP
   Cross maps only if marker M1 is present on owner.  :NO-MAP Do not
   cross map links.

   If AFTER is present, it is an element marked with M.  We propagate
   only those M-markers placed after this node was marked.  If M1 is
   not used, it should be NIL.

   RESTRICT-M, if present, is a marker already present on some
   elements in the network.  In this scan we place M only on elements
   that already have RESTRICT-M."
  (declare (fixnum m))
  (scan-loop
    ;; Check incoming B wires for various link types.
    (dolist (z (incoming-b-wires e))
      (let ((z-flags (flags z)))
	(declare (fixnum z-flags))
	;; Check that the link is active and unblocked.
	(when (fast-usable-element? z)
	  (cond
	    ;; Is this link a negation or CANCEL link?
	    ((all-bits-on? z-flags cancel-up-mask)
	     ;; Yes, mark the target node as cancelled.
	     (let ((target (a-wire z)))
	       (when (and target (fast-markable-element? target))
		 (fast-mark target m-cancel))))
	    ;; Is this an IS-A or EQ link?
	    ((all-bits-on? z-flags up-mask)
	     (let ((target (a-wire z)))
	       ;; Is the target unblocked and not yet marked?
	       (when (and target
			  (fast-markable-element? target))
		 ;; Yes, mark the target with marker M.
		 (faster-mark target m))))
	    ;; Is Z an H-element?
	    (t (h-downscan b-wire))))))
    ;; Check and mark the elements whose parent-wires connect to E.
    ;; Cross parent wires of map-nodes in downward direction only.
    (dolist (target (incoming-parent-wires e))
      (when (fast-markable-element? target)
	;; If TARGET is a map-node, process according to MODE.
	(if (fast-map-node? target)
	    (when (or (eq mode :cross-map)
		      ;; If :GATE-MAP, marker M1 must be on owner.
		      (and (eq mode :gate-map)
			   (context-wire target)
			   (any-bits-on?
			    (bits (context-wire target))
			    m1-mask)))
	      (faster-mark target m))
	    (faster-mark target m))))
    ;; Check incoming A wires for EQ and H-links.
    (dolist (z (incoming-a-wires e))
      (let ((z-flags (flags z)))
	(declare (fixnum z-flags))
	;; Check that the link is active, unblocked.
	(when (fast-usable-element? z)
	  (cond
	    ;; Is this an EQ-link?
	    ((all-bits-on? z-flags eq-mask)
	     (let ((target (b-wire z)))
	       ;; Is the target unblocked and not yet marked?
	       (when (and target (fast-markable-element? target))
		 ;; Yes, mark the target with M.
		 (faster-mark target m))))
	    ;; Is Z an H-element?
	    (t (h-downscan a-wire))))))
    ;; Check incoming C wires for H-links.
    (dolist (z (incoming-c-wires e))
      (let ((z-flags (flags z)))
	(declare (fixnum z-flags))
	;; Is Z active, unblocked, and an H-link?
	(when (fast-usable-element? z)
	  (h-downscan c-wire))))))

(defun eq-scan-internal (m mode m1 after restrict-m)
  "Internal function to perform an eq-scan, not activating
   descriptions.  This is a non-keyword function, sacrificing some
   clarity for speed.

   M is the marker to eq-scan.  If MODE is :GATE-MAP, cross maps only
   if marker M1 is present on owner.  Else, do not cross map links.

   If AFTER is present, it is an element marked with M.  We propagate
   only those M-markers placed after this node was marked.  If M1 is
   not used, it should be NIL.

   RESTRICT-M, if present, is a marker already present on some
   elements in the network.  In this scan we place M only on elements
   that already have RESTRICT-M."
  (declare (fixnum m))
  (scan-loop
   ;; Check incoming A wires for EQ links.
   (dolist (z (incoming-a-wires e))
     (let ((z-flags (flags z)))
       (declare (fixnum z-flags))
       ;; Check that the link is active, unblocked.
       (when (and (fast-usable-element? z)
		  (all-bits-on? z-flags eq-mask))
	 (let ((target (b-wire z)))
	   ;; Is the target unblocked and not yet marked?
	   (when (and target (fast-markable-element? target))
	     ;; Yes, mark the target with M.
	     (faster-mark target m))))))
   ;; Check incoming B wires for active EQ links.
   (dolist (z (incoming-b-wires e))
     (let ((z-flags (flags z)))
       (declare (fixnum z-flags))
       ;; Check that the link is active, unblocked.
       (when (and (fast-usable-element? z)
		  (all-bits-on? z-flags eq-mask))
	 (let ((target (a-wire z)))
	   ;; Is the target unblocked and not yet marked?
	 (when (and target (fast-markable-element? target))
	   ;; Yes, mark the target with M.
	   (faster-mark target m))))))
   ;; Consider crossing parent wires if in :GATE-MAP mode.
   (when (eq mode :gate-map)
     (when (and (fast-map-node? e)
		(context-wire e)
		(any-bits-on? (bits (context-wire e))
			      m1-mask)
		(parent-wire e)
		(fast-markable-element? (parent-wire e)))
       (fast-mark (parent-wire e) m))
     (dolist (z (incoming-parent-wires e))
     (when (and (fast-map-node? z)
		(context-wire z)
		(any-bits-on? (bits (context-wire z))
			      m1-mask))
       (fast-mark z m))))))

(defmacro x-scan (internal-scan)
  "Internal macro to provide the elements common to UPSCAN, DOWNSCAN,
   and EQ-SCAN.  Expects to be in a lexical context where the
   following variables are bound: START-ELEMENT, M, MODE, M1,
   AUGMENT, and RESTRICT-M."
  `(progn
    (check-legal-marker-pair m)
    (unless augment
      (clear-marker-pair m))
    (when m1 (check-legal-marker-pair m1))
    (cond
      ;; If there's a start element, mark it.
      (start-element
       (setq start-element (lookup-element-test start-element))
       (mark start-element m))
      ;; This case is OK.
      ((and augment (> (fast-marker-count m) 0)))
      ;; Not OK.  Complain to the user.
      (t (error "If you don't supply a start-element, you must ~
		 set :AUGMENT and mark at least one element with M.")))
    ;; Then call the INTERNAL-SCAN to do a simple scan with M.
    (,internal-scan m mode m1 nil restrict-m)))

(defun upscan (start-element m
			     &key
			     (mode :cross-map)
			     (m1 nil)
			     (augment nil)
			     (restrict-m nil))
  "M is a user-marker.  Mark START-ELEMENT with marker M and do a full
   upscan, propagating M upward to supertypes of START-ELEMENT.

   MODE is one of :CROSS-MAP, :GATE-MAP, or :NO-MAP.  The default is
   :CROSS-MAP.  The others are specialized scan types with respect to
   propagating across map connections.  For some modes, we need an
   additional marker, which is supplied as :M1.

   If :AUGMENT is non-T, add new M markers, but don't clear any
   existing ones.  If START-ELEMENT is NIL but :AUGMENT is T, just
   propagate existing M markers.  (It is an error if no M-markers
   exist.)

   :RESTRICT-M, if present, is a marker already present on some
   elements in the network.  In this scan we place M only on elements
   that already have RESTRICT-M.

   Returns the number of elements marked with M after the scan."
  (declare (fixnum m))
  (x-scan upscan-internal))

(defun downscan (start-element m
			       &key
			       (mode :cross-map)
			       (m1 nil)
			       (augment nil)
			       (restrict-m nil))
  "M is a user-marker.  Mark START-ELEMENT with marker M and do a full
   downscan, propagating M downward to subtypes and instances of
   START-ELEMENT.

   MODE is one of :CROSS-MAP, :GATE-MAP, or :NO-MAP.  The default is
   :CROSS-MAP.  The others are specialized scan types with respect to
   propagating across map connections.  For some modes, we need an
   additional marker, which is supplied as :M1.

   If :AUGMENT is non-T, add new M markers, but don't clear any
   existing ones.  If START-ELEMENT is NIL but :AUGMENT is T, just
   propagate existing M markers.  (It is an error if no M-markers
   exist.)

   :RESTRICT-M, if present, is a marker already present on some
   elements in the network.  In this scan we place M only on elements
   that already have RESTRICT-M.

   Returns the number of elements marked with M after the scan."
  (declare (fixnum m))
  (x-scan downscan-internal))

(defun eq-scan (start-element m
			       &key
			       (mode :cross-map)
			       (m1 nil)
			       (augment nil)
			       (restrict-m nil))
  "M is a user-marker.  Mark START-ELEMENT with marker M and do a full
   eq-scan, propagating M to nodes that are equivalent to
   START-ELEMENT.

   MODE is one of :CROSS-MAP, :GATE-MAP, or :NO-MAP.  The default is
   :CROSS-MAP.  The others are specialized scan types with respect to
   propagating across map connections.  For some modes, we need an
   additional marker, which is supplied as :M1.

   If :AUGMENT is non-T, add new M markers, but don't clear any
   existing ones.  If START-ELEMENT is NIL but :AUGMENT is T, just
   propagate existing M markers.  (It is an error if no M-markers
   exist.)

   :RESTRICT-M, if present, is a marker already present on some
   elements in the network.  In this scan we place M only on elements
   that already have RESTRICT-M.

   Returns the number of elements marked with M after the scan."
  (declare (fixnum m))
  (x-scan eq-scan-internal))

;;; ========================================================================
(subsection "Uppermost/Lowermost Scans")

;;; Scans to determine if an element is uppermost or lowermost of
;;; those marked with M. 

(defun lowermost? (e m)
  "Assume element E is marked with marker M.  Determine whether there
   are any other M-marked elements among E's immediate children."
  (declare (fixnum m))
  (setq e (lookup-element e))
  (dolist (child (incoming-parent-wires e))
    (when (fast-marker-on? child m)
      (return-from lowermost? nil)))
  (dolist (link (incoming-b-wires e))
    (let ((a-element nil))
      (and (or (fast-is-a-link? link))
	   (fast-active-element? link)
	   (setq a-element (a-wire link))
	   (fast-marker-on? a-element m)
	   (not (eq a-element e))
	   (return-from lowermost? nil))))
  t)

(defun uppermost? (e m)
  "Assume element E is marked with marker M.  Determine whether there
   are any other M-marked elements among E's immediate parents."
  (declare (fixnum m))
  (setq e (lookup-element e))
  (when (fast-marker-on? (parent-wire e) m)
    (return-from uppermost? nil))
  (dolist (link (incoming-a-wires e))
    (let ((b-element nil))
      (and (or (fast-is-a-link? link))
	   (fast-active-element? link)
	   (setq b-element (b-wire link))
	   (fast-marker-on? b-element m)
	   (not (eq b-element e))
	   (return-from uppermost? nil))))
  t)

;;; ========================================================================
(subsection "Boolean Operations Over Marked Elements")

(defun mark-boolean (m must-be-set must-be-clear
		       &optional
		       (flags-set 0)
		       (flags-clear 0))
  "M is a marker.  MUST-BE-SET and MUST-BE-CLEAR are lists of markers
   indicated by name or by number.  Scan all elements in the current
   context.  If an element has all the MUST-BE-SET bits on and all the
   MUST-BE-CLEAR bits off, set marker M on that element.  If supplied
   the FLAGS-SET and FLAGS-CLEAR are integer masks indicating which
   flag bits in the element must be on or off."
  (declare (fixnum m flags-set flags-clear))
  (check-legal-marker m)
  (clear-marker m)
  (let ((set-mask (make-mask must-be-set))
	(clear-mask (make-mask must-be-clear)))
    (declare (fixnum set-mask clear-mask))
    (if (not (= 0 set-mask))
	;; We know that elements of interest have at least one mark set.
	;; Save time by scanning along the shortest marker chain.
	(let ((shortest 0)
	      (length (1+ (the fixnum *n-elements*))))
	  (declare (fixnum shortest length))
	  ;; Find the shortest marker chain to scan.
	  (dotimes (mm n-markers)
	    (declare (fixnum mm))
	    (if (not (= 0 (logand set-mask (marker-bit mm))))
		(let ((n-marked (fast-marker-count mm)))
		  (declare (fixnum n-marked))
		  ;; If any of the "must be set" markers is not on any
		  ;; elements, we can go home early.
		  (cond ((= 0 n-marked)
			 (return-from mark-boolean 0))
			((< n-marked length)
			 (setq shortest mm)
			 (setq length n-marked))))))
	  (do-marked (e shortest)
	    (let ((ebits (bits e))
		  (fbits (flags e)))
	      (declare (fixnum ebits))
	      (if (and (all-bits-on? ebits set-mask)
		       (all-bits-off? ebits clear-mask)
		       (all-bits-on? fbits flags-set)
		       (all-bits-off? fbits  flags-clear))
		  (fast-mark e m)))))
	;; Elements of interest may have no marks set, so we must scan
	;; all elements in the KB.
	(do-elements (e)
	  (let ((ebits (bits e))
		(fbits (flags e)))
	    (declare (fixnum ebits fbits))
	    ;; Don't check set-mask, since it is zero, but do check
	    ;; that the node is active -- if not, don't mark it.
	    (when (and (all-bits-off? ebits clear-mask)
		       (all-bits-on? fbits flags-set)
		       (all-bits-off? fbits flags-clear)
		       (or *ignore-context*
			   (all-bits-on? ebits *activation-mask*)))
		(fast-mark e m))))))
  (fast-marker-count m))

(defun unmark-boolean (m must-be-set must-be-clear
			 &optional
			 (flags-set 0)
			 (flags-clear 0))
  "M is a marker.  MUST-BE-SET and MUST-BE-CLEAR are lists of markers
   indicated by name or by number.  Scan all elements in the KB.  If
   an element has all the MUST-BE-SET bits on and all the
   MUST-BE-CLEAR bits off, remove marker M from that element.  If
   supplied the FLAGS-SET and FLAGS-CLEAR are integer masks indicating
   which flag bits in the element must be on or off."
  (declare (fixnum m flags-set flags-clear))
  (let ((set-mask (make-mask must-be-set))
	(clear-mask (make-mask must-be-clear))
	(unmark-me nil))
    (declare (fixnum set-mask clear-mask))
    ;; Just scan the set of elements marked with M, looking for the other
    ;; markers and flags.
    (do-marked (e m)
      (when unmark-me
	(faster-unmark unmark-me m)
	(setq unmark-me nil))
      (let ((ebits (bits e))
	    (fbits (flags e)))
	(declare (fixnum ebits fbits))
	(when (and (all-bits-on? ebits set-mask)
		   (all-bits-off? ebits clear-mask)
		   (all-bits-on? fbits flags-set)
		   (all-bits-off? fbits flags-clear))
	  (setq unmark-me e))))
    (when unmark-me (faster-unmark unmark-me m)))
  (fast-marker-count m))

(defun mark-intersection (list m)
  "Given list LIST of elements, which should all be type-nodes, place
   marker M on every element that is in the intersection of all they
   types in LIST.  Return the count."
  (declare (fixnum m))
  (when (null list)
    (clear-marker m)
    (return-from mark-intersection 0))
  ;; Allocate a temporary marker.
  (with-markers (m1)
    (progn
      ;; Mark the members of the first type.
      (downscan (car list) m)
      (dolist (next-type (cdr list))
	;; Punt if the intersection so far is null.
	(when (= (marker-count m) 0)
	  (return-from mark-intersection 0))
	;; Downscan from the next type with M1.
	(downscan next-type m1)
	;; Remove any M marker that does not also have M1.
	(unmark-boolean m nil (list m1)))))
  (fast-marker-count m))

(defun violated-split? (m)
  "Some set of elements has been marked with M1 and upscanned.  Look
   for and return any split that is violated -- that is, it has M on
   more than one of the disjoint items.  We return only the first
   violated split found, and NIL if all are OK."
  (declare (fixnum m))
  (check-legal-marker-pair m)
  (let ((markable-mask
	 ;; If *IGNORE-CONTEXT*, don't check for activation.
	 (if *ignore-context* 0 *activation-mask*))
	(blocking-mask (logior *context-cancel-mask*
			       *activation-cancel-mask*
			       (marker-bit (get-cancel-marker m)))))
    (declare (fixnum markable-mask blocking-mask))
    (do-marked (e m)
      (dolist (split (incoming-split-wires e))
	(when (fast-usable-element? split)
	  (dolist (item (split-wires split))
	    (when (and (fast-marker-on? item m)
		       (not (eq item e)))
	      (return-from violated-split? split))))))))

(defun incompatible? (e1 e2)
  "We are proposing to join two elements E1 and E2 by an EQ or IS-A link.  See if
   there is a problem with this.  Mark these elements with M1 and M2 respectively and
   upscan both.  Look for elements marked with M1 and the cancellation marker for
   M2 and vice versa.  Also look for active split links that are violated -- that is
   they have M1 on one of the split elements and M2 on a different one.  Return the
   first offending element we find, or NIL if all is OK."
  (setq e1 (lookup-element-test e1))
  (setq e2 (lookup-element-test e2))
  (with-markers (m1 m2)
    (progn
      ;; Upscan E1 and E2 with separate markers.
      (upscan e1 m1)
      (upscan e2 m2)
      ;; Examine whether the cancel marker of M1 is on any M2-marked node.
      (do-marked (forbidden (get-cancel-marker m1))
        (when (fast-marker-on? forbidden m2)
          ;; We found a problem, return the conflicted node.
          (return-from incompatible? forbidden)))
      ;; And vice versa.
      (do-marked (forbidden (get-cancel-marker m2))
        (when (fast-marker-on? forbidden m1)
          ;; We found a problem, return nil and the forbidden node.
          (return-from incompatible? forbidden)))
      ;; Check for violated splits.
      (let
          ;; Set up some masks to check whether the split is active and
          ;; not cancelled.
          ((markable-mask
             ;; If *IGNORE-CONTEXT*, don't check for activation of the split.
             (if *ignore-context* 0 *activation-mask*))
           (blocking-mask (logior *context-cancel-mask*
                                  *activation-cancel-mask*
                                  (marker-bit (get-cancel-marker m1))
                                  (marker-bit (get-cancel-marker m2)))))
        (declare (fixnum markable-mask blocking-mask))
        ;; Scan all M1-marked elements.
        (do-marked (e m1)
          ;; For each, scan the splits of which it is a part.
          (dolist (split (incoming-split-wires e))
            ;; If the split is active and not cancelled...
            (when (fast-usable-element? split)
              ;; See if M2 is on some other item in the split.
              (dolist (item (split-wires split))
                (when (and (fast-marker-on? item m2)
                           (not (eq item e)))
                  ;; Report the violated split, if any.
                  (return-from incompatible? split))))))))))

;; Occasionally useful utility, provided by Cinar.

(defun which-split-member? (e split)
  "Given an element E and a split-node SPLIT, finds which split
   type E is an inferior of. If none of the types is a supertype
   of E, NIL is returned."
  (setq e (lookup-element-test e))
  (setq split (lookup-element-test split))
  (unless (split? split)
    (error "~A is not a split-node." split))
  ;; Mark the superiors of elem and check which
  ;; of the marked elements belongs to the split
  (with-markers (m)
    (progn
      (upscan e m)
      (dolist (member (split-wires split))
	(when (marker-on? member m)
	  (return-from which-split-member? member)))))
  nil)

;;; ========================================================================
(subsection "Marker Scans For Roles, Relations, and Statements")

(defun mark-role (role owner m &key (downscan t) augment)
  "OWNER is any element.  ROLE is a node that is a role of OWNER,
   directly or by inheritance.  Place marker M on all nodes X such
   that 'X is the ROLE of OWNER'.  If :AUGMENT, don't clear M before
   marking new nodes.  Downscan the M markers unless :DOWNSCAN is
   explicitly NIL.  Returns the number of elements ultimately marked
   with M."
  (check-legal-marker-pair m)
  (unless augment (clear-marker-pair m))
  (setq owner (lookup-element-test owner))
  (setq role (lookup-element-test role))
  (let ((role-owner (context-wire role)))
    ;; Punt if ROLE is not a role-node attached to an owner.
    (unless (and role-owner (fast-role-node? role))
      (commentary "~S is not a role node." role)
      (return-from mark-role (fast-marker-count m)))
    ;; Allocate a marker for the owner and superiors.
    (with-markers (m-owner)
      (progn
	;; Standard upscan from OWNER using M-OWNER.
	(mark owner m-owner)
	(upscan-internal m-owner :cross-map nil nil nil)
	;; If ROLE is not inherited by OWNER, return.
	(unless (marker-on? role-owner m-owner)
	  (return-from mark-role (fast-marker-count m)))
	;; Put marker M on ROLE.
	(fast-mark role m)
	;; Now eq-scan or downscan M with M-OWNER as focus.
	(if downscan
	    (downscan-internal m :gate-map m-owner nil nil)
	    (eq-scan-internal m :gate-map m-owner nil nil))))
    (fast-marker-count m)))

(defun mark-role-inverse (role player m &key (downscan t) augment)
  "Place marker M on all nodes X such that 'PLAYER is the ROLE of X'.
   If :AUGMENT, don't clear M before marking new nodes.  Downscan the
   M markers unless :DOWNSCAN is explicitly NIL.  Returns the number
   of elements ultimately marked with M."
  (check-legal-marker-pair m)
  (unless augment (clear-marker-pair m))
  (setq player (lookup-element-test player))
  (setq role (lookup-element-test role))
  ;; Allocate markers.
  (with-markers (m1 m2)
    (progn
      ;; Upscan M1 from PLAYER, crossing map links.
      (fast-mark player m1)
      (upscan-internal m1 :cross-map nil nil nil)
      ;; If we didn't mark ROLE on that upscan, no winners.
      (unless (marker-on? role m1)
	(return-from mark-role-inverse (fast-marker-count m)))
      ;; Now use M2 to retrace M1 path(s) from ROLE to PLAYER.
      (fast-mark role m2)
      (downscan-internal m2 :cross-map nil nil m1)
      ;; Look for lowermost map-nodes marked with M2, and with M2
      ;; above.  Their owners are the winners, so mark them with M.
      (do-marked (map m2)
	(when (and (fast-map-node? map)
		   ;; Make sure we reached this map-node from above.
		   (fast-marker-on? (parent-wire map) m2)
		   ;; Make sure this is the lowermost map-node
		   ;; in the chain attached to OWNER.
		   (block lowermost-map
		     (dolist (child (incoming-parent-wires map))
		       (when (fast-marker-on? child m2)
			 (return-from lowermost-map nil)))
		     t)
		   (context-wire map))
	  ;; This map-node's owner is a winner, mark with M.
	  (fast-mark (context-wire map) m)))))
  ;; If :DOWNSCAN, mark inferiors of the actual owner.
  (when downscan (downscan-internal m :cross-map nil nil nil))
  (fast-marker-count m))

(defmacro cross-rel (m-rel m-source m-target reverse mark-link)
  "Cross REL-LINKs that are active and marked.  If REVERSE is T, cross
   B-to-A; if NIL, cross A-to-B. If MARK-LINK is T, mark the link we
   are crossing with M-TARGET, rather than the node on the other side."
  `(let* ((markable-mask
	   ;; If *IGNORE-CONTEXT*, don't check for activation.
	   (if *ignore-context* 0 *activation-mask*))
	  (blocking-mask 
	   (logior *context-cancel-mask*
		   *activation-cancel-mask*
		   (make-mask (list (get-cancel-marker ,m-source)
				    (get-cancel-marker ,m-rel)))))
	  (blocking-m-mask (logior blocking-mask (marker-bit ,m-target)))
	  (target nil))
     (declare (fixnum markable-mask blocking-mask blocking-m-mask))
     ;; Scan all elements with M-SOURCE marker.
     (do-marked (x ,m-source)
       ;; Cross statement links under REL in desired direction, marking
       ;; target with M-TARGET.
       (dolist (link (,(if reverse 'incoming-b-wires 'incoming-a-wires)
		      x))
	 (when (and (fast-marker-on? link ,m-rel)
		    (fast-statement? link)
		    (fast-usable-element? link))
	   ;; Decide what to mark: the link itself, its B-element, or its A-element.
	   (setq target (cond (,mark-link link)
			      (,reverse (a-wire link))
			      (t (b-wire link))))
	   ;; Check and mark it.
	   (when (fast-markable-element? target)
	     (fast-mark target m)))))))

(defun mark-rel-internal (rel a m fwd rev mark-link
			      downscan augment recursion-allowance)
  "Internal function to put marker M on all elements E such that (if
   FWD) 'A REL E' and (if REV) 'E REL A'. If DOWNSCAN, mark subtypes
   and instances of E.  If AUGMENT, do not clear M before doing this
   operation. If MARK-LINK, put marker M on the relation link to
   target node E, rather than the element E."
  (declare (fixnum recursion-allowance))
  (check-legal-marker-pair m)
  (unless augment (clear-marker-pair m))
  (setq a (lookup-element-test a))
  (setq rel (lookup-element-test rel))
  (unless (fast-relation? rel)
    (error "~S not a relation." rel))
  (when (symmetric? rel)
    (setq fwd t)
    (setq rev t))
  ;; Allocate the markers.
  (with-markers (m-a m-rel)
    (prog ((transitive (fast-transitive? rel))
	   ;; Element most recently marked with M. Might be NIL.
	   (prev-marked-element (svref *last-marked-element* m)))
       ;; Do a full downscan from REL using M-REL.
       (downscan rel m-rel)
       ;; Execute the body below here at least once, and potentially
       ;; loop back here if the relation is transitive.
       TRANSITIVE-LOOP
       ;; Simple upscan from A using M-A.  Mark map-nodes but
       ;; do not cross them.
       (mark a m-a)
       (upscan-internal m-a :no-map nil nil nil)
       ;; Cross any marked rel-links A-to-B.
       (when fwd (cross-rel m-rel m-a m nil mark-link))
       ;; Cross any marked rel-links B-to-A.
       (when rev (cross-rel m-rel m-a m t mark-link))
       ;; Now explore the descriptions in which A plays a role.
       (unless (<= recursion-allowance 0)
	 (do-marked (source m-a)
	   ;; Look for map-nodes marked with M-A.
	   (when (fast-map-node? source)
	     (mark-rel-description source m m-rel fwd rev mark-link
				   downscan recursion-allowance))))
       ;; Final eq-scan or downscan of M.
       (when (> (fast-marker-count m) 0)
	 (if downscan
	     (downscan nil m :augment t)
	     (eq-scan nil m :augment t)))
       ;; If REL is a transitive relation, and if there are any newly
       ;; M-marked nodes we have not yet processed, pick the first of
       ;; these, make it the new A node, and jump back to
       ;; TRANSITIVE-LOOP. Iterate until there are no more new ones.
       (when (and transitive
		  (not (eq prev-marked-element
			   (svref *last-marked-element* m))))
	 (if prev-marked-element
	     (setq a (svref (next-marked-element prev-marked-element) m))
	     (setq a (svref *first-marked-element* m)))
	 (setq prev-marked-element a)
	 (clear-marker-pair m-a)
	 (go TRANSITIVE-LOOP))))
  (fast-marker-count m))

(defun mark-rel-description (source m m-rel fwd rev mark-link
			     downscan recursion-allowance)
  "Function called within MARK-REL-INTERNAL to explore inherited
   descriptions. May recurse, up to limits set by the allowance
   variables."
  (declare (fixnum recursion-allowance))
  (let ((description (context-wire source)))
    (when description
      ;; Allocate three new markers to explore this description.
      (with-markers (m-desc m-source m-target)
	(progn
	  ;; Do a full upscan with M-DESC.
	  (upscan description m-desc)
	  ;; Now upscan from SOURCE within this focus, using
	  ;; M-SOURCE.  Cross map-wires within the activated
	  ;; description, but not others.
	  (mark source m-source)
	  (upscan-internal m-source :gate-map m-desc nil nil)
	  ;; Cross any marked rel-links A-to-B.
	  (when fwd (cross-rel m-rel m-source m-target nil mark-link))
	  ;; Cross any marked rel-links B-to-A.
	  (when rev (cross-rel m-rel m-source m-target t mark-link))
	  ;; Now recursively explore any descriptions in which SOURCE
	  ;; plays a role.
	  (unless (<= recursion-allowance 0)
	    (do-marked (map m-source)
	      ;; Look for map-nodes newly marked with M-SOURCE.
	      (when (and (not (eq map source))
			 (fast-map-node? map))
		(mark-rel-description map m-target m-rel
				      fwd rev mark-link downscan
				      (- recursion-allowance 1)))))
	  ;; Now eq-scan or downscan M-TARGET with M-DESC as focus.
	  (if downscan
	      (downscan-internal m-target :gate-map m-desc nil nil)
	      (eq-scan-internal m-target :gate-map m-desc nil nil))
	  ;; The M-TARGET markers still within the description are
	  ;; cleared.  Those that have escaped are converted to
	  ;; M markers.
	  (externalize-marker m-target m m-desc))
	(commentary
	 "Not enough markers for full search of relations.~
          Continuing...")))))

(defun externalize-marker (m1 m2 m-desc)
  "This is a specialized version of CONVERT-MARKER.  For every element
   marked with M1, mark it with M2 (if it was not marked with M2
   already) and clear M1 from E.  But if the node in question has a
   context wire to a node with the M-DESC marker, just clear M1."
  (do-marked (node m1)
    (let ((cxt (context-wire node)))
      (unless (and cxt (fast-marker-on? cxt m-desc))
	(fast-mark node m2))))
  (clear-marker m1))

(defun mark-rel
    (rel a m
	 &key
	 (downscan t)
	 augment
	 (recursion-allowance *default-recursion-allowance*))
  "REL is a relation node.  A is any element.  Place marker M on all
   elements E such that 'A REL E'.  If :AUGMENT, don't clear M before
   marking new nodes.  Downscan the M markers unless :DOWNSCAN is
   explicitly NIL. :RECURSION-ALLOWANCE controls the number of levels
   of descriptions we will explore.  Returns the number of elements
   ultimately marked with M."
  (multiple-value-bind (element tag)
      (lookup-element-test rel)
    (setq rel element)
    ;; If the REL argument is an english name with an
    ;; :INVERSE-RELATION tag, we do the inverse operation.
    (if (eq tag :inverse-relation)
	(mark-rel-internal rel a m nil t nil
			   downscan augment recursion-allowance)
	(mark-rel-internal rel a m t nil nil
			   downscan augment recursion-allowance))))

(defun mark-rel-inverse
    (rel a m
	 &key
	 (downscan t)
	 augment
	 (recursion-allowance *default-recursion-allowance*))
  "A is any element.  REL is a relation node.  Place marker M on all
   elements E such that 'E REL A'.  If :AUGMENT, don't clear M before
   marking new nodes.  Downscan the M markers unless :DOWNSCAN is
   explicitly NIL. :RECURSION-ALLOWANCE controls the number of levels
   of descriptions we will explore.  Returns the number of elements
   ultimately marked with M."
  (multiple-value-bind (element tag)
      (lookup-element-test rel)
    (setq rel element)
    ;; If the REL argument is an english name with an
    ;; :INVERSE-RELATION tag, we do the forward operation.
    (if (eq tag :inverse-relation)
	(mark-rel-internal rel a m t nil nil
			   downscan augment recursion-allowance)
	(mark-rel-internal rel A m nil t nil
			   downscan augment recursion-allowance))))

;;; ========================================================================
(subsection "Context Activation")

(defun activation-scan (c-marker a-marker)
  (declare (fixnum c-marker a-marker))
  (clear-marker a-marker)
  ;; First mark every active context-node with the activation marker.
  (do-marked (ce c-marker)
    (fast-mark ce a-marker))
  ;; Then, for every node already marked with activation marker, check its
  ;; incoming context wires and mark all of those nodes.
  (do-marked (ae a-marker)
    (dolist (e (incoming-context-wires ae))
      (fast-mark e a-marker)))
  (fast-marker-count a-marker))

(defun in-context (e)
  "E is an element representing a context.  Activate it as the current
   context."
  ;; Activate context E.
  (setq e (lookup-element-test e))
  (unless (eq *context* e)
    (setq *context* e)
    ;; Activate the new context and its superiors.
    (let ((*ignore-context* t))
      (upscan e *context-marker*))
    ;; Place activation markers.
    (activation-scan *context-marker* *activation-marker*)
    e))

(defun refresh-context ()
  "Keep the current *context*, butforce and update the context and
   activation markers in case something got messed up."
  ;; Activate the context and its superiors.
  (let ((*ignore-context* t))
    (upscan *context* *context-marker*))
    ;; Place activation markers.
    (activation-scan *context-marker* *activation-marker*)
    *context*)

;;; ***************************************************************************
(Section "Queries and Predicates")
;;; ***************************************************************************

(subsection "Predicates on the Is-A Hierarchy")

;;; These operations must be as fast as possible.
(declaim (optimize (speed 3) (space 0) (safety 0)))

(defun simple-is-x-a-y? (x y)
  "Predicate to determine whether type or individual X is known to be of
   type Y in the current context."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (with-markers (m1)
    (progn
      (upscan x m1)
      (marker-on? y m1))))

(defun is-x-a-y? (x y)
  "Predicate to determine whether type or individual X is or can be a
   Y in the current context.  Returns :YES if X is known to be a Y,
   :NO if X definitely cannot be a Y, and :MAYBE otherwise.  For the
   :NO response, returns a second value, the split or negation link
   that is causing the conflict."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (with-markers (m1)
    (let ((err nil))
      ;; Mark superiors of X with M1.
      (upscan x m1)
      (cond
	;; If Y is explicitly NOT a superior due to a negation link,
	;; the answer is :NO.  Return the negation link as the second
	;; value.
	((marker-on? y (get-cancel-marker m1))
	 (values
	  :no
	  (find-is-not-a-link y m1)))
	;; If Y is marked as a superior, the answer is :YES.
	((marker-on? y m1)
	 :yes)
	;; If X and Y are on different branches of a split, the answer
	;; is :NO.  Return the split as a second value.
	((setq err (incompatible? x y))
	   (values :no err))
	;; Otherwise, the answer is :MAYBE.
	(t :maybe)))))

(defun find-is-not-a-link (node m1)
  "NODE got tagged with a cancel-marker M1C during an upscan of
   marker M1.  Find the is-not-a link responsible, or one of them if there are
   more than one."
  (dolist (link (incoming-b-elements node))
    (when (and (is-not-a-link? link)
	       (marker-on? (a-element link) m1))
      (return-from find-is-not-a-link link))))

(defun simple-is-x-eq-y? (x y)
  "Predicate to determine whether node X is known to be identical to
   node Y in the current context."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (with-markers (m1)
    (progn
      (eq-scan x m1)
      (marker-on? y m1))))

(defun is-x-eq-y? (x y)
  "Predicate to determine whether type or individual X is or can be
   equal to Y in the current context.  Returns :YES if X is known to
   be equal to Y, :NO if X definitely cannot be equal to Y, and :MAYBE
   otherwise.  For the :NO response, returns a second value, the split
   or negation link that is causing the conflict."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (with-markers (m1)
    (let ((err nil))
      ;; Mark equals of X with M1.
      (eq-scan x m1)
      (cond
	;; If Y is explicitly NOT an equal due to a negation link,
	;; the answer is :NO.  Return the negation link as the second
	;; value.
	((marker-on? y (get-cancel-marker m1))
	 (values
	  :forbidden
	  (find-not-eq-link y m1)))
	;; If Y is marked as equal, the answer is :YES.
	((marker-on? y m1)
	 :yes)
	;; If X and Y are on different branches of a split, the answer
	;; is :NO.  Return the split as a second value.
	((setq err (incompatible? x y))
	   (values :no err))
	;; Otherwise, the answer is :MAYBE.
	(t :maybe)))))

(defun find-not-eq-link (node m1)
  "NODE got tagged with a cancel-marker M1C during an eq-scan of
   marker M1.  Find the not-eq link responsible, or one of them if there are
   more than one."
  (dolist (link (incoming-b-elements node))
    (when (and (not-eq-link? link)
	       (marker-on? (a-element link) m1))
      (return-from find-not-eq-link link)))
  (dolist (link (incoming-a-elements node))
    (when (and (not-eq-link? link)
	       (marker-on? (b-element link) m1))
      (return-from find-not-eq-link link))))  

;;; ========================================================================
(subsection "Basic List and Show Machinery")

;;; Use normal compilation policy, where safety counts as much as speed.
(declaim (optimize (speed 1) (space 1) (safety 1)))

;;; In general, these functions take any valid specifier for element
;;; arguments and check the marker arguments.

;;; In most cases, we have a MARK function and corresponding LIST and SHOW
;;; functions.  The MARK function does the real work, marking whatever set
;;; the user wants to see.

;;; The corresponding LIST function just returns the specified set in the
;;; form of a list, useful for passing to other programs.

(defun list-elements ()
  "Return a list of all elements in the KB."
  (let ((l nil))
    (do ((e *first-element* (next-kb-element e))
	 (i 0 (1+ i)))
	((null e))
      (declare (fixnum i))
      (push e l))
    (nreverse l)))

(defun list-marked (m)
  "Return a list of all elements marked with marker M."
  (check-legal-marker m)
  (let ((l nil))
    (do ((e (svref *first-marked-element* m)
	    (svref (next-marked-element e) m)))
	((null e))
      (push e l))
    (nreverse l)))

(defun list-not-marked (m)
  "Return a list of all elements not marked with marker M."
  (with-markers (m1)
    (progn
      (mark-boolean m1 () (list m) 0 0)
      (list-marked m1))))

;;; The SHOW function prints out the set in a human-readable form.  We
;;; don't want these functions to just stupidly print out thousands of
;;; elements unless the user explicitly asks for that.  So in general,
;;; these functions default to printing the first 100 elements in the
;;; requested set, with a notation indicating that there are more.

;;; The :N keyword argument, an integer, specifies the maximum number of
;;; elements to be printed.  The default value is 100.

;;; The :ALL keyword, followed by T, forces printing of *all* the selected
;;; elements.

;;; The :LAST keyword argument, an integer, says to print the last n
;;; selected elements instead of the first n.

;;; Normally you supply only one of :N, :ALL, or :LAST.

;;; The :HEADING keyword is a string to print at the start of the list.

(defun show-elements (&key (n 100)
			   (all nil)
			   (last nil))
  "Print out a table of all elements in the KB."
  (declare (fixnum n))
  (format *show-stream* "~%Elements in the Knowledge Base:~2%")
  (if (and last (integerp last))
      (setq n last))
  (if (or all (null n))
      (setq n *n-elements*))
  (let ((skip 0))
    (declare (fixnum skip))
    (when (and last (< n *n-elements*))
      (setq skip (- *n-elements* n))
      (setq n *n-elements*)
      (format *show-stream* "Skipping ~D elements...~%" skip))
    (do ((e *first-element* (next-kb-element e))
	 (i 0 (1+ i)))
	((or (null e) (>= i n))
	 (unless (null e)
	   (format *show-stream* "... and ~D more."
		   (- *n-elements* n))))
      (declare (fixnum i))
      (if (>= i skip)
	  (format *show-stream* "~S~%" e)))
    (values)))

(defun show-marked (m &key (n 100)
		      (all nil)
		      (last nil)
		      (heading nil))
  "Print out a table of all elements with marker M."
  (declare (fixnum m n))
  (check-legal-marker m)
  (if heading 
      (format *show-stream* "~%~A~2%" heading)
      (format *show-stream* "~%"))
  ;;; If nothing to print, say so.
  (when (= (marker-count m) 0)
    (format *show-stream* "No elements to print.~%")
    (return-from show-marked (values)))
  (if (and last (integerp last))
      (setq n last))
  (if (or all (null n))
      (setq n *n-elements*))
  (let ((skip 0))
    (declare (fixnum skip))
    (when (and last (< n (fast-marker-count m)))
      (setq skip (- (fast-marker-count m) n))
      (setq n *n-elements*)
      (format *show-stream* "Skipping ~D marked elements...~%" skip))
    (do ((e (svref *first-marked-element* m)
	    (svref (next-marked-element e) m))
	 (i 0 (1+ i)))
	((or (null e) (>= i n))
	 (unless (null e)
	   (format *show-stream* "... and ~D more."
		   (- (fast-marker-count m) n))))
      (declare (fixnum i))
      (if (>= i skip)
	  (format *show-stream* "~S~%" e)))
    (values)))

(defun show-not-marked (m &key
			  (n 100)
			  (all nil)
			  (last nil)
			  (heading nil))
  "Print out a table of all elements in the current context not marked
   with marker M."
  (with-markers (m1)
    (progn
      (mark-boolean m1 () (list m) 0 0)
      (show-marked m1 :n n :all all :last last :heading heading))))    


;;; ========================================================================
(subsection "Mark, List, Show Operations on the Is-A Hierarchy")

(defun mark-children (e m)
  "Mark the immediate descendents of element E."
  (let ((*one-step* t))
    (downscan e m))
  (unmark e m)
  (fast-marker-count m))

(defun list-children (e)
  "List the immediate child nodes of element E."
  (with-markers (m1)
    (progn
      (mark-children e m1)
      (list-marked m1))))

(defun show-children (e &key
			(n 100)
			(all nil)
			(last nil))
  "Show the immediate child nodes of element E."
  (with-markers (m1)
    (progn
      (mark-children e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Immediate Inferiors of ~A:" e)))))

(defun mark-parents (e m)
  "Mark the immediate ancestors of element E."
  (let ((*one-step* t))
    (upscan e m))
  (unmark e m)
  (fast-marker-count m))

(defun list-parents (e)
  "List the immediate parent nodes of element E."
  (with-markers (m1)
    (progn
      (mark-parents e m1)
      (list-marked m1))))

(defun show-parents (e &key
		       (n 100)
		       (all nil)
		       (last nil))
  "Show the immadiate parent nodes of element E."
  (with-markers (m1)
    (progn
      (mark-parents e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Immediate Superiors of ~A:" e)))))

(defun mark-supertypes (e m)
  "Mark the supertypes of element E in the current context with marker M,
   not including E itself."
  (setq e (lookup-element-test e))
  (upscan e m)
  (unmark e m)
  (fast-marker-count m))

(defun list-supertypes (e)
  "E is an element.  Return a list of all the types containing E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-supertypes e m1)
      (list-marked m1))))

(defun show-supertypes (e &key
			  (n 100)
			  (all nil)
			  (last nil))
  "E is an element.  Show all the types containing E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-supertypes e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Supertypes of ~A:" e)))))

(defun mark-superiors (e m)
  "Mark the superiors of element E in the current context with marker M,
   not including E itself."
  (mark-supertypes e m))

(defun list-superiors (e)
  "E is an element.  Return a list of all the superiors of E in the
   type hierarchy in the current context."
  (list-supertypes e))

(defun show-superiors (e &key
			  (n 100)
			  (all nil)
			  (last nil))
  "E is an element.  Show all the superiors of E in the type hierarchy
   in the current context."
  (show-supertypes e :n n :all all :last last))

(defun mark-inferiors (e m)
  "Mark the inferiors of element E in the current context with marker M,
   including both subtypes and individuals, but not E itself."
  (setq e (lookup-element-test e))
  (downscan e m)
  (unmark e m)
  (fast-marker-count m))

(defun list-inferiors (e)
  "E is an element.  Return a list of all the inferiors of E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-inferiors e m1)
      (list-marked m1))))

(defun show-inferiors (e &key
			 (n 100)
			 (all nil)
			 (last nil))
  "E is an element.  Show all the inferiors of E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-inferiors e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Inferiors of ~A:" e)))))

(defun mark-equals (e m)
  "Mark the nodes EQ-linked to element E in the current context with
   marker M, but not E itself."
  (setq e (lookup-element-test e))
  (eq-scan e m)
  (unmark e m)
  (fast-marker-count m))

(defun list-equals (e)
  "E is an element.  Return a list of all the the nodes EQ-linked to
   E, directly or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-equals e m1)
      (list-marked m1))))

(defun show-equals (e &key
		      (n 100)
		      (all nil)
		      (last nil))
  "E is an element.  Show all the elements EQ-linked to E, directly or
   indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-equals e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Equals of ~A:" e)))))

(defun mark-subtypes (e m)
  "Mark the subtypes of element E in the current context with marker M, not
   including any individuals, or E itself."
  (setq e (lookup-element-test e))
  (with-markers (m1)
    (progn
      (downscan e m1)
      (unmark e m1)
      (do-marked (x m1)
	(when (fast-type-node? x)
	  (mark x m)))))
  (fast-marker-count m))

(defun list-subtypes (e)
  "E is an element.  Return a list of all the subtypesof E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-subtypes e m1)
      (list-marked m1))))

(defun show-subtypes (e &key
			(n 100)
			(all nil)
			(last nil))
  "E is an element.  Show all the subtypes of E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-subtypes e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Subtypes of ~A:" e)))))

(defun mark-instances (e m)
  "Mark the instances of element E in the current context with marker M."
  (setq e (lookup-element-test e))
  (with-markers (m1)
    (progn
      (downscan e m1)
      (unmark e m1)
      (do-marked (x m1)
	(when (or (fast-proper-indv-node? x)
		  (fast-statement? x))
	  (mark x m))))
    (fast-marker-count m)))

(defun list-instances (e)
  "E is an element.  Return a list of all the instances of E in the current
   context."
  (with-markers (m1)
    (progn
      (mark-instances e m1)
      (list-marked m1))))

(defun show-instances (e &key
			 (n 100)
			 (all nil)
			 (last nil))
  "E is an element.  Show all the instances of E, directly
   or indirectly, in the current context."
  (with-markers (m1)
    (progn
      (mark-instances e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Instances of ~A:" e)))))

(defun list-intersection (superiors)
  "List all the elements in the intersection of the types in the SUPERIORS list."
  (with-markers (m1)
    (progn
      (mark-intersection superiors m1)
      (list-marked m1))))

(defun show-intersection (superiors &key
				    (n 100)
				    (all nil)
				    (last nil))
  "Show all the elements in the intersection of the types in the SUPERIORS list."
  (with-markers (m1)
    (progn
      (mark-intersection superiors m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "In intersection of ~A:" superiors)))))

(defun mark-lowermost (m1 m2 &key (augment nil))
  "From the set of elements currently marked with M1, look for those that
   have no immediate descendants marked with M1.  Mark these with M2."
  (check-legal-marker-pair m1)
  (check-legal-marker-pair m2)
  (unless augment (clear-marker-pair m2))
  (do-marked (x m1)
    (when (lowermost? x m1)
      (fast-mark x m2)))
  (fast-marker-count m2))

(defun list-lowermost (m)
  "From the set of elements currently marked with M, list the ones that are
   lowermost in the type hierarchy."
  (with-markers (m1)
    (progn
      (mark-lowermost m m1)
      (list-marked m1))))

(defun show-lowermost (m &key
			 (n 100)
			 (all nil)
			 (last nil))
  "From the set of elements currently marked with M, showthe ones that are
   lowermost in the type hierarchy."
  (with-markers (m1)
    (progn
      (mark-lowermost m m1)
      (show-marked
       m1 :n n :all all :last last
       :heading "Lowermost marked elements:"))))

(defun mark-uppermost (m1 m2 &key (augment nil))
  "From the set of elements currently marked with M1, look for those that
   have no parents marked with M1.  Mark these with M2."
  (check-legal-marker-pair m1)
  (check-legal-marker-pair m2)
  (unless augment (clear-marker-pair m2))
  (do-marked (x m1)
    (when (uppermost? x m1)
      (fast-mark x m2)))
  (fast-marker-count m2))

(defun list-uppermost (m)
  "From the set of elements currently marked with M, list the ones that are
   uppermost in the type hierarchy."
  (with-markers (m1)
    (progn
      (mark-uppermost m m1)
      (list-marked m1))))

(defun show-uppermost (m &key
			 (n 100)
			 (all nil)
			 (last nil))
  "From the set of elements currently marked with M, show the ones
   that are uppermost in the type hierarchy."
  (with-markers (m1)
    (progn
      (mark-uppermost m m1)
      (show-marked
       m1 :n n :all all :last last
       :heading "Uppermost marked elements:"))))

;;; These mark/list/show functions find violations of splits or
;;; complete splits in the network -- that is, situations where some
;;; element is below two different branches of the split or proposed
;;; split.

;;; Normally these situations will never occur if the network is built
;;; in the customary order, in which new splits are created before the
;;; various types being split are populated with subtypes and
;;; instances.  Split violation is tested when each new is-a link is
;;; added to the network.  However, importing KBs from other sources
;;; can result in scrambling the order in which splits are created, so
;;; we need to test for existing violations at the time when we create
;;; a new split, and perhaps at other times.

(defun mark-split-violations (s m &key (augment nil))
  "S is either a split-element or a list of nodes that we plan to
   split. Check whether there are any elements that violate that
   split.  That is, are there any nodes (type or individual) in in the
   currently-active context that are inferiors of more than one branch
   of the split.  Mark all violators with marker M.

   If :AUGMENT is T, add to the current M-marked set rather than
   clearing M at the start.

   Return the number of nodes that are marked with M."
  ;; Check and clear marker M.
  (check-legal-marker-pair m)
  (unless augment (clear-marker-pair m))
  ;; Turn S into a list of elements that are or will be split,
  ;; unless it is a list already.
  (unless (listp s)
    (setq s (lookup-element-test s))
    (unless (fast-split? s)
      (error "~S is not a split." s))
    (setq s (split-wires s)))
  ;; Only do this check if there are at least two elements in the split-set.
  (when (>= (length s) 2)
    ;; Get markers needed for this search.
    (with-markers (m1 m2 m3)
      (progn
	;; Mark all nodes below the first element with M1.
	(downscan (car s) m1)
	;; Loop over all other elements E in S looking for a problem.
	(dolist (e (cdr s))
	  ;; Mark all elements below E with M2.
	  (downscan e m2)
	  ;; For all elements with M1 and M2 set, add M3.
	  (mark-boolean m3 (list m1 m2) ())
	  ;; Turn all M2 markers into M1.  We test subsequent Es against
	  ;; ALL previously-marked nodes.  And then, iterate down the
	  ;; list.
	  (convert-marker m2 m1))
	;; Convert the uppermost M3 markers (if any) to M markers.
	(mark-uppermost m3 m :augment t))
      (commentary
       "Not enough markers available, unable to check for existing split violators.")))
  ;; Return the marker-count for M.
  (marker-count m))

(defun list-split-violations (s)
  "S is either a split-element or a list of member-elements for a
   proposed split.  Return a list of all uppermost elements that
   violate this split in the current context.  These are elements that
   appear under more than one of types being split."
  (with-markers (m)
    (progn
      (mark-split-violations s m)
      (list-marked m))))

(defun show-split-violations (s &key
			      (n 100)
			      (all nil)
			      (last nil))
  "S is either a split-element or a list of member-elements for a
   proposed split.  Sho all the uppermost elements that violate this
   split in the current context.  These are elements that appear under
   more than one of types being split."
  (with-markers (m)
    (progn
      (mark-split-violations s m)
      (if (= 0 (marker-count m))
	  (format t "No split violations. ~%")
	  (show-marked
	   m :n n :all all :last last
	   :heading (format nil "Split violations under ~A:" s))))))

(defun mark-all-split-violations (m)
  "Scan all splits in the current context.  For each, do MARK-SPLIT-VIOLATIONS
   with AUGMENT to mark all uppermost violators in this context, if any.  Return
   the number of M-marked nodes."
  (check-legal-marker-pair m)
  (clear-marker m)
  ;; Get a marker MS and mark all the split elements in the current context.
  (with-markers (ms)
    (progn
      (downscan *split* ms)
      ;; For each MS-marked split X, do MARK-SPLIT-VIOLATIONS, leaving marker
      ;; M on all uppermost violators.  Do this with AUGMENT NIL so that we
      ;; collect violators from all the splits.
      (do-marked (x ms)
	(when (split? x)
	  (mark-split-violations x m :augment t)))
      ;; Return the marker-count for M.  If all is well, this is 0.
      (marker-count m))
    (commentary
     "Not enough markers available, unable to check for all split violations.")))

(defun list-all-split-violations ()
  "Return a list of all uppermost elements that violate any split in
   the current context. If NIL, no violations exist."
  (with-markers (m)
    (progn
      (mark-all-split-violations m)
      (list-marked m))))

(defun show-all-split-violations (s &key
			      (n 100)
			      (all nil)
			      (last nil))
  "Show all the uppermost elements that violate any
   split in the current context, if any violations exist."
  (with-markers (m)
    (progn
      (mark-all-split-violations m)
      (if (= 0 (marker-count m))
	  (format t "No split violations. ~%")
	  (show-marked
	   m :n n :all all :last last
	   :heading (format nil "Split violations under ~A:" s))))))



;;; %%% DEPRECATE OR FIX THESE. We need to find a more intuitive way to
;;; separate useful answers from non-useful ones.

(defun good-answer? (e)
  "Given an element E, determine if it would be a good answer to a
   query such as THE-X-OF-Y.  In general, we exclude roles and defined
   types or individuals."
  (lookup-element e)
  (not (or (fast-map-node? e)
	   (fast-role-node? e)
	   (fast-defined-type? e))))

(defun any-good-answer? (m)
  "From the set of elements marked with M, find any element that
   satisfies the GOOD-ANSWer? predicate and return it.  If none
   exist, return nil."
  (do-marked (e m)
    (when (good-answer? e)
      (return-from any-good-answer? e))))
  
(defun list-good-answers (m)
  "From the set of nodes currently marked with M, list the ones that
   satisfy GOOD-ANSWER?"
  (let ((answers nil))
    (do-marked (x m)
      (when (good-answer? x)
	(push x answers)))
    answers))

(defun mark-answers (m1 m2 &key strict)
  "Scan a set of potential answers, currently marked with M1.  For
   each equivalent group, mark one with M2, preferring those that
   satisfy the GOOD-ANSWER? predicate.  If :strict, accept ONLY good
   answers.  The selected elements are marked with M2."
  (check-legal-marker-pair m1)
  (check-legal-marker-pair m2)
  (clear-marker-pair m2)
  (with-markers (m-eq m-done)
    (do-marked (e m1)
      (unless (fast-marker-on? e m-done)
	(eq-scan e m-eq)
	(let ((winner nil))
	  (do-marked (x m-eq)
	    (when (good-answer? x)
	      (setq winner x)))
	  (cond (winner
		 (fast-mark winner m2))
		((not strict)
		 (fast-mark e m2)))
	  (convert-marker m-eq m-done)))))
  (fast-marker-count m2))

(defun mark-uppermost-answers (m1 m2 &key strict)
  "From the set of elements marked with M1, find the uppermost good
   answers in this set and mark them with M2.  If :strict, accept ONLY
   good answers, else return the best in each set."
  (with-markers (m-upper)
    (progn
      (mark-uppermost m1 m-upper)
      (mark-answers m-upper m2 :strict strict))))

(defun mark-lowermost-answers (m1 m2 &key strict)
  "From the set of elements marked with M1, find the lowermost good
   answers in this set and mark them with M2.  If :strict, accept ONLY
   good answers, else return the best in each set."
  (with-markers (m-lower)
    (progn 
      (mark-lowermost m1 m-lower)
      (mark-answers m-lower m2 :strict strict))))

(defun acceptable-role-filler (m)
  "From the set of nodes currently marked with M, find the one that
   would serve best as an answer for THE-X-OF-Y and similar.one that
   is acceptable as an answer.  Currently we accept proper
   individuals, types, and statements. If more than one, pick one at
   random.  If none, return NIL."
  (do-marked (x m)
    (when (or (fast-proper-indv-node? x)
	      (fast-type-node? x)
	      (fast-statement? x))
      (return-from acceptable-role-filler x))))

(defun mark-proper (m1 m2)
  "From the set of elements currently marked with M1, look for the lowermost
   elements that are proper individuals.  Mark these with M2."
  (check-legal-marker-pair m1)
  (check-legal-marker-pair m2)
  (clear-marker-pair m2)
  (do-marked (x m1)
    (when (and (lowermost? x m1)
	       (fast-proper-indv-node? x))
      (fast-mark x m2)))
  (fast-marker-count m2))

(defun list-proper (m)
  "From the set of elements currently marked with M, list any proper
   individual nodes that are lowermost in the type hierarchy."
  (with-markers (m1)
    (progn
      (mark-proper m m1)
      (list-marked m1))))

(defun show-proper (m &key
		      (n 100)
		      (all nil)
		      (last nil))
  "From the set of elements currently marked with M, show any proper
   individual nodes that are lowermost in the type hierarchy."
  (with-markers (m1)
    (progn
      (mark-proper m m1)
      (show-marked
       m1 :n n :all all :last last))))

(defun mark-most-specific (m1 m2)
  "From the set of elements currently marked with M1, mark with M2 the ones
   that are the most specific.  That is, mark any proper individual nodes
   and, if none of those are present, the lowermost type or generic nodes."
  (check-legal-marker-pair m1)
  (check-legal-marker-pair m2)
  (clear-marker-pair m2)
  (when (> (fast-marker-count m1) 0)
    (with-markers (m3)
      (progn
	(mark-lowermost m1 m3)
	(cond
	  ((> (mark-boolean m2 (list m3) () proper-indv-mask 0) 0))
	  (t (mark-boolean m2 (list m3) nil))))))
  (fast-marker-count m2))


(defun list-most-specific (m)
  "From the set of elements currently marked with M1, list the ones that
   are the most specific.  That is, any named individual nodes and, if none
   of those are present, lowermost type or generic nodes."
  (with-markers (m1)
    (progn
      (mark-most-specific m m1)
      (list-marked m1))))

(defun show-most-specific (m &key
                             (n 100)
                             (all nil)
                             (last nil)
			     heading)
  "From the set of elements currently marked with M, print the ones that
   are the most specific.  That is, any named individual nodes and, if none
   of those are present, lowermost type or map nodes."
  (with-markers (m1)
    (progn
      (mark-most-specific m m1)
      (show-marked m1 :n n :all all :last last :heading heading))))

;;; END DEPRECATE.

;;; ========================================================================
(subsection "Miscellaneous Show Functions")

(defun show-element-counts ()
  "Print a table showing the number of elements of each type."
  (let ((n-loaded-elements 0)
	(n-number 0)
	(n-string 0)
	(n-function 0)
	(n-struct 0)
	(n-proper-indv 0)
	(n-generic-indv 0)
	(n-indv 0)
	(n-type 0)
	(n-indv-map 0)
	(n-type-map 0)
	(n-is-a 0)
	(n-eq 0)
	(n-has 0)
	(n-cancel 0)
	(n-relation 0)
	(n-statement 0)
	(n-complete-split 0)
	(n-other-split 0)
	(n-negation 0)
	(i 0))
    (do-elements (e)
      (incf i)
      (when (eq e (car *last-loaded-elements*))
	(setq n-loaded-elements i))
      (cond
       ((fast-indv-map-node? e)
	(incf n-indv-map))
       ((fast-type-map-node? e)
	(incf n-type-map))
       ((fast-number-node? e)
	(incf n-number)
	(incf n-indv))
       ((fast-string-node? e)
	(incf n-string)
	(incf n-indv))
       ((fast-function-node? e)
	(incf n-function)
	(incf n-indv))
       ((fast-struct-node? e)
	(incf n-struct)
	(incf n-indv))
       ((fast-proper-indv-node? e)
	(incf n-proper-indv)
	(incf n-indv))
       ((fast-generic-indv-node? e)
	(incf n-generic-indv)
	(incf n-indv))
       ((fast-type-node? e)
	(incf n-type))
       ((fast-is-a-link? e)
	(incf n-is-a))
       ((fast-eq-link? e)
	(incf n-eq))
       ((fast-has-link? e)
	(incf n-has))
       ((fast-cancel-link? e)
	(incf n-cancel))
       ((fast-relation? e)
	(incf n-relation))
       ((fast-statement? e)
	(incf n-statement))
       ((fast-is-not-a-link? e)
	(incf n-negation))
       ((fast-not-eq-link? e)
	(incf n-negation))
       ((fast-has-no-link? e)
	(incf n-negation))
       ((fast-not-statement? e)
	(incf n-negation))
       ((fast-complete-split? e)
	(incf n-complete-split))
       ((fast-split? e)
	(incf n-other-split))))
    (format *show-stream*
"~&Knowledge Base Element Counts
	       
Elements: ~20T~10:D
New Elements: ~20T~10:D

Indv Nodes: ~20T~10:D
  Number Nodes: ~20T~10:D
  String Nodes: ~20T~10:D
  Function Nodes: ~20T~10:D
  Struct Nodes: ~20T~10:D
  Proper Indv: ~20T~10:D
  Generic Indv: ~20T~10:D

Type Nodes: ~20T~10:D

Indv Map Nodes: ~20T~10:D
Type Map Nodes: ~20T~10:D

Is-A Links: ~20T~10:D
EQ Links: ~20T~10:D
Has Links: ~20T~10:D
Cancel Links: ~20T~10:D
Relations: ~20T~10:D
Statements: ~20T~10:D

Comp Splits: ~20T~10:D
Other Splits: ~20T~10:D

Negations: ~20T~10:D

English Names: ~20T~10:D
"
	    *n-elements*
	    (- *n-elements* n-loaded-elements)
	    n-indv
	    n-number
	    n-string
	    n-function
	    n-struct
	    n-proper-indv
	    n-generic-indv
	    n-type
	    n-indv-map
	    n-type-map
	    n-is-a
	    n-eq
	    n-has
	    n-cancel
	    n-relation
	    n-statement
	    n-complete-split
	    n-other-split
	    n-negation
	    (hash-table-count *english-dictionary*))
  (values)))

(defun element-counts ()
  "Deprecated synonm for SHOW-ELEMENT-COUNTS."
  (show-element-counts))

(defun show-marker-counts ()
  "Prints the number of elements marked with each marker."
  (format *show-stream* "~&Marker counts:~%")
  (dotimes (i n-markers)
    (format *show-stream* "~2D: ~D~%" i (fast-marker-count i)))
  (values))

(defun marker-counts ()
  "Deprecated synonym for SHOW-MARKER-COUNTS."
  (show-marker-counts))

(defun show-ontology (&key (under *thing*) (n -1))
  "Print every node in the ontology.  This output is designed to be
   read by humans, not programs.  If :UNDER keyword is supplied with a
   Scone-element as its argument, print only those elements under this
   element in the type hierarnchy.  If :N is supplied and non-nil,
   stop after N elements are printed."
  ;; Initialize counter.
  (setq *show-ontology-counter* n)
  ;; Put marker M on every node we print, so that we don't recurse down from
  ;; it again.
  (setq under (lookup-element-test under))
  (with-markers (m)
    (show-ontology1 under 0 m)))

(defun show-ontology1 (under indent m)
  "Internal function used by SHOW-ONTOLOGY.  This does the actual printing
   of the under node, then recurses."
  ;; If UNDER is not a node, or if N hits 0, print nothing and return.
  (when (and (not (= *show-ontology-counter* 0))
	     (or (fast-type-node? under)
		 (fast-proper-indv-node? under)))
    ;; Print the label and UNDER node.
    (decf *show-ontology-counter*)
    (format
     *show-stream*
     "~&~VT~A{~A}~A "
     indent
     (cond ((fast-type-node? under) "Type: ")
	   ((fast-proper-indv-node? under) "Indv: "))
     (internal-name under)
     (if (marker-on? under m) "*" ""))
    ;; Print all the English names.
    (dolist (x (get-english-names under))
      (unless (eq (car x) (internal-name under))
	(format *show-stream* " ~S" (car x))))
    ;; If UNDER has not been visited before, mark it and recurse.
    (unless (marker-on? under m)
      (let ((next-indent (+ indent *show-ontology-indent*)))
	(mark under m)
	(dolist (x (nreverse (list-children under)))
	  (show-ontology1 x next-indent m))))))

;;; ************************************************************************
(section "Operations on Roles, Relations, and Statements")
;;; ************************************************************************

;;; These operations must be as fast as possible.
(declaim (optimize (speed 3) (space 0) (safety 0)))

;;; ========================================================================
(subsection "Access Functions")

(defun mark-the-x-of-y (x y m)
  "Mark with M all elements E that collectively rerpesent the X
   of Y.  Do not propagate M down to subtypes and instances of this
   role."
  (mark-role x y m :downscan nil))

(defun mark-all-x-of-y (x y m)
  "Mark with M all elements E such that E is an X of Y."
  (mark-role x y m :downscan t))

(defun the-x-of-y (x y)
  "Find the most specific good-answer node representing the X of Y,
   and return that node if it exists.  Else return NIL."
  (with-markers (m1 m2)
    (progn
      (mark-role x y m1 :downscan nil)
      (mark-most-specific m1 m2)
      (or (any-good-answer? m2)
          ;; If no good answer, try running rule engine and
          ;; check for good answers again.
          (progn
            (check-rule-x-of-y x y)
            (mark-role x y m1 :downscan nil)
            (mark-most-specific m1 m2)
            (any-good-answer? m2))))))

(defun list-all-x-of-y (x y)
  "Return a list with all X of Y."
  (with-markers (m)
    (progn
      (mark-role x y m :downscan t)
      (list-most-specific m))))

(defun show-all-x-of-y (x y &key (n 100) (all nil) (last nil))
  "Print out all the X of Y."
  (with-markers (m)
    (progn
      (mark-role x y m :downscan t)
      (show-most-specific m :n n :all all :last last))))

(defun mark-the-x-inverse-of-y (x y m)
  "Mark with M all elements E such that the X of E is Y.  Do not
   propagate M down to subtypes and instances of this role."
  (mark-role-inverse x y m :downscan nil))

(defun mark-all-x-inverse-of-y (x y m)
  "Mark with M all elements E such that the X of E is Y."
  (mark-role-inverse x y m :downscan t))

(defun the-x-inverse-of-y (x y)
  "Find the most specific proper node E such that the X of E is Y.  If
   no such E exists, return NIL.  If there are more than one E, return
   one of them."
  (with-markers (m1 m2)
    (progn
      (mark-role-inverse x y m1 :downscan nil)
      (mark-most-specific m1 m2)
      (acceptable-role-filler m2))))

(defun list-all-x-inverse-of-y (x y)
  "Return a list with all nodes E such that the X of E is Y."
  (with-markers (m)
    (progn
      (mark-role-inverse x y m :downscan t)
      (list-most-specific m))))

(defun show-all-x-inverse-of-y (x y &key (n 100) (all nil) (last nil))
  "Print out all nodes E such that the X of E is Y."
  (with-markers (m)
    (progn
      (mark-role-inverse x y m :downscan t)
      (show-most-specific m :n n :all all :last last))))

(defun mark-roles (e m1)
  "Mark all the roles defined for element E, whether defined directly or by
   inheritance."
  (setq e (lookup-element-test e))
  (check-legal-marker-pair m1)
  (clear-marker-pair m1)
  (with-markers (m2)
    (progn
      (upscan e m2)
      ;; Scan all marked nodes, looking for attached roles.
      (do-marked (x m2)
	;; Look for role-relations connected to the marked nodes.
	(dolist (y (incoming-context-wires x))
	  (when (fast-role-node? y)
	    (mark y m1))))))
  (fast-marker-count m1))

(defun list-roles (e)
  "List all the roles attached to element E in the current context,
   directly or by inheritance."
  (with-markers (m1)
    (progn
      (mark-roles e m1)
      (list-marked m1))))

(defun show-roles (e &key
                     (n 100)
                     (all nil)
                     (last nil))
  "Print out the roles attached to element E in the current context.
   Uses TEMP-MARKER to mark the owner.  Leaves RESULT-MARKER on the
   role-nodes actually selected."
  (with-markers (m1)
    (progn
      (mark-roles e m1)
      (show-marked
       m1 :n n :all all :last last
       :heading (format nil "Roles defined for ~A:" e)))))

(defun the-x-role-of-y (x y)
  "Find and return the role-node or map-node directly representing the
   X-role of Y.  If this does not exist, create it."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (if (eq (context-wire x) y)
      ;; X itself is the desired role.
      x
      (or
       ;; Look for a map of X.
       (with-markers (m)
	 (progn
	   ;; Mark the X of Y with M.
	   (mark-role x y m :downscan nil)
	   ;; Scan M-marked nodes looking for the map-node we want.
	   (dolist (node (incoming-context-wires y))
	     (when (and (fast-marker-on? node m)
			(fast-map-node? node))
	       (return-from the-x-role-of-y node)))))
       ;; Node doesn't exist yet.  Create and return it.
       (new-map x y))))


;;; ========================================================================
(subsection "Predicates")

(to-do
  "Maybe replace this with a more efficient version that uses a
   modified upscan for A and B.  Also, it is useful if this function
   returns a second value, which is the specific REL we should cancel
   if we want to cancel this.")


(defun statement-true? (a rel b)
  "Predicate to determine if there is a REL relationship between elements A
   and B."
  (multiple-value-bind (element tag)
      (lookup-element-test rel)
    (setq rel element)
    ;; If the REL argument is an english name with an
    ;; :INVERSE-RELATION tag, use the relation element but flip the
    ;; arguments around.
    (when (eq tag :inverse-relation)
      (rotatef a b)))
  (setq a (lookup-element-test a))
  (setq b (lookup-element-test b))
  (with-markers (m)
    (progn
      (mark-rel rel a m)
      (marker-on? b m))))

(defun can-x-have-a-y? (x y)
  "Predicate to determine whether element X can have a Y role in the
   current context.  That is, return T if Y is a role of X or some node
   above X, else NIL."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (with-markers (m1)
    (progn
      (upscan x m1)
      (and (context-wire y)
	   (marker-on? (context-wire y) m1)))))

(defun can-x-be-the-y-of-z?  (x y z)
  "Predicate to determine whether it would be legal to state that X is the
   Y of Z.  That is, would any splits or other constraints be violated?  If
   it is OK, return T.  If it is not, return NIL with the unhappy split or
   constraint as the second return value.  If there are multiple problems,
   we just return the first one we hit."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (setq z (lookup-element-test z))
  (if (can-x-have-a-y? z y)
      (let ((problem (incompatible? x (the-x-role-of-y y z))))
	(if problem (values nil problem) t))
      (values nil :role-undefined)))

(defun can-x-be-a-y-of-z?  (x y z)
  "Predicate to determine whether it would be legal to state that X is a Y
   of Z.  That is, would any splits or other constraints be violated?  If
   it is OK, return T.  If it is not, return NIL with the unhappy split or
   constraint as the secodn return value.  If there are multiple problems,
   we just return the first one we hit."
  (can-x-be-the-y-of-z? x y z))

;;; ========================================================================
(subsection "Adding New Roles or Fillers")

(defun x-is-the-y-of-z (x y z)
  "If it is legal for X to fill the Y-role of Z, create a map-node and
   EQ-link to make it so.  Return the link."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (setq z (lookup-element-test z))
  (unless (or *no-kb-error-checking*
	      (can-x-be-the-y-of-z? x y z))
    (unless (and *deduce-owner-type-from-role*
		 (fixup-role-filler x y z))
      (commentary
       "~S cannot be the ~S of ~S.  Continuing..." x y z)
      (return-from x-is-the-y-of-z nil)))
  (let ((eq-link (new-eq x (the-x-role-of-y y z))))
    (when eq-link
      ;; Check if any rules are now satisfied.
      (check-rule-x-y-z x y z))
    eq-link))

(defun fixup-role-filler (x y z)
  "We want to add a value X to the Y role of Z, but Z does not have a
   Y.  See if we can fix this by creating an is-a link.  If so, do it.
   If not, return nil."
  (let ((y-owner))
    (when (and (role-node? y)
	       (setq y-owner (context-wire y))
	       (not (incompatible? x y))
	       (not (incompatible? z y-owner)))
      (commentary "Assuming ~S is a ~S because it has a ~S." z y-owner y)
      (new-is-a z y-owner))))

(defun x-is-a-y-of-z (x y z)
  "Y of Z should be a type-role.  If it is legal for X to be of this type,
   create and return a statement-link that makes it so."
  (setq x (lookup-element-or-defer x))
  (setq y (lookup-element-test y))
  (setq z (lookup-element-test z))
  (unless (or *no-kb-error-checking*
	      (eq x :create)
	      (can-x-be-a-y-of-z? x y z))
    (unless (and *deduce-owner-type-from-role*
		 (fixup-role-filler x y z))
      (commentary
       "~S cannot be a ~S of ~S.  Continuing..." x y z)
      (return-from x-is-a-y-of-z nil)))
  (let ((is-a-link (new-is-a x (the-x-role-of-y y z))))
    (when is-a-link
      ;; Check if any rules are now satisfied.
      (check-rule-x-y-z x y z))
    is-a-link))

(defun the-x-of-y-is-a-z (x y z)
  "Find or create the node for the X of Y.  Then, if it is legal to do so,
   add a new IS-A link from this node to Z."
  (setq x (lookup-element-test x))
  (setq y (lookup-element-test y))
  (setq z (lookup-element-test z))
  (let ((role-node (the-x-role-of-y x y)))
    (new-is-a role-node z)))

;;; Negated versions.

(defun x-is-not-the-y-of-z (x y z)
  "Create a negated EQ between X and the Y of Z"
  (new-not-eq x (the-x-role-of-y  y z)))

(defun x-is-not-a-y-of-z (x y z)
  "Y of Z should be a type-role. Create a not-is-a link between X and
   Y of Z."
  (new-is-not-a x (the-x-role-of-y y z)))

(defun the-x-of-y-is-not-a-z (x y z)
  "Find or create the node for the X of Y. Then, add a new IS-NOT-A link
   from this node to Z"
  (new-is-not-a (the-x-role-of-y x y) z))

;;; ========================================================================
(subsection "List and Show Functions on Relations")

(defun list-rel (rel a &key (downscan t))
  "List all individual elements B for which A REL B is true."
  (with-markers (m1)
    (progn
      (mark-rel rel a m1 :downscan downscan)
      (list-most-specific m1))))

(defun show-rel (rel a
		     &key
		     (n 100)
		     (all nil)
		     (last nil)
		     (downscan t))
  "Show all individual elements B such that A REL B."
  (with-markers (m1)
    (progn
      (mark-rel rel a m1 :downscan downscan)
      (show-most-specific
       m1 :n n :all all :last last
       :heading (format nil "Individual X such that ~A ~A X is true."
			a rel)))))

(defun list-rel-inverse (rel b &key (downscan t))
  "List all individual elements A for which A REL B is true."
  (with-markers (m1)
    (progn
      (mark-rel-inverse rel b m1 :downscan downscan)
      (list-most-specific m1))))

(defun show-rel-inverse (rel b
                             &key
                             (n 100)
                             (all nil)
                             (last nil)
			     (downscan t))
  "Show all individual elements A such that A REL B."
  (with-markers (m1)
    (progn
      (mark-rel-inverse rel b m1 :downscan downscan)
      (show-most-specific
       m1 :n n :all all :last last
       :heading (format nil "Individual X such that X ~A ~A is true."
			rel b)))))


;;; ***************************************************************************
(section "Cardinality Functions")
;;; ***************************************************************************

(to-do
 "Implement the cardinality functions.")

#|

(defun does-x-have-a-y? (x y)
  "Predicate to determine whether element X definitely has a Y role in the
   current context.  This is indicated by an explicit {exists for}
   statement from Y to X, either direct or inherited.  Y must be a
   role-node.  If true, return three values: T, the {exists for} statement,
   and the C-node of the link, representing the number of Ys per X.  The
   C-node may be NIL, indicating that we know nothing about the
   cardinality.  If X has no Y, we just return NIL."
  ;; Easy, because a has-link is just a statement.
  (statement-true? x *has-link* y))

|#

;;; ***************************************************************************
(section "Element Access")
;;; ***************************************************************************

(subsection "Internal Element Names and Namespaces")

;;; The syntax {iname} designates the internal name of a Scone element.

;;; If the open-curly-brace is immediately followed by a string, as in
;;; {"Santo Domingo"}, this designates a STRING element. If a
;;; string-element representing the identical string already exists,
;;; it will be returned and no new element will be created.

;;; Similarly, if the curly braces surround a Lisp number, function,
;;; or defstruct, we create the corresponding NUMBER or FUNCTION or
;;; STRUCT.  Again, an existing element with the same value may be
;;; returned instead.

;;; Otherwise, we check whether the named element already exists, and
;;; return it if so.  If the element doesn't yet exist, we create and
;;; return an ELEMENT-INAME structure.

;;; If the iname string contains a colon, the part before the colon
;;; identifies the namespace, while the part after the colon is the
;;; element's name, which must be unique within its namespace.

;;; If the string does not contain a colon, we assume that the namespace is
;;; the one specified by *namespace*, which is set up by the IN-NAMESPACE
;;; function.

;;; Internal names are matched in a case-insensitive way, but the case of
;;; the internal name string is preserved for more attractive printing.
;;; Exception: Internal names of STRING nodes are always case sensitive.
;;; Strings identifying namespaces are always case-insensitive.

;;; NOTE: For efficiency, the ELEMENT-INAME structure was defined earlier,
;;; just after ELEMENT itself.

;;; Use normal compilation policy, where safety counts as much as speed.

(declaim (optimize (speed 1) (space 1) (safety 1)))

(defun print-element-iname (e stream depth)
  "The ELEMENT-INAME structure prints in the original curly-brace
   notation."
  (declare (stream stream)
	   (ignore depth))
  (format stream "{~A}" (element-iname-value e)))

(to-do "Figure out a dump/reload format for structure elements.")

(defun read-element (s c)
  "This function implements the character macro for
   open-curly-brace. It reads the contents of the curly baraces and
   produces a structure of type ELEMENT-INAME with the appropriate
   type and value.  This is later converted into an element.  TYPE is
   one of :ELEMENT, :STRING, :NUMBER, or :FUNCTION.  The value is
   either the name-string of a normal element (may contain a namespace
   and colon) or, for primitive elements, the printed form of a Lisp
   object."
  (declare (ignore c))
  (if (char= (peek-char t s) #\")
      ;; If the first char after the curly-open-bracket is a
      ;; double-quote, this is a string node.  Read the string itself,
      ;; then create the element-iname structure of type :STRING.
      (let ((string (read s))
	    (next-char (read-char s)))
	;; If there is anything after the string but before the
	;; close-brace, signal an error.
	(if (char= next-char #\} )
	    (make-element-iname :type :string
				:value string)
	    (error "Ill-formed String element.")))
      ;; It's not a string.  Read in everything up to the closing brace
      ;; into a string.
      (let* ((char-list nil)
	     (string nil))
	(do ((char (read-char s) (read-char s)))
	    ((char= char #\} )
	     (setq string (coerce (nreverse char-list) 'string)))
	  (push char char-list))
	;; If the first two characters are sharp-quote, it's a function
	;; element.  Read the function name or lambda expression.
	(cond ((and (char= (aref string 0) #\# )
		    (char= (aref string 1) #\' ))
	       (make-element-iname :type :function
				   :value (eval (read-from-string string))))
	      ;; If there's a colon in the string, it's a vanilla element.
	      ((position #\: string)
	       (return-from read-element
		 (make-element-iname :type :element
				     :value string)))
	      (t
	       ;; Now it's safe to try a Lisp READ of the string contents.
	       ;; This is the easiest way to see if we have a legal number.
	       (multiple-value-bind (value end)
		   (read-from-string string)
		 (cond
		   ;; If there's extraneous stuff in the string, it's
		   ;; a vanilla element.
		   ((not (= end (length string)))
		    (make-element-iname :value string))
		   ;; Maybe it's a number element.
		   ((typep value 'number)
		    (make-element-iname :type :number
					:value value))
		   ;; None of the above.  Just a simple element.
		   (t (make-element-iname :value string)))))))))

(set-macro-character #\{
		     #'read-element)

(defun lookup-element (name &key syntax-tags)
  "Given an element name, look up and return the corresponding
   element.  If the element doesn't yet exist, return NIL.  In the
   case of a primitive element type (string, number, function,
   structure), create it.

   If we are given a string, we look this up as the English name of an
   element, which may require disambiguation.  :SYNTAX-TAGS, if
   present, is a list of tags that we will consider in this lookup."
  (typecase name
    ;; If it's already an element, return it.
    (element name)
    ;; If it's an iname-structure, look up the element, or create it
    ;; if it is a primitive element representing a Lisp object.
    (element-iname
     (lookup-element-iname name))
    ;; If it's a string, look it up as an English word or phrase.
    (string
     (let ((defs (lookup-definitions name syntax-tags)))
       (cond ((null defs) nil)
	     ((null (cdr defs))
	      (values (car (car defs))
		      (cdr (car defs))))
	     (t (disambiguate name defs
			      syntax-tags)))))
    ;; If it's a number or function, create and return 
    ;; the element.
    (number (new-number name))
    (function (new-function name))
    (structure-object (new-struct name))))

(defun lookup-element-iname (name)
  "This does the work for LOOKUP-ELEMENT for the case
   where the item being looked up is known to be an element-iname."
  (let* ((type (element-iname-type name))
	 (value (element-iname-value name))
	 (colon-pos)
	 (namespace-name)
	 (namespace)
	 (iname-string))
    (case type
      (:element
       (if (setq colon-pos  (position #\: value))
	   ;; It's an iname with an explicit namespace, followed by a
	   ;; colon.  Pick it apart and look it up. Return NIL on failure.
	   (progn 
	     (setq namespace-name  (subseq value 0 colon-pos))
	     (setq namespace
		   (gethash (string-upcase namespace-name) *namespaces*))
	     (when namespace
	       (setq iname-string (subseq value (+ colon-pos 1) nil))
	       ;; Return the iname if it exists in the specified
	       ;; namespace.
	       (lookup-string-in-namespace iname-string namespace)))
	   ;; It's a vanilla iname.  Look it up in default namespace.
	   (lookup-string-in-namespace value *namespace*)))
      (:string (new-string value))
      (:function (new-function value))
      (:number (new-number value)))))

(defun lookup-element-or-defer (name &key syntax-tags)
  "Given an element name, look up and return the corresponding element.  If
   the element doesn't yet exist, return the argument.  If we are given a
   string, we look this up as an English name.  :SYNTAX-TAGS, if present,
   is a list of tags that we will consider in this lookup."
  (multiple-value-bind (element tag)
      (lookup-element name :syntax-tags syntax-tags)
    (if element
	(values element tag)
	name)))

(defun lookup-element-test (name &key syntax-tags)
  "Given an element name, look up and return the corresponding element.  If
   the element doesn't yet exist, signal an error.  If we are given a
   string, we look this up as an English name.  :SYNTAX-TAGS, if present,
   is a list of tags that we will consider in this lookup."
  (multiple-value-bind (element tag)
      (lookup-element name :syntax-tags syntax-tags)
    (cond (element
	   (values element tag))
	  ;; If the element is not yet defined, but
	  ;; *CREATE-UNDEFINED-ELEMENTS* is T, create the element on the
	  ;; fly as a type-node with parent {thing}.
	  ((and *create-undefined-elements*
		(not *defer-unknown-connections*)
		(typep name '(or element-iname string)))
	   (when *comment-on-element-creation*
	     (commentary
	      "~S is unknown.  Defining it as a type under {undefined thing}."
	      name))
	   (setq element (new-type name *undefined-thing*)))
	  ;; It's not an element and we didn't create one.  Signal an
	  ;; error.
	  (t (error "Element ~S does not exist." name)))))

(defun extract-namespace (string)
  "Return the part of STRING before the colon, or nil if there is no colon."
  (let ((pos (position #\: string)))
    (if pos
	(subseq string 0 pos)
	nil)))

(defun extract-name (string)
  "Return the part of the string after the colon, if it
   exists.  Otherwise, return the whole string."
   (let ((pos (position #\: string))
	 (posL (position #\{ string))
	 (posR (position #\} string)))
     (if pos
	 (subseq string (+ pos 1) posR)
       (if posL 
	   (subseq string (+ posL 1) posR)
	 (subseq string 0 posR)))))

;;; Define the NAMESPACE structure.

(defstruct (namespace
   (:constructor internal-make-namespace)
   (:print-function print-namespace))
  ;; A string that is the name of the namespace.
  (name)
  ;; The hashtable associating names with elements.
  (nametable (make-hash-table :test #'equal))
  ;; A namespace may optionally include another namespace.
  (include nil))

(defun print-namespace (e stream depth)
  (declare (ignore depth))
  (format stream "#<namespace ~S>" (namespace-name e)))

(defun make-namespace (namespace-name &key (include nil))
  "Make a new empty namespace with STRING as the name.  If :include is
   specified, that namespace is included in the one we are creating.
   :include may be either a namespace object of a string naming one."
  (check-type namespace-name string)
  ;; Coerce the :include argtument to a namespace object or nil.
  (typecase include
    (null)
    (namespace)
    (string (setq include (get-namespace include)))
    (t (error "The :include argument must be a namespace or string.")))
  ;; Create the namespace structure.
  (let ((ns (internal-make-namespace
	     :name namespace-name
	     :include include)))
    ;; Register the namespace-name for later lookup.
    (setf (gethash (string-upcase namespace-name) *namespaces*) ns)
    ns))

(defun get-namespace (namespace-name &key (include nil))
  "Given a string identifying a namespace, return the corresponding
   namespace object, or create it if necessary.  The lookup is
   case-insensitive."
  (or (gethash (string-upcase namespace-name) *namespaces*)
      (make-namespace namespace-name :include include)))

(defun element-name (e)
  "Given an element E, produce a string representing the element's
   iname, with curly brackets around it.  The global variable
   *PRINT-NAMESPACE-IN-ELEMENTS* controls whether we include the namespace,
   followed by a colon."
  (let ((namespace (namespace e))
	(iname (internal-name e)))
    (cond ((string-node? e)
	   ;; Special-case print formula for string nodes.
	   (format nil "{~S}" iname))
	  ((and (not (fast-primitive-node? e))
		(or (eq *print-namespace-in-elements* t)
		    (and (eq *print-namespace-in-elements* :maybe)
			 (not (eq namespace *namespace*)))))
	   (format nil "{~A:~A}"
		   (namespace-name namespace)
		   iname))
	  (t (format nil "{~A}" iname)))))

;; Quick and dirty function for extracting inames.  Note that this
;; doesn't include the namespace, and the output for a string object
;; {"foo"} is indistiguishable from the output for element {foo}.
(defun iname (x)
  "Given an X, either an element or an element-iname structure, return the
   internal name.  Normally this will be a string, but if this is a primitive
   node it could be some Lisp object."
  (etypecase x
    (element (internal-name x))
    (element-iname (element-iname-value x))))

(defun in-namespace (namespace-name &key (include nil))
  "The namespace designated by NAMESPACE-NAME becomes the new
   default namespace.  Create this namespace if it doesn't already
   exist."
  (check-type namespace-name string)
  ;; Look up the string in *NAMESPACES*.
  (let ((ns (get-namespace namespace-name :include include)))
    ;; Set up *NAMESPACE*
    (setq *namespace* ns)))

(defun lookup-string-in-namespace (string namespace)
  "Internal function.  Lookup the STRING as an iname in the specified
   NAMESPACE object.  If it isn't there, follow the chain of included
   namespaces as far as it goes.  Return NIL is the name is not found."
  (setq string (string-upcase string))
  (and namespace
       (or (gethash string (namespace-nametable namespace))
	   (lookup-string-in-namespace
	    string
	    (namespace-include namespace)))))

(defun register-internal-name (iname-struct e)
  "The INAME-STRUCT argument is an ELEMENT-INAME structure.  E is an
   element.  Extract the string and register it as an internal name for
   later lookup.  Assume we have already checked that the name is not in
   use."
  (when iname-struct
    (let* ((string (element-iname-value iname-struct))
	   (namespace-name (extract-namespace string))
	   (namespace))
      (cond (namespace-name
	     ;; If the string included a namespace, use it.
	     ;; Convert the string to a namespace structure.
	     (setq namespace (get-namespace namespace-name)))
	    ;; If the string didn't have an namespace, use
	    ;; use *NAMESPACE*.
	    (t (setq namespace *namespace*)))
      ;; Register the name.
      (setf (gethash (string-upcase (extract-name string))
		     (namespace-nametable namespace))
	    e)
      ;; Save the namespace and iname string in the element.
      (setf (namespace e) namespace)
      (setf (internal-name e) (extract-name string))
      e)))

(defun unregister-internal-name (e)
  "Remove the entry for an element's internal name."
  (setq e (lookup-element-test e))
  (let* ((in (internal-name e))
	 (namespace (namespace e)))
    (cond ((fast-primitive-node? e)
	   (remhash in namespace))
	  (t (setq in (string-upcase in))
	     (remhash in (namespace-nametable namespace))))))


;;; ========================================================================
(subsection "Basic Machinery for English (External) Names")

;;; Use normal compilation policy, where safety counts as much as speed.
(declaim (optimize (speed 1) (space 1) (safety 1)))

;;; Scone does not (yet) attempt to provide a serious natural-language
;;; interface.  We assume that some external package does the parsing of NL
;;; inputs and queries.  However, Scone does collect and make available a
;;; dictionary (hashtable) of natural-language words and phrases (Common
;;; Lisp strings) that refer directly to Scone elements.  It is convenient
;;; to include these natural-langauge labels in the same KB files that
;;; create the elements.

;;; For now, Scone supports only one natural language: English.  We hope to
;;; support additional natural languages in the future.  However, the
;;; choice of languages may be limited because few Common Lisp
;;; implementations support the full Unicode character set.

;;; The association from English strings to Scone elements is stored in the
;;; *ENGLISH-DICTIONARY* hash-table.

;;; For each association we also store a syntax tag, such as :NOUN or :ADJ.
;;; The idea is that an external NL front-end will do the parsing or
;;; part-of-speech assignment, and then will use the Scone dictionary to
;;; find one or more elements corresponding to the sting/tag combination.

;;; The syntax tags currently allowed are stored in the variable
;;; *LEGAL-SYNTAX-TAGS*.  The following tags are currently defined:

;;; :NOUN -- Either a common name or a proper name for the entities
;;; represented by the element.  "Clyde" or "elephant".

;;; :ADJ -- An adjective like "blue", which is associated with a type-node.
;;; Gramatically, you normally combine the adjective with some noun,
;;; perhaps obtained from some superior of the type-node in question.  So
;;; we would say "a blue object" rather than just "a blue".

;;; :ADJ-NOUN -- A word like "solid" that can be used either as a noun or
;;; as an adjective.

;;; :ROLE -- A word like "population" that designates a role under some
;;; type node.  Normally used with "of" or a possessive form like "India's".

;;; :INVERSE-ROLE -- Name given to the inverse of a role relation.  For
;;; example, if the role is "PARENT", the inverse-role may be "CHILD".  So
;;; if we are told that "X is the CHILD of Y", we can convert this to "Y is
;;; the PARENT of X".

;;; :RELATION -- A word like "employs" or "hates" that represents some
;;; relation that is not an of-role or possessive.

;;; :INVERSE-RELATION -- Name given to the inverse of a relation.  For
;;; example, if the relation is "TALLER THAN", the inverse-role may be
;;; "SHORTER THAN".  So if we are told that "X is taller than Y", we can
;;; conver this to "Y is shorter than X".

;;; :VERB -- A verb that may or may not be an action verb.

;;; :ADVERB --An adverb like "fast" which is associated with a type-node
;;; or relation

(defun english (element &rest r)
  "Define one or more English names for the specified ELEMENT. The
   arguments after the element are strings or syntax tags. Initially we
   assume that each English name receives the syntax-tag :NOUN, but the
   default changes if a different syntax-tag is encountered in the R list.
   The special tag :INAME is used to indicate that ELEMENT's internal name
   becomes an English name with the current syntax tag.  However, if
   :NO-INAME is present in the R list, that cancels any :INAME tag.  Log
   this KB change."
  (setq element (lookup-element-test element))
  (english-internal element r)
  element)

(defun english-internal (element namelist)
  "Internal call to do the work of the user-level ENGLISH function.
   This is called directly by MAKE-ELEMENT.  Assumes that any KB logging is done
   by the caller."
  ;; The default syntax-tag is :NOUN until someone changes it.
  (let ((syntax-tag :noun))
    ;; Scan the list of arguments.
    (dolist (x namelist)
      (cond ((eq x :no-iname))
	    ((eq x :iname)
	     (unless (member :no-iname namelist)
	       ;; :INAME is equivalent to a string with the elemnt's internal
	       ;; name.
	       (let ((iname (internal-name element)))
		 (unless (typep iname 'string)
		   (error "Element ~S has no internal name."
			  element))
		 (register-definition iname element syntax-tag))))
	    ((typep x 'string)
	     ;; If it's a string, register the definition.
	     (register-definition x element syntax-tag))
	    ((and (typep x 'keyword)
		  (member x *legal-syntax-tags*))
	     ;; If it's a syntax tag, use this tag for subsequent args.
	     (setq syntax-tag x))
	    (t (error "~S unknown argument to ENGLISH." x))))
    element))

(defun merge-english-inverse (fwd inv tag)
  "Create a merged argument list suitable for passing to ENGLISH by
   combining the FWD and INV argument lists.  Convert the INV list by
   turning :ROLE and :RELATION into their inverses, and stick TAG onto
   the front of that list.  Then merge them and return the result."
  ;; FWD and INV must be lists.
  (unless (listp fwd) (setq fwd (list fwd)))
  (unless (listp inv) (setq inv (list inv)))
  (append fwd
	  (cons tag
		(mapcar #'flip-direction inv))))

(defun flip-direction (x)
  (case x
    (:relation :inverse-relation)
    (:role :inverse-role)
    (:inverse-relation :relation)
    (:inverse-role :role)
    (t x)))

(defun convert-underscores (string)
  "We use this in several places where we MIGHT be handed a multi-word
   string with underscores in it where spaces should be.  This converts
   the string if necessary and returns the results."
  (substitute #\space #\_ string))

(defun register-definition (string
			    element
			    syntax-tag)
  "Store an association from STRING to (ELEMENT . SYNTAX-TAG) in the
   *ENGLISH-DICTIONARY* hashtable.  Since a string may have multiple
   definitions, the hashtabl entry is a list if it exists at all.  Also
   store (STRING . SYNTAX-TAG) in the ELEMENT.  Assume that this KB change
   has already been logged."
  (setq element (lookup-element-test element))
  ;; As a rule, we don't want to preserve underscores-as-spaces that
  ;; come from sources like WordNet.
  (setq string (convert-underscores string))
  (let* ((up-string (string-upcase string))
	 (dictionary *english-dictionary*)
	 (definitions (gethash up-string dictionary))
	 (element-names
	  (get-element-property element :english-names))
	 (new1 (cons string syntax-tag))
	 (new2 (cons element syntax-tag)))
    (unless (member new2 definitions :test #'equal)
      (setf (gethash up-string dictionary)
	    (nconc definitions (list new2)))
      (set-element-property element
			    :english-names
			    (nconc element-names (list new1))))))

(defun unregister-definition (string
			      element)
  "Remove all mentions of ELEMENT from the entry for STRING in
   *ENGLISH-DICTIONARY*.  Assume that this KB change has already been
   logged."
  (setq element (lookup-element-test element))
  ;; Get rid of underscores in the string we are looking for.
  (setq string (convert-underscores string))
  (let* ((dictionary *english-dictionary*)
	 (up-string (string-upcase string))
	 (all-english-tags (get-element-property element
						 :english-names))
	 (new-english-tags
	  (remove-if
	   #'(lambda (x) (string= (string-upcase (car x))
				  up-string))
	   all-english-tags))
	 (all-definitions (gethash up-string dictionary))
	 (new-definitions
	  (remove-if
	   #'(lambda (x) (equal (car x) element))
	   all-definitions)))
    (if new-definitions
	(setf (gethash up-string dictionary) new-definitions)
	(remhash up-string dictionary))
    ;; Also remove the english name from ELEMENT's list
    ;; of :english tags
    (cond (new-english-tags
	   (set-element-property element :english-names
				 new-english-tags))
	  (t
	   (clear-element-property element :english-names)))))

(defun unregister-all-names (element)
  "Unregister all English names associated with the specified ELEMENT."
  (setq element (lookup-element-test element))
  (let ((names (get-element-property element :english-names)))
    (dolist (n names)
      (unregister-definition (car n)
                             element)))
  element)

(defun lookup-definitions (string &optional syntax-tags)
  "Return all the elemnts that are definitions of STRING.  Each value will
   be of form (ELEMENT . SYNTAX-TAG).  If the SYNTAX-TAGS argument is
   supplied, it is a list of tags.  Return only those definitions that
   are associated with one of these tags.  But if the list contains :OTHER,
   any tag is OK."
  ;; Get rid of underscores in the lexical item we are looking for.
  (setq string (convert-underscores string))
  (let ((values (gethash (string-upcase string) *english-dictionary*)))
    (if (and syntax-tags (not (member :other syntax-tags)))
	(setq values
	      (remove-if-not
	       #'(lambda (x) (member (cdr x) syntax-tags))
	       values))
	values)))

(defun mark-named-elements (string m &optional (syntax-tag nil))
  "Mark all the elements whose external name is STRING with marker M.  If
   the SYNTAX-TAGargument is supplied, mark only elements whose definition
   is of that type."
  (check-legal-marker m)
  (let ((elements (lookup-definitions string syntax-tag)))
    (dolist (e elements)
      (when (typep (car e) 'element)
        (mark (car e) m))))
  (fast-marker-count m))

(defun get-english-names (element &optional (syntax-tag nil))
  "Get the list of names for ELEMENT.  A name is of the form (string . .
   syntax-tag).  If the SYNTAX-TAG argument is supplied, return a list with
   only names of that type."
  (setq element (lookup-element-test element))
  (let ((names (get-element-property element :english-names)))
    (if syntax-tag
        (setq names
	      (remove-if-not
	       #'(lambda (x) (eq (cdr x) syntax-tag))
	       names))
	names)))

(defun disambiguate (name definition-list syntax-tags)
  "Given a list of possible definitions (element.tag pairs) for an
   English word, try to disambiguate it based on the value of
   *disambiguate-policy*.  If :ERROR, just signal an error.  If :ASK,
   ask the user.  SYNTAX-TAGS provides context when querying the
   user."
  (ecase *disambiguate-policy*
    ;; If :ERROR, just signal an error.
    (:error "The English word ~S with syntax ~S is ambiguous."
	    name syntax-tags)
    ;; If :ASK, print out the options, with the parent of each
    ;; candidate element, then ask the user to choose one by number.
    (:ask
     ;; Print a header.
     (if syntax-tags
	 (format t "~%The English word ~S with syntax ~S is ambiguous.  ~
		 Possible meanings:~2%"
		 name syntax-tags)
	 (format t "~%The English word ~S is ambiguous.  ~
		 Possible meanings:~2%"
		 name))
     ;; Print each of the definitions, preceded by a numerical index.
     (do ((defs definition-list (cdr defs))
	  (n 0 (1+ n)))
	 ((null defs))
       (let ((def (car defs)))
	 (format t "~D: ~S, of element-type ~S, syntax ~S~%"
		 n (car def) (element-type-label (car def)) (cdr def))
	 (format t "    Parent: ~A~2%" (parent-wire (car def)))))
     ;; Now ask the users to choose.
     (format t "Type the number of the meaning you want: ")
     (force-output t)
     (let ((n (read t)))
       (terpri t)
       (values (car (nth n definition-list))
	       (cdr (nth n definition-list)))))
    ;; If :FIRST, just return the first value.  Use this when
    ;; in non-interactive situations that would rather risk being
    ;; wrong than trip up the system.
    (:first
     (values (car (car definition-list))
	     (cdr (car definition-list))))))

(defun element-type-label (e)
  "Return s string describing what type of element E is."
  (cond
    ((fast-map-node? e) "Map")
    ((fast-integer-node? e) "Integer")
    ((fast-number-node? e) "Number")
    ((fast-string-node? e) "String")
    ((fast-function-node? e) "Function")
    ((fast-struct-node? e) "Struct")
    ((fast-primitive-node? e) "Primitive-Indv")
    ((fast-proper-indv-node? e) "Proper-Indv")
    ((fast-indv-node? e) "Generic Indv")
    ((fast-type-node? e) "Type")
    ((fast-is-a-link? e) "Is-A")
    ((fast-is-not-a-link? e) "Is-Not-A")
    ((fast-eq-link? e) "Eq")
    ((fast-not-eq-link? e) "Not-Eq")
    ((fast-has-link? e) "Has")
    ((fast-has-no-link? e) "Has-No")
    ((fast-cancel-link? e) "Cancel")
    ((fast-complete-split? e) "CSplit")
    ((fast-split? e) "Split")
    ((fast-relation? e) "Relation")
    ((fast-statement? e) "Statement")
    ((fast-not-statement? e) "Not-Statement")
    (t "Unknown")))


;;; ========================================================================
(subsection "List and Show Functions on External Names")

(defun list-synonyms (string &optional (syntax-tag nil))
  "Find all the synonyms of STRING in the KB.  Returns a list of synonym
   sets, each os which is itself a list.  The first element of the
   synonym-set list is an element, followed by alternative names for that
   element.  If the SYNTAX-TAG argument is supplied, consider only
   definitions of STRING that correspond to that tag."
  (let ((defs (lookup-definitions string syntax-tag))
        (result nil))
    ;; For every definition of name-string...
    (dolist (d defs)
      (let ((synset nil)
            (element (car d)))
        (when (typep element 'element)
          ;; Scan the list of that element's names.
          (dolist (n (get-element-property element :english-names))
            ;; Filter out STRING itself.
            (unless (equal string (car n))
              ;; For the rest, extract the string and save it.
              (push (car n) synset)))
          (push (cons element (nreverse synset)) result))))
    (nreverse result)))

(defun show-synonyms (string &optional (syntax-tag nil))
  "Print out the synonyms of STRING in human-readable form."
  (let ((syns (list-synonyms string syntax-tag)))
    (cond ((null syns)
	   (format t "~%The name ~S is not defined in the KB." string))
	  ((null (cdr syns))
	   (if (null (cdr (car syns)))
	       (format t "~%The name ~S has only one meaning and no synonyms."
		       string)
	       (format t "~%Synonyms of ~S:~{ ~S~}"
		       string (cdr (car syns)))))
	  (t (format t "~%The name ~S has multiple synonym sets:~%"
		     string)
	     (dolist (syn syns)
	       (format t "~%  Meaning: ~S~%" (car syn))
	       (if (null (cdr syn))
		   (format t "   No synonyms.~%")
		   (format t "  Synonyms:~{ ~S~}~%" (cdr syn))))))
    (values)))

(defun list-dictionary-entries ()
  "List all entries in the dictionary.  This is primarily for debugging."
  (let ((dl nil))
    (maphash
     #'(lambda (x y) (push (list x y) dl))
     *english-dictionary*)
    dl))

(defun show-dictionary-entries ()
  "Print all entries in the dictionary.  This is primarily for debugging."
  (maphash
   #'(lambda (x y) (format t "~S => ~S~2%" x y))
   *english-dictionary*))


;;; ***************************************************************************
(section "Housekeeping")
;;; ***************************************************************************

(subsection "Loading KB Files")

;;; Use normal compilation policy, where safety counts as much as speed.
(declaim (optimize (speed 1) (space 1) (safety 1)))

(defun load-kb (filename &key (verbose nil))
  "Read in a knowledge base in text format.  This is basically just a
   wrapper around the Common Lisp LOAD function, but it does some essential
   housekeeping. Loads and uses the dictionary file (if there is one) to
   preserve permanently assigned element IDs."
  (let* ((*verbose-loading* (or *verbose-loading* verbose))
	 (*no-kb-error-checking* *no-kb-error-checking*)
	 (pathname
	  (merge-pathnames (pathname filename)
			   *default-kb-pathname*))
	 (*load-kb-stream* (open pathname
				 :direction :input
				 :external-format :utf-8))
	 ;; When reading a KB file, set this variable.
	 (*loading-kb-file* t)
	 ;; The file may set *DEFER-UNKNOWN-CONNECTIONS*.  Make sure that
	 ;; this stays local.
	 (*defer-unknown-connections* nil))
    (commentary "Loading KB \"~A\"." filename)
    (force-output *terminal-io*)
    ;; Push last-loaded-file info now so we can roll back if we get an
    ;; error during loading.
    (load *load-kb-stream*
	  :verbose *verbose-loading*
	  :print *verbose-loading*)
    (process-deferred-connections)
    (push pathname *loaded-files*)
    (push *last-element* *last-loaded-elements*)
    ;; Add n extra newline after each file load, which may include
    ;; a number of commentary lines.
    (commentary "Load of \"~A\" completed.  ~D elements total.~%"
		filename *n-elements*)
    (force-output *terminal-io*)
    (values)))

(defun process-deferred-connections ()
  "Go through the *DEFERRED-CONNECTIONS* list and connect all the wires
   that can be connected at this time.  Leave the rest on the list."
  (when *deferred-connections*
    (when *verbose-loading*
      (commentary "Deferred connections: ~S." *deferred-connections*))
    (let ((still-deferred nil)
          (connected 0)
          (remaining 0))
      (dolist (c *deferred-connections*)
        (let ((wire (first c))
              (source-element (second c))
              (target-element (lookup-element (third c))))
          (cond ((null target-element)
                 (push c still-deferred)
                 (incf remaining))
		(t (connect-wire wire source-element target-element)
		   (incf connected)))))
      (setq *deferred-connections* still-deferred)
      (commentary "~D deferred connections fixed, ~D remain."
		  connected remaining))))

(defun defer-connection (tag source target)
  "If a wire cannot be connected now because the target element doesn't
   exist yet, we may defer the connection and make it later.  This allows
   for creation of circularities in the KB."
  (when target
    (unless *defer-unknown-connections*
      (error "Element ~S is not (yet) defined." target))
    (push (list tag source target) *deferred-connections*)))

;;; ***************************************************************************
(section "KB Checkpointing and Persistence")
;;; ***************************************************************************

;;; Machinery for checkpointing a KB file that has been modified by
;;; addition of new elements.  The resulting files can be loaded by
;;; LOAD-KB.

;;; NOTE: At present the checkpoint format is text rather than some
;;; optimized binary fasload format.  So dumping and loading are not
;;; super-fast, but at least we bypass most of the consistency checking
;;; that occurs when we create elements from scratch.

(defun checkpoint-kb (&optional (filename "checkpoint"))
  "Dump out all KB elements into a lisp file.  This can be loaded into a
   new Scone to recreate the KB state at the time of the checkpoint.  We do
   not dump the elements that came from the BOOTSTRAP file, so the new Lisp
   must already have loaded that file.  Any missing fields in the FILENAME
   argument are filled in from the corresponding fields in
   *DEFAULT-KB-PATHNAME*."
  (let* ((pathname (merge-pathnames filename *default-kb-pathname*))
	 (skip t))
    (with-open-file (dump pathname
			  :direction :output
			  :if-exists :supersede
			  :external-format :utf-8)
      (format dump "~&;;; Scone Checkpoint File ~A~%" pathname)
      (format dump ";;; Dumped on ~A.~2%" (current-time-string))
      (format dump "(setq *defer-unknown-connections* t)~%")
      (format dump "(check-loaded-files '(\"bootstrap\"))~%")
      (format dump "(new-iname-prefix ~A)~%" *iname-prefix*)
      (do-elements (e)
	(unless skip
	  (dump-element e dump))
	(when (eq e *last-bootstrap-element*)
	  (setq skip nil))))
    (commentary "Checkpointed KB to ~S." pathname)
    (values)))

(defun checkpoint-new (&optional (filename "checkpoint-new"))
  "Dump into a lisp file all the elements created since completion of the
   last LOAD-KB.  Use LOAD-KB to read this dumped file into a Lisp that
   already has the same set of files loaded.  Any missing fields in the
   FILENAME argument are filled in from the corresponding fields in
   *DEFAULT-KB-PATHNAME*."
  (let* ((pathname (merge-pathnames filename *default-kb-pathname*))
	 (skip t))
    (with-open-file (dump pathname
			  :direction :output
			  :if-exists :supersede
			  :external-format :utf-8)
      (format dump ";;; Scone New-Element Checkpoint File ~A~%" pathname)
      (format dump ";;; Dumped on ~A.~2%" (current-time-string))
      (format dump "(setq *defer-unknown-connections* t)~%")
      ;; Note what files are loaded, so that when the log file is read
      ;; back in, this will be checked.
      (format dump "~&(check-loaded-files '~S)~%"
	      (mapcar #'pathname-name *loaded-files*))
      (format dump "~&(new-iname-prefix ~A)~%" *iname-prefix*)
      (do-elements (e)
	(unless skip
	  (dump-element e dump))
	(when (eq e (car *last-loaded-elements*))
	  (setq skip nil)))
      (format dump
	      "(setq *iname-counter* ~A)~%"
	      (+ *iname-counter* 1)))
    (commentary "Checkpointed KB to ~S." pathname)
    (values)))

(defun dump-element (e stream)
  (format
   stream
   "(make-element ~S ~S ~S ~S ~S ~S ~S '~S NIL ~A '~S '~S)~%"
   (flags e)
   (if (primitive-node? e) (internal-name e) e)
   (parent-wire e)
   (context-wire e)
   (a-wire e)
   (b-wire e)
   (c-wire e)
   (unravel-english-names e)
   ;; Make sure the element name ends up in the right hashtable.
   (cond ((fast-number-node? e) "*number-hashtable*")
	 ((fast-string-node? e) "*string-hashtable*")
	 ((fast-function-node? e) "*function-hashtable*")
	 ((fast-struct-node? e) "*struct-hashtable*")
	 (t "nil"))
   (definition e)
   ;; Save the element's property list, but not the :ENGLISH
   ;; property, which we handle separately.
   (remf (properties e) :english)))

(defun unravel-english-names (e)
  (let ((list nil))
    (dolist (name (get-english-names e))
      (push (cdr name) list)
      (push (car name) list))
    (nreverse list)))

;;; Function to check whether the expected set of files have been loaded.

(defun check-loaded-files (file-list)
  "Check whether all the files in FILE-LIST are currently loaded in Scone.
   Check only the filename itself, not the directory or extension.  Don't
   worry about load order or any extra files that have been loaded.  If we
   fail the test, signal a continuable error."
  (do ((list1 file-list (cdr list1)))
      ((null list1) t)
    (let ((name (pathname-name (car list1))))
      (unless (member name *loaded-files*
		      :test #'same-filename)
	(cerror 
	 "Continue despite the missing file."
	 "File ~S should be loaded, but is not." name)))))

(defun same-filename (x y)
  (string-equal x (pathname-name y)))

;;; Update *INAME-PREFIX* to a new value.

(defun new-iname-prefix (n)
  (setq *iname-prefix*
	(+ 1 (max *iname-prefix* n)))
  (setq *iname-counter* 0))

;;; Small utility function to print the current time.

(defvar *dow-vector*
  (vector "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
	  "Saturday" "Sunday"))

(defun current-time-string ()
  "Create a human-readable string representing the current time in this time
   zone."
  (multiple-value-bind (sec min hr d m y dow dst zone)
		       (get-decoded-time)
    (format nil
	    "~A, ~A/~A/~A at ~A:~A:~A, Time Zone: ~@D~A"
	    (aref *dow-vector* dow)
	    m d y
	    hr min sec
	    zone
	    (if dst " (DST)" ""))))

;;; ***************************************************************************
(section "Removing Elements from the KB")
;;; ***************************************************************************

;;; Use normal compilation policy, where safety counts as much as speed.
(declaim (optimize (speed 1) (space 1) (safety 1)))

;;; Removing an element can leave things in an odd state, but we still need
;;; some mechanism to do this.

(defun remove-element (e)
  "Disconnect element E from the network and remove it."
  (setq e (lookup-element-test e))
  ;; Remove all markers from this element so that we don't break any
  ;; marker-chains.
  (dotimes (m n-markers)
    (declare (fixnum m))
    (fast-unmark e m))
  ;; Unhook all the wires coming into E.
  (disconnect-wire :parent e)
  (disconnect-wire :context e)
  (disconnect-wire :a e)
  (disconnect-wire :b e)
  (disconnect-wire :c e)
  ;; Remove E from the chain of elements.
  (cond ((eq e *first-element*)
	 (setq *first-element* (next-kb-element e)))
	(t (let ((prev (previous-element e)))
	     (setf (next-kb-element prev)
		   (next-kb-element e))
	     (when (eq e *last-element*)
	       (setq *last-element* prev)))))
  ;; Fix up count.
  (decf *n-elements*)
  ;; Remove E from name hashtables.
  (unregister-internal-name e)
  (unregister-all-names e)
  *n-elements*)

(defun remove-last-element ()
  "Remove the last element created."
  (remove-element *last-element*)
  *n-elements*)

(defun remove-elements-after (e)
  "Given some element E, remove all elements added later than E, starting
   with the last." 
  (setq e (lookup-element-test e))
  (loop
    (when (or (null *last-element*)
	      (eq *last-element* e))
      (return nil))
    (remove-element *last-element*)))

(defun remove-last-loaded-file ()
  "Remove all the elements, starting with the last-created, until we have
   removed all elements created by the last loaded file (plus any created
   by hand after that).  Pops the last file off *LOADED-FILES* and
   *LAST-LOADED-ELEMENTS*.  Returns the number of elements remaining in the
   KB."
  (cond ((car *last-loaded-elements*)
         (let ((new-last (cadr *last-loaded-elements*)))
           (do ((e *last-element* *last-element*))
               ((or (null e) (eq e new-last)))
             (remove-element e)))
         (pop *loaded-files*)
         (pop *last-loaded-elements*)
         *n-elements*)
        (t (error "No last loaded file to remove."))))

(defun remove-currently-loading-file ()
  "Remove all the elements, starting with the last-created, until we have
   removed all elements created by the file that is currently being loaded.
   This is useful for backing out of a file-load if an error aborts the
   load.  Returns the number of elements remaining in the KB."
  (let ((new-last (car *last-loaded-elements*)))
    (do ((e *last-element* *last-element*))
        ((or (null e) (eq e new-last)))
      (remove-element e)))
  (pop *loaded-files*)
  *n-elements*)

;;; NOTE: REMOVE-CONTEXT may leave some dangling pointers coming from
;;; other contexts.  Is this a problem?  Can it cause crashes?

(defun remove-context (c)
  "Remove context C, all sub-contexts, and all their contents.  Return
   the number of elements actually removed."
  (setq c (lookup-element-test c))
  (unless (simple-is-x-a-y? c *universal*)
    (error "~S is not a context." c))
  (with-markers (m1 m2)
    (progn
      (downscan c m1)
      (activation-scan m1 m2)
      (let ((old-n-elements *n-elements*))
	(do-marked (e m2)
	  (remove-element m2))
	(- *n-elements* old-n-elements)))))

;;; ***************************************************************************
(section "Checking for Problems")
;;; ***************************************************************************

;;; Useful utility functions for exploring split violations.

(defun find-split-violations ()
  "Scan all elements in the KB.  Mark each one that by itself violates
   a split.  Find the uppermost marked elements.  For each of these
   print the element and the members of the split."
  (with-markers (m1 m2 m3)
    (progn
      ;; For All Elements...
      (do-elements (e)
	;; Use M1 To Upscan From Element E.
	(upscan e m1)
	(let ((s (violated-split? m1)))
	  (when s
	    ;; If E Violates Any Split, Mark E With M2.
	    (mark e m2))))
      ;; Now Use M3 To Mark The Uppermost Elements That Violate Any
      ;; Splits.
      (mark-uppermost m2 m3)
      ;; For All The Uppermost Violators, Print The Element And The
      ;; Split.
      (do-marked (e m3)
	;; Retrieve The Violated Split Again.
	(upscan e m1)
	(let ((s (violated-split? m1)))
	  (when s
	    (format t "~%~s violates split ~s"
		    e  (reverse (split-wires s)))))))))

(defun find-path (lower upper)
  "Find and print all the elements on the path (or paths) between the
   LOWER and UPPER elements."
  (setq lower (lookup-element-test lower))
  (setq upper (lookup-element-test upper))
  (with-markers (m1 m2)
    (progn
      (upscan lower m1)
      (downscan upper m2)
      (do-marked (e m1)
	(when (marker-on? e m2)
	  (format t "~%~S" e))))))

(defun split-violations-below? (e1 e2)
  "This tests whether creating an IS-A or EQ link from element E1 to
   element E2 will cause some split to be violated by any element
   below E1.  If there are violations, print the uppermost conflicted
   elements and return t.  if no violations, return nil."
  (setq e1 (lookup-element-test e1))
  (setq e2 (lookup-element-test e2))
  (with-markers (m1 m2 m3)
    (progn
      ;; These are the elements below E1.
      (downscan e1 m1)
      ;; Find all splits above E2.
      (upscan e2 m2)
      (do-marked (x m2)
	(dolist (split (incoming-split-wires x))
	  ;; Mark with M3 all members of the split other than X.
	  (dolist (y (split-wires split))
	    (unless (eq y x)
	      (mark y m3)))))
      ;; Downscan M3 from all split members at once.  Elements with
      ;; both M1 and M3 have a problem.  Mark these with M2.
      (clear-marker m2)
      (downscan nil m3 :augment t)
      (do-marked (z m3)
	(when (marker-on? z m1)
	  (mark z m2)))
      ;; If we have any M2 elements, report the uppermost ones.
      (if (= (fast-marker-count m2) 0)
	  nil
	  (progn
	    (format t "~%Uppermost violations below ~S and ~S:"
		    e1 e2)
	    (clear-marker m1)
	    (mark-uppermost m2 m1)
	    (do-marked (loser m1)
	      (format t "~% ~S" loser))
	    t)))))

 
