;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Time representation for Scone's core knowledge base.
;;;
;;; Original Author: E. Cinar Sahin
;;; Current Author and Maintainer: Scott E. Fahlman
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
(in-context {general})

;;; Time model based loosely on the "KANI Time Ontology" by Richard
;;; Fikes and Selene Makarios, Stanford University Knowledge Systems
;;; Lab Tech Report KSL-04-05.
;;; URL: http://www.ksl.stanford.edu/KSL_Abstracts/KSL-04-05.html

;;; This time representation is intended to work with the Gregorian
;;; Calendar.  All references in time using this KB should be in the
;;; Gregorian Calendar format.

;;; --------------------------------------------------------------
;;; Time references
;;; --------------------------------------------------------------

;; Concepts that are used to reference time
(new-type {time reference} {intangible})
(new-type {time point} {time reference}
	  :english '("time" "instant"))
(new-type {time interval} {time reference}
	  :english '("time" "period" "duration"))

;;; A time interval is defined by two time points.
(new-indv-role {time interval start} {time interval}
              {time point}
              :english '(:role "starting time" "start time" "start"))

(new-indv-role {time interval finish} {time interval}
              {time point}
              :english '(:role "finish time" "finish"
                               "ending time" "end time" "end"))

;;; The duration of a time interval is a measure of time
(new-indv-role {time interval duration} {time interval} {time measure})

;;; Convenience functions for time intervals
(defun ti-start (ti)
  (or (the-x-of-y {time interval start} ti)
      (the-x-role-of-y {time interval start} ti)))
(defun ti-finish (ti)
  (or (the-x-of-y {time interval finish} ti)
      (the-x-role-of-y {time interval finish} ti)))
(defun ti-duration (ti)
  (or (the-x-of-y {time interval duration} ti)
      (the-x-role-of-y {time interval duration} ti)))


;;; --------------------------------------------------------------
;;; Time Reference Relations
;;; --------------------------------------------------------------
;;; The Allen relations (Allen 87) define basic relationships
;;; between time references. They are described in more detail below.
;;; --------------------------------------------------------------

;; Two time references are equal if they refer to exactly the same time
;; Visual: ---***---
;;         ---ooo---
;;
;; This is represented with the EQ link

;; The before relation is strict, i.e. the two time references have no
;; time in common. The symbol < will denote this relation (the
;; symbol > will denote the inverse 'after' relation)
;; Visual: ---****-------
;;         --------oooo--
(new-relation {before}
	      :a-inst-of {time reference}
	      :b-inst-of {time reference}
	      :inverse '(:relation "after"))

;; The contains relation, or inverse during
;; tr >= start(ti) && tr <= finish(ti)
;; Visual: ---**********---
;;         ---ooooooo------
(new-relation {contains time}
	      :a-inst-of {time interval}
	      :b-inst-of {time reference}
	      :transitive t)

;; (inverse) The TR is during TI.
;; tr > start(ti) && tr < finish(ti)
;; Visual: ----*********----
;;         -----ooooooo-----
(new-relation {contains time strict}
	      :parent {contains time}
	      :inverse '(:relation "during")
	      :transitive t)

;; Two intervals overlap
;; start(t1) < start(t2) && start(t2) < finish(t1) && finish(t1) < finish(t2)
;; Visual: ---***---
;;         ----ooo--
(new-relation {overlaps}
	      :a-inst-of {time interval}
	      :b-inst-of {time interval}
	      :symmetric t)

;; A reference starts a time interval if it references the start of the
;; interval but does not reference the finish
;; Visual: --***-----
;;         --ooooo---
(new-relation {is started by}
	      :parent {contains time}
	      :inverse '(:relation "starts"))

;; A reference finishes an time interval if it references the finish of the
;; interval but does not reference the start
;; Visual: ----****--
;;         --oooooo-
(new-relation {is finished by}
	      :parent {contains time}
	      :inverse '(:relation "finishes"))


;; An interval meets another one, if it finishes when the other one starts
;; Visual: -----ooo---
;;         --***------
(new-relation {meets time}
	      :a-inst-of {time interval}
	      :b-inst-of {time interval})


;;; --------------------------------------------------------------
;;; Time Standards: Calendar Systems and Time Zones
;;; --------------------------------------------------------------
;;; Calendar systems define an origin-point, and how the time line
;;; should be divided up according to the solar and lunar cycles. This
;;; KB only represents the Gregorian calendar, so this part of the
;;; representation has been omitted. Time zones are used, however.
;;; --------------------------------------------------------------


;;; All measureable qualities have a standard of measure.
(new-type {time standard} {intangible})
(new-type {calendar system} {time standard} :english '(:noun "calendar"))

;; Calendar systems have origin-points such as the birth of Christ etc.
(new-indv-role {calendar system origin-point}
	       {calendar system}
	       {time point})
(new-members {calendar system}
   '({gregorian calendar} {islamic calendar} {jewish calendar}))

;; We will define the origin-point of some systems when we can create
;; time point instances

;; there are two kinds of time zones
(new-type {time zone} {time standard})
(new-complete-split-subtypes {time zone}
  '({daylight time} {standard time}))

;;; every time zone has an offset which is a time measure
(new-indv-role {time zone offset} {time zone} {time measure})

;; References to time are modulo a time standard
(new-indv-role {time point calendar} {time point} {calendar system})
(new-indv-role {time point zone} {time point} {time zone})
(new-indv-role {time interval calendar} {time interval} {calendar system})
(new-indv-role {time interval zone} {time interval} {time zone})

(defun define-time-zone (tz offset &optional long short daytime)
  "TZ is the iname of the time zone (usually GMT +/-XX). Offset
   is the hourly offset from GMT."

  (let ((tz (new-indv (make-element-iname :value tz) {standard time}))
	(dtz (new-indv (make-element-iname
			:value (format nil "~A Summer" tz))
		       {daylight time})))
    (when long
      (english tz (format nil "~A Standard Time" long))
      (english dtz (format nil "~A Daylight Time" long)))
    (when short (english tz short))
    (when daytime (english dtz daytime))
    (x-is-the-y-of-z
     (new-measure offset {hour unit})
     {time zone offset}
     tz)
    (x-is-the-y-of-z (new-measure (1+ offset) {hour unit})
		     {time zone offset}
		     dtz)
    (values tz dtz)))

(defun get-zone-offset (zone)
  "Given a time-zone, return the offset for the zone measured 
   in hours."
  (setq zone (lookup-element zone))
  (unless (is-x-a-y? zone {time zone})
    (error "~A is not a time-zone!" zone))
  (let ((measure (the-x-of-y {time zone offset} zone))
	(offset nil))
    (unless measure
      (error "time zone offset not defined for ~A" zone))
    (setq offset (the-x-of-y {measure magnitude} measure))
    (unless offset
      (error "~A has no magnitude set!" measure))
    (internal-name offset)))

(defun get-time-zone (offset daylight)
  "Given an offset measured in hours, and whether it's daylight time
   or not, return the time zone element representing these properties."
  (dolist (zone (list-instances (if daylight
				    {daylight time}
				    {standard time})))
    (when (= offset (get-zone-offset zone))
      (return-from get-time-zone zone))))

(dolist (m `(("GMT -12" -12)
	     ("GMT -11" -11)
	     ("GMT -10" -10 "Hawaii-Aleutian" "HST" "HDT")
	     ("GMT -9.30" -9.5)
	     ("GMT -9" -9 "Alaska" "AKST" "AKDT")
	     ("GMT -8" -8 "Pacific" "PST" "PDT")
	     ("GMT -7" -7 "Mountain" "MST" "MDT")
	     ("GMT -6" -6 "Central" "CST" "CDT")
	     ("GMT -5" -5 "Eastern" "EST" "EDT")
	     ("GMT -4" -4 "Atlantic" "AST" "ADT")
	     ("GMT -3.30" -3.5 "Newfoundland" "NST" "NDT")
	     ("GMT -3" -3)
	     ("GMT -2" -2)
	     ("GMT -1" -1)
	     ("GMT" 0 "Greenwich" "UTC" "BST")
	     ("GMT +1" 1 "Central European" "CET" "CEST")
	     ("GMT +2" 2 "Eastern European" "EET" "EEST")
	     ("GMT +3" 3)
	     ("GMT +3.30" 3.5)
	     ("GMT +4" 4)
	     ("GMT +4.30" 4.5)
	     ("GMT +5" 5)
	     ("GMT +5.30" 5.5)
	     ("GMT +5.45" 5.75)
	     ("GMT +6" 6)
	     ("GMT +6.30" 6.5)
	     ("GMT +7" 7)
	     ("GMT +8" 8)
	     ("GMT +8.45" 8.75)
	     ("GMT +9" 9)
	     ("GMT +9.30" 9.5 "Australian Central" "ACST" "ACDT")
	     ("GMT +10" 10 "Australian Eastern" "AEST" "AEDT")
	     ("GMT +10.30" 10.5)
	     ("GMT +11" 11)
	     ("GMT +11.30" 11.5)
	     ("GMT +12" 12)
	     ("GMT +12.45" 12.75)
	     ("GMT +13" 13)
	     ("GMT +14" 14)))
  (apply 'define-time-zone m))

(english {GMT} "Greenwich Mean Time")
(english {GMT Summer} "British Summer Time")

;;; --------------------------------------------------------------
;;; Defined Intervals
;;; --------------------------------------------------------------
;;; These intervals are parts of the time line, which is divided up
;;; according to some time standard. In this KB only the Gregorian
;;; calendar is defined.  Defined intervals are usually referred by
;;; indices, however months and days of the week do have names.
;;; --------------------------------------------------------------

(new-type {defined interval} {time interval})

(new-indv-role {defined interval index}
	       {defined interval}
	       {integer})

(defun get-interval-index (interval)
  "Given a defined interval INTERVAL, returns the index it is
   reffered to by."
  (setq interval (lookup-element-test interval))
  (unless (is-x-a-y? interval {defined interval})
    (error "~A is not a defined interval." interval))
  (let ((i (the-x-of-y {defined interval index} interval)))
    (when i (internal-name i))))

;; These are the intervals we really care about in this KB
(new-split-subtypes {defined interval}
  '({calendar year}
    {calendar month}
    {calendar day}
    {calendar week}
    {clock hour}
    {clock minute}
    {clock second}))

;; Defined intervals' durations
(x-is-the-y-of-z (new-measure 1 {second unit})
		 {time interval duration} {clock second})
(x-is-the-y-of-z (new-measure 1 {minute unit})
		 {time interval duration} {clock minute})
(x-is-the-y-of-z (new-measure 1 {hour unit})
		 {time interval duration} {clock hour})
(x-is-the-y-of-z (new-measure 1 {day unit})
		 {time interval duration} {calendar day})
(x-is-the-y-of-z (new-measure 1 {week unit})
		 {time interval duration} {calendar week})
(x-is-the-y-of-z (new-measure 1 {month unit})
		 {time interval duration} {calendar month})
(x-is-the-y-of-z (new-measure 1 {year unit})
		 {time interval duration} {calendar year})

;; Lets define the calendar months, note these are all types
(new-complete-split-subtypes {calendar month}
			     '({January} {February} {March} {April} {May}
			       {June} {July} {August} {September} {October}
			       {November} {December}))

;; There are two kinds of Februaries:
(new-complete-split-subtypes {February} 
  '({leap February} {non-leap February}))

;; There are two kinds of years
(new-complete-split-subtypes {calendar year}
  '({leap year} {non-leap year}))
#|
(x-is-the-y-of-z (new-measure 1 {leap year unit})
		 {time interval duration}
		 {leap year})
(x-is-the-y-of-z (new-measure 1 {normal year unit})
		 {time interval duration}
		 {non-leap year})


;; Months have 28-31 days 
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {January})
(x-is-the-y-of-z (new-measure 1 {28-day month unit})
		 {time interval duration} {February})
(x-is-the-y-of-z (new-measure 1 {29-day month unit})
		 {time interval duration} {leap February})
(x-is-the-y-of-z (new-measure 1 {28-day month unit})
		 {time interval duration} {non-leap February})
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {March})
(x-is-the-y-of-z (new-measure 1 {30-day month unit})
		 {time interval duration} {April})
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {May})
(x-is-the-y-of-z (new-measure 1 {30-day month unit})
		 {time interval duration} {June})
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {July})
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {August})
(x-is-the-y-of-z (new-measure 1 {30-day month unit})
		 {time interval duration} {September})
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {October})
(x-is-the-y-of-z (new-measure 1 {30-day month unit})
		 {time interval duration} {November})
(x-is-the-y-of-z (new-measure 1 {31-day month unit})
		 {time interval duration} {December})
|#

(define-cycle {days in week}
	     {calendar day}
	     '(("Monday" "Mon")
	       ("Tuesday" "Tue")
	       ("Wednesday" "Wed")
	       ("Thursday" "Thur")
	       ("Friday" "Fri")
	       ("Saturday" "Sat")
	       ("Sunday" "Sun")))

;;; --------------------------------
;;; Leap Year Query
;;; --------------------------------

(defun leap-year? (year)
 "YEAR is a gregorian year or a year index (maybe negative). If this
  is a leap year in the Gregorian calendar, return T. Otherwise,
  return NIL."
 ;;; %%% add option for number element.
 (unless (numberp year)
   (setq year (lookup-element-test year))
   (unless (is-x-a-y? year {calendar year})
     (error "~A is not a {calendar year}." year))
   (setq year (get-interval-index year)))
 (unless year
   (error "Given year is has no index, or NIL was supplied as an argument to leap-year?"))
 (cond ((= 0 (mod year 400)) t)
       ((= 0 (mod year 100)) nil)
       ((= 0 (mod year 4)) t)
       (t nil)))

;;; ----------------------------
;;; DEFINED INTERVAL CREATION
;;; ----------------------------

;;; %%% Improve this user interface.
(defun get-calendar-day (&key ind dow iname)
  "Function create a calendar day. IND, if given, is the index of the day
   in the month. DOW, if given, is the day of the week (0=Mon..6=Sun)"

  (if (and dow (numberp dow) (<= 0 dow) (< dow 7))
      (cond ((= dow 0)
	     (setq iname (new-indv iname {Monday})))
	    ((= dow 1)
	     (setq iname (new-indv iname {Tuesday})))
	    ((= dow 2)
	     (setq iname (new-indv iname {Wednesday})))
	    ((= dow 3)
	     (setq iname (new-indv iname {Thursday})))
	    ((= dow 4)
	     (setq iname (new-indv iname {Friday})))
	    ((= dow 5)
	     (setq iname (new-indv iname {Saturday})))
	    ((= dow 6)
	     (setq iname (new-indv iname {Sunday}))))
    (setq iname (new-indv iname {calendar day})))
  (when (and ind (integerp ind) (> ind 0) (< ind 32))
    (x-is-the-y-of-z  (new-integer ind) {defined interval index} iname))
  iname)

(defun get-calendar-week (week &key iname)
  "Function to create a calendar week with index WEEK."
  (when (and (integerp week) (> week 0) (< week 53))
    (setq iname
	  (new-indv iname {calendar week}
		    :english `(:no-iname ,(format nil "Week ~A" week))))
    (x-is-the-y-of-z (new-integer week) {defined interval index} iname)
    iname))

(defun get-calendar-month (month &key iname (leap :UNKNOWN))
  "Function to create a calendar month with index MONTH."
  (when (and (integerp month) (> month 0) (< month 13))
    (let ((parent (cond ((= month 1)  {January})  
			((= month 2) 
			 (cond ((eq leap :UNKNOWN)
				{February})
			       (leap {leap February})
			       (t {non-leap February})))
			((= month 3)  {March})
			((= month 4)  {April})
			((= month 5)  {May})
			((= month 6)  {June})
			((= month 7)  {July})
			((= month 8)  {August})
			((= month 9)  {September})
			((= month 10) {October})
			((= month 11) {November})
			((= month 12) {December}))))
      (setq iname
	    (new-indv iname parent
		      :english `(:no-iname ,(format nil "Month ~A" month))))
      (x-is-the-y-of-z  (new-integer month)
			{defined interval index} iname)
      iname)))

(defun get-calendar-year (year &key (ce t))
  "Function to get or create the calendar year with index YEAR.
   If CE is NIL the year will be a BCE year."
  (when (and (integerp year) (> year 0))
    (let ((iname 
	   (if ce
	       (make-element-iname :value (format nil "~A CE" year))
	       (make-element-iname :value (format nil "~A BCE" year)))))
      (or (lookup-element iname)
	  (let (y)
	    (if (leap-year? year)
		(setq y (new-indv iname {leap year}))
	      (setq y (new-indv iname {non-leap year})))
	    (if ce
		(x-is-the-y-of-z (new-integer year)
				 {defined interval index} iname)
		(x-is-the-y-of-z (new-integer (- 0 year))
				 {defined interval index} iname))
	    y)))))

;; We want to create coherent references, so we have to relate
;; time intervals somehow
(new-type-role {month of year} {calendar year} {calendar month})
(new-type-role {week of year} {calendar year} {calendar week})
(new-type-role {week of month} {calendar month} {calendar week})
(new-type-role {day of month} {calendar month} {calendar day})
(new-type-role {day of week} {calendar week} {calendar day})
(new-type-role {hour of day} {calendar day} {clock hour} :n 24)
(new-type-role {minute of hour} {clock hour} {clock minute} :n 60)
(new-type-role {second of minute} {clock minute} {clock second} :n 60)

(define-cycle {hours in day}
	      {clock hour}
 '(("1AM" "1")
   ("2AM" "2")
   ("3AM" "3")
   ("4AM" "4")
   ("5AM" "5")
   ("6AM" "6")
   ("7AM" "7")
   ("8AM" "8")
   ("9AM" "9")
   ("10AM" "10")
   ("11AM" "11")
   ("12PM" "12")
   ("1PM" "13")
   ("2PM" "14")
   ("3PM" "15")
   ("4PM" "16")
   ("5PM" "17")
   ("6PM" "18")
   ("7PM" "19")
   ("8PM" "20")
   ("9PM" "21")
   ("10PM" "22")
   ("11PM" "23")
   ("12AM" "0")))

;;; ------------------------------------------------------------------
;;; Calendar interval creation functions

(defun get-clock-second (s &key iname)
  "Creates and returns a clock second with index S."
  (when (and (integerp s) (>= s 0) (< s 60))
    (setq iname (new-indv iname {clock second} :english `(:no-iname ,(format nil "second ~A" s))))
    (x-is-the-y-of-z s {defined interval index} iname)
    iname))

(defun get-clock-minute (m &key iname)
  "Creates and returns a clock minute with index M."
  (when (and (integerp m) (>= m 0) (< m 60))
    (setq iname (new-indv iname {clock minute} :english `(:no-iname ,(format nil "minute ~A" m))))
    (x-is-the-y-of-z m {defined interval index} iname)
    iname))

(defun get-clock-hour (h &key iname)
  "Creates and returns a clock hour with index H."
  (when (and (integerp h) (>= h 0) (< h 24))
    (let ((parent (cond ((= h 0) {12AM})  ((= h 1) {1AM})  ((= h 2)  {2AM})   ((= h 3) {3AM})
			((= h 4) {4AM})   ((= h 5) {5AM})  ((= h 6)  {6AM})   ((= h 7) {7AM})
			((= h 8) {8AM})   ((= h 9) {9AM})  ((= h 10) {10AM})  ((= h 11) {11AM})
			((= h 12) {12PM}) ((= h 13) {1PM}) ((= h 14) {2PM})   ((= h 15) {3PM})
			((= h 16) {4PM})  ((= h 17) {5PM}) ((= h 18) {6PM})   ((= h 19) {7PM})
			((= h 20) {8PM})  ((= h 21) {9PM}) ((= h 22) {10PM})  ((= h 23) {11PM}))))
      (setq iname (new-indv iname parent :english `(:no-iname ,(format nil "hour ~A" h))))
      (x-is-the-y-of-z h {defined interval index} iname)
      iname)))



;;; --------------------------------------------------------------
;;; 6. Time References: Points and Intervals (part 2)
;;; --------------------------------------------------------------
;;; Now that we have the basic representational bricks laid, now
;;; we construct the necessary knowledge for representing
;;; the details of time points and intervals

;; Time points are defined by:
(new-indv-role {time point year} {time point} {calendar year})
(new-indv-role {time point month} {time point} {calendar month})
(new-indv-role {time point day} {time point} {calendar day})
(new-indv-role {time point hour} {time point} {clock hour})
(new-indv-role {time point minute} {time point} {clock minute})
(new-indv-role {time point second} {time point} {clock second})

;;; A {date} is a kind of {time point}.
;;; %%% We should add that it has no hour, minute, or second values,
;;; %%% but defer that for now.
(new-type {date} {time point})

;; We have to integrate all the roles to be sub-intervals of each other
;; this needs some hacking in the KB
(let ((c  {time point calendar})
      (z  {time point zone})
      (y  {time point year})
      (mo {time point month})
      (d  {time point day})
      (h  {time point hour})
      (m  {time point minute})
      (s  {time point second}))

  ;; calendar(tp) = timestd(year(tp))
  (new-eq c (the-x-role-of-y {time interval calendar} y))
  ;; calendar(tp) = timestd(month(tp))
  (new-eq c (the-x-role-of-y {time interval calendar} mo))
  ;; calendar(tp) = timestd(day(tp))
  (new-eq c (the-x-role-of-y {time interval calendar} d))

  ;; zone(tp) = timestd(hour(tp))
  (new-eq z (the-x-role-of-y {time interval zone} h))
  ;; zone(tp) = timestd(minute(tp))
  (new-eq z (the-x-role-of-y {time interval zone} m))
  ;; zone(tp) = timestd(second(tp))
  (new-eq z (the-x-role-of-y {time interval zone} s))
  
  ;; month(tp) ISA month(year(tp))
  (new-is-a mo (the-x-role-of-y {month of year} y))
  ;; day(tp) ISA day(month(tp))
  (new-is-a d (the-x-role-of-y {day of month} mo))
  ;; hour(tp) ISA hour(day(tp))
  (new-is-a h (the-x-role-of-y {hour of day} d))
  ;; minute(tp) ISA minute(hour(tp))
  (new-is-a m (the-x-role-of-y {minute of hour} h))
  ;; second(tp) ISA second(minute(tp))
  (new-is-a s (the-x-role-of-y {second of minute} m)))


(defun new-time-point (&key iname
			    (calendar {gregorian calendar})
			    (zone {GMT +5})
			    year month day hour minute second)
  "Creates and returns a new time point instance with given roles."

  (let ((tp (new-indv iname {time point})))
    (when calendar
      (x-is-the-y-of-z (lookup-element-test calendar)
		       {time point calendar} tp))
    (when zone
      (x-is-the-y-of-z (lookup-element-test zone)
		       {time point zone} tp))
    (when year
      (x-is-the-y-of-z  (lookup-element-test year)
			{time point year} tp))
    (when month
      (x-is-the-y-of-z (lookup-element-test month)
		       {time point month} tp))
    (when day
      (x-is-the-y-of-z  (lookup-element-test day)
			{time point day} tp))
    (when hour
      (x-is-the-y-of-z  (lookup-element-test hour)
			{time point hour} tp))
    (when minute
      (x-is-the-y-of-z  (lookup-element-test minute)
			{time point minute} tp))
    (when second
      (x-is-the-y-of-z  (lookup-element-test second)
			{time point second} tp))
    tp))



;;; Convenience function to create a time interval given two
;;; time-points.

;; The BEFORE? function is defined later but used here.  This
;; proclamation prevents a distractign style warning as the file
;; loads.
(proclaim '(ftype (function (t t &key (:create t)) t) before?))

(defun new-time-interval (tp1 tp2 &key
			      iname
			      (calendar {gregorian calendar})
			      (zone {GMT -5}))
  "Create a new time interval with the specified start and end
  time-points and optional iname."
  (unless (before? tp1 tp2)
    (error "~A is not before ~A." tp1 tp2))
  (let ((ti (new-indv iname {time interval})))
    (x-is-the-y-of-z calendar {time interval calendar} ti)
    (x-is-the-y-of-z zone {time interval zone} ti)
    (x-is-the-y-of-z tp1 {time interval start} ti)
    (x-is-the-y-of-z tp2 {time interval finish} ti)
    ti))


;;; Define the origin-point of the Gregorian Calendar
(new-time-point :iname {gregorian origin-point}
		:zone {GMT}
		:year (get-calendar-year 1)
		:month (get-calendar-month 1)
		:day (get-calendar-day :ind 1))

;;; Define the origin-point of the Unix time system.
(new-time-point :iname {unix origin-point}
		:zone {GMT}
		:year (get-calendar-year 1970)
		:month (get-calendar-month 1)
		:day (get-calendar-day :ind 1)
		:hour (get-clock-hour 0)
		:minute (get-clock-minute 0)
		:second (get-clock-second 0))



;;; --------------------------------------------------------------
;;; Time Reference Conversion Functions (Zone only)
;;; --------------------------------------------------------------


(defun tp-in-zone-helper (tp role)
  (let ((r (the-x-of-y role tp)))
    (when r
      (setq r (get-interval-index r)))
    (unless r
      (commentary "~A has missing time information (check index): ~A" tp role)
      (return-from tp-in-zone-helper nil))
    r))


(defun tp-in-zone (tp zone2 &key iname)
  "TP is a {time-point} individual. ZONE2 is a time zone individual.
   A new time point is created in the given zone which represents
   the same time point as TP. It only looks for a certain information
   if it absolutely needs it, hence it is able to convert partially
   describe time points in the description is sufficient."
  (when (null zone2)
    (return-from tp-in-zone tp))
    (setq tp (lookup-element-test tp))
  (let ((zone1 (the-x-of-y {time point zone} tp))
	(minute (the-x-of-y {time point minute} tp))
	(hour (the-x-of-y {time point hour} tp))
	(day (the-x-of-y {time point day} tp))
	(month (the-x-of-y {time point month} tp))
	(year (the-x-of-y {time point year} tp))
	(offset1 nil)
	(offset2 nil)
	(new-p nil)) ;; returned point
    (unless zone1
      (commentary "~A has missing clock-time information." tp)
      (return-from tp-in-zone nil))
    ;; if the zones are the same return tp
    (when (is-x-eq-y? zone1 zone2)
      (return-from tp-in-zone tp))
        ;; find the offsets
    (setq offset1 (get-zone-offset zone1))
    (setq offset2 (get-zone-offset zone2))
    (unless (and offset1 offset2)
      (commentary "Unknown time zone offsets for ~A or ~A." zone1 zone2)
      (return-from tp-in-zone nil))
    ;; calculate the total offset and put it in offset1
    (setq offset1 (- offset2 offset1))    ;; if the total offset is not a whole number we need the minute
    (multiple-value-bind (q r) (floor offset1)
      (when (/= r 0)
	(setq minute (tp-in-zone-helper tp {time point minute}))
	(unless minute (return-from tp-in-zone nil))
	(setq minute (+ minute (* 60 r))))
      (when (/= q 0)
	(setq hour (tp-in-zone-helper tp {time point hour}))
	(unless hour (return-from tp-in-zone nil))
	;; calculate the new hour
	(setq hour (+ q hour))))
    ;; Here we're done with the offset, and we updated the minute
    ;; and hour roles. Now we have to check if the minute and hour
    ;; are out of range. If they are, we'll propagate the changes
    ;; up the levels as necessary.

    ;; if minute is < 0 or > 59 update hour and minute
    (when (and minute (numberp minute) (or (< minute 0) (> minute 59)))
      ;; we're going to need the hour, make sure we have it
      (unless (and hour (numberp hour))
	(setq hour (tp-in-zone-helper tp {time point hour}))
	(unless hour (return-from tp-in-zone nil)))

      (cond ((< minute 0)
	     (setq hour (- hour 1))
	     (setq minute (+ minute 60)))
	    ((> minute 59)
	     (setq hour (+ hour 1))
	     (setq minute (- minute 60)))))
    ;; if hour is < 0 or > 23 we need the day
    (when (and hour (numberp hour) (or (< hour 0) (> hour 23)))
      (setq day (tp-in-zone-helper tp {time point day}))
      (unless day (return-from tp-in-zone nil))
      
      (cond ((< hour 0)
	     (setq day (- day 1))
	     (setq hour (+ hour 24)))
	    ((> hour 23)
	     (setq day (+ day 1))
	     (setq hour (- hour 24))))
      ;; if day is < 0 or > 28 we need the month
      (when (or (< day 1) (> day 28))
	(setq month (tp-in-zone-helper tp {time point month}))
	(unless month (return-from tp-in-zone nil))
	(cond
	  ;; we're going back a day
	  ((= day 0)
	   (setq month (- month 1))
	   ;;set the day according to the month
	   (cond ((or (= month 1) (= month 3) (= month 5) 
		      (= month 7) (= month 8) (= month 10) (= month 12))
		  (setq day 31))
		 ((or (= month 4) (= month 6) (= month 9) (= month 11))
		  (setq day 30))
		 ;; if the month is feb check the leap year stuff
		 ((= month 2)
		  (setq year (tp-in-zone-helper tp {time point year}))
		  (unless year (return-from tp-in-zone nil))
		  (if (leap-year? year)
		      (setq day 29)
		      (setq day 28)))
		 ;; if the month is 0 we gotta update the year, month and day
		 ((= month 0)
		  (setq year (tp-in-zone-helper tp {time point year}))
		  (unless year (return-from tp-in-zone nil))
		  (setq day 31)
		  (setq month 12)
		  (setq year (1- year))
		  (when (= year 0) (setq year -1)))))
	  ;; we might go a day forward
	  ;; check if february on non-leap year
	  ((= day 29)
	   (when (= month 2)
	     (setq year (tp-in-zone-helper tp {time point year}))
	     (unless year (return-from tp-in-zone nil))	     
	     (when (not (leap-year? year))
	       (setq day 1)
	       (setq month 3))))
	  ;; check if february on leap-year
	  ((= day 30)
	   (when (= month 2)
	     (setq day 1)
	     (setq month 3)))
	  ;; check if we're in a 30-day month
	  ((= day 31)
	   (when (or (= month 4) (= month 6) (= month 9) (= month 11))
	     (setq day 1)
	     (setq month (1+ month))))
	  ((= day 32)
	   (setq day 1)
	   (setq month (1+ month))
	   ;; if we have to update the year
	   (when (> month 12)
	     (setq month 1)
	     (setq year (tp-in-zone-helper tp {time point year}))
	     (unless year (return-from tp-in-zone nil))
	     (setq year (1+ year))
	     (when (= year 0) (setq year 1)))))))
    ;; finally we have everything we need...
    ;; if any thing is NIL it wasn't needed. 
    (if minute
	(when (numberp minute)
	  (setq minute (get-clock-minute minute)))
	(setq minute (the-x-role-of-y {time point minute} tp)))
    (if hour
	(when (numberp hour)
	  (setq hour (get-clock-hour hour)))
	(setq hour (the-x-role-of-y {time point hour} tp)))
    (if day
	(when (numberp day)
	  (setq day (get-calendar-day :ind day)))
	(setq day (the-x-role-of-y {time point day} tp)))
    (if month
	(when (numberp month)
	  (setq month (get-calendar-month month)))
	(setq month (the-x-role-of-y {time point month} tp)))
    (if year
	(when (numberp year)
	  (setq year (get-calendar-year (abs year) :ce (> year 0))))
	(setq year (the-x-role-of-y {time point year} tp)))
    ;;create the new time point and make it eq to the old
    (setq new-p (new-time-point
		 :iname iname
		 :zone zone2
		 :second (or (the-x-of-y {time point second} tp)
			     (the-x-role-of-y {time point second} tp))
		 
		 :minute minute
		 :hour hour
		 :day day
		 :month month
		 :year year))
    (new-eq tp new-p)
    new-p))

(defun ti-in-zone (ti zone &key iname)
  (setq ti (lookup-element-test ti))
  (setq zone (lookup-element-test zone))
  (let ((tp1 (ti-start ti))
	(tp2 (ti-finish ti))
	(new-ti nil))
    (setq tp1 (tp-in-zone tp1 zone))
    (setq tp2 (tp-in-zone tp2 zone))
    (setq new-ti (new-time-interval tp1 tp2 :iname iname :zone zone))
    (new-eq ti new-ti)
    new-ti))

;;; --------------------------------------------------------------
;;; Time Reference Comparison Functions
;;; --------------------------------------------------------------

;; ----------------------------
;; Comparison Helper Functions
;; ----------------------------

(defun tp-comp-tp-helper (tp1 tp2 role)
  "Given two time points and a descriptor role, compares
   the roles of each time point and returns one of :BEFORE,
   :EQUAL, :AFTER or :UNSURE."
  (let ((r1 (the-x-of-y role tp1))
	(r2 (the-x-of-y role tp2)))
    ;; if any one of them is not defined, we're not sure
    (cond ((or (null r1) (null r2))
	   :UNSURE)
	  (t
	   ;;otherwise we can do an exact comparison!
	   (setq r1 (get-interval-index r1))
	   (setq r2 (get-interval-index r2))
	   (cond ((= r1 r2) :EQUAL)
		 ((< r1 r2) :BEFORE)
		 ((> r1 r2) :AFTER))))))

;;; ---------------------------------------------------
;;; TP-X-TP?: Relation finder for time points
;;; ---------------------------------------------------

(defun tp-x-tp? (tp1 tp2 &key (create t))
  "Compares the two time points and returns one of :BEFORE, :AFTER, :EQUAL
   or :UNSURE. If the returned symbol is not :UNSURE, it represents
   the relation between TP1 and TP2. If the returned symbol is not
   :UNSURE and :CREATE is non-nil, the statement between the time points
   is created."
  (setq tp1 (lookup-element-test tp1))
  (setq tp2 (lookup-element-test tp2))
  ;; if there already is a relation, don't bother the check
  (cond ((statement-true? tp1 {before} tp2)
	 (return-from tp-x-tp? :BEFORE))
	((statement-true? tp2 {before} tp1)
	 (return-from tp-x-tp? :AFTER))
	((is-x-eq-y? tp1 tp2)
	 (return-from tp-x-tp? :EQUAL)))
  ;; exhaustive check in the same time zone
  (let* ((zone (the-x-of-y {time point zone} tp1))
	 (tp2 (tp-in-zone tp2 zone))
	 (certain? t) ;; there's no evidence for doubt
	 (res nil))   ;; result variable
    ;; year check
    (setq res (tp-comp-tp-helper tp1 tp2 {time point year}))
    (cond ((eq res :AFTER)
	   (when create (new-statement tp2 {before} tp1))
	   (return-from tp-x-tp? :AFTER))
	  ((eq res :BEFORE)
	   (when create (new-statement tp1 {before} tp2))
	   (return-from tp-x-tp? :BEFORE))
	  ((eq res :UNSURE)
	   (setq certain? nil)))
    ;; month check
    (setq res (tp-comp-tp-helper tp1 tp2 {time point month}))
    (cond ((eq res :AFTER)
	   (when create (new-statement tp2 {before} tp1))
	   (return-from tp-x-tp? :AFTER))
	  ((eq res :BEFORE)
	   (when create (new-statement tp1 {before} tp2))
	   (return-from tp-x-tp? :BEFORE))
	  ((eq res :UNSURE)
	   (setq certain? nil)))
    ;; day check
    (setq res (tp-comp-tp-helper tp1 tp2 {time point day}))
    (cond ((eq res :AFTER)
	   (when create (new-statement tp2 {before} tp1))
	   (return-from tp-x-tp? :AFTER))
	  ((eq res :BEFORE)
	   (when create (new-statement tp1 {before} tp2))
	   (return-from tp-x-tp? :BEFORE))
	  ((eq res :UNSURE)
	   (setq certain? nil)))
    ;; hour check
    (setq res (tp-comp-tp-helper tp1 tp2 {time point hour}))
    (cond ((eq res :AFTER)
	   (when create (new-statement tp2 {before} tp1))
	   (return-from tp-x-tp? :AFTER))
	  ((eq res :BEFORE)
	   (when create (new-statement tp1 {before} tp2))
	   (return-from tp-x-tp? :BEFORE))
	  ((eq res :UNSURE)
	   (setq certain? nil)))
    ;; minute check
    (setq res (tp-comp-tp-helper tp1 tp2 {time point minute}))
    (cond ((eq res :AFTER)
	   (when create (new-statement tp2 {before} tp1))
	   (return-from tp-x-tp? :AFTER))
	  ((eq res :BEFORE)
	   (when create (new-statement tp1 {before} tp2))
	   (return-from tp-x-tp? :BEFORE))
	  ((eq res :UNSURE)
	   (setq certain? nil)))
    ;; second check
    (setq res (tp-comp-tp-helper tp1 tp2 {time point second}))
    (cond ((eq res :AFTER)
	   (when create (new-statement tp2 {before} tp1))
	   (return-from tp-x-tp? :AFTER))
	  ((eq res :BEFORE)
	   (when create (new-statement tp1 {before} tp2))
	   (return-from tp-x-tp? :BEFORE))
	  ((eq res :UNSURE)
	   (setq certain? nil)))
    ;; if we reached here, either everything was EQ
    ;; and we are certain, or there is uncertainty somewhere
    (if certain? :EQUAL :UNSURE)))


;;; ---------------------------------------------------
;;; TP-X-TP?: Relation finder for time points
;;; ---------------------------------------------------

(defun ti-x-ti? (ti1 ti2 &key (create t))
  "Compares the two time intervals and returns one of :BEFORE,
   :AFTER, :EQUAL, :CONTAINS, :CONTAINS-STRICT, :STARTED-BY, :FINISHED-BY,
   :OVERLAPS or :UNSURE. If the value is not :UNSURE, then it
   represents the relation that holds between TI1 and TI2."
  (setq ti1 (lookup-element-test ti1))
  (setq ti2 (lookup-element-test ti2))
  (let* ((ti1s (ti-start  ti1))
	 (ti1f (ti-finish ti1))
	 (ti2s (ti-start  ti2))
	 (ti2f (ti-finish ti2))
	 (1s2s (tp-x-tp? ti1s ti2s :create create))
	 (1s2f (tp-x-tp? ti1s ti2f :create create))
	 (1f2s (tp-x-tp? ti1f ti2s :create create))
	 (1f2f (tp-x-tp? ti1f ti2f :create create)))
    (cond
      ;; Equality
      ((and (eq 1s2s :EQUAL) (eq 1f2f :EQUAL))
       (unless (is-x-eq-y? ti1 ti2)
	 (when create (new-eq ti1 ti2)))
       (return-from ti-x-ti? :EQUAL))
      ;; Before
      ((eq 1f2s :BEFORE)
       (unless (statement-true? ti1 {before} ti2)
	 (when create (new-statement ti1 {before} ti2)))
       (return-from ti-x-ti? :BEFORE))
      ;; After
      ((eq 1s2f :AFTER)
       (unless (statement-true? ti2 {before} ti1)
	 (when create (new-statement ti2 {before} ti1)))
       (return-from ti-x-ti? :AFTER))
      ;; Meets
      ((eq 1f2s :EQUAL)
       (unless (statement-true? ti1 {meets time} ti2)
	 (when create (new-statement ti1 {meets time} ti2)))
       (return-from ti-x-ti? :MEETS))
      ;; Contains-strict
      ((and (eq 1s2s :BEFORE) (eq 1f2f :AFTER))
       (unless (statement-true? ti1 {contains time strict} ti2)
	 (when create (new-statement ti1 {contains time strict} ti2)))
       (return-from ti-x-ti? :CONTAINS-STRICT))
      ;; Started by
      ((and (eq 1s2s :EQUAL) (eq 1f2f :AFTER))
       (unless (statement-true? ti1 {is started by} ti2)
	 (when create (new-statement ti1 {is started by} ti2))
       (return-from ti-x-ti? :STARTED-BY)))
      ;; Finished by
      ((and (eq 1s2s :BEFORE) (eq 1f2f :EQUAL))
       (unless (statement-true? ti1 {is finished by} ti2)
	 (when create (new-statement ti1 {is finished by} ti2)))
       (return-from ti-x-ti? :FINISHED-BY))
      ;; Contains
      ((and (or (eq 1s2s :EQUAL) (eq 1s2s :BEFORE))
	    (or (eq 1f2f :EQUAL) (eq 1f2f :AFTER)))
       (unless (statement-true? ti1 {contains time} ti2)
	 (when create (new-statement ti1 {contains time} ti2)))
       (return-from ti-x-ti? :CONTAINS))
      ;; Overlaps
      ((or (and (eq 1s2s :BEFORE) (eq 1f2s :AFTER) (eq 1f2f :BEFORE))
	   (and (eq 1s2s :AFTER) (eq 1s2f :BEFORE) (eq 1f2f :AFTER)))
       (unless (statement-true? ti1 {overlaps} ti2)
	 (when create (new-statement ti1 {overlaps} ti2)))
       (return-from ti-x-ti? :OVERLAPS))
      ((or (eq 1s2s :UNSURE) (eq 1s2f :UNSURE)
	   (eq 1f2s :UNSURE) (eq 1f2f :UNSURE))
       :UNSURE)
      (t nil))))

;; ---------------------------------------------
;; Equality Relations
;; ---------------------------------------------

(defun tp-eq-tp? (tp1 tp2 &key (create t))
  (when (is-x-eq-y? tp1 tp2)
    (return-from tp-eq-tp? t))
  (let ((ret (tp-x-tp? tp1 tp2 :create create)))
    (cond ((eq ret :EQUAL) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

(defun ti-eq-ti? (ti1 ti2 &key (create t))
  (when (is-x-eq-y? ti1 ti2)
    (return-from ti-eq-ti? t))
  (let ((ret (ti-x-ti? ti1 ti2 :create create)))
    (cond ((eq ret :EQUAL) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

;; ---------------------------------------------
;; Before Predicate
;; ---------------------------------------------

(defun before? (t1 t2 &key (create t))
  (setq t1 (lookup-element-test t1))
  (setq t2 (lookup-element-test t2))
  (unless (and (is-x-a-y? t1 {time reference})
	       (is-x-a-y? t2 {time reference}))
    (error "Both arguments must be time references!"))
  (when (statement-true? t1 {before} t2)
    (return-from before? t))
  (let ((ret nil))
    (if (is-x-a-y? t1 {time point})
	(if (is-x-a-y? t2 {time point})
	    (setq ret (tp-x-tp? t1 t2 :create create))
	    (setq ret (tp-x-tp? t1 (ti-start t2) :create create)))
	(if (is-x-a-y? t2 {time point})
	    (setq ret (tp-x-tp? (ti-finish t1) t2 :create create))
	    (setq ret (ti-x-ti? t1 t2 :create create))))
    ;; ret contains the answer
    (cond ((eq ret :BEFORE) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

;; ---------------------------------------------
;; After Predicate
;; ---------------------------------------------

(defun after? (t1 t2 &key (create t))
  (setq t1 (lookup-element-test t1))
  (setq t2 (lookup-element-test t2))
  (unless (and (is-x-a-y? t1 {time reference})
	       (is-x-a-y? t2 {time reference}))
    (error "Both arguments must be time references!"))
  (when (statement-true? t2 {before} t1)
    (return-from after? t))
  (let ((ret nil))
    (if (is-x-a-y? t1 {time point})
	(if (is-x-a-y? t2 {time point})
	    (setq ret (tp-x-tp? t1 t2 :create create))
	    (setq ret (tp-x-tp? t1 (ti-finish t2) :create create)))
	(if (is-x-a-y? t2 {time point})
	    (setq ret (tp-x-tp? (ti-start t1) t2 :create create))
	    (setq ret (ti-x-ti? t1 t2 :create create))))
    ;; ret contains the answer
    (cond ((eq ret :AFTER) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

;; ----------------------------
;; During and Containment Relations
;; ----------------------------

(defun t-during-ti? (t1 ti &key (create t))
  "Test if the Allen during relation holds. In other words,
   if start(TI2) > T1 < end(TI2) then return T. Otherwise
   return NIL or if unsure, return :UNSURE."
  (setq t1 (lookup-element-test t1))
  (setq ti (lookup-element-test ti))
  (unless (and (is-x-a-y? t1 {time reference})
	       (is-x-a-y? ti {time interval}))
    (error "Arg1 must be a time reference and Arg2 must be a time interval!"))
  ;; if there is a strict containment relation, don't bother checking
  (when (statement-true? ti {contains time strict} t1)
    (return-from t-during-ti? t))
  (let ((ret1 nil)
	(ret2 nil))
    (cond ((is-x-a-y? t1 {time point})
	   (setq ret1 (tp-x-tp? t1 (ti-start ti) :create create))
	   (setq ret2 (tp-x-tp? t1 (ti-finish ti) :create create))
	   (cond ((and (eq ret1 :AFTER) (eq ret2 :BEFORE))
		  (when create (new-statement ti {contains time strict} t1))
		  t)
		 ((or (eq ret1 :UNSURE) (eq ret2 :UNSURE)) :UNSURE)
		 (t nil)))
	  (t
	   (setq ret1 (ti-x-ti? ti t1 :create create))
	   (cond ((eq ret1 :CONTAINS-STRICT) t)
		 ((eq ret1 :UNSURE) :UNSURE)
		 (t nil))))))


(defun ti-contains-t? (ti t1 &key (create t))
  "Test if the Allen contains relation holds. In other words,
   if start(TI) <= T <= end(TI) then return T. Otherwise
   return NIL or if unsure, return :UNSURE."
  (setq ti (lookup-element-test ti))
  (setq t1 (lookup-element-test t1))
  (unless (and (is-x-a-y? ti {time interval})
	       (is-x-a-y? t1 {time reference}))
    (error "Arg1 must be a time interval and Arg2 must be a time reference!"))

  ;; if there is a containment relation, don't bother checking
  (when (statement-true? ti {contains time} t1)
    (return-from ti-contains-t? t))
  (let ((ret1 nil)
	(ret2 nil))
    (cond ((is-x-a-y? t1 {time point})
	   (setq ret1 (tp-x-tp? t1 (ti-start ti) :create create))
	   (setq ret2 (tp-x-tp? t1 (ti-finish ti) :create create))
	   (cond ((and (or (eq ret1 :EQUAL) (eq ret1 :BEFORE))
		       (or (eq ret2 :EQUAL) (eq ret2 :AFTER)))
		  (when create (new-statement ti {contains time} t1))
		  t)
		 ((or (eq ret1 :UNSURE) (eq ret2 :UNSURE)) :UNSURE)
		 (t nil)))
	  (t
	   (setq ret1 (ti-x-ti? ti t1 :create create))
	   (cond ((eq ret1 :CONTAINS) t)
		 ((eq ret1 :UNSURE) :UNSURE)
		 (t nil))))))

(defun ti-overlaps-ti? (ti1 ti2 &key (create t))
  "Test if the Allen overlaps relation holds. If two overlaps,
   return T, otherwise Nil, if unsure return :UNSURE."
  (setq ti1 (lookup-element-test ti1))
  (setq ti2 (lookup-element-test ti2))

  (unless (and (is-x-a-y? ti1 {time interval})
	       (is-x-a-y? ti2 {time interval}))
    (error "Arg1 must be a time interval and Arg2 must be a time interval!"))
  ;; if there is a containment relation, don't bother checking
  (when (statement-true? ti1 {overlaps} ti2)
    (return-from ti-overlaps-ti? t))
  (let ((ret (ti-x-ti? ti1 ti2 :create create)))
    (cond ((eq ret :OVERLAPS) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

(defun ti-starts-ti? (ti1 ti2 &key (create t))
  "Test if the Allen starts relations holds. If ti1 starts
   ti2 then return T, otherwise NIL, if unsure return :UNSURE."
  (setq ti1 (lookup-element-test ti1))
  (setq ti2 (lookup-element-test ti2))
  (unless (and (is-x-a-y? ti1 {time interval})
	       (is-x-a-y? ti2 {time interval}))
    (error "Arg1 must be a time interval and Arg2 must be a time interval!"))
  ;; if there is a containment relation, don't bother checking
  (when (statement-true? ti2 {is started by} ti1)
    (return-from ti-starts-ti? t))
  (let ((ret (ti-x-ti? ti2 ti1 :create create)))
    (cond ((eq ret :STARTED-BY) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))


(defun ti-finishes-ti? (ti1 ti2 &key (create t))
  "Test if the Allen finishes relation holds. If ti1 finishes
   ti2 then return T, otherwise NIL, if unsure return :UNSURE."
  (setq ti1 (lookup-element-test ti1))
  (setq ti2 (lookup-element-test ti2))
  (unless (and (is-x-a-y? ti1 {time interval})
	       (is-x-a-y? ti2 {time interval}))
    (error "Arg1 must be a time interval and Arg2 must be a time interval!"))
  ;; if there is a containment relation, don't bother checking
  (when (statement-true? ti2 {is finished by} ti1)
    (return-from ti-finishes-ti? t))
  (let ((ret (ti-x-ti? ti2 ti1 :create create)))
    (cond ((eq ret :FINISHED-BY) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

(defun ti-meets-ti? (ti1 ti2 &key (create t))
  "Test if the Allen meets relation holds. If ti1 meets
   ti2 then return T, otherwise NIL, if unsure return :UNSURE."
  (setq ti1 (lookup-element-test ti1))
  (setq ti2 (lookup-element-test ti2))
  (unless (and (is-x-a-y? ti1 {time interval})
	       (is-x-a-y? ti2 {time interval}))
    (error "Arg1 must be a time interval and Arg2 must be a time interval!"))
  ;; if there is a containment relation, don't bother checking
  (when (statement-true? ti1 {meets time} ti2)
    (return-from ti-meets-ti? t))
  (let ((ret (ti-x-ti? ti1 ti2 :create create)))
    (cond ((eq ret :MEETS) t)
	  ((eq ret :UNSURE) :UNSURE)
	  (t nil))))

;;; --------------------------------------------------------------
;;; Time Description Functions
;;; --------------------------------------------------------------

(defun tp-description (tp)
  "Given a time point generates a human readable string representing the
  time point."
  (setq tp (lookup-element-test tp))
  (let ((calendar (the-x-of-y {time point calendar} tp))
	(zone (the-x-of-y {time point zone} tp))
	(year (the-x-of-y {time point year} tp))
	(month (the-x-of-y {time point month} tp))
	(day (the-x-of-y {time point day} tp))
	(hour (the-x-of-y {time point hour} tp))
	(minute (the-x-of-y {time point minute} tp))
	(second (the-x-of-y {time point second} tp)))
    (format nil "~A:~A:~A (~A), ~A-~A-~A (~A)"
	    (if hour (get-interval-index hour) "?")
	    (if minute (get-interval-index minute) "?")
	    (if second (get-interval-index second) "?")
	    (if zone (internal-name zone) "?")
	    (if day (get-interval-index day) "?")
	    (if month (get-interval-index month) "?")
	    (if year 
		(format nil "~A~A" (abs (get-interval-index year))
			(if (< (get-interval-index year) 0)
			    "BCE" "CE"))
		"?")
	    (if calendar (internal-name calendar) "?"))))

(defun ti-description (ti)
  (setq ti (lookup-element-test ti))
  (let ((s (ti-start ti))
	(f (ti-finish ti)))
    
    (format nil "[~A  --  ~A]" (tp-description s) (tp-description f))))


;;; --------------------------------------------------------------
;;; UNIX Time Converter
;;; --------------------------------------------------------------

;; The time in (milli)seconds since 00:00, 1-Jan-1900 UTC
(defun tp-from-utime (utime &key iname (milli nil))
  (when (integerp utime)
    (when milli
      (setq utime (floor (/ utime 1000))))
    (multiple-value-bind (sec min hr
			  day-ind mon yr
			  day daylight zone)
	(decode-universal-time utime)
      ;; figure out the zone and the day-of-week
      (setq zone (get-time-zone zone daylight))
      (new-time-point :iname iname
		      :zone zone
		      :year (get-calendar-year yr)
		      :month (get-calendar-month mon)
		      :day (get-calendar-day :ind day-ind :dow day)
		      :hour (get-clock-hour hr)
		      :minute (get-clock-minute min)
		      :second (get-clock-second sec)))))


;; The time in seconds since 00:00, 1-Jan-1970 UTC
(defun tp-from-unix (unix &key iname (milli nil))
  (when (integerp unix)
    (when milli
      (setq unix (floor (/ unix 1000))))
    ;; unix + unix-origin-point = utime
    (tp-from-utime
     (+ unix
	(encode-universal-time 0 0 0 1 1 1970 0))
     :iname iname)))

(defun get-now (&key iname)
  (tp-from-utime (get-universal-time) :iname iname))

(defun parse-time-point (time-string)
  "Given a string that represents a time point, tries
   to parse it into a time point. It currently supports
   three kinds of strings:
     o unix time in milliseconds (13digits)
     o unix time in seconds (10digits)
     o YYYYMMDDHHMM (12digits) : doesn't set the time zone"
  (multiple-value-bind (n num-digits)
      (parse-integer time-string :junk-allowed t)
    (cond ((= num-digits 12)
	   (let* ((year   (floor (/ n (expt 10 8))))
		  (month  (floor (/ (- n    (* year (expt 10 8)))   (expt 10 6))))
		  (day    (floor (/ (- n (+ (* year  (expt 10 8))
					    (* month (expt 10 6)))) (expt 10 4))))
		  (hour   (floor (/ (- n (+ (* year  (expt 10 8))
					    (* month (expt 10 6))
					    (* day   (expt 10 4)))) (expt 10 2))))
		  (minute (floor (- n (+ (* year  (expt 10 8))
					 (* month (expt 10 6))
					 (* day   (expt 10 4))
					 (* hour  (expt 10 2)))))))
	     (format t "~A~%" (list year month day hour minute))
	     (new-time-point :year (get-calendar-year year)
			     :month (get-calendar-month month)
			     :day (get-calendar-day :ind day)
			     :hour (get-clock-hour hour)
			     :minute (get-clock-minute minute))))
	  ((= num-digits 10)
	   (tp-from-unix n))
	  ((= num-digits 13)
	   (tp-from-unix n :milli t))
	  (t
	   (commentary "The string type is not supported or it is not well-formed.")))))

;;; --------------------------------------------------------------
;;; Time Interval Duration Calculations
;;; --------------------------------------------------------------
;;; Given a time interval function to calculate the duration
;;; with the best possible accuracy.

(defun duration-helper (tp1 tp2 role)
  "Given two time points and a descriptor role, returns the difference
   in terms of seconds of the roles of the TPs. If either role isn't defined
   for a TP then returns :UNSURE. (it doesn't include the duration of TP2 in the
   result)."
  (let ((r1 (the-x-of-y role tp1))
	(r2 (the-x-of-y role tp2)))
    ;; if any one of them is not defined, we're not sure
    (cond ((or (null r1) (null r2)) :UNSURE)
	  (t
	   ;;otherwise we can do an exact comparison
	   (setq r1 (get-interval-index r1))
	   (setq r2 (get-interval-index r2))

	   (cond ((is-x-eq-y? role {time point day})
		  (* 24 3600 (- r2 r1)))
		 ((is-x-eq-y? role {time point hour})
		  (* 3600 (- r2 r1)))
		 ((is-x-eq-y? role {time point minute})
		  (* 60 (- r2 r1)))
		 ((is-x-eq-y? role {time point second})
		  (* (- r2 r1)))
		 (t (error "Given role doesn't exist for time points!")))))))


(defun get-duration (tp1 tp2)
  "Given two time points TP1 and TP2, where TP1 < TP2, calculates
   and returns a time measure that represents the duration between
   TP1 and TP2. The duration calculated is accurate as possible,
   however if one time point is not accurate as the other one
   we will use the lower accuracy to calculate the duration. i.e.
   the duration between Feb, 1 2007 and 23PM, Feb, 5, 2007 is
   *about* 4 days."
  ;; make sure tp1 < tp2 or we're unsure
  (unless (before? tp1 tp2)
    (error "~A is after ~A." tp2 tp1))
  (let* ((tp1 (lookup-element tp1))
	 (zone (the-x-of-y {time point zone} tp1))
	 (tp2 (tp-in-zone tp2 zone))
	 ;; differences for each descriptor role
	 (year 0)
	 (month 0)
	 (day 0)
	 (hour 0)
	 (minute 0)
	 (second 0)
	 ;; role indeces for each tp
	 (r1 nil)
	 (r2 nil))
    (setq r1 (the-x-of-y {time point year} tp1))
    (setq r2 (the-x-of-y {time point year} tp2))
    ;; Year difference calculation
    (cond ((or (null r1) (null r2)) (setq year :UNSURE))
	  (t
	   (setq r1 (get-interval-index r1))
	   (setq r2 (get-interval-index r2))
	   ;; we know r1 <= r2
	   (dotimes (i (- r2 r1))
	     (if (leap-year? (+ i r1))
		 (setq year (+ year 31622400))
		 (setq year (+ year 31536000))))))
    (when (eq year :UNSURE)
      (return-from get-duration :UNSURE))
    ;; Month difference calculation
    (setq r1 (the-x-of-y {time point month} tp1))
    (setq r2 (the-x-of-y {time point month} tp2))
    (cond ((or (null r1) (null r2)) (setq month :UNSURE))
	  (t 
	   (setq r1 (get-interval-index r1))
	   (setq r2 (get-interval-index r2))
	   (let ((min-m (min r1 r2))
		 (max-m (max r1 r2))
		 (cur-m nil))
	     (dotimes (i (- max-m min-m))
	       (setq cur-m (+ i min-m))
	       (cond ((= 2 cur-m)
		      (if (leap-year? (the-x-of-y {time point year} tp2))
			  (setq month (+ month (* 29 24 3600)))
			  (setq month (+ month (* 28 24 3600)))))
		     ((or (= 1 cur-m) (= 3 cur-m) (= 5 cur-m)
			  (= 7 cur-m) (= 8 cur-m) (= 10 cur-m) (= 12 cur-m))
		      (setq month (+ month (* 31 24 3600))))
		     (t
		      (setq month (+ month (* 30 24 3600))))))
	     (when (> r1 r2)
	       (setq month (* -1 month))))))
    (when (eq month :UNSURE)
      (return-from get-duration (new-measure year {second})))
    ;; day check
    (setq day (duration-helper tp1 tp2 {time point day}))
    (when (eq day :UNSURE)
      (return-from get-duration (new-measure (+ month year) {second})))
    ;; if month < 0 then we have to subtract 1 from day so we don't count tp2
    (when (< month 0)
      (setq day (- day 86400)))
    ;; hour check
    (setq hour (duration-helper tp1 tp2 {time point hour}))
    (when (eq hour :UNSURE)
      (return-from get-duration (new-measure (+ day month year) {second})))
    ;; if day < 0 then we have to subtract 1 from hour so we don't count tp2
    (when (< day 0)
      (setq hour (- hour 3600)))
    ;; minute check
    (setq minute (duration-helper tp1 tp2 {time point minute}))
    (when (eq minute :UNSURE)
      (return-from get-duration (new-measure (+ hour day month year) {second})))
    ;; if hour < 0 then we have to subtract 1 from minute so we don't count tp2
    (when (< hour 0)
      (setq minute (- minute 60)))
    ;; second check
    (setq second (duration-helper tp1 tp2 {time point second}))
    ;; if minute < 0 then we have to subtract 1 from second so we don't count tp2
    (when (eq second :UNSURE)
      (return-from get-duration (new-measure (+ minute hour day month year) {second})))
    (when (< minute 0)
	(setq second (1- second)))
    (new-measure (+ year month day hour minute second) {second})))

(defun get-interval-duration (ti)
  (let ((tp1 (ti-start ti))
	(tp2 (ti-finish ti))
	(dur (ti-duration ti)))
    (when (null dur)
      (setq dur (get-duration tp1 tp2))
      (x-is-the-y-of-z dur {time interval duration} ti))
    dur))

;;; --------------------------------------------------------------
;;; Defvars
;;; --------------------------------------------------------------
(defvar *time-point* (lookup-element {time point}))
(defvar *time-interval* (lookup-element {time interval}))
(defvar *time-interval-start* (lookup-element {time interval start}))
(defvar *time-interval-end* (lookup-element {time interval end}))

