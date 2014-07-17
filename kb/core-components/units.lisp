;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; Units, measures, and quantities for Scone's core knowledge base.
;;;
;;; Author & Maintainer: Scott E. Fahlman
;;; Contributor:  E. Cinar Sahin
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

;;; The set of all qualities measured by units.
(new-type {measurable quality} {stuff})

;;; Generally qualities are intangible.
(new-is-a {measurable quality} {intangible})

;;; The set of all units.
(new-type {unit} {intangible})

;;; Every unit measures some quality.
(new-indv-role {unit measurable quality}
	       {unit}
	       {measurable quality}
	       :english '(:role "quality" "measurable quality"))

;;; We use MEASURE for a value that has a magnitude (a number) and a unit,
;;; such as {16.0 ton} or {5280 foot} or {3/4 gigabyte}.
(new-type {measure} {intangible})
(new-indv-role {measure magnitude} {measure} {number}
              :english '(:role "magnitude"))
(new-indv-role {measure unit} {measure} {unit}
              :english '(:role "unit"))

;;; For each mesasurable quality, there's a measure type.
(new-indv-role {measure type} {measurable quality} {measure})

;;; Create some splits for qualities, units and measure types.  These will
;;; be populated as we create new types.
(new-split nil :iname {measurable quality split})
(new-split nil :iname {unit split})
(new-split nil :iname {measure type split})

;;; Function to create a new measurable quality and related structures.

(defun new-measurable-quality (iname &rest english)
  "Create a new measurable quality with the specified INAME and optional
   additional names.  For each measurable quality such as {area}, we
   create a new measure-type such as {area measure}.  Also add these to
   splits to keep them distinct from other qualities and measure types."
  (let* ((quality (new-indv iname {measurable quality} :english english))
	 (measure-type
	  (new-type (make-element-iname
		     :value (format nil "~A measure"
				    (internal-name quality)))
		    {measure})))
    (x-is-the-y-of-z measure-type {measure type} quality)
    (add-to-split {measurable quality split} quality)
    (add-to-split {measure type split} measure-type)
    quality))

;;; We use QUANTITY for a particular blob of some subtance.  It has a
;;; STUFF and a MEASURE, such as "sixteen tons of coal" or "ten
;;; minutes (of time)".

(new-type {quantity} {intangible})
(new-indv-role {quantity measure} {quantity} {measure})
(new-indv-role {quantity stuff} {quantity} {stuff})

;;; Each measurable quality has a base-unit. This could be
;;; conventional or could be an arbitrary choice.
(new-indv-role {base unit} {measurable quality} {unit})

;;; Each unit is either a base unit for the quality or is defined as
;;; some ratio times another unit, called the "relative unit". The
;;; relative unit does not have to be the base-unit, but for each unit
;;; there must be a directed chain of relative units that leads back
;;; to the quality's base-unit.  So we might define {meter} as the
;;; base-unit for {length}, {centimeter} as 1/100 meters, {inch} as
;;; 2.54 centimeters, and {foot} as 12 inches.  Define the roles to
;;; implement this.
(new-indv-role {unit relative unit} {unit} {unit})
(new-indv-role {unit relative unit ratio} {unit} {number})

;;; For some unit pairs, such as degrees Farenheit to degrees Celsius
;;; there is a special converstion function.  We express this with a
;;; {unit converts to} relation.  The function takes one arg, the number
;;; of A units, and returns a number, the corresponding number of B units.
(new-relation {unit converts to}
	      :a-inst-of {unit}
	      :b-inst-of {unit}
	      :c-inst-of {function})

(defun new-unit (unit-iname quality &optional relative-unit (ratio 1)
			    &rest english)
  "Define a new element representing a unit.  UNIT-INAME is the new
   unit's iname.  QUALITY is a pre-existing measurable quality.

   The new unit is defined as RATIO times some other existing unit,
   the RELATIVE-UNIT.  If RELATIVE-UNIT is not supplied, the new unit
   becomes the base-unit for this QUALITY.  The RELATIVE-UNIT does not
   have to be the base-unit, but for each unit there must be a
   directed chain of relative units that leads back to the quality's
   base-unit.

   If ENGLISH is present, it becomes the :ENGLISH argument for the new
   unit, providing alternative English names."
  (setq quality (lookup-element-test quality))
  (unless (is-x-a-y? quality {measurable quality})
    (error "Element ~A is not a known measurable quality." quality))
  ;; Create the element for this unit.
  (let* ((unit (new-indv unit-iname {unit})))
    (when english (apply #'english unit english))
    ;; Record the associated measurable quality.
    (x-is-the-y-of-z quality {unit measurable quality} unit)
    (cond
      ;; If there is a RELATIVE-UNIT, record it and the RATIO.
      (relative-unit
       (setq relative-unit (lookup-element-test relative-unit))
       (setq ratio (lookup-element-test ratio))
       (let ((relative-unit-quality
	      (the-x-of-y {unit measurable quality} relative-unit)))
	 (unless (eq quality relative-unit-quality)
	   (error "The quality of relative unit ~A is ~A, not ~A."
		  relative-unit relative-unit-quality quality))
	 ;; Record the relative unit and ratio.
	 (x-is-the-y-of-z relative-unit {unit relative unit} unit)
	 (x-is-the-y-of-z ratio {unit relative unit ratio} unit)))
      ;; If there is no relative unit, complain if the quality already
      ;; has a base-unit.
      ((the-x-of-y {base unit} quality)
       (error "~A already has a base unit: ~A"
	      quality
	      (the-x-of-y {base unit} quality)))
      ;; Else, this becomes the quality's base unit.
      (t (x-is-the-y-of-z unit {base unit} quality)
	 (x-is-the-y-of-z unit {unit relative unit} unit)
	 (x-is-the-y-of-z 1 {unit relative unit ratio} unit)))
    ;; Split this unit from other units.
    (add-to-split {unit split} unit)
    ;; Return the unit element.
    unit))

;;; Function to create a new measure.

(defun new-measure (magnitude unit)
  "Create a new measure, given a MAGNITUDE and a UNIT."
  (setq unit (lookup-element-test unit))
  (unless (is-x-a-y? unit {unit})
    (error "~A is not a known {unit}." unit))
  (setq magnitude (lookup-element-test magnitude))
  (unless (is-x-a-y? magnitude {number})
    (error "~A is not a number." magnitude))
  ;; Give this new measure a human-readable name.
  (let ((name (make-element-iname
	       :value
	       (format Nil "measure ~A ~A"
		       (internal-name magnitude)
		       (internal-name unit)))))
    (or
     ;; If an identical measure (same name) already exists, just return that.
     (lookup-element name :syntax-tags '(:noun))
     ;; Doesn't exist, create and return the new measure element E.
     (let* ((quality (the-x-of-y {unit measurable quality} unit))
	    (measure-type (the-x-of-y {measure type} quality))
	    (e (new-indv name measure-type)))
       (x-is-the-y-of-z magnitude {measure magnitude} e)
       (x-is-the-y-of-z unit {measure unit} e)
       e))))

;;; Function to create a new quantity.

(defun new-quantity (magnitude unit stuff)
  "A conveninence function to create a new quantity, given a MAGNITUDE,
   a UNIT, and some kind of STUFF."
  (setq stuff (lookup-element-test stuff))
  (unless (is-x-a-y? stuff {stuff})
    (error "~A is not of type {stuff}." stuff))
  (let* ((m (new-measure magnitude unit))
	 ;; Give this new quantity a human-readable name.
	 (name (make-element-iname
		:value
		(format Nil "Quantity ~A of ~A"
			;; Strip off the "Measure " of the
			;; measure's name.
			(subseq (internal-name m) 8)
			(internal-name stuff)))))
    (or
     ;; If an identical quantity (same name) already exists, just
     ;; return that.
     (lookup-element name :syntax-tags '(:noun))
     ;; Doesn't exist, create and return the new quantity element
     ;; E.
     (let ((e (new-indv name {quantity})))
       (x-is-the-y-of-z m {quantity measure} e)
       (x-is-the-y-of-z stuff {quantity stuff} e)
       e))))

;;; Convert a measure to some other unit.
(defun convert-measure (measure unit)
  "Given a MEASURE element, return an equiavlent measure with the
   specified UNIT."
  (setq unit (lookup-element-test unit))
  (unless (is-x-a-y? unit {unit})
    (error "~A is not a known unit." unit))
  (setq measure (lookup-element-test measure))
#| %%% This doesn't work until we add statement-matching functions.
  
  ;; Sometimes we have a special conversion function from the unit in
  ;; the measure to the target unit.  This is stored in a
  ;; {unit converts to} statement with the function as the C argument.
  ;; See if we have one of those, and apply it if so.
  (let* ((measure-unit (the-x-of-y {measure unit} measure))
	 (conversion-rel
	  (statement-true? measure-unit {unit converts to} unit))
	 (conversion-fn (and conversion-rel
			     (c-wire conversion-rel)
			     (internal-name (c-wire conversion-rel)))))
    (when conversion-fn
      (return-from convert-measure
	(new-measure
	 (funcall conversion-fn
		  (internal-name (the-x-of-y {measure magnitude} measure)))
	 unit))))
|#
  ;; It's the normal case -- no special conversion function.  Just find
  ;; the correct ratio for conversion of these units.
  (let* ((quality (the-x-of-y {unit measurable quality} unit))
	 (base-unit (the-x-of-y {base unit} quality))
	 (measure-to-base-ratio 1)
	 (unit-to-base-ratio 1))
    ;; Unless the meaure and the unit relate to the same measurable
    ;; quality, complain.
    (unless (eq (the-x-of-y {measure measurable quality} measure)
		quality)
      (error "The unit ~A is not of the proper type." unit))
    ;;; Compute the ratio of the measure to the base unit for this
    ;;; quality.  Follow the chain down to the base unit.
    (do ((u (the-x-of-y {measure unit} measure))
	 (r (internal-name
	     (the-x-of-y {measure magnitude} measure))))
	((eq u base-unit)
	 (setq measure-to-base-ratio r))
      (setq r (* r (internal-name
		     (the-x-of-y {unit relative unit ratio} u))))
      (setq u (the-x-of-y {unit relative unit} u)))
    ;;; Now do the same for the UNIT..
    (do ((u unit)
	 (r 1))
	((eq u base-unit)
	 (setq unit-to-base-ratio r))
      (setq r (* r (internal-name
		     (the-x-of-y {unit relative unit ratio} u))))
      (setq u (the-x-of-y {unit relative unit} u)))
    (new-measure (/ measure-to-base-ratio unit-to-base-ratio)
		 unit)))  
  
;;; Define the commonly-used metric variants of a base unit.

;;; %%% We've got an abiguity with metric prefixes "M" and "m" until
;;; we put in some sort of case-sensitive machinery for English-name
;;; matching.

(defvar *metric-prefixes*
  '(("kilo" 3 "k")
    ("mega" 6 "M")
    ("giga" 9 "G")
    ("tera" 12 "T")
    ("centi" -2 "c")
    ("milli" -3 "m")
    ("micro" -6 "u")
    ("nano"  -9 "n")
    ("pico" -12 "p"))
  "For each of the commonly-used metric prefixes, list the long prefix
   name, the exponent, and the short-prefix name.")

(defun metric-prefix-expand (base-unit base-abbrev)
  "Given a BASE-UNIT for some quality, with abbreviation BASE-ABBREV,
   create all the commonly used multiples of this, based on standard
   metric prefixes."
  (setq base-unit (lookup-element-test base-unit))
  (let ((quality (the-x-of-y {unit measurable quality} base-unit))
	(base-name (internal-name base-unit)))
    ;; For each item in *METRIC-PREFIXES*, extract the components and
    ;; create a new unit.
    (dolist (mp *metric-prefixes*)
      (let ((unit-name
	     (format nil "~A~A"(first mp) base-name))
	    (abbrev-name
	     (format nil "~A~A" (third mp) base-abbrev))
	    (exponent (second mp)))
	;; Now make the new unit.
	(new-unit (make-element-iname :value unit-name)
		  quality
		  base-unit
		  (expt 10 exponent)
		  abbrev-name)))))

;;; Now we can define some measurable qualities and units.

;;; %%% Figure out how the NLP side should handle abbreviations, where
;;; %%% the period may be present or absent.

;;; Length
(new-measurable-quality {length} "distance")
(new-unit {meter} {length} nil 1 "m")
(metric-prefix-expand {meter} "m")
(english {micrometer} "micron")
(new-unit {inch} {length} {centimeter} 2.54 "in")
(new-unit {foot} {length} {inch} 12 "ft")
(new-unit {yard} {length} {foot} 3 "yd")
(new-unit {mile} {length} {foot} 5280 "mi")
(new-unit {light year} {length} {meter} 9.405284e15)
(new-unit {parsec} {length} {light year} 3.26163626)
(new-unit {furlong} {length} {mile} 1/8)
(new-unit {cubit} {length} {foot} 1.5)
(new-unit {league} {length} {mile} 3)
(new-unit {fathom} {length} {foot} 6)
(new-unit {smoot} {length} {inch} 67)

;;; Area
(new-measurable-quality {area})
(new-unit {square meter} {area} nil 1 "sq m")
(new-unit {square centimeter} {area} {square meter} (expt 1/100 2) "sq cm")
(new-unit {square millimeter} {area} {square meter} (expt 1/1000 2) "sq mm")
(new-unit {square kilometer} {area} {square meter} (expt 1000 2) "sq Km")
(new-unit {square inch} {area} {square centimeter} (expt 2.54 2) "sq in")
(new-unit {square foot} {area} {square inch} (expt 12 2) "sq ft")
(new-unit {square yard} {area} {square foot} (expt 3 2) "sq yd")
(new-unit {square mile} {area} {square foot} (expt 5280 2) "sq mi")
(new-unit {acre} {area} {square mile} 1/640)
(new-unit {hectare} {area} {square meter} 10000 "ha")

;;; Volume
(new-measurable-quality {volume})
(new-unit {cubic meter} {volume} nil 1 "cu m")
(new-unit {cubic centimeter} {volume} {cubic meter} (expt 1/100 3)
	  "cu cm" "cc" "milliliter" "mL")
(new-unit {cubic millimeter} {volume} {cubic meter} (expt 1/1000 3) "cu mm")
(new-unit {cubic kilometer} {volume} {cubic meter} (expt 1000 3) "cu Km")
(new-unit {cubic inch} {volume} {cubic centimeter} (expt 2.54 3) "cu in")
(new-unit {cubic foot} {volume} {cubic inch} (expt 12 3)  "cu ft")
(new-unit {cubic yard} {volume} {cubic foot} (expt 3 3) "cu yd")
(new-unit {cubic mile} {volume} {cubic foot} (expt 5280 3) "cu mi")
(new-unit {liter} {volume} {cubic meter} 1/1000 "L")
(new-unit {milliliter} {volume} {liter} 1/1000 "mL")
(new-unit {quart} {volume} {liter} 0.946352946 "qt")
(new-unit {gallon} {volume} {quart} 4 "g")
(new-unit {pint} {volume} {quart} 1/2 "pt")
(new-unit {cup} {volume} {pint} 1/2 "C")
(new-unit {fluid ounce} {volume} {pint} 1/16 "fl oz" "oz")
(new-unit {tablespoon} {volume} {fluid ounce} 1/2 "tbs")
(new-unit {teaspoon} {volume} {tablespoon} 1/3 "tsp")

;;; Mass
(new-measurable-quality {mass})
(new-unit {gram} {mass} nil 1 "g")
(metric-prefix-expand {gram} "g")
(new-unit {pound} {mass} {gram}  453.59237 "lb")
(new-unit {ounce} {mass} {pound} 1/16 "oz")
(new-unit {ton} {mass} {pound} 2000)
(new-unit {stone} {mass} {pound} 14)

;;; Temperature

(new-measurable-quality {temperature})
(new-unit {degree Kelvin} {temperature} nil 1 "K" "degree K")
(new-unit {degree Celsius} {temperature} {degree Kelvin} 1
	  "C" "degree centigrade" "degree C")
(new-unit {degree Fahrenheit} {temperature} {degree Kelvin} 5/9
	  "F" "degree F")
;;; %%% These conversions don't work yet.
(new-statement {degree Kelvin} {unit converts to} {degree Celsius}
	       :c #'(lambda (x) (- x 272.15)))
(new-statement {degree Celsius} {unit converts to} {degree Kelvin}
	       :c #'(lambda (x) (+ x 272.15)))
(new-statement {degree Celsius} {unit converts to} {degree Fahrenheit}
	       :c #'(lambda (x) (+ (* x 9/5) 32)))
(new-statement {degree Fahrenheit} {unit converts to} {degree Celsius}
	       :c #'(lambda (x) (* (- x 32) 5/9)))
(new-statement {degree Kelvin} {unit converts to} {degree Fahrenheit}
	       :c #'(lambda (x) (- (* x 9/5) 457.87)))
(new-statement {degree Fahrenheit} {unit converts to} {degree Kelvin}
	       :c #'(lambda (x) (* (+ x 457.87) 5/9)))

;;; Information or data
(new-measurable-quality {information} "data")
(new-unit {bit} {information} nil 1)
(new-unit {byte} {information} {bit} 8)
(metric-prefix-expand {byte} "B")

;;; Frequency
(new-measurable-quality {frequency})
(new-unit {Hertz} {frequency} nil 1 "Hz")
(metric-prefix-expand {Hertz} "Hz")

;;; Power
(new-measurable-quality {power})
(new-unit {Watt} {power} nil 1)
(metric-prefix-expand {Watt} "W")
(new-unit {horsepower} {power} {watt} 745.699872 "HP")

;;; Money
(new-measurable-quality {money})
(new-unit {dollar} {money} nil 1 "dollar (US)" "$")
(new-unit {cent} {money} {dollar} 1/100)
;;; %%% Add other currencies here, but use IF-NEEDED to get real-time
;;; lookup of exchange rate.

;;; Time Units

;; Note: To avoid internal-name conflicts, we use the word "unit" in
;; each of these unit names.  We do not do this with all units, but it
;; is important here.
(new-measurable-quality {time})
(new-unit {second unit} {time} nil 1 "second" "sec" "s")
(metric-prefix-expand {second unit} "sec")
(new-unit {minute unit} {time} {second unit} 60 "minute" "min" "m")
(new-unit {hour unit} {time} {minute unit} 60 "hour" "hr" "h")
(new-unit {day unit} {time} {hour unit} 24 "day" "d")
(new-unit {week unit} {time} {day unit} 7 "week" "wk")
(new-unit {28-day month unit} {time} {day unit} 28)
(new-unit {29-day month unit} {time} {day unit} 29)
(new-unit {30-day month unit} {time} {day unit} 30)
(new-unit {31-day month unit} {time} {day unit} 31)
(new-unit {normal year unit} {time} {day unit} 365 "year" "yr")
(new-unit {leap year unit} {time} {day unit} 366 "leap year")
;;; These approximations are good enough for much common-sense reasoning.
(new-unit {month unit} {time} {day unit} 30 "month")
(new-unit {year unit} {time} {day unit} 365.25 "year")
(new-unit {decade unit} {time} {year unit} 10)
(new-unit {century unit} {time} {year unit} 100)
(new-unit {millennium unit} {time} {year unit} 1000)

