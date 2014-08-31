;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; General knowledge of geography, government, and politics for
;;; Scone's core knowledge base.
;;;
;;; Authors: Scott E. Fahlman
;;;          Engin Cinar Sahin
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

;;;; HIERACHY OF LAND AND WATER AREAS

;;; Note: This is pretty spotty because to do this right we really need
;;; 2-D visualization.

;;; Split the areas on earth into land, water, and wetlands.
;;; Some areas may not be in any of these classes, usually because they
;;; contain a combination of land and water.
(new-split-subtypes {geographical area}
		    '({land area} {water area} {wetland area}))

(new-indv-role {geographical area area}
	       {geographical area}
	       {area measure}
	       :english '(:role "area" "size"))

(english {wetland area} "wetland" "marsh")

;;;; LAND AREAS

(new-indv-role {population} {land area} {integer}
	       :english '(:role "population" "size"))


;;; Topographical land-area types.
(new-split-subtypes {land area}
		    '({continent} {island} {isthmus} {peninsula}))

;;; Define a few of each type of topographical area.

;;; By convention, these are all the continents.  I'm not
;;; quite sure what to do about {Eurasia}.
(new-complete-members {continent}
 '({North America} {South America} {Europe} {Asia} {Africa}
   {Australia (continent)} {Antarctica}))

(english {North America} :iname)
(english {South America} :iname)
(english {Europe} :iname)
(english {Asia} :iname)
(english {Africa} :iname)
(english {Antarctica} :iname)

;;; NOTE: We will let the name "Australia" refer to the country and
;;; tie that to {australia (coontinent)} with an EQ-link.
; 
;;; Some other continent-like areas.
(new-indv {The Americas}
	  {land area}
	  :english :iname)

(new-indv {Central America}
	  {land area}
	  :english :iname)

(new-indv {Caribbean Region}
	  {land area}
	  :english :iname)

(new-indv {Latin America}
	  {land area}
	  :english :iname)

(new-indv {Oceania}
	  {land area}
	  :english :iname)

(x-is-a-y-of-z {North America} {part} {The Americas})
(x-is-a-y-of-z {South America} {part} {The Americas})
(x-is-a-y-of-z {Central America} {part} {The Americas})
(x-is-a-y-of-z {Caribbean Region} {part} {The Americas})
(x-is-a-y-of-z {Central America} {part} {North America})

(x-is-a-y-of-z {Central America} {part} {Latin America})
(x-is-a-y-of-z {South America} {part} {Latin America})
(x-is-a-y-of-z {Caribbean Region} {part} {Latin America})


;;; Some islands.  Islands that are also countries get their
;;; primary definitions elsewhere. 

(new-members {island}
 '({Greenland (island)} {Borneo} {Sumatra} {Great Britain} {Honshu}
   {Kyushu} {Shikoku} {Hokkaido} {Manhattan} {Long Island}))

(new-members {isthmus}
 '({Isthmus of Panama} {Isthmus of Suez} {Isthmus of Corinth}))

(new-members {peninsula}
 '({Arabian Peninusla} {Iberian Peninsula} {Scandinavian Peninsula}
   {Baja California} {Italian Peninsula}  {Balkan Paninsula}))

;;; Terrain types.
(new-split-subtypes {land area}
 '(({mountainous} :adj)
   ({hilly} :adj)
   ({flat} :adj)))

(new-split-subtypes {land area}
   '({wooded} {grassland} {urban} {farmland} {desert}
     {frozen}))

(english {wooded} :adj "wooded" :adj-noun "forest")
(english {grassland} "grassland" "prairie" "savannah")
(english {urban} :adj "urban" "developed")
(english {farmland} "farmland" :adj "agriculutral")
(english {desert} "desert" :adj "arid" "dry")
(english {frozen} :adj "frozen" "frigid" "icy" :noun "tundra")

;;; Political divisions.

(new-split-subtypes {land area}
 '({country} {possession} {municipality} {federal district})
 :iname {political division}
 :english :iname)

(english {country} :iname)
(english {country} "nation" "state")
(english {municipality} :iname)
(english {federal district} :iname)
(english {possession} :role "possession")

;;; These two types seem to be exclusive.

(new-split-subtypes {country}
  '({country with states} {country with provinces}))

;;; This one is not exclusive.  E.g. Canada has both
;;; provinces and territories.

(new-type {country with territories} {country})

(new-type-role {state}
	       {country with states}
               {land area}
	       :english '(:role "state"))

(x-is-a-y-of-z {state}
	       {part}
	       {country with states})

; (get-set-node {state})

(new-type-role {province}
	       {country with provinces}
               {land area}
	       :english '(:role :iname))

(x-is-a-y-of-z {province}
	       {part} {country with provinces})

; (get-set-node {province})

(new-type-role {territory}
	       {country with territories}
               {land area}
	       :english '(:role :iname))

(x-is-a-y-of-z {territory}
	       {part}
	       {country with territories})

; (get-set-node {territory})

(new-split-subtypes {municipality}
 '({city} {town} {village}))

;;; We want to talk about the cities OF a certain country
;;; as well as talking bout cities in general.  We can only use
;;; the {city} as an iname for one or the other element in the same
;;; file, but can use "city" as the external anem for both.

(new-type-role {city (role)} {country}
	       {city}
	       :english '(:role "city"))

(new-indv-role {national capital} {country}
	       {city}
	       :english '(:role "capital" "national capital"))

(new-indv-role {provincial capital}
	       {province}
	       {city}
	       :english '(:role "capital" "provincial capital"))

(new-indv-role {state capital}
	       {state}
               {city}
	       :english '(:role "capital" "state capital"))

;;;; WATER AREAS

(new-split-subtypes {water area}
 '({fresh-water area} {salt-water area}))

(english {fresh-water area} :adj "fresh-water")
(english {salt-water area} :adj "salt-water")

;;; River-class is any narrow flowing water: river, creek, etc.
;;; Ocean-class is some part of the connected world-ocean.
;;; Lake-class is not attached to the world-ocean, except by river.

(new-split-subtypes {water area}
 '({river-class} {ocean-class} {lake-class}))

;;; River types.
(new-split-subtypes {river-class}
  '({river} {creek} {brook}))

(new-split-subtypes {river-class}
  '({navigable} {non-navigable}))

(new-is-a {creek} {non-navigable})
(new-is-a {brook} {non-navigable})
(new-is-a {river-class} {fresh-water area})

(new-indv-role {river-class length} {river-class}
               {length measure}
	       :english '(:role "length"))

;;; Some assorted rivers, just for illustration.
(new-members {river}
  '({Nile River} {Amazon River} {Jordan River} {St. Lawrence River}
    {Hudson River} {Allegheny River} {Monogahela River} {Ohio River}
    {Mississippi River} {Missouri River} {Yangtze River} {Ganges River}
    {Tigris River} {Euphrates River}))

;;; Lake types.
(new-split-subtypes {lake-class}
  '({lake} {pond}))

(new-is-a {lake-class} {fresh-water area})
(new-type {salt lake}
	  {lake}
	  :english :iname)
(new-is-not-a {salt lake} {fresh-water area})
(new-is-a {salt lake} {salt-water area})

;;; Some assorted lakes, just for illustration.
(new-members {lake}
  '({Lake Superior} {Lake Huron} {Lake Michigan} {Lake Erie} {Lake Ontario}
    {Lake Victoria} {Lake Titicaca} {Lake Baikal} {Lake Tahoe} {Loch Ness}
    {Lake Maracaibo}))

(new-members {salt lake}
  '({Great Salt Lake} {Dead Sea} {Caspian Sea} {Aral Sea}))

;;; Oceanic types.
(new-split-subtypes {ocean-class}
  '({ocean} {sea} {bay} {strait}))

(new-is-a {ocean-class} {salt-water area})

;;; Some assorted oceanic areas, just for illustration.

(new-complete-members {ocean}
 '({Atlantic Ocean} {Pacific Ocean} {Indian Ocean} {Arctic Ocean}))

;;; Let {sea} include any defined fairly large oceanic area, including some
;;; that we conventionally call a gulf or bay.
(new-members {sea}
 '({Mediterranean Sea} {Black Sea} {Red Sea} {North Sea} {Baltic Sea}
   {Gulf of Mexico} {Persian Gulf} {Gulf of California} {Hudson Bay}))

;;; Bays are smaller and mostly surrounded.
(new-members {bay}
 '({San Francisco Bay} {Tampa Bay} {Chesapeake Bay} {Bay of Naples}))

;;; Straits are oceanic areas surrounded on two sides by water.
(new-members {strait}
 '({Strait of Magellan} {Strait of Hormuz} {Golden Gate}
   {Strait of Gibraltar} {English Channel}))

;;;; POLITICS

(new-indv-role {nationality} {person}
	       {country}
	       :english '(:role "nationality"))

(new-type-role {government official} {country}
	       {person}
	       :english '(:role "government official"))

;;;; COUNTRY ATTRIBUTES

;;; Add some attributes of a country.

(new-type {natural resource}
 {thing}
 :english :iname)

(new-indv-role {independence day} {country}
	       {date}
	       :english :iname)

(new-indv-role {natural resource (role)} {country}
 {natural resource}
 :english '(:role "natural resource"))

(new-indv-role {coastline} {country}
 {common:length measure}
 :english '(:role "coastline"))

(new-indv-role {gdp real growth rate} {country}
 {floating-point number}
 :english '(:role "GDP (real growth rate)"))

(new-indv-role {population below poverty line} {country}
 {floating-point number}
 :english '(:role "population below poverty line"))

(new-indv-role {inflation rate consumer prices} {country}
 {floating-point number}
 :english '(:role "inflation rate" "inflation rate (consumer prices)"))


(new-indv-role {unemployment rate} {country}
 {floating-point number}
 :english '(:role "unemployment rate"))

(new-indv-role {industrial production growth rate} {country}
 {floating-point number}
 :english '(:role "industrial production growth rate"))

(defun def-city (name)
  (or (lookup-element name)
      (new-indv name {geopolitics:city})))


#|
;;;; SPECIFIC COUNTRIES

;;; Define some specific countries and their properties.

;;; United States
(new-indv {United States} {country}
	  :english '("United States"
		     "United States of America"
		     "USA"
		     "America"))

(new-is-a {United States} {country with states})

(let ((us-state (create-the-x-of-y {state}
				   {United States})))
  (new-complete-members us-state
    '({Alabama} {Alaska} {Arizona} {Arkansas} {California}
      {Colorado} {Connecticut} {Delaware} {Florida} {Georgia}
      {Hawaii} {Idaho} {illinois} {Indiana} {Iowa}
      {Kansas} {Kentucky} {Louisiana} {Maine} {Maryland}
      {Massachusetts} {Michigan} {Minnesota} {Mississippi} {Missouri}
      {Montana} {Nebraska} {Nevada} {New Hampshire} {New Jersey}
      {New Mexico} {New York} {North Carolina} {North Dakota} {Ohio}
      {Oklahoma} {Oregon} {Pennsylvania} {Rhode Island} {South Carolina}
      {South Dakota} {Tennessee} {Texas} {Utah} {Vermont}
      {Virginia} {Washington} {West Virginia} {Wisconsin} {Wyoming})))

(the-x-of-y-is-z {national capital} {United States} 
  (new-indv {Washington D.C.}
	    {city}
	    :english '("Washington D.C." "Washington")))

(the-x-of-y-is-z {population} {United States}
  (new-number 278058881))

(the-x-of-y-is-z {area} {United States}
  (new-measure {3535000} {square mile} {area measure}))

;;; Canada

(new-indv {Canada}
	  {country}
	  :english :iname)

(new-is-a {Canada} {country with provinces})

(let ((canadian-province (create-the-x-of-y
			  {province}
			  {Canada})))
  (new-complete-members canadian-province
    '({Alberta} {British Columbia} {Manitoba} {New Brunswick}
      {Newfoundland} {Nova Scotia} {Ontaria} {Prince Edward Island}
      {Quebec} {Saskatchewan})))

(new-is-a {Canada} {country with territories})

(let ((canadian-territory (create-the-x-of-y
			   {territory}
			   {Canada})))
  (new-complete-members canadian-territory
    '({Northwest Territories} {Yukon Territory} {Nunavut})))

(the-x-of-y-is-z {national capital} {Canada} 
  (new-indv {Ottawa} {city}))

(the-x-of-y-is-z {population} {Canada}
  (new-number 31592805))

(the-x-of-y-is-z {area} {Canada}
  (new-measure {3556000} {square mile} 
               {area measure}))

(defun def-country (country region &rest alt-names)
  ;; If the country doesn't already exist in the KB, create it.
  (setq country (lookup-element country))
  (unless (typep country 'element)
    (setq country (new-indv country
			    {geopolitics:country}
			    :english :iname)))
  ;; Note what continent or region this country is a part of.
  (setq region (lookup-element-test region))
  (x-is-a-y-of-z country {common:part} region)
  ;; Register all the alternative names.
  (dolist (n alt-names)
    (english country n)))

;;; A few example countries.

(def-country {Afghanistan} {Asia} "Islamic State of Afghanistan")
(def-country {Japan} {Asia} "Nihon" "Nippon")
(def-country {Mexico} {North America})
(def-country {Cuba} {Latin America})
(def-country {Haiti} {Latin America})

;;;; Cities

(defun def-city (city country &rest alt-names)
  ;; If the city doesn't already exist in the KB, create it.
  (setq city (lookup-element city))
  (unless (typep city 'element)
    (setq city (new-indv city
			 {geopolitics:city}
			 :english :iname)))
  ;; Note what country this city is a part of.
  (setq country (lookup-element-test country))
  (x-is-a-y-of-z city {common:part} country)
  ;; Register all the alternative names.
  (dolist (n alt-names)
      (english city n)))

;;; A few example cities.

(def-city {Kabul} {Afghanistan})
(def-city {Kyoto} {Japan})
(def-city {Tokyo} {Japan} "Edo")

;;; Statements

(new-statement {United States} {area adjacent to} {Canada})
(new-statement {Mexico} {area adjacent to} {United States})

(new-statement {Cuba} {area near} {United States})
(new-statement {Cuba} {area near} {Haiti})

(new-statement {United States} {area more populous than} {Canada})
(new-statement {Canada} {area more populous than} {United States})

;; Every country is bigger than every city in area (in the
;; absence of exceptions).
(new-statement {country} {area greater than} {city})
|#








