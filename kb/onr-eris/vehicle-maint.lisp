;;; ***************************************************************************
;;; Knowledge about vehicles, vehicle maintenance, and tools.
;;; Created for our ONR "Episodic Representation and Reasoning for Human-Robot
;;; interaction project.
;;;
;;; Contributors: Laleh Roosta Pour
;;;               Scott E. Fahlman
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

;;; Basic knowledge of vehicle types.

(new-type {vehicle} {thing})
(new-split '({vehicle} {physical object}))

(new-split-subtypes {vehicle}
 '({motor vehicle} {watercraft} {spacecraft} {aircraft} {bicycle}))

(new-split-subtypes {motor vehicle}
		    '({motorcycle}
		      {car}
		      {truck}
		      {bus}
		      {train}))


(new-split-subtypes {watercraft}
		    '({ship}
		      {boat}))

(new-split-subtypes {car}
 '({four door} {two door}))
(new-type {sedan} {four door})
(new-type {coupe} {two door})

;;; ----------------------------------------------------------------------
;;; Some parts of a car.

(new-type-role {wheel of car} {car} :parent {car part} :n 4)
(new-type-role {trunk of car} {car} :parent {car part} :n 1)
(new-type-role {door of car} {four car} :parent {car part} :n 4)
(new-type-role {door of car} {two car} :parent {car part} :n 2)

(new-type-role {tire} {wheel} :parent {wheel part} :n 1)
(new-type-role {lugnut} {wheel} :parent {wheel part} :n 5)
(new-type-role {lugbolt} {wheel} :parent {wheel part} :n 5)

;;; ----------------------------------------------------------------------
;;; Tools for vehicle maintenance.

(new-type {tool} {thing})
(new-type {hardware} {tool})
(new-type {hand tool} {tool})
(new-type {fastener} {hardware})

(new-type {wrench} {hand tool})
(new-type {lug wrench} {wrench})
(new-type {socket wrench} {wrench}) 

(new-type {jack} {hand tool})
(new-type {hydraulic jack} {jack})
;;;TODO This type allows you to work underneath the vehicle
(new-type {trolley jack} {hydraulic jack})
;;;TODO Provides enough pressure to lift a vehicle weighing up to several tons
(new-type {hydraulic bottle jack} {hydraulic jack})
(new-type {scissor jack} {jack})

(new-type {lugnut} {hand tool})
(new-type {nut} {fastener})
(new-type {stud} {fastener})
(new-type {bolt} {fastener})
