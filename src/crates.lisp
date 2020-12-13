;; Octaspire Crates 2 - Puzzle Game
;; Copyright 2020 octaspire.com
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
(in-package :crates2)

(defclass crate ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)))

(defclass wall (crate)
  ())

(defclass moving (crate)
  ((v :initarg :v
      :accessor v)))

(defclass player (moving)
  ())

;; Generic functions

(defgeneric update (obj)
  (:documentation "Update a crate"))

(defgeneric visual (obj)
  (:documentation "Get visual representation for a crate"))

;; Methods

(defmethod update ((obj wall))
  (format t "update wall"))

(defmethod update ((obj player))
  (format t "update player"))

(defmethod visual ((obj wall))
  #\x)

(defmethod visual ((obj player))
  #\o)

