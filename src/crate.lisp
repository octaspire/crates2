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

;; Classes

(defclass crate ()
  ((x :initarg :x
      :initform 0
      :accessor crate-x)
   (y :initarg :y
      :initform 0
      :accessor crate-y)
   (z :initarg :z
      :initform 0
      :accessor crate-z)
   (visible :initarg :visible
            :accessor crate-visible)
   (state :initarg :state
          :initform :idle
          :accessor crate-state)))

;; Generic functions

(defgeneric update (self)
  (:documentation "Update a crate"))

(defgeneric east (self)
  (:documentation "Handle east event. Doesn't have to be movement related."))

(defgeneric west (self)
  (:documentation "Handle west event. Doesn't have to be movement related."))

(defgeneric north (self)
  (:documentation "Handle north event. Doesn't have to be movement related."))

(defgeneric south (self)
  (:documentation "Handle south event. Doesn't have to be movement related."))

(defgeneric visual (self)
  (:documentation "Get visual representation for a crate"))

(defgeneric collide (moving target)
  (:documentation "Handle MOVING crate colliding into TARGET"))

(defgeneric handle-input (self input)
  (:documentation "React to input"))

;; Methods

(defmethod update ((self crate)))

(defmethod visual ((self crate))
  (when (crate-visible self)
    (call-next-method)))