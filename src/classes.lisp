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

(defclass moving (crate)
  ((velocity :initarg :velocity
             :initform :zero
             :accessor velocity)
   (active :initarg :active
           :initform t
           :accessor active)))

(defclass wall (crate)
  ())

(defclass slope-en (crate)
  ())

(defclass slope-es (crate)
  ())

(defclass slope-wn (crate)
  ())

(defclass slope-ws (crate)
  ())

(defclass pushed (moving)
  ())

(defclass turnstile-e1 (crate)
  ())

(defclass turnstile-w1 (crate)
  ())

(defclass turnstile-n1 (crate)
  ())

(defclass turnstile-s1 (crate)
  ())

(defclass turnstile-e (turnstile-e1)
  ())

(defclass turnstile-w (turnstile-w1)
  ())

(defclass turnstile-n (turnstile-n1)
  ())

(defclass turnstile-s (turnstile-s1)
  ())

(defclass block-timer (crate)
  ((time :initarg :time
         :accessor block-timer-time
         :initform 10)
   (uptime :initarg :uptime
           :accessor block-timer-uptime
           :initform 0)
   (durable :initarg :durable
           :accessor block-timer-durable
            :initform t)))

(defclass block-counter (crate)
  ((count :initarg :count
          :accessor block-counter-count
          :initform 10)
   (touches :initarg :touches
            :accessor block-counter-touches
            :initform 0)))

(defclass pass-timer (crate)
  ((time :initarg :time
         :accessor pass-timer-time
         :initform 10)
   (uptime :initarg :uptime
           :accessor pass-timer-uptime
           :initform 0)))

(defclass pass-counter (crate)
  ((count :initarg :count
          :accessor pass-counter-count
          :initform 10)
   (passes :initarg :passes
           :accessor pass-counter-passes
           :initform 0)))

(defclass exit (crate)
  ((activated :initarg :activated
              :accessor exit-activated
              :initform nil)
   (delay :initarg :delay
          :accessor exit-delay
          :initform 0)))

(defclass key (crate)
  ())

(defclass pulled (moving)
  ((puller :initarg :puller
           :accessor pulled-puller
           :initform nil)
   (east :initarg :east
           :accessor pulled-east
         :initform nil)
   (west :initarg :west
         :accessor pulled-west
         :initform nil)
   (north :initarg :north
          :accessor pulled-north
         :initform nil)
   (south :initarg :south
          :accessor pulled-south
          :initform nil)))

(defclass player (moving)
  ((delay :initform 0
          :accessor player-delay)))

(defclass vacuum (crate)
  ((full :initarg :full
         :accessor full
         :initform nil)))

;; Generic functions

(defgeneric time-left (self)
  (:documentation "Calculate time left in crate SELF"))

(defgeneric touches-left (self)
  (:documentation "Calculate number of touches left in crate SELF"))

(defgeneric lament (self)
  (:documentation "Make crate SELF lamented"))

(defgeneric lamentedp (self)
  (:documentation "Tell whether crate SELF lamented or not"))

;; Methods

(defmethod lament ((self crate))
  (setf (crate-state self) :lamented))

(defmethod lamentedp ((self crate))
  (eq (crate-state self) :lamented))
