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

(defclass updatable ()
  ())

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
   (frame :initarg :frame
          :initform 0
          :accessor crate-frame)
   (state :initarg :state
          :initform :idle
          :accessor crate-state)))

(defclass moving (crate)
  ((velocity :initarg :velocity
             :initform :zero
             :accessor velocity)
   (active :initarg :active
           :initform t
           :accessor active)
   (tail :initarg :tail
         :initform nil
         :accessor moving-tail)
   (specials :initform nil
             :accessor moving-specials)
   (airborne :initform nil
          :accessor moving-airborne)))

(defclass wall (crate)
  ())

(defclass stepper (crate updatable)
  ())

(defclass slope (crate updatable)
  ((active-step :initarg :active-step
                :initform 0
                :accessor slope-active-step)))

(defclass slope-en (slope)
  ())

(defclass slope-es (slope)
  ())

(defclass slope-wn (slope)
  ())

(defclass slope-ws (slope)
  ())

(defclass pushed (moving updatable)
  ())

(defclass turnstile (crate updatable)
  ((active-step :initarg :active-step
                :initform 0
                :accessor turnstile-active-step)))

(defclass turnstile-e1 (turnstile)
  ())

(defclass turnstile-w1 (turnstile)
  ())

(defclass turnstile-n1 (turnstile)
  ())

(defclass turnstile-s1 (turnstile)
  ())

(defclass turnstile-e (turnstile-e1)
  ())

(defclass turnstile-w (turnstile-w1)
  ())

(defclass turnstile-n (turnstile-n1)
  ())

(defclass turnstile-s (turnstile-s1)
  ())

(defclass bomb (crate updatable)
  ((time :initarg :time
         :accessor bomb-time
         :initform 10)
   (uptime :initarg :uptime
           :accessor bomb-uptime
           :initform 0)
   (durable :initarg :durable
            :accessor bomb-durable
            :initform nil)))

(defclass block-timer (crate updatable)
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

(defclass pass-timer (crate updatable)
  ((time :initarg :time
         :accessor pass-timer-time
         :initform 10)
   (uptime :initarg :uptime
           :accessor pass-timer-uptime
           :initform 0)))

(defclass pass-counter (crate updatable)
  ((count :initarg :count
          :accessor pass-counter-count
          :initform 10)
   (passes :initarg :passes
           :accessor pass-counter-passes
           :initform 0)))

(defclass exit (crate updatable)
  ((activated :initarg :activated
              :accessor exit-activated
              :initform nil)
   (delay :initarg :delay
          :accessor exit-delay
          :initform 0)))

(defclass key (crate updatable)
  ((active-step :initarg :active-step
                :initform 0
                :accessor key-active-step)
   (key-collector :initform nil
                  :accessor key-collector)))

(defclass special-jump (crate updatable)
  ((active-step :initarg :active-step
                :initform 0
                :accessor special-jump-active-step)
   (target :initarg :target
                :initform nil
                :accessor special-jump-target)))

(defclass pulled (moving)
  ((puller :initarg :puller
           :accessor pulled-puller
           :initform nil)
   (activation-side :initform nil
                    :accessor pulled-activation-side)
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

(defclass toggle (crate)
  ((east :initarg :east
         :accessor toggle-east
         :initform nil)
   (west :initarg :west
         :accessor toggle-west
         :initform nil)
   (north :initarg :north
          :accessor toggle-north
          :initform nil)
   (south :initarg :south
          :accessor toggle-south
          :initform nil)))

(defclass player (moving updatable)
  ((delay :initform 0
          :accessor player-delay)
   (pending-input :initform nil
                  :accessor player-pending-input)
   (controlled :initform nil
               :accessor player-controlled)))

(defclass automaton (moving)
  ((program :initform (string "")
            :accessor automaton-program)
   (programmer :initform nil
               :accessor automaton-programmer)
   (wait-duration :initform 9
                  :accessor automaton-wait-duration
                  :allocation :class)
   (delay :initform 0
          :accessor automaton-delay)))

(defclass vacuum (crate updatable)
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
