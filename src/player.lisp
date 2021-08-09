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

;; Constructors

(defun make-player (x y)
  (make-instance 'player :x x :y y :z 0))

;; Methods

(defmethod visual ((self player))
  (if (active self)
      (if (moving-airborne self)
          (list "player-airborne")
          (list (format nil "player-active-~2,'0d" (crate-frame self))))
      (list "player-hidden")))

(defmethod update ((self player))
  (ecase (crate-state self)
    (:idle (player-update-idle self))
    (:lamented (player-update-lamented self)))
  (call-next-method))

(defmethod collide ((self player) (target crate))
  (call-next-method))

(defmethod collide ((self player) (target exit))
  (setf (active self) nil))

(defmethod collide ((self player) (target vacuum))
  (lament self)
  (call-next-method))

(defmethod handle-input ((self player) input)
  (let ((controlled (player-controlled self)))
    (if controlled
        (handle-input controlled input)
        (when (and input (active self))
          (ecase input
            (:east    (setf (velocity self) input))
            (:west    (setf (velocity self) input))
            (:north   (setf (velocity self) input))
            (:south   (setf (velocity self) input))
            (:action1 (jump self))
            (:action2  nil)
            (:action3  nil)
            (:backspace nil))
          (setf (player-pending-input self) nil)))))

(defmethod collide ((self player) (target pushed))
  (let ((vself (velocity self))
        (vtarget (velocity target)))
    (if (eq vtarget :zero)
        (setf (velocity target) vself)
        (setf (velocity target) (if (head-on-collision-p vself vtarget) :zero vself))))
  (call-next-method))

;; Functions

(defmethod get-input ((self player))
  (let ((input (car *input*)))
    (unless input
      (setf input (player-pending-input self)))
    input))

(defun player-update-idle (self)
  (let ((input (get-input self))
        (frame (1+ (crate-frame self))))
    (setf (crate-frame self) (mod frame 7))
    (if (stationaryp self)
        (handle-input self input)
        (if (eq input :action1)
            (handle-input self input)
            (setf (player-pending-input self) input)))))

(defun player-update-lamented (self)
  (incf (player-delay self))
  (when (> (player-delay self) 3)
    (request-restart-level)))
