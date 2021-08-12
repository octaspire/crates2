;; Octaspire Crates 2 - Puzzle Game
;; Copyright 2020, 2021 octaspire.com
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

(defun make-pulled-ensw (x y)
  (make-instance 'pulled :x x :y y :z 0 :north t :south t :east t :west t))

(defun make-pulled-e (x y)
  (make-instance 'pulled :x x :y y :z 0 :east t))

(defun make-pulled-w (x y)
  (make-instance 'pulled :x x :y y :z 0 :west t))

(defun make-pulled-s (x y)
  (make-instance 'pulled :x x :y y :z 0 :south t))

(defun make-pulled-n (x y)
  (make-instance 'pulled :x x :y y :z 0 :north t))

(defun make-pulled-ns (x y)
  (make-instance 'pulled :x x :y y :z 0 :north t :south t))

;; Methods

(defmethod update ((self pulled))
  (ecase (crate-state self)
    (:idle
     nil)
    (:pulled
     (let* ((puller (pulled-puller self))
            (puller-velocity (velocity puller))
            (puller-side (on-which-side-is-other self puller 1))
            (pulled-a-side (pulled-activation-side self)))
       (unless (and (eq pulled-a-side puller-side)
                    (or (eq puller-velocity :zero) (eq puller-velocity pulled-a-side)))
         (pulled-set-pull-on self puller nil))))
    (:lamented nil))
  (call-next-method))

(defmethod visual ((self pulled))
  (let ((result (list "pulled-idle"))
        (side   (pulled-activation-side self)))
    ;; EAST
    (if (pulled-east self)
        (nconc result (list (if (eq side :east)
                                "pulled-east-handle-active"
                                "pulled-east-handle-idle")))
        (nconc result (list "pulled-east-no-handle")))
    ;; WEST
    (if (pulled-west self)
        (nconc result (list (if (eq side :west)
                                "pulled-west-handle-active"
                                "pulled-west-handle-idle")))
        (nconc result (list "pulled-west-no-handle")))
    ;; NORTH
    (if (pulled-north self)
        (nconc result (list (if (eq side :north)
                                "pulled-north-handle-active"
                                "pulled-north-handle-idle")))
        (nconc result (list "pulled-north-no-handle")))
    ;; SOUTH
    (if (pulled-south self)
        (nconc result (list (if (eq side :south)
                                "pulled-south-handle-active"
                                "pulled-south-handle-idle")))
        (nconc result (list "pulled-south-no-handle")))
    result))

(defmethod pulled-set-pull-on ((self pulled) (puller moving) on)
  (when (and on (eq (crate-state self) :idle))
    (crates2-ui:ui-play-sound :pulled-activate)
    (crates2:add-updatable self)
    (setf (pulled-activation-side self) (on-which-side-is-other self puller 1))
    (setf (velocity self) (pulled-activation-side self))
    (setf (pulled-puller self) puller)
    (setf (crate-state self) :pulled))
  (when (and (eq (crate-state self) :pulled) (not on))
    (crates2:remove-updatable self)
    (setf (velocity self) :zero)
    (setf (pulled-activation-side self) :zero)
    (setf (pulled-puller self) nil)
    (setf (crate-state self) :idle)
    (crates2-ui:ui-play-sound :pulled-activate)))

(defmethod collide ((self pulled) (target player))
  (when (eq (crate-state self) :idle)
    (let ((side (on-which-side-is-other self target)))
      (ecase side
        (:east (if (pulled-east self)
                   (pulled-set-pull-on self target t)))
        (:west (if (pulled-west self)
                   (pulled-set-pull-on self target t)
                   (crates2-ui:ui-play-sound :hit-wall)))
        (:north (if (pulled-north self)
                    (pulled-set-pull-on self target t)))
        (:south (if (pulled-south self)
                    (pulled-set-pull-on self target t)))
        (:zero nil)))))
