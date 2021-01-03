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

;; Methods

(defmethod update ((self pulled))
  (ecase (crate-state self)
    (:idle
     nil)
    (:pulled
     (let* ((puller (pulled-puller self))
            (side (on-which-side-is-other self puller)))
       (ecase side
         (:east
          (let ((on (pulled-east self)))
            (pulled-set-pull-on self puller on)))
         (:west
          (let ((on (pulled-west self)))
            (pulled-set-pull-on self puller on)))
         (:north
          (let ((on (pulled-north self)))
            (pulled-set-pull-on self puller on)))
         (:south
          (let ((on (pulled-south self)))
            (pulled-set-pull-on self puller on)))
         (:zero nil))))
    (:lamented nil))
  (call-next-method))

(defmethod visual ((self pulled))
  (let ((result (list "pulled-idle"))
        (side   (on-which-side-is-other self (pulled-puller self))))
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
  (setf (crate-state self) (if on :pulled :idle))
  (setf (pulled-puller self) (if on puller nil))
  (let ((side (on-which-side-is-other self puller)))
    (setf (velocity self) (if on side :zero))))

(defmethod collide ((self pulled) (target player))
  (let ((side (on-which-side-is-other self target)))
    (ecase side
      (:east (when (pulled-east self)
               (pulled-set-pull-on self target t)))
      (:west (when (pulled-west self)
               (pulled-set-pull-on self target t)))
      (:north (when (pulled-north self)
                (pulled-set-pull-on self target t)))
      (:south (when (pulled-south self)
                (pulled-set-pull-on self target t)))
      (:zero nil))))
