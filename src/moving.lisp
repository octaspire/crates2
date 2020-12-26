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

;; Generic functions

(defgeneric collide (self other)
  (:documentation "Collide SELF with OTHER"))

(defgeneric movingp (self)
  (:documentation "Predicate telling whether moving SELF is in motion"))

(defgeneric stationaryp (self)
  (:documentation "Predicate telling whether moving SELF is not moving"))

;; Methods

(defmethod movingp ((self moving))
  (not (stationaryp self)))

(defmethod stationaryp ((self moving))
  (eq (velocity self) :zero))

(defmethod update ((self moving))
  (when (active self)
    (let ((v (velocity self)))
      (ecase v
        (:east  (east self))
        (:west  (west self))
        (:north (north self))
        (:south (south self))
        (:zero))))
  (call-next-method))

(defmethod west ((self moving))
  (let* ((x (- (crate-x self) 1))
         (crate (find-at x (crate-y self) (crate-z self))))
    (if (< x 0)
        (lament self)
        (if crate
            (handle-collision self crate)
            (setf (crate-x self) x)))))

(defmethod east ((self moving))
  (let* ((x (+ (crate-x self) 1))
         (crate (find-at x (crate-y self) (crate-z self))))
    (if (>= x *level-width*)
        (lament self)
        (if crate
            (handle-collision self crate)
            (setf (crate-x self) x)))))

(defmethod north ((self moving))
  (let* ((y (- (crate-y self) 1))
         (crate (find-at (crate-x self) y (crate-z self))))
    (if (< y 0)
        (lament self)
        (if crate
            (handle-collision self crate)
            (setf (crate-y self) y)))))

(defmethod south ((self moving))
  (let* ((y (+ (crate-y self) 1))
         (crate (find-at (crate-x self) y (crate-z self))))
    (if (>= y *level-height*)
        (lament self)
        (if crate
            (handle-collision self crate)
            (setf  (crate-y self) y)))))

(defmethod collide ((self moving) (target crate))
  (setf (velocity self) :zero))

(defmethod collide ((self moving) (target vacuum))
  (setf (crate-x target) (crate-x self))
  (setf (crate-y target) (crate-y self))
  (setf (crate-z target) (crate-z self)))

;; Functions

(defun handle-collision (c1 c2)
  (collide c1 c2)
  (collide c2 c1))

