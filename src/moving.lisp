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
  (setf (moving-tail self) nil)
  (when (active self)
    (let ((v (velocity self))
          (airborne (moving-airborne self)))
      (ecase v
        (:east  (east self))
        (:west  (west self))
        (:north (north self))
        (:south (south self))
        (:zero))
      (when airborne
        (fall self))))
  (call-next-method))

(defmethod tail-x ((self moving))
  (let ((tail (moving-tail self)))
    (if tail
        (first tail)
        (crate-x self))))

(defmethod tail-y ((self moving))
  (let ((tail (moving-tail self)))
    (if tail
        (second tail)
        (crate-y self))))

(defmethod tail-z ((self moving))
  (let ((tail (moving-tail self)))
    (if tail
        (third tail)
        (crate-z self))))

(defmethod move-to ((self moving) x y z)
  (let ((old-x (crate-x self))
        (old-y (crate-y self))
        (old-z (crate-z self)))
    (unless (is-at-p self x y z)
      (let ((dx (- x old-x))
            (dy (- y old-y))
            (dz (- z old-z)))
        (setf (crate-x self) x
              (crate-y self) y
              (crate-z self) z)
        (set-tail self
                  (+ old-x (/ dx 2))
                  (+ old-y (/ dy 2))
                  (+ old-z (/ dz 2)))))))

(defmethod set-tail ((self moving) x y z)
  (setf (moving-tail self) (list x y z)))

(defmethod jump ((self moving))
  (let* ((z (+ (crate-z self) 1))
         (specials (moving-specials self))
         (airborne (moving-airborne self))
         (crate (find-at (crate-x self) (crate-y self) z)))
    (unless airborne
      (when specials
        (setf (moving-specials self) (cdr (moving-specials self)))
        (crates2-ui:ui-play-sound :special)
        (if crate
            (handle-collision self crate)
            (setf (moving-airborne self) 1))))))

(defmethod fall ((self moving))
  (when (moving-airborne self)
    (setf (moving-airborne self) (1- (moving-airborne self)))
    (when (< (moving-airborne self) 0)
      (setf (moving-airborne self) nil))))

(defmethod west ((self moving))
  (let* ((x (- (crate-x self) 1))
         (crate (find-at x (crate-y self) (crate-z self))))
    (if (< x 0)
        (lament self)
        (if crate
            (handle-collision self crate)
            (move-to self x (crate-y self) (crate-z self))))))

(defmethod east ((self moving))
  (let* ((x (+ (crate-x self) 1))
         (crate (find-at x (crate-y self) (crate-z self))))
    (if (>= x *level-width*)
        (lament self)
        (if crate
            (handle-collision self crate)
            (move-to self x (crate-y self) (crate-z self))))))

(defmethod north ((self moving))
  (let* ((y (- (crate-y self) 1))
         (crate (find-at (crate-x self) y (crate-z self))))
    (if (< y 0)
        (lament self)
        (if crate
            (handle-collision self crate)
            (move-to self (crate-x self) y (crate-z self))))))

(defmethod south ((self moving))
  (let* ((y (+ (crate-y self) 1))
         (crate (find-at (crate-x self) y (crate-z self))))
    (if (>= y *level-height*)
        (lament self)
        (if crate
            (handle-collision self crate)
            (move-to self (crate-x self) y (crate-z self))))))

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

