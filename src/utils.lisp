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

;; Functions

(defun replace-substr-at-transparent-whitespace (input index new)
  "Replace part of INPUT starting at INDEX using sub string NEW"
  (let ((substring-len (length new)))
    (loop for i from 0 to (- substring-len 1)
          do (let ((new-char (aref new i)))
               (when
                   (not (equal new-char #\Space))
                 (setf (aref input (+ index i)) new-char)))))
  input)

(defun find-at (x y z)
  "Get crate at (X,Y,Z) or NIL if location is empty."
  (find-if #'(lambda (crate)
               (if (and (= (crate-x crate) x)
                        (= (crate-y crate) y)
                        (= (crate-z crate) z))
                   crate
                   nil))
           (get-current-level)))

(defun find-at-of-type (x y z type)
  "Get crate of TYPE at (X,Y,Z) or NIL if location is empty or crate is not of TYPE."
  (let ((crate (find-at x y z)))
    (if crate
        (if (subtypep (type-of crate) type)
            crate
            nil)
        nil)))

(defun head-on-collision-p (v1 v2)
  "Predicate telling whether velocities V1 and V2 can cause
head on collision - i.e. if the velocities are in opposite
directions."
  (ecase v1
    (:east  (if (eq v2 :west)  t nil))
    (:west  (if (eq v2 :east)  t nil))
    (:north (if (eq v2 :south) t nil))
    (:south (if (eq v2 :north) t nil))
    (:zero  nil)))

(defun between-inclusive-p (n a b)
  "Predicate telling whether N is in range [A, B]."
  (let ((aa (min a b))
        (bb (max a b)))
    (and (>= n aa) (<= n bb))))

(defun on-which-side-i-am (i other &optional (slack 0))
  (if other
      (let ((ix (crate-x i))
            (iy (crate-y i))
            (ox (crate-x other))
            (oy (crate-y other)))
        (if (= iy oy)
            (if (between-inclusive-p ix (- ox 1) (- ox 1 slack))
                :west
                (if (between-inclusive-p ix (+ ox 1) (+ ox 1 slack))
                    :east
                    :zero))
            (if (= ix ox)
                (if (between-inclusive-p iy (- oy 1) (- oy 1 slack))
                    :north
                    (if (between-inclusive-p iy (+ oy 1) (+ oy 1 slack))
                        :south
                        :zero))
                :zero)))
      :zero))

(defun on-which-side-is-other (i other &optional (slack 0))
  (if other
      (let ((side (on-which-side-i-am i other slack)))
        (ecase side
          (:north :south)
          (:south :north)
          (:east  :west)
          (:west  :east)
          (:zero  :zero)))
      :zero))

(defun move-to (crate x y z)
  (setf (crate-x crate) x)
  (setf (crate-y crate) y)
  (setf (crate-z crate) z))

(defun move-other-to-my-side (i other side)
  "Move crate OTHER to SIDE of crate I"
  (ecase side
    (:north (move-to other (crate-x i) (- (crate-y i) 1) (crate-z i)))
    (:south (move-to other (crate-x i) (+ (crate-y i) 1) (crate-z i)))
    (:east  (move-to other (+ (crate-x i) 1) (crate-y i) (crate-z i)))
    (:west  (move-to other (- (crate-x i) 1) (crate-y i) (crate-z i)))
    (:zero  nil)))
