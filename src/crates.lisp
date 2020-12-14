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

(defun find-at (x y z)
  "Get crate at (X,Y,Z) or NIL if location is empty."
  (find-if #'(lambda (crate)
               (if (and (= (x crate) x)
                        (= (y crate) y)
                        (= (z crate) z))
                   crate
                   nil))
           *level*))

;; Classes

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

(defgeneric east (obj)
  (:documentation "Move crate east"))

(defgeneric west (obj)
  (:documentation "Move crate west"))

(defgeneric north (obj)
  (:documentation "Move crate north"))

(defgeneric south (obj)
  (:documentation "Move crate south"))

(defgeneric visual (obj)
  (:documentation "Get visual representation for a crate"))

(defgeneric escape (crate)
  (:documentation "Handle CRATE flying out of the level"))

(defgeneric collide (target crate)
  (:documentation "Handle CRATE colliding into TARGET"))

;; Methods

(defmethod update ((obj crate))
  (format t "update crate "))

(defmethod update ((obj wall))
  (format t "update wall ")
  (call-next-method))

(defmethod update ((obj moving))
  (format t "update moving ")
  (let ((v (v obj)))
    (ecase v (:east  (east obj))
             (:west  (west obj))
             (:north (north obj))
             (:south (south obj))))
  (call-next-method))

(defmethod update ((obj player))
  (format t "update player ")
  (call-next-method))

(defmethod visual ((obj wall))
  #\x)

(defmethod visual ((obj player))
  #\o)

(defmethod west ((obj moving))
  (format t "west~%")
  (let* ((x (- (x obj) 1))
         (crate (find-at x (y obj) (z obj))))
    (if (< x 0)
        (escape obj)
        (if crate
            (collide obj crate)
            (setf (x obj) x)))))

(defmethod east ((obj moving))
  (let* ((x (+ (x obj) 1))
         (crate (find-at x (y obj) (z obj))))
    (if (>= x *level-width*)
        (escape obj)
        (if crate
            (collide obj crate)
            (setf (x obj) x)))))

(defmethod north ((obj moving))
  (let* ((y (+ (y obj) 1))
         (crate (find-at (x obj) y (z obj))))
    (if (>= y *level-height*)
        (escape obj)
        (if crate
            (collide obj crate)
            (setf (y obj) y)))))

(defmethod south ((obj moving))
  (let* ((y (- (y obj) 1))
         (crate (find-at (x obj) y (z obj))))
    (if (< y 0)
        (escape obj)
        (if crate
            (collide obj crate)
            (setf (y obj) y)))))

(defmethod escape ((crate player))
  (setf *running* nil))
