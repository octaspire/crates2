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

(defun find-at-of-type (x y z type)
  "Get crate of TYPE at (X,Y,Z) or NIL if location is empty or crate is not of TYPE."
  (let ((crate (find-at x y z)))
    (if crate
        (if (subtypep (type-of crate) type)
            crate
            nil)
        nil)))

;; Classes

(defclass crate ()
  ((x :initarg :x
      :accessor x)
   (y :initarg :y
      :accessor y)
   (z :initarg :z
      :accessor z)
   (visible :initarg :visible
      :accessor visible)))

(defclass wall (crate)
  ())

(defclass vacuum (crate)
  ((full :initarg :full
         :accessor full
         :initform nil)))

(defclass moving (crate)
  ((v :initarg :v
      :accessor v)
   (active :initarg :active
           :initform t
           :accessor active)))

(defclass player (moving)
  ((lamented :initarg lamented
             :initform nil
             :accessor lamented)))

;; Generic functions

(defgeneric update (self)
  (:documentation "Update a crate"))

(defgeneric east (self)
  (:documentation "Move crate east"))

(defgeneric west (self)
  (:documentation "Move crate west"))

(defgeneric north (self)
  (:documentation "Move crate north"))

(defgeneric south (self)
  (:documentation "Move crate south"))

(defgeneric visual (self)
  (:documentation "Get visual representation for a crate"))

(defgeneric escape (crate)
  (:documentation "Handle CRATE flying out of the level"))

(defgeneric collide (target crate)
  (:documentation "Handle CRATE colliding into TARGET"))

;; Methods

(defmethod update ((self crate))
  (format t "update crate "))

(defmethod update ((self wall))
  (format t "update wall ")
  (call-next-method))

(defmethod update ((self vacuum))
  (format t "update vacuum ")
  (let ((crate (find-at-of-type (x self) (y self) 0 'moving)))
    (when crate
      (setf (active crate) nil)
      (when (typep crate 'player)
        (setf (lamented crate) t)
        (setf (full self) t))))
  (call-next-method))

(defmethod update ((self moving))
  (format t "update moving ")
  (when (active self)
    (let ((v (v self)))
      (ecase v (:east  (east self))
             (:west  (west self))
             (:north (north self))
             (:south (south self)))))
  (call-next-method))

(defmethod update ((self player))
  (format t "update player ")
  (call-next-method))

(defmethod visual ((self crate))
  (when (visible self)
    (call-next-method)))

(defmethod visual ((self wall))
  #\x)

(defmethod visual ((self player))
  (if (active self)
      #\o
      nil))

(defmethod visual ((self vacuum))
  (if (full self)
      #\V
      #\v))

(defmethod west ((self moving))
  (format t "west~%")
  (let* ((x (- (x self) 1))
         (crate (find-at x (y self) (z self))))
    (if (< x 0)
        (escape self)
        (if crate
            (collide self crate)
            (setf (x self) x)))))

(defmethod east ((self moving))
  (let* ((x (+ (x self) 1))
         (crate (find-at x (y self) (z self))))
    (if (>= x *level-width*)
        (escape self)
        (if crate
            (collide self crate)
            (setf (x self) x)))))

(defmethod north ((self moving))
  (let* ((y (+ (y self) 1))
         (crate (find-at (x self) y (z self))))
    (if (>= y *level-height*)
        (escape self)
        (if crate
            (collide self crate)
            (setf (y self) y)))))

(defmethod south ((self moving))
  (let* ((y (- (y self) 1))
         (crate (find-at (x self) y (z self))))
    (if (< y 0)
        (escape self)
        (if crate
            (collide self crate)
            (setf (y self) y)))))

(defmethod escape ((self moving))
  (setf (active self) nil)
  (when (typep self 'player)
    (setf (lamented self) t)))
