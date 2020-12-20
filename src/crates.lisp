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

;; Classes

(defclass crate ()
  ((x :initarg :x
      :accessor crate-x)
   (y :initarg :y
      :accessor crate-y)
   (z :initarg :z
      :accessor crate-z)
   (visible :initarg :visible
            :accessor crate-visible)))

(defclass wall (crate)
  ())

(defclass exit (crate)
  ((activated :initarg :activated
              :accessor exit-activated
              :initform nil)))

(defclass vacuum (crate)
  ((full :initarg :full
         :accessor full
         :initform nil)))

(defclass moving (crate)
  ((velocity :initarg :velocity
             :initform :zero
             :accessor velocity)
   (active :initarg :active
           :initform t
           :accessor active)))

(defclass player (moving)
  ((lamented :initarg lamented
             :initform nil
             :accessor lamented)))

(defclass pushed (moving)
  ())

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

(defgeneric escape (self)
  (:documentation "Handle CRATE flying out of the level"))

(defgeneric collide (moving target)
  (:documentation "Handle MOVING crate colliding into TARGET"))

(defgeneric movingp (self)
  (:documentation "Predicate telling whether moving SELF is in motion"))

(defgeneric stationaryp (self)
  (:documentation "Predicate telling whether moving SELF is not moving"))

(defgeneric handle-input (self input)
  (:documentation "React to input"))

;; Methods

(defmethod update ((self crate)))

(defmethod update ((self wall))
  (call-next-method))

(defmethod update ((self vacuum))
  (let ((crate (find-at-of-type (crate-x self) (crate-y self) 0 'moving)))
    (when crate
      (setf (active crate) nil)
      (when (typep crate 'player)
        (setf (lamented crate) t)
        (request-restart-level)
        (setf (full self) t))))
  (call-next-method))

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

(defmethod update ((self player))
  (let ((input (car *input*)))
    (when (and input (stationaryp self))
      (handle-input self input)))
  (call-next-method))

(defmethod visual ((self crate))
  (when (crate-visible self)
    (call-next-method)))

(defmethod visual ((self wall))
  #\x)

(defmethod visual ((self exit))
  (if (exit-activated self)
      #\E
      #\e))

(defmethod visual ((self player))
  (if (active self)
      #\o
      nil))

(defmethod visual ((self vacuum))
  (if (full self)
      #\V
      #\v))

(defmethod visual ((self pushed))
  #\p)

(defmethod west ((self moving))
  (let* ((x (- (crate-x self) 1))
         (crate (find-at x (crate-y self) (crate-z self))))
    (if (< x 0)
        (escape self)
        (if crate
            (collide self crate)
            (setf (crate-x self) x)))))

(defmethod east ((self moving))
  (let* ((x (+ (crate-x self) 1))
         (crate (find-at x (crate-y self) (crate-z self))))
    (if (>= x *level-width*)
        (escape self)
        (if crate
            (collide self crate)
            (setf (crate-x self) x)))))

(defmethod north ((self moving))
  (let* ((y (- (crate-y self) 1))
         (crate (find-at (crate-x self) y (crate-z self))))
    (if (< y 0)
        (escape self)
        (if crate
            (collide self crate)
            (setf (crate-y self) y)))))

(defmethod south ((self moving))
  (let* ((y (+ (crate-y self) 1))
         (crate (find-at (crate-x self) y (crate-z self))))
    (if (>= y *level-height*)
        (escape self)
        (if crate
            (collide self crate)
            (setf  (crate-y self) y)))))

(defmethod escape ((self moving))
  (setf (active self) nil)
  (when (typep self 'player)
    (setf (lamented self) t)
    (request-restart-level)))

(defmethod collide ((self moving) (target crate))
  (setf (velocity self) :zero))

(defmethod collide ((self player) (target crate))
  (call-next-method))

(defmethod collide ((self player) (target exit))
  (request-next-level)
  (call-next-method))

(defmethod movingp ((self moving))
  (not (stationaryp self)))

(defmethod stationaryp ((self moving))
  (eq (velocity self) :zero))

(defmethod handle-input ((self player) input)
  (ecase input
    (:east  (setf (velocity self) input))
    (:west  (setf (velocity self) input))
    (:north (setf (velocity self) input))
    (:south (setf (velocity self) input))))

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

(defmethod collide ((self player) (target pushed))
  (let ((vplayer (velocity self))
        (vpulled (velocity target)))
    (if (eq vpulled :zero)
        (setf (velocity target) vplayer)
        (setf (velocity target) (if (head-on-collision-p vplayer vpulled) :zero vplayer))))
  (call-next-method))
