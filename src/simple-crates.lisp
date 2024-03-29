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

(defun make-wall (x y)
  (make-instance 'wall :x x :y y :z 0))

(defun make-pushed (x y &optional (velocity :zero))
  (make-instance 'pushed :x x :y y :z 0 :velocity velocity))

;; Generic functions

;; Methods

(defmethod update ((self wall))
  (call-next-method))

(defmethod visual ((self wall))
  (list (format nil "wall-idle-~2,'0d" (crate-frame self))))

(defmethod collide ((self wall) (target moving))
  (crates2-ui:ui-play-sound :hit-wall))

(defmethod collide ((self pushed) (target moving))
  (setf (velocity self) (on-which-side-i-am self target))
  (setf (velocity target) :zero))

(defmethod collide ((self pushed) (target crate))
  (setf (velocity self) :zero))

(defmethod visual ((self pushed))
  (list "pushed-idle"))

