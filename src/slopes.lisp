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

;; Methods

;; EN

(defmethod update ((self slope-en))
  (call-next-method))

(defmethod visual ((self slope-en))
  #\1)

(defmethod collide ((self slope-en) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:east
       (move-other-to-my-side self target :north)
       (setf (velocity target) :north))
      (:north
       (move-other-to-my-side self target :east)
       (setf (velocity target) :east)))))

;; ES

(defmethod update ((self slope-es))
  (call-next-method))

(defmethod visual ((self slope-es))
  #\2)

(defmethod collide ((self slope-es) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:east
       (move-other-to-my-side self target :south)
       (setf (velocity target) :south))
      (:south
       (move-other-to-my-side self target :east)
       (setf (velocity target) :east)))))

;; WN

(defmethod update ((self slope-wn))
  (call-next-method))

(defmethod visual ((self slope-wn))
  #\3)

(defmethod collide ((self slope-wn) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:west
       (move-other-to-my-side self target :north)
       (setf (velocity target) :north))
      (:north
       (move-other-to-my-side self target :west)
       (setf (velocity target) :west)))))

;; WS

(defmethod update ((self slope-ws))
  (call-next-method))

(defmethod visual ((self slope-ws))
  #\4)

(defmethod collide ((self slope-ws) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:west
       (move-other-to-my-side self target :south)
       (setf (velocity target) :south))
      (:south
       (move-other-to-my-side self target :west)
       (setf (velocity target) :west)))))
