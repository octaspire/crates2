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

;; E1

(defmethod update ((self turnstile-e1))
  (call-next-method))

(defmethod visual ((self turnstile-e1))
  ">>1>>1")

(defmethod collide ((self turnstile-e1) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:west
       (move-other-to-my-side self target :east)))))


;; W1

(defmethod update ((self turnstile-w1))
  (call-next-method))

(defmethod visual ((self turnstile-w1))
  "1<<1<<")

(defmethod collide ((self turnstile-w1) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:east
       (move-other-to-my-side self target :west)))))

;; N1

(defmethod update ((self turnstile-n1))
  (call-next-method))

(defmethod visual ((self turnstile-n1))
  "^^^111")

(defmethod collide ((self turnstile-n1) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:south
       (move-other-to-my-side self target :north)))))

;; S1

(defmethod update ((self turnstile-s1))
  (call-next-method))

(defmethod visual ((self turnstile-s1))
  "...111")

(defmethod collide ((self turnstile-s1) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:north
       (move-other-to-my-side self target :south)))))

;; E

(defmethod update ((self turnstile-e))
  (call-next-method))

(defmethod visual ((self turnstile-e))
  ">>>>>>")

(defmethod collide ((self turnstile-e) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:west
       (call-next-method)
       (setf (velocity target) :east)))))


;; W

(defmethod update ((self turnstile-w))
  (call-next-method))

(defmethod visual ((self turnstile-w))
  "<<<<<<")

(defmethod collide ((self turnstile-w) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:east
       (call-next-method)
       (setf (velocity target) :west)))))

;; N

(defmethod update ((self turnstile-n))
  (call-next-method))

(defmethod visual ((self turnstile-n))
  "^^^^^^")

(defmethod collide ((self turnstile-n) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:south
       (call-next-method)
       (setf (velocity target) :north)))))

;; S

(defmethod update ((self turnstile-s))
  (call-next-method))

(defmethod visual ((self turnstile-s))
  "......")

(defmethod collide ((self turnstile-s) (target moving))
  (let ((side (on-which-side-is-other self target)))
    (case side
      (:north
       (call-next-method)
       (setf (velocity target) :south)))))

