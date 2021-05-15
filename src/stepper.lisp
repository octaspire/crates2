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

;; Constructors

(defun make-stepper (x y)
  (make-instance 'stepper :x x :y y :z -1))

;; Methods

(defmethod update ((self stepper))
  (let ((crate (find-at-of-type (crate-x self) (crate-y self) 0 'moving)))
    (case (crate-state self)
      (:idle
       (when crate
         (setf (crate-state self) :active)
         (setf (velocity crate) :zero)))
      (:active
       (unless crate
         (setf (crate-state self) :idle)))))
  (call-next-method))

(defmethod visual ((self stepper))
  (if (eq (crate-state self) :active)
      (list "stepper-active")
      (list "stepper-idle")))
