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

(defun make-vacuum (x y)
  (make-instance 'vacuum :x x :y y :z -1))

;; Methods

(defmethod update ((self vacuum))
  (ecase (crate-state self)
    (:idle
     (let ((frame (1+ (crate-frame self)))
           (crate (find-at-of-type (crate-x self) (crate-y self) 0 'moving)))
       (setf (crate-frame self) (mod frame 8))
       (when (and crate (not (moving-airborne crate)))
         (crates2-ui:ui-play-sound :hit-vacuum)
         (setf (crate-state self) :broken)
         (lament crate)
         (when (subtypep (type-of crate) 'moving)
           (setf (velocity crate) :zero)
           (move-to crate (crate-x self) (crate-y self) (crate-z self))))))
    (:broken nil))
  (call-next-method))

(defmethod visual ((self vacuum))
  (list "vacuum-body"
        (format nil "gear-~2,'0d" (crate-frame self))))
