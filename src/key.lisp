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

(defmethod visual ((self key))
  (list (format nil "key-idle-~2,'0d" (crate-frame self))))

(defmethod update ((self key))
  (let ((frame (1+ (crate-frame self))))
    (setf (crate-frame self) (mod frame 9)))
  (call-next-method))

(defmethod collide ((self key) (target player))
  (ecase (crate-state self)
    (:idle
     (setf (velocity target) (on-which-side-i-am self target))
     (lament self))
    (:lamented nil)))
