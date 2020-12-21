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

;; Classes

(defclass vacuum (crate)
  ((full :initarg :full
         :accessor full
         :initform nil)))

;; Methods

(defmethod update ((self vacuum))
  (let ((crate (find-at-of-type (crate-x self) (crate-y self) 0 'moving)))
    (when crate
      (setf (active crate) nil)
      (when (typep crate 'player)
        (setf (lamented crate) t)
        (setf (full self) t))))
  (call-next-method))

(defmethod visual ((self vacuum))
  (if (full self)
      #\V
      #\v))
