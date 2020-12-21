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

(defclass exit (crate)
  ((activated :initarg :activated
              :accessor exit-activated
              :initform nil)
   (delay :initarg :delay
              :accessor exit-delay
              :initform 0)))

;; Methods

(defmethod update ((self exit))
  (ecase (crate-state self)
    (:idle nil)
    (:activated
     (if (< (exit-delay self) 3)
         (incf (exit-delay self))
         (request-next-level))))
  (call-next-method))

(defmethod visual ((self exit))
  (if (exit-activated self)
      #\E
      #\e))