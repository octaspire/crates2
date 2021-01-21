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

;; Methods

(defmethod toggle-on-p ((self toggle))
  (and (toggle-east self)
       (toggle-west self)
       (toggle-north self)
       (toggle-south self)))

(defmethod visual ((self toggle))
  (let ((result (list "toggle-idle")))
    ;; EAST
    (if (toggle-east self)
        (nconc result (list "toggle-east-on"))
        (nconc result (list "toggle-east-off")))
    ;; WEST
    (if (toggle-west self)
        (nconc result (list "toggle-west-on"))
        (nconc result (list "toggle-west-off")))
    ;; NORTH
    (if (toggle-north self)
        (nconc result (list "toggle-north-on"))
        (nconc result (list "toggle-north-off")))
    ;; SOUTH
    (if (toggle-south self)
        (nconc result (list "toggle-south-on"))
        (nconc result (list "toggle-south-off")))
    result))

(defmethod collide ((self toggle) (target moving))
  (let ((side (on-which-side-is-other self target)))
      (ecase side
        (:east (setf (toggle-east self) t))
        (:west (setf (toggle-west self) t))
        (:north (setf (toggle-north self) t))
        (:south (setf (toggle-south self) t))
        (:zero nil))))
