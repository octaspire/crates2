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

(defmethod update ((self player))
  (ecase (crate-state self)
    (:idle (player-update-idle self))
    (:lamented (player-update-lamented self)))
  (call-next-method))

(defmethod visual ((self player))
  (if (active self)
      "OOOOOO"
      "      "))

(defmethod collide ((self player) (target crate))
  (call-next-method))

(defmethod collide ((self player) (target exit))
  (setf (active self) nil))

(defmethod collide ((self player) (target vacuum))
  (setf (active self) nil)
  (setf (crate-state self) :lamented)
  (setf (lamented self) t)
  (call-next-method))

(defmethod handle-input ((self player) input)
  (when (active self)
    (ecase input
      (:east  (setf (velocity self) input))
      (:west  (setf (velocity self) input))
      (:north (setf (velocity self) input))
      (:south (setf (velocity self) input)))))

(defmethod collide ((self player) (target pushed))
  (let ((vplayer (velocity self))
        (vpulled (velocity target)))
    (if (eq vpulled :zero)
        (setf (velocity target) vplayer)
        (setf (velocity target) (if (head-on-collision-p vplayer vpulled) :zero vplayer))))
  (call-next-method))

;; Functions

(defun player-update-idle (self)
  (let ((input (car *input*)))
    (when (and input (stationaryp self))
      (handle-input self input))))

(defun player-update-lamented (self)
  (incf (player-delay self))
  (when (> (player-delay self) 3)
    (request-restart-level)))
