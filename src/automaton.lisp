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

(defmethod visual ((self automaton))
  (ecase (crate-state self)
    (:idle        (list "automaton-idle"))
    (:programming (list "automaton-programming"))
    (:executing   (if (= (crate-z self) 0)
                      (list "automaton-executing")
                      (list "automaton-executing-hover")))))

(defmethod update ((self automaton))
  (ecase (crate-state self)
    (:idle        (automaton-update-idle self))
    (:programming (automaton-update-programming self))
    (:executing   (automaton-update-executing self))
    (:lamented    (automaton-update-lamented self)))
  (call-next-method))

(defmethod collide ((self automaton) (target player))
  (setf (active target) nil)
  (setf (automaton-programmer self) target)
  (setf (crate-state self) :programming)
  (setf (player-controlled target) self)
  (crates2-ui:ui-play-sound :hit-automaton)
  (call-next-method))

(defmethod append-instruction ((self automaton) instruction)
  (crates2-ui:ui-play-sound :automaton-key)
  (setf (automaton-program self)
        (str:concat (automaton-program self) (string instruction))))

(defmethod remove-instruction-back ((self automaton))
  (crates2-ui:ui-play-sound :automaton-key)
  (setf (automaton-program self)
        (str:substring 0 -1 (automaton-program self))))

(defmethod remove-instruction-front ((self automaton))
  (setf (automaton-program self)
        (str:substring 1
                       (length (automaton-program self))
                       (automaton-program self))))

(defmethod stop-programming ((self automaton))
  (crates2-ui:ui-play-sound :automaton-key)
  (setf (crate-state self) :executing)
  (setf (active (automaton-programmer self)) t)
  (setf (player-controlled (automaton-programmer self)) nil)
  (setf (automaton-programmer self) nil)
  (setf *program* ""))

(defmethod handle-input ((self automaton) input)
  (when (and input (eq (crate-state self) :programming))
    (ecase input
      (:east      (append-instruction self #\→))
      (:west      (append-instruction self #\←))
      (:north     (append-instruction self #\↑))
      (:south     (append-instruction self #\↓))
      (:action1   (append-instruction self #\☐))
      (:action2   (append-instruction self #\❌))
      (:action3   (stop-programming self))
      (:backspace (remove-instruction-back self)))))

(defmethod automaton-step ((self automaton))
  (let* ((program (automaton-program self))
         (proglen (length program)))
    (when (> proglen 0)
      (let ((c (str:s-first program)))
        (alexandria:switch (c :test #'equal)
          ("→" (setf (velocity self) :east))
          ("←" (setf (velocity self) :west))
          ("↑" (setf (velocity self) :north))
          ("↓" (setf (velocity self) :south))
          ("☐" (setf (crate-z self) (1+ (crate-z self))))
          ("❌" (setf (velocity self) :zero))))
      (remove-instruction-front self))))

(defmethod automaton-reset ((self automaton))
  (setf (crate-state self) :idle)
  (setf (automaton-program self) ""))

;; Functions

(defun automaton-update-idle (self))

(defun automaton-update-lamented (self))

(defun automaton-update-programming (self)
  (setf *program* (automaton-program self)))

(defun automaton-update-executing (self)
  (setf (velocity self) :zero)
  (setf (crate-z self) 0)
  (let* ((program (automaton-program self))
         (proglen (length program)))
    (if (> proglen 0)
        (automaton-step self)
        (automaton-reset self))))

