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

(defun make-block-timer (x y time)
  (make-instance 'block-timer :x x :y y :z 0 :time time))

;; Methods

(defmethod update ((self block-timer))
  (ecase (crate-state self)
    (:idle nil)
    (:active
     (incf (block-timer-uptime self) *frame-duration-default*)
     (when (<= (time-left self) 0)
       (lament self)))
    (:lamented nil))
  (call-next-method))

(defmethod time-left ((self block-timer))
  (ceiling (- (block-timer-time self)
              (block-timer-uptime self))))

(defmethod visual ((self block-timer))
  (let ((result "number-")
        (passstr (format nil "~2,'0d" (time-left self))))
    (setf result (concatenate 'string result passstr))
    (list (if (block-timer-durable self)
              "block-timer-durable"
              "block-timer")
          result)))

(defmethod collide ((self block-timer) (target moving))
  (ecase (crate-state self)
    (:idle
     (setf (crate-state self) :active)
     (crates2-ui:ui-play-sound :hit-wall)
     (crates2-ui:ui-play-sound :hit-counter))
    (:active
     (crates2-ui:ui-play-sound :hit-wall)
     (crates2-ui:ui-play-sound :hit-counter)
     (unless (block-timer-durable self)
       (lament self)))
    (:lamented nil)))

