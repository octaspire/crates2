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

(defmethod update ((self block-timer))
  (ecase (block-timer-state self)
    (:idle nil)
    (:active
     (incf (block-timer-uptime self) 0.5)
     (when (<= (time-left self) 0)
       (lament self)))
    (:lamented nil))
  (call-next-method))

(defmethod time-left ((self block-timer))
  (ceiling (- (block-timer-time self)
              (block-timer-uptime self))))

(defmethod lament ((self block-timer))
  (setf (block-timer-state self) :lamented)
  (setf (lamented self) t))

(defmethod visual ((self block-timer))
  (let ((durstr (if (block-timer-durable self) "D" "|"))
        (statstr (if (eq (block-timer-state self) :idle) "|" "X"))
        (timestr (format nil "~2,'0d" (time-left self))))
    (format nil "BT~A~A~A" durstr statstr timestr)))

(defmethod collide ((self block-timer) (target moving))
  (ecase (block-timer-state self)
    (:idle (setf (block-timer-state self) :active))
    (:active (unless (block-timer-durable self)
               (lament self)))
    (:lamented nil)))
