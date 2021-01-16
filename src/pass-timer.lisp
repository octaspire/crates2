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

(defmethod time-left ((self pass-timer))
  (ceiling (- (pass-timer-time self)
              (pass-timer-uptime self))))

(defmethod visual ((self pass-timer))
  (let ((result "pass-timer-")
        (timestr (format nil "~2,'0d" (time-left self))))
    (setf result (concatenate 'string result timestr))
    (list result)))

(defmethod update ((self pass-timer))
  (ecase (crate-state self)
    (:idle
     (let ((crate (find-at-of-type (crate-x self) (crate-y self) 0 'moving)))
       (when crate
         (setf (crate-state self) :countdown))))
    (:countdown
     (incf (pass-timer-uptime self) *frame-duration-default*)
     (when (<= (time-left self) 0)
       (setf (crate-state self) :triggered)))
    (:triggered
     (let* ((x (crate-x self))
            (y (crate-y self))
            (z 0)
            (crate (find-at x y z)))
       (unless crate
         (request-attaching (make-instance 'wall :x x :y y :z z))
         (lament self))))
    (:lamented nil)))
