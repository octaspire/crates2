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

(defmethod purge-ring ((self bomb) ring)
  (let* ((x1 (max (- (crate-x self) ring) 0))
         (y1 (max (- (crate-y self) ring) 0))
         (x2 (min (+ (crate-x self) ring) (- *level-width* 1)))
         (y2 (min (+ (crate-y self) ring) (- *level-height* 1)))
         (z (crate-z self)))
    (loop for y from y1 to y2
          do
             (loop for x from x1 to x2
                   do
                      (let ((crate (find-at x y z)))
                        (when crate
                          (typecase crate
                            (player (lament crate))
                            (bomb (unless (bomb-durable crate)
                                      (activate crate))))))))))

(defmethod activate ((self bomb))
  (when (eq (crate-state self) :idle)
    (setf (crate-state self) :active)))

(defmethod update ((self bomb))
  (ecase (crate-state self)
    (:idle nil)
    (:active
     (incf (bomb-uptime self) *frame-duration-default*)
     (when (<= (time-left self) 0)
       (setf (crate-state self) :explosion1)))
    (:explosion1
     (setf (crate-state self) :explosion2)
     (crates2-ui:ui-play-sound :explosion))
    (:explosion2
     (purge-ring self 1)
     (setf (crate-state self) :explosion3))
    (:explosion3
     (setf (crate-state self) :explosion4))
    (:explosion4
     (purge-ring self 2)
     (setf (crate-state self) :exploded))
    (:exploded
     (lament self))
    (:lamented nil))
  (call-next-method))

(defmethod time-left ((self bomb))
  (ceiling (- (bomb-time self)
              (bomb-uptime self))))

(defmethod visual ((self bomb))
  (let* ((time (time-left self))
         (tstr (format nil "number-~2,'0d" time))
         (result (list (if (bomb-durable self)
                           "bomb-durable"
                           "bomb"))))
    (when (> time 0)
      (setf result (nconc result (list tstr))))
    (case (crate-state self)
      (:explosion1
       (setf result (nconc result (list "bomb-ring-1"))))
      (:explosion2
       (setf result (nconc result (list "bomb-ring-2"))))
      (:explosion3
       (setf result (nconc result (list "bomb-ring-3"))))
      (:explosion4
       (setf result (nconc result (list "bomb-ring-4")))))
    result))

(defmethod collide ((self bomb) (target moving))
  (crates2-ui:ui-play-sound :hit-wall)
  (crates2-ui:ui-play-sound :bomb-on)
  (case (crate-state self)
    (:idle (setf (crate-state self) :active))
    (:active (unless (bomb-durable self)
               (setf (crate-state self) :explosion1)))
    (:lamented nil)))

