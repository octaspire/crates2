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

(defmethod touches-left ((self block-counter))
  (- (block-counter-count self)
     (block-counter-touches self)))

(defmethod visual ((self block-counter))
  (let ((result "number-")
        (passstr (format nil "~2,'0d" (touches-left self))))
    (setf result (concatenate 'string result passstr))
    (list "block-counter" result)))

(defmethod collide ((self block-counter) (target moving))
  (ecase (crate-state self)
    (:idle (incf (block-counter-touches self))
     (when (<= (touches-left self) 0)
       (lament self)))
    (:lamented nil)))

