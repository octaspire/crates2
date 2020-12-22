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

(defparameter *num-levels* 3)

(defun load-level (index)
  (ecase index
    (0 (load-level-0))
    (1 (load-level-1))
    (2 (load-level-2))))

(defun load-level-0 ()
  (list (make-instance 'exit     :x 1 :y 3 :z 0)
        (make-instance 'player   :x 3 :y 3 :z 0)
        (make-instance 'pushed   :x 5 :y 3 :z 0)))

(defun load-level-1 ()
  (list (make-instance 'exit     :x 10 :y 6  :z 0)
        (make-instance 'slope-en :x 3  :y 6  :z 0)
        (make-instance 'slope-es :x 3  :y 2  :z 0)
        (make-instance 'slope-ws :x 14 :y 2  :z 0)
        (make-instance 'slope-wn :x 14 :y 6  :z 0)
        (make-instance 'player   :x 8  :y 6 :z 0)))

(defun load-level-2 ()
  (list (make-instance 'exit     :x 1 :y 2 :z 0)
        (make-instance 'vacuum   :x 3 :y 2 :z -1)
        (make-instance 'player   :x 5 :y 2 :z 0)))
