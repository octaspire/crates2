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

(defparameter *num-levels* 12)

(defun load-level (index)
  (ecase index
    (0 (list (list nil nil nil :east nil nil nil :east)
             (list (make-instance 'exit        :x 6 :y 3 :z 0)
                   (make-instance 'turnstile-e :x 3 :y 3 :z 0)
                   (make-instance 'player      :x 1 :y 3 :z 0))))
    (1 (list (list nil nil nil :west nil nil nil :west)
             (list (make-instance 'exit        :x 1 :y 3 :z 0)
                   (make-instance 'turnstile-w :x 3 :y 3 :z 0)
                   (make-instance 'player      :x 6 :y 3 :z 0))))
    (2  (list (list nil nil nil :north nil nil nil :north)
              (list (make-instance 'exit        :x 3 :y 1 :z 0)
                    (make-instance 'turnstile-n :x 3 :y 3 :z 0)
                    (make-instance 'player      :x 3 :y 6 :z 0))))
    (3  (list (list nil nil nil :south nil nil nil :south)
              (list (make-instance 'exit        :x 3 :y 6 :z 0)
                    (make-instance 'turnstile-s :x 3 :y 3 :z 0)
                    (make-instance 'player      :x 3 :y 1 :z 0))))
    (4 (list (list nil nil nil :east nil nil nil :east)
             (list (make-instance 'exit         :x 6 :y 3 :z 0)
                   (make-instance 'turnstile-e1 :x 3 :y 3 :z 0)
                   (make-instance 'player       :x 1 :y 3 :z 0))))
    (5 (list (list nil nil nil :west nil nil nil :west)
             (list (make-instance 'exit         :x 1 :y 3 :z 0)
                   (make-instance 'turnstile-w1 :x 3 :y 3 :z 0)
                   (make-instance 'player       :x 6 :y 3 :z 0))))
    (6  (list (list nil nil nil :north nil nil nil :north)
              (list (make-instance 'exit         :x 3 :y 1 :z 0)
                    (make-instance 'turnstile-n1 :x 3 :y 3 :z 0)
                    (make-instance 'player       :x 3 :y 6 :z 0))))
    (7  (list (list nil nil nil :south nil nil nil :south)
              (list (make-instance 'exit         :x 3 :y 6 :z 0)
                    (make-instance 'turnstile-s1 :x 3 :y 3 :z 0)
                    (make-instance 'player       :x 3 :y 1 :z 0))))
    (8 (list (list :west nil nil nil nil nil nil nil )
             (list (make-instance 'exit     :x 10 :y 6  :z 0)
                   (make-instance 'slope-en :x 3  :y 6  :z 0)
                   (make-instance 'slope-es :x 3  :y 2  :z 0)
                   (make-instance 'slope-ws :x 14 :y 2  :z 0)
                   (make-instance 'slope-wn :x 14 :y 6  :z 0)
                   (make-instance 'player   :x 8  :y 6 :z 0))))
    (9 (list (list :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
             (list (make-instance 'exit :x 1 :y 2 :z 0)
                   (make-instance 'vacuum :x 3 :y 2 :z -1)
                   (make-instance 'player :x 5 :y 2 :z 0))))
    (10 (list (list nil nil nil nil :west nil nil nil nil :west nil nil nil nil)
              (list (make-instance 'exit        :x 1 :y 3 :z 0)
                    (make-instance 'block-timer :x 4 :y 3 :z 0)
                    (make-instance 'player      :x 8 :y 3 :z 0))))
    (11 (list (list nil nil nil nil :west nil nil nil :east nil nil nil :west nil nil nil :east nil nil nil :west)
              (list (make-instance 'exit          :x 1 :y 3 :z 0)
                    (make-instance 'block-counter :x 3 :y 3 :z 0 :count 2)
                    (make-instance 'player        :x 5 :y 3 :z 0)
                    (make-instance 'block-counter :x 8 :y 3 :z 0))))))

