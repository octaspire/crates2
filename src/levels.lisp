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

(defparameter *num-levels* 24)

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
    (9 (list (list :west nil nil nil nil nil nil nil nil nil nil nil)
             (list (make-instance 'exit :x 1 :y 2 :z 0)
                   ;; (make-instance 'vacuum :x 3 :y 2 :z -1)
                   (make-instance 'player :x 5 :y 2 :z 0))))
    (10 (list (list nil nil nil nil :west nil nil nil nil :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil :west)
              (list (make-instance 'exit        :x 1 :y 3 :z 0)
                    ;; (make-instance 'block-timer :x 4 :y 3 :z 0)
                    (make-instance 'player      :x 8 :y 3 :z 0))))
    (11 (list (list nil nil nil nil :west nil nil nil :east nil nil nil :west nil nil nil :east nil nil nil :west)
              (list (make-instance 'exit          :x 1 :y 3 :z 0)
                    (make-instance 'block-counter :x 3 :y 3 :z 0 :count 2)
                    (make-instance 'player        :x 5 :y 3 :z 0)
                    (make-instance 'block-counter :x 8 :y 3 :z 0))))
    (12 (list (list nil nil nil nil :west nil nil nil nil nil :east nil nil nil nil nil :west nil nil nil nil nil nil nil :east nil nil nil nil :south)
              (list (make-instance 'wall         :x 1 :y 3 :z 0)
                    (make-instance 'exit         :x 3 :y 5 :z 0)
                    (make-instance 'pass-counter :x 4 :y 3 :z 1 :count 3)
                    (make-instance 'player       :x 5 :y 3 :z 0)
                    (make-instance 'wall         :x 8 :y 3 :z 0))))
    (13 (list (list nil nil nil nil :west)
              (list (make-instance 'wall         :x 1 :y 3 :z 0)
                    (make-instance 'exit         :x 5 :y 5 :z 0)
                    ;; (make-instance 'pass-timer   :x 4 :y 3 :z 1 :time 10)
                    (make-instance 'player       :x 6 :y 5 :z 0)
                    (make-instance 'wall         :x 8 :y 3 :z 0))))
    (14 (list (list nil nil nil nil :west nil nil nil nil nil nil nil nil :south)
              (list (make-instance 'wall         :x 1 :y 3 :z 0)
                    (make-instance 'exit         :x 6 :y 5 :z 0)
                    (make-instance 'exit         :x 2 :y 5 :z 0)
                    (make-instance 'key          :x 4 :y 3 :z 0)
                    (make-instance 'player       :x 6 :y 3 :z 0))))
    (15 (list (list nil nil nil nil :west nil nil nil nil nil nil nil nil :south nil nil nil nil nil :north nil nil nil nil nil :east nil nil nil nil nil nil :south)
              (list (make-instance 'wall         :x 1 :y 3 :z 0)
                    (make-instance 'wall         :x 2 :y 0 :z 0)
                    (make-instance 'wall         :x 7 :y 1 :z 0)
                    (make-instance 'exit         :x 6 :y 5 :z 0)
                    (make-instance 'player       :x 6 :y 3 :z 0)
                    (make-instance 'pulled       :x 2 :y 5 :z 0 :north t))))
    (16 (list (list nil nil nil nil
                    :east  nil nil nil nil nil nil nil nil
                    :west  nil nil nil nil nil
                    :north nil nil nil nil nil
                    :south nil nil nil nil nil nil nil nil nil nil nil nil
                    :west  nil nil nil nil nil nil nil nil
                    :east  nil nil nil nil nil nil nil
                    :south nil nil nil nil
                    :north)
              (list (make-instance 'pulled       :x 4 :y 3 :z 0 :east t)
                    (make-instance 'pulled       :x 5 :y 9 :z 0 :north t)
                    (make-instance 'player       :x 6 :y 3 :z 0)
                    (make-instance 'pulled       :x 8 :y 3 :z 0 :west t)
                    (make-instance 'pulled       :x 5 :y 2 :z 0 :south t)
                    (make-instance 'pulled       :x 2 :y 8 :z 0 :east t)
                    (make-instance 'pulled       :x 9 :y 8 :z 0 :west t)
                    (make-instance 'pulled       :x 8 :y 9 :z 0 :north t)
                    (make-instance 'exit         :x 8 :y 2 :z 0))))
    (17 (list (list nil nil
                    :east nil nil nil nil nil nil
                    :north nil nil nil nil nil nil nil nil
                    :east nil nil nil
                    :north nil nil
                    :west nil nil
                    :south nil nil nil nil
                    :west nil nil nil nil
                    :north nil nil nil nil nil
                    :east nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil nil nil
                    :south nil nil nil nil nil
                    :west nil nil nil nil
                    :south nil nil nil
                    :west nil nil nil nil
                    :south nil nil
                    :west nil nil nil
                    :north nil nil nil
                    :west nil nil
                    :north nil nil
                    :west nil nil
                    :north nil nil nil nil nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil
                    :east nil nil
                    :south nil nil nil nil nil
                    :north nil nil nil nil
                    :east nil nil nil nil
                    :north nil nil
                    :west nil nil nil
                    :north nil nil nil nil nil nil
                    :south nil nil nil nil nil nil nil
                    :west nil nil
                    :north nil nil nil nil
                    :west nil nil nil nil nil ; this takes us out from the heart to (0,5)
                    :east nil
                    :south nil nil
                    :east nil nil
                    :south nil nil nil
                    :east nil nil nil nil nil
                    :north nil nil
                    :east nil nil
                    :north nil nil
                    :east nil nil nil nil nil
                    :east)
              (list (make-instance 'wall        :x 1  :y 0 :z 0)
                    (make-instance 'wall        :x 6  :y 1 :z 0)
                    (make-instance 'key         :x 7  :y 1 :z 0)
                    (make-instance 'slope-ws    :x 10 :y 1 :z 0)
                    (make-instance 'wall        :x 13 :y 2 :z 0)
                    (make-instance 'wall        :x 3  :y 3 :z 0)
                    (make-instance 'wall        :x 4  :y 3 :z 0)
                    (make-instance 'wall        :x 8  :y 3 :z 0)
                    (make-instance 'wall        :x 9  :y 3 :z 0)
                    (make-instance 'wall        :x 2  :y 4 :z 0)
                    (make-instance 'key         :x 4  :y 4 :z 0)
                    (make-instance 'turnstile-e :x 5  :y 4 :z 0)
                    (make-instance 'turnstile-s :x 7  :y 4 :z 0)
                    (make-instance 'key         :x 8  :y 4 :z 0)
                    (make-instance 'wall        :x 10 :y 4 :z 0)
                    (make-instance 'slope-es    :x 0  :y 5 :z 0)
                    (make-instance 'turnstile-w :x 2  :y 5 :z 0)
                    (make-instance 'wall        :x 6  :y 5 :z 0)
                    (make-instance 'turnstile-e :x 10 :y 5 :z 0)
                    (make-instance 'turnstile-e :x 12 :y 5 :z 0)
                    (make-instance 'block-timer :x 14 :y 5 :z 0 :time 18)
                    (make-instance 'wall        :x 2  :y 6 :z 0)
                    (make-instance 'wall        :x 10 :y 6 :z 0)
                    (make-instance 'exit        :x 16 :y 6 :z 0)
                    (make-instance 'wall        :x 2  :y 7 :z 0)
                    (make-instance 'wall        :x 10 :y 7 :z 0)
                    (make-instance 'wall        :x 0  :y 8 :z 0)
                    (make-instance 'wall        :x 3  :y 8 :z 0)
                    (make-instance 'wall        :x 9  :y 8 :z 0)
                    (make-instance 'slope-wn    :x 14 :y 8 :z 0)
                    (make-instance 'wall        :x 4  :y 9 :z 0)
                    (make-instance 'wall        :x 8  :y 9 :z 0)
                    (make-instance 'wall        :x 1  :y 10 :z 0)
                    (make-instance 'wall        :x 5  :y 10 :z 0)
                    (make-instance 'wall        :x 7  :y 10 :z 0)
                    (make-instance 'wall        :x 11 :y 10 :z 0)
                    (make-instance 'turnstile-n :x 6  :y 11 :z 0)
                    (make-instance 'wall        :x 9  :y 12 :z 0)
                    (make-instance 'wall        :x 3  :y 13 :z 0)
                    (make-instance 'wall        :x 7  :y 14 :z 0)
                    (make-instance 'player      :x 0  :y 14 :z 0))))
    (18 (list (list nil nil
                    :west nil nil nil nil nil nil
                    :south nil nil nil nil
                    :east nil nil nil nil
                    :north nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil nil
                    :north nil nil nil
                    :west nil nil
                    :east nil nil nil nil
                    :north nil nil nil nil nil
                    :west nil nil nil nil
                    :south nil nil nil
                    :north nil nil nil nil
                    :west nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil
                    :west nil nil nil nil
                    :south)
              (list (make-instance 'wall          :x 5  :y 0 :z 0)
                    (make-instance 'block-counter :x 7  :y 0 :count 1)
                    (make-instance 'block-counter :x 1  :y 1 :count 1)
                    (make-instance 'block-counter :x 9  :y 1 :count 1)
                    (make-instance 'wall          :x 4  :y 2)
                    (make-instance 'wall          :x 6  :y 2)
                    (make-instance 'block-counter :x 0  :y 3 :count 1)
                    (make-instance 'key           :x 3  :y 3)
                    (make-instance 'wall          :x 4  :y 3)
                    (make-instance 'wall          :x 6  :y 3)
                    (make-instance 'key           :x 7  :y 3)
                    (make-instance 'wall          :x 2  :y 4)
                    (make-instance 'wall          :x 3  :y 4)
                    (make-instance 'wall          :x 4  :y 4)
                    (make-instance 'wall          :x 6  :y 4)
                    (make-instance 'wall          :x 7  :y 4)
                    (make-instance 'wall          :x 8  :y 4)
                    (make-instance 'wall          :x 0  :y 5)
                    (make-instance 'player        :x 5  :y 5)
                    (make-instance 'wall          :x 10 :y 5)
                    (make-instance 'wall          :x 2  :y 6)
                    (make-instance 'wall          :x 3  :y 6)
                    (make-instance 'wall          :x 4  :y 6)
                    (make-instance 'wall          :x 6  :y 6)
                    (make-instance 'wall          :x 7  :y 6)
                    (make-instance 'wall          :x 8  :y 6)
                    (make-instance 'key           :x 3  :y 7)
                    (make-instance 'wall          :x 4  :y 7)
                    (make-instance 'wall          :x 6  :y 7)
                    (make-instance 'key           :x 7  :y 7)
                    (make-instance 'block-counter :x 10 :y 7 :count 1)
                    (make-instance 'wall          :x 4  :y 8)
                    (make-instance 'wall          :x 6  :y 8)
                    (make-instance 'block-counter :x 1  :y 9 :count 1)
                    (make-instance 'block-counter :x 9  :y 9 :count 1)
                    (make-instance 'exit          :x 1  :y 10)
                    (make-instance 'block-counter :x 3  :y 10 :count 1)
                    (make-instance 'wall          :x 5  :y 10))))
    (19 (list (list nil nil
                    :north nil nil nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil nil nil
                    nil nil nil nil nil nil nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil
                    :south nil nil nil nil nil
                    :north nil nil nil nil nil
                    :east nil nil nil nil nil
                    :south nil nil nil nil nil
                    :west nil nil nil nil nil
                    :south nil nil nil nil nil
                    :west nil nil nil nil nil
                    :north nil nil nil nil nil
                    :west nil nil nil nil nil
                    :north nil nil nil nil nil
                    :west)
              (list (make-instance 'slope-es      :x 6  :y 0 :z 0)
                    (make-instance 'slope-ws      :x 11 :y 0 :z 0)
                    (make-instance 'block-timer   :x 2  :y 2 :z 0 :time 18)
                    (make-instance 'exit          :x 0  :y 3 :z 0)
                    (make-instance 'turnstile-s   :x 3  :y 3 :z 0)
                    (make-instance 'player        :x 6  :y 3 :z 0)
                    (make-instance 'pass-counter  :x 11 :y 3 :z 1 :count 2)
                    (make-instance 'wall          :x 9  :y 4 :z 0)
                    (make-instance 'wall          :x 4  :y 5 :z 0)
                    (make-instance 'wall          :x 1  :y 6 :z 0)
                    (make-instance 'wall          :x 8  :y 6 :z 0)
                    (make-instance 'key           :x 3  :y 7 :z 0)
                    (make-instance 'wall          :x 3  :y 8 :z 0)
                    (make-instance 'slope-en      :x 6  :y 8 :z 0)
                    (make-instance 'slope-wn      :x 11 :y 8 :z 0)
                    (make-instance 'wall          :x 5  :y 9 :z 0))))
    (20 (list (list nil nil
                    :east nil nil nil nil nil
                    :west nil nil
                    :north nil nil
                    :south nil nil
                    :east nil nil nil
                    :north nil nil
                    :east nil nil nil
                    :west nil nil
                    :north nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil nil nil
                    :west nil nil
                    :north nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :north nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil nil
                    :north nil nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil nil
                    :north nil nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil nil
                    :north nil nil nil nil nil
                    :east nil nil nil nil nil
                    :south nil nil nil nil nil
                    :west nil nil nil nil
                    :south nil
                    :north nil nil nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil
                    :west nil nil nil nil
                    :north nil
                    :west nil nil
                    :south nil
                    :west nil nil
                    :south nil
                    :north nil nil nil
                    :east nil nil nil nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :south nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :south nil
                    :west nil nil
                    :south nil
                    :north nil nil nil
                    :east nil nil nil nil nil nil nil nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :south nil nil nil
                    :west nil nil
                    :north nil
                    :west nil nil
                    :south nil
                    :west nil nil
                    :north nil
                    :west)
              (list (make-instance 'wall          :x 0  :y 0 :z 0)
                    (make-instance 'wall          :x 1  :y 0 :z 0)
                    (make-instance 'wall          :x 2  :y 0 :z 0)
                    (make-instance 'wall          :x 3  :y 0 :z 0)
                    (make-instance 'wall          :x 4  :y 0 :z 0)
                    (make-instance 'wall          :x 5  :y 0 :z 0)
                    (make-instance 'wall          :x 6  :y 0 :z 0)
                    (make-instance 'wall          :x 7  :y 0 :z 0)
                    (make-instance 'wall          :x 8  :y 0 :z 0)
                    (make-instance 'wall          :x 9  :y 0 :z 0)
                    (make-instance 'wall          :x 10 :y 0 :z 0)
                    (make-instance 'wall          :x 11 :y 0 :z 0)
                    (make-instance 'wall          :x 12 :y 0 :z 0)
                    (make-instance 'wall          :x 13 :y 0 :z 0)
                    (make-instance 'wall          :x 14 :y 0 :z 0)
                    (make-instance 'wall          :x 15 :y 0 :z 0)
                    (make-instance 'wall          :x 16 :y 0 :z 0)
                    (make-instance 'wall          :x 17 :y 0 :z 0)
                    (make-instance 'wall          :x 18 :y 0 :z 0)
                    (make-instance 'wall          :x 0  :y 1 :z 0)
                    (make-instance 'block-counter :x 6  :y 1 :z 0 :count 1)
                    (make-instance 'block-counter :x 10 :y 1 :z 0 :count 1)
                    (make-instance 'block-counter :x 14 :y 1 :z 0 :count 1)
                    (make-instance 'key           :x 17 :y 1 :z 0)
                    (make-instance 'wall          :x 18 :y 1 :z 0)
                    (make-instance 'wall          :x 0  :y 2 :z 0)
                    (make-instance 'wall          :x 2  :y 2 :z 0)
                    (make-instance 'wall          :x 3  :y 2 :z 0)
                    (make-instance 'wall          :x 6  :y 2 :z 0)
                    (make-instance 'wall          :x 7  :y 2 :z 0)
                    (make-instance 'wall          :x 10 :y 2 :z 0)
                    (make-instance 'wall          :x 11 :y 2 :z 0)
                    (make-instance 'wall          :x 14 :y 2 :z 0)
                    (make-instance 'wall          :x 15 :y 2 :z 0)
                    (make-instance 'wall          :x 16 :y 2 :z 0)
                    (make-instance 'turnstile-s   :x 17 :y 2 :z 0)
                    (make-instance 'wall          :x 18 :y 2 :z 0)
                    (make-instance 'wall          :x 0  :y 3 :z 0)
                    (make-instance 'pulled        :x 5  :y 3 :z 0 :north t :south t)
                    (make-instance 'pulled        :x 9 :y 3 :z 0 :north t :south t)
                    (make-instance 'pulled        :x 13 :y 3 :z 0 :north t :south t)
                    (make-instance 'wall          :x 18 :y 3 :z 0)
                    (make-instance 'wall          :x 0  :y 4 :z 0)
                    (make-instance 'wall          :x 1  :y 4 :z 0)
                    (make-instance 'wall          :x 2  :y 4 :z 0)
                    (make-instance 'wall          :x 3  :y 4 :z 0)
                    (make-instance 'wall          :x 4  :y 4 :z 0)
                    (make-instance 'wall          :x 6  :y 4 :z 0)
                    (make-instance 'wall          :x 7  :y 4 :z 0)
                    (make-instance 'wall          :x 8  :y 4 :z 0)
                    (make-instance 'wall          :x 10 :y 4 :z 0)
                    (make-instance 'wall          :x 11 :y 4 :z 0)
                    (make-instance 'wall          :x 12 :y 4 :z 0)
                    (make-instance 'wall          :x 14 :y 4 :z 0)
                    (make-instance 'wall          :x 15 :y 4 :z 0)
                    (make-instance 'wall          :x 16 :y 4 :z 0)
                    (make-instance 'wall          :x 18 :y 4 :z 0)
                    (make-instance 'exit          :x 0  :y 5 :z 0)
                    (make-instance 'player        :x 2  :y 5 :z 0)
                    (make-instance 'slope-ws      :x 6  :y 5 :z 0)
                    (make-instance 'slope-ws      :x 10 :y 5 :z 0)
                    (make-instance 'slope-ws      :x 14 :y 5 :z 0)
                    (make-instance 'wall          :x 18 :y 5 :z 0)
                    (make-instance 'wall          :x 0  :y 6 :z 0)
                    (make-instance 'wall          :x 4  :y 6 :z 0)
                    (make-instance 'wall          :x 8  :y 6 :z 0)
                    (make-instance 'wall          :x 12 :y 6 :z 0)
                    (make-instance 'wall          :x 18 :y 6 :z 0)
                    (make-instance 'wall          :x 0  :y 7 :z 0)
                    (make-instance 'wall          :x 1  :y 7 :z 0)
                    (make-instance 'wall          :x 2  :y 7 :z 0)
                    (make-instance 'wall          :x 3  :y 7 :z 0)
                    (make-instance 'wall          :x 4  :y 7 :z 0)
                    (make-instance 'pulled        :x 5  :y 7 :z 0 :north t)
                    (make-instance 'wall          :x 6  :y 7 :z 0)
                    (make-instance 'wall          :x 7  :y 7 :z 0)
                    (make-instance 'wall          :x 8  :y 7 :z 0)
                    (make-instance 'pulled        :x 9  :y 7 :z 0 :north t)
                    (make-instance 'wall          :x 10 :y 7 :z 0)
                    (make-instance 'wall          :x 11 :y 7 :z 0)
                    (make-instance 'wall          :x 12 :y 7 :z 0)
                    (make-instance 'pulled        :x 13 :y 7 :z 0 :north t)
                    (make-instance 'wall          :x 14 :y 7 :z 0)
                    (make-instance 'wall          :x 15 :y 7 :z 0)
                    (make-instance 'wall          :x 16 :y 7 :z 0)
                    (make-instance 'wall          :x 17 :y 7 :z 0)
                    (make-instance 'wall          :x 18 :y 7 :z 0))))
    (21 (list (list nil nil
                    :north nil nil nil
                    :west :west :west
                    :north nil nil
                    :south nil nil
                    :west
                    :north nil nil
                    :south nil nil
                    :west :west
                    :north :north
                    :east nil nil nil
                    :north
                    :west nil nil nil
                    :north
                    :east nil
                    :west nil
                    :south :south :south
                    :east :east :east :east
                    :north nil nil
                    :west nil nil
                    :north nil nil
                    :east
                    :south nil
                    :east nil nil nil nil nil
                    :south nil nil
                    :west :west :west
                    :north nil nil
                    :south nil nil
                    :east
                    :north nil nil
                    :south nil nil
                    :east
                    :north nil nil nil
                    :west nil nil nil
                    :east nil nil nil
                    :south nil nil nil
                    :west :west :west :west :west
                    :north nil
                    :west nil
                    :north :north :north
                    :east nil nil
                    :west nil nil
                    :north
                    :east nil nil nil
                    :south nil nil
                    :east nil
                    :south nil
                    :west nil
                    :north nil nil nil
                    :west nil
                    :south :south :south :south :south :south
                    :east nil
                    :north nil
                    :south nil
                    :east :east :east :east :east :east :east :east
                    :north nil
                    :south nil
                    :east
                    :north nil
                    :south nil
                    :east :east
                    :north :north
                    :west nil nil nil
                    :north nil
                    :east nil nil nil
                    :north
                    :west nil
                    :east nil
                    :south :south :south
                    :west :west :west :west
                    :north nil nil
                    :east nil nil
                    :north nil
                    :west nil
                    :south nil
                    :west nil nil nil nil nil nil nil nil nil
                    :south :south
                    :east :east :east :east :east :east :east
                    :north nil nil nil
                    :south nil nil nil
                    :east
                    :north nil nil nil
                    :south nil nil nil
                    :west :west
                    :north nil nil nil
                    :east nil nil nil
                    :south nil
                    :west nil nil nil nil nil nil nil nil
                    :south :south
                    :east :east :east :east :east :east :east :east :east :east :east
                    :north nil
                    :east nil
                    :north :north :north
                    :west nil nil nil
                    :east nil nil nil
                    :north
                    :west nil nil nil
                    :south nil nil nil
                    :west nil
                    :south
                    :east nil
                    :north nil nil nil
                    :east nil nil
                    :north :north :north :north :north
                    :west :west
                    :south nil nil
                    :north nil nil
                    :west
                    :south nil nil
                    :north nil nil
                    :east :east :east
                    :south :south
                    :west nil nil nil
                    :south
                    :west
                    :east nil nil nil
                    :south
                    :west nil
                    :east nil
                    :north :north
                    :west nil
                    :north nil
                    :west :west :west
                    :south nil nil nil
                    :east nil nil
                    :south nil
                    :west
                    :north nil nil
                    :west nil nil nil nil nil
                    :north nil nil nil
                    :east :east :east :east :east :east
                    :south nil nil
                    :north nil nil
                    :east
                    :south :south :south :south
                    :west nil nil
                    :east nil nil
                    :south
                    :west nil nil
                    :east nil nil
                    :south
                    :west nil nil nil
                    :north nil nil nil
                    :west nil nil
                    :north nil nil nil
                    :south nil nil nil
                    :east nil nil nil
                    :north :north :north :north
                    :west nil nil nil nil nil nil nil
                    :north nil nil
                    :east :east :east
                    :south nil nil
                    :north nil nil
                    :west
                    :south nil nil
                    :north nil nil
                    :west :west :west :west
                    :south nil nil
                    :north nil nil
                    :west
                    :south nil nil
                    :north nil nil
                    :west :west
                    :south :south
                    :east nil nil nil
                    :south nil
                    :west nil nil nil
                    :south
                    :east nil nil nil
                    :west nil nil nil
                    :north :north
                    :east nil nil
                    :north nil nil
                    :east :east :east
                    :south nil nil nil
                    :west nil nil
                    :south nil
                    :east nil
                    :north nil
                    :east nil nil nil nil nil nil nil nil nil nil
                    :north :north
                    :west :west :west :west :west :west :west
                    :south nil nil nil
                    :north nil nil nil
                    :west
                    :south nil nil nil
                    :north nil nil nil
                    :east :east
                    :south nil nil nil
                    :west nil nil nil nil
                    :east nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil
                    :north :north
                    :west :west :west :west :west :west :west :west :west :west :west :west
                    :south :south :south :south
                    :east nil nil
                    :west nil nil
                    :south
                    :east nil nil
                    :west nil nil
                    :south
                    :east nil nil nil
                    :north nil nil nil
                    :south nil nil
                    :east nil
                    :west nil nil nil
                    :north :north :north :north :north
                    :east nil
                    :north nil
                    :east :east :east :east :east
                    :south nil nil nil nil
                    :west nil nil
                    :south nil
                    :north nil
                    :east nil nil nil nil nil nil nil nil
                    :north :north
                    :west :west :west :west :west :west
                    :south nil nil nil
                    :west nil nil
                    :east nil nil nil
                    :north nil
                    :east nil nil nil nil
                    :north :north
                    :west :west :west :west :west :west
                    :south nil nil nil nil
                    :west nil nil nil
                    :south nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil nil
                    :north :north
                    :west :west :west :west :west :west :west :west :west :west :west :west
                    :south :south :south :south :south :south :south
                    :east nil nil
                    :north nil nil nil nil
                    :east nil nil
                    :south nil nil
                    :east nil
                    :west nil nil
                    :north nil nil nil
                    :east nil nil
                    :south nil nil
                    :east)
              (list (make-instance 'pulled :x 3  :y 3 :z 0 :north t :south t :east t :west t) ; Top line
                    (make-instance 'pulled :x 4  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 3  :y 4 :z 0 :north t :south t :east t :west t) ; Second line
                    (make-instance 'toggle :x 4  :y 4 :z 0)
                    (make-instance 'pulled :x 5  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'toggle :x 10 :y 4 :z 0)
                    (make-instance 'pulled :x 11 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 3  :y 5 :z 0 :north t :south t :east t :west t) ; Third line
                    (make-instance 'pulled :x 4  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 6 :z 0 :north t :south t :east t :west t) ; Fourth line
                    (make-instance 'pulled :x 5  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 7 :z 0 :north t :south t :east t :west t) ; Fifth line
                    (make-instance 'pulled :x 6  :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'exit   :x 7  :y 7 :z 0)
                    (make-instance 'pulled :x 8  :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 8 :z 0 :north t :south t :east t :west t) ; Mirror fourth line
                    (make-instance 'pulled :x 5  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 3  :y 9 :z 0 :north t :south t :east t :west t) ; Mirrored third line
                    (make-instance 'pulled :x 4  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 3  :y 10 :z 0 :north t :south t :east t :west t) ; Mirrored second line
                    (make-instance 'toggle :x 4  :y 10 :z 0)
                    (make-instance 'pulled :x 5  :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'toggle :x 10 :y 10 :z 0)
                    (make-instance 'pulled :x 11 :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 3  :y 11 :z 0 :north t :south t :east t :west t) ; Mirrored top line
                    (make-instance 'pulled :x 4  :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'player :x 7  :y 14 :z 0)
                    (make-instance 'stepper :x 1  :y 1 :z -1) ; Top row
                    (make-instance 'stepper :x 2  :y 1 :z -1)
                    (make-instance 'stepper :x 3  :y 1 :z -1)
                    (make-instance 'stepper :x 4  :y 1 :z -1)
                    (make-instance 'stepper :x 5  :y 1 :z -1)
                    (make-instance 'stepper :x 6  :y 1 :z -1)
                    (make-instance 'stepper :x 7  :y 1 :z -1)
                    (make-instance 'stepper :x 8  :y 1 :z -1)
                    (make-instance 'stepper :x 9  :y 1 :z -1)
                    (make-instance 'stepper :x 10 :y 1 :z -1)
                    (make-instance 'stepper :x 11 :y 1 :z -1)
                    (make-instance 'stepper :x 12 :y 1 :z -1)
                    (make-instance 'stepper :x 13 :y 1 :z -1)
                    (make-instance 'stepper :x 1  :y 2 :z -1) ; West column
                    (make-instance 'stepper :x 1  :y 3 :z -1)
                    (make-instance 'stepper :x 1  :y 4 :z -1)
                    (make-instance 'stepper :x 1  :y 5 :z -1)
                    (make-instance 'stepper :x 1  :y 6 :z -1)
                    (make-instance 'stepper :x 1  :y 7 :z -1)
                    (make-instance 'stepper :x 1  :y 8 :z -1)
                    (make-instance 'stepper :x 1  :y 9 :z -1)
                    (make-instance 'stepper :x 1  :y 10 :z -1)
                    (make-instance 'stepper :x 1  :y 11 :z -1)
                    (make-instance 'stepper :x 1  :y 12 :z -1)
                    (make-instance 'stepper :x 1  :y 13 :z -1) ; Bottom row
                    (make-instance 'stepper :x 2  :y 13 :z -1)
                    (make-instance 'stepper :x 3  :y 13 :z -1)
                    (make-instance 'stepper :x 4  :y 13 :z -1)
                    (make-instance 'stepper :x 5  :y 13 :z -1)
                    (make-instance 'stepper :x 6  :y 13 :z -1)
                    (make-instance 'stepper :x 7  :y 13 :z -1)
                    (make-instance 'stepper :x 8  :y 13 :z -1)
                    (make-instance 'stepper :x 9  :y 13 :z -1)
                    (make-instance 'stepper :x 10 :y 13 :z -1)
                    (make-instance 'stepper :x 11 :y 13 :z -1)
                    (make-instance 'stepper :x 12 :y 13 :z -1)
                    (make-instance 'stepper :x 13 :y 13 :z -1)
                    (make-instance 'stepper :x 13 :y 2 :z -1) ; East column
                    (make-instance 'stepper :x 13  :y 3 :z -1)
                    (make-instance 'stepper :x 13  :y 4 :z -1)
                    (make-instance 'stepper :x 13  :y 5 :z -1)
                    (make-instance 'stepper :x 13  :y 6 :z -1)
                    (make-instance 'stepper :x 13  :y 7 :z -1)
                    (make-instance 'stepper :x 13  :y 8 :z -1)
                    (make-instance 'stepper :x 13  :y 9 :z -1)
                    (make-instance 'stepper :x 13  :y 10 :z -1)
                    (make-instance 'stepper :x 13  :y 11 :z -1)
                    (make-instance 'stepper :x 13  :y 12 :z -1))))
    (22 (list (list nil nil
                    :west nil nil nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil
                    :west nil nil nil nil nil nil nil
                    :east nil nil nil nil nil nil nil
                    :north nil nil nil
                    :west nil nil nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil
                    :north nil nil
                    :west nil
                    :east nil nil nil nil nil nil
                    :north nil nil nil nil
                    :west nil nil nil
                    :south nil nil nil nil nil nil nil
                    :south nil
                    :north nil nil nil nil nil nil nil
                    :east nil nil nil
                    :south nil nil nil nil nil nil nil
                    :west nil nil nil
                    :north nil nil nil nil nil
                    :west nil nil nil
                    :south nil nil nil
                    :west nil nil
                    :north nil nil
                    :west nil nil
                    :east nil nil nil nil nil
                    :north nil nil
                    :west nil nil nil
                    :south nil nil nil
                    :west nil nil
                    :north nil nil
                    :west nil nil nil
                    :east nil nil nil nil
                    :south nil nil
                    :west nil nil
                    :east nil nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil nil
                    :north nil nil nil nil nil
                    :west nil nil nil
                    :south nil nil nil
                    :west nil nil nil
                    :north nil nil
                    :west nil nil nil nil nil nil nil nil)
              (list (make-instance 'pulled :x 2  :y 2 :z 0 :north t :south t :east t :west t) ; Top pulled line
                    (make-instance 'pulled :x 3  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 2 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 3 :z 0 :north t :south t :east t :west t) ; Second pulled line
                    (make-instance 'pulled :x 3  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 4 :z 0 :north t :south t :east t :west t) ; Third pulled line
                    (make-instance 'pulled :x 3  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 5 :z 0 :north t :south t :east t :west t) ; Fourth pulled line
                    (make-instance 'pulled :x 3  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 6 :z 0 :north t :south t :east t :west t) ; Fifth pulled line (3 empty)
                    (make-instance 'pulled :x 3  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 6 :z 0 :north t :south t :east t :west t)
                    ;; Three empty
                    (make-instance 'pulled :x 11 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 7 :z 0 :north t :south t :east t :west t) ; Sixth pulled line (5 empty)
                    (make-instance 'pulled :x 3  :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 7 :z 0 :north t :south t :east t :west t)
                    ;; Five empty
                    (make-instance 'pulled :x 12 :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 8 :z 0 :north t :south t :east t :west t) ; Seventh pulled line (7 empty)
                    (make-instance 'pulled :x 3  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 8 :z 0 :north t :south t :east t :west t)
                    ;; Seven empty
                    (make-instance 'pulled :x 13 :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 9 :z 0 :north t :south t :east t :west t) ; Eight pulled line (7 empty) player's line
                    (make-instance 'pulled :x 3  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 9 :z 0 :north t :south t :east t :west t)
                    ;; Seven empty
                    (make-instance 'pulled :x 13 :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 10 :z 0 :north t :south t :east t :west t) ; Ninth pulled line (7 empty)
                    (make-instance 'pulled :x 3  :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 10 :z 0 :north t :south t :east t :west t)
                    ;; Seven empty
                    (make-instance 'pulled :x 13 :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 10 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 11 :z 0 :north t :south t :east t :west t) ; Tenth pulled line (5 empty)
                    (make-instance 'pulled :x 3  :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 11 :z 0 :north t :south t :east t :west t)
                    ;; Five empty
                    (make-instance 'pulled :x 12 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 11 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 12 :z 0 :north t :south t :east t :west t) ; Eleventh pulled line (3 empty)
                    (make-instance 'pulled :x 3  :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 12 :z 0 :north t :south t :east t :west t)
                    ;; Three empty
                    (make-instance 'pulled :x 11 :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 12 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 13 :z 0 :north t :south t :east t :west t) ; Third to last pulled line
                    (make-instance 'pulled :x 3  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 13 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 14 :z 0 :north t :south t :east t :west t) ; Second to last pulled line
                    (make-instance 'pulled :x 3  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 14 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 2  :y 15 :z 0 :north t :south t :east t :west t) ; Last pulled line
                    (make-instance 'pulled :x 3  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 4  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 5  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9  :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 11 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 12 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 13 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 14 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 15 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 16 :y 15 :z 0 :north t :south t :east t :west t)
                    (make-instance 'exit   :x 9  :y 0 :z 0)
                    (make-instance 'exit   :x 0  :y 9 :z 0)
                    (make-instance 'exit   :x 18 :y 9 :z 0)
                    (make-instance 'exit   :x 9  :y 18 :z 0)
                    (make-instance 'player :x 9  :y 9 :z 0))))
    (23 (list (list nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil nil nil nil nil
                    :south nil nil nil nil nil nil
                    :north nil nil nil nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil nil
                    :east nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :west nil nil nil nil nil
                    :east nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :south nil
                    :north nil nil
                    :west nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil
                    :west nil nil nil nil nil nil nil nil nil
                    :south nil nil nil nil nil nil nil
                    :east nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :south nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil nil nil nil nil nil
                    :south nil nil nil nil nil nil nil
                    :east nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil nil nil nil nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :south nil nil
                    :north nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil
                    :west nil nil nil nil nil nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil
                    :north nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil
                    :west nil nil nil nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil nil
                    :north nil nil nil nil nil
                    :west nil nil nil nil nil nil nil nil nil nil
                    :north nil nil
                    :east nil nil nil nil nil nil nil
                    :south nil nil nil
                    :east nil nil nil
                    :west nil nil nil nil nil
                    :north nil nil nil
                    :east nil nil nil
                    :south nil nil nil
                    :east nil nil nil nil
                    :south nil nil nil nil nil
                    :west nil nil nil nil)
              (list (make-instance 'wall   :x 0  :y 0 :z 0) ; Top row
                    (make-instance 'wall   :x 1  :y 0 :z 0)
                    (make-instance 'wall   :x 2  :y 0 :z 0)
                    (make-instance 'wall   :x 3  :y 0 :z 0)
                    (make-instance 'wall   :x 4  :y 0 :z 0)
                    (make-instance 'wall   :x 5  :y 0 :z 0)
                    (make-instance 'wall   :x 6  :y 0 :z 0)
                    (make-instance 'wall   :x 7  :y 0 :z 0)
                    (make-instance 'wall   :x 8  :y 0 :z 0)
                    (make-instance 'wall   :x 0  :y 1 :z 0) ; Second row
                    (make-instance 'player :x 1  :y 1 :z 0)
                    (make-instance 'wall   :x 8  :y 1 :z 0)
                    (make-instance 'wall   :x 9  :y 1 :z 0)
                    (make-instance 'wall   :x 10 :y 1 :z 0)
                    (make-instance 'wall   :x 11 :y 1 :z 0)
                    (make-instance 'wall   :x 12 :y 1 :z 0)
                    (make-instance 'wall   :x 13 :y 1 :z 0)
                    (make-instance 'wall   :x 14 :y 1 :z 0)
                    (make-instance 'wall   :x 15 :y 1 :z 0)
                    (make-instance 'wall   :x 0  :y 2 :z 0) ; Third row
                    (make-instance 'wall   :x 15 :y 2 :z 0)
                    (make-instance 'wall   :x 0  :y 3 :z 0) ; Fourth row
                    (make-instance 'pulled :x 14 :y 3 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 15 :y 3 :z 0)
                    (make-instance 'wall   :x 0  :y 4 :z 0) ; Fifth row
                    (make-instance 'wall   :x 3  :y 4 :z 0)
                    (make-instance 'wall   :x 4  :y 4 :z 0)
                    (make-instance 'pulled :x 5  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7  :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 8  :y 4 :z 0)
                    (make-instance 'wall   :x 9  :y 4 :z 0)
                    (make-instance 'pulled :x 10 :y 4 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 11  :y 4 :z 0)
                    (make-instance 'wall   :x 12  :y 4 :z 0)
                    (make-instance 'wall   :x 15  :y 4 :z 0)
                    (make-instance 'wall   :x 0   :y 5 :z 0) ; Sixth row
                    (make-instance 'wall   :x 3   :y 5 :z 0)
                    (make-instance 'wall   :x 4   :y 5 :z 0)
                    (make-instance 'pulled :x 5   :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6   :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 7   :y 5 :z 0)
                    (make-instance 'wall   :x 8   :y 5 :z 0)
                    (make-instance 'wall   :x 9   :y 5 :z 0)
                    (make-instance 'pulled :x 10  :y 5 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 11   :y 5 :z 0)
                    (make-instance 'wall   :x 12   :y 5 :z 0)
                    (make-instance 'wall   :x 15   :y 5 :z 0)
                    (make-instance 'wall   :x 0    :y 6 :z 0) ; Seventh row
                    (make-instance 'wall   :x 3    :y 6 :z 0)
                    (make-instance 'wall   :x 4    :y 6 :z 0)
                    (make-instance 'pulled :x 5    :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6    :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 7    :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 8    :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 9    :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 10   :y 6 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 11   :y 6 :z 0)
                    (make-instance 'wall   :x 12   :y 6 :z 0)
                    (make-instance 'wall   :x 15   :y 6 :z 0)
                    (make-instance 'wall   :x 0    :y 7 :z 0) ; Eight row
                    (make-instance 'wall   :x 3    :y 7 :z 0)
                    (make-instance 'wall   :x 4    :y 7 :z 0)
                    (make-instance 'pulled :x 5    :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6    :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'exit   :x 8    :y 7 :z 0)
                    (make-instance 'pulled :x 10   :y 7 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 11   :y 7 :z 0)
                    (make-instance 'wall   :x 12   :y 7 :z 0)
                    (make-instance 'wall   :x 15   :y 7 :z 0)
                    (make-instance 'wall   :x 0    :y 8 :z 0) ; Ninth row
                    (make-instance 'wall   :x 3    :y 8 :z 0)
                    (make-instance 'wall   :x 4    :y 8 :z 0)
                    (make-instance 'pulled :x 5    :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'pulled :x 6    :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 7    :y 8 :z 0)
                    (make-instance 'wall   :x 8    :y 8 :z 0)
                    (make-instance 'wall   :x 9    :y 8 :z 0)
                    (make-instance 'pulled :x 10   :y 8 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 11   :y 8 :z 0)
                    (make-instance 'wall   :x 12   :y 8 :z 0)
                    (make-instance 'wall   :x 15   :y 8 :z 0)
                    (make-instance 'wall   :x 0    :y 9 :z 0) ; Tenth row
                    (make-instance 'pulled :x 14   :y 9 :z 0 :north t :south t :east t :west t)
                    (make-instance 'wall   :x 15   :y 9 :z 0)
                    (make-instance 'wall   :x 0    :y 10 :z 0) ; Eleventh row
                    (make-instance 'wall   :x 1    :y 10 :z 0)
                    (make-instance 'wall   :x 15   :y 10 :z 0)
                    (make-instance 'wall   :x 1    :y 11 :z 0) ; Last row
                    (make-instance 'wall   :x 2    :y 11 :z 0)
                    (make-instance 'wall   :x 3    :y 11 :z 0)
                    (make-instance 'wall   :x 4    :y 11 :z 0)
                    (make-instance 'wall   :x 5    :y 11 :z 0)
                    (make-instance 'wall   :x 6    :y 11 :z 0)
                    (make-instance 'wall   :x 7    :y 11 :z 0)
                    (make-instance 'wall   :x 8    :y 11 :z 0)
                    (make-instance 'wall   :x 9    :y 11 :z 0)
                    (make-instance 'wall   :x 10   :y 11 :z 0)
                    (make-instance 'wall   :x 11   :y 11 :z 0)
                    (make-instance 'wall   :x 12   :y 11 :z 0)
                    (make-instance 'wall   :x 13   :y 11 :z 0)
                    (make-instance 'wall   :x 14   :y 11 :z 0)
                    (make-instance 'wall   :x 15   :y 11 :z 0))))))

