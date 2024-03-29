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

(defun level-get-nth (index)
  (elt *levels* index))

(defun level-get-par (level)
  (1- (count-if-not #'null (caddr level))))

(defun current-par ()
  (- *level-par* *level-steps*))

(defparameter *levels* (make-array 0 :adjustable t :fill-pointer t))

(vector-push-extend
 '(list (list
         "Introducing Exit"
         "Go To the Exit")
   (list
    nil nil
    :east nil nil nil nil nil nil nil)
   (list
    (make-player       4  4)
    (make-exit         10 4)))
 *levels*)

(vector-push-extend
 '(list (list
         "Introducing Keys"
         "Collect All the Keys")
   (list
    nil nil
    :west nil nil nil
    :east nil nil nil nil nil nil)
   (list
    (make-wall         0 4)
    (make-key          1 4)
    (make-player       4 4)
    (make-key          6 4)
    (make-exit         8 4)))
 *levels*)

(vector-push-extend
 '(list (list
         "Widdershins"
         "The Direction Matters")
   (list nil nil
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
   (list
    (make-wall          5  0)
    (make-block-counter 7  0  1)
    (make-block-counter 1  1  1)
    (make-block-counter 9  1  1)
    (make-wall          4  2)
    (make-wall          6  2)
    (make-block-counter 0  3  1)
    (make-key           3  3)
    (make-wall          4  3)
    (make-wall          6  3)
    (make-key           7  3)
    (make-wall          2  4)
    (make-wall          3  4)
    (make-wall          4  4)
    (make-wall          6  4)
    (make-wall          7  4)
    (make-wall          8  4)
    (make-wall          0  5)
    (make-player        5  5)
    (make-wall          10 5)
    (make-wall          2  6)
    (make-wall          3  6)
    (make-wall          4  6)
    (make-wall          6  6)
    (make-wall          7  6)
    (make-wall          8  6)
    (make-key           3  7)
    (make-wall          4  7)
    (make-wall          6  7)
    (make-key           7  7)
    (make-block-counter 10 7  1)
    (make-wall          4  8)
    (make-wall          6  8)
    (make-block-counter 1  9  1)
    (make-block-counter 9  9  1)
    (make-exit          1 10)
    (make-block-counter 3 10  1)
    (make-wall          5 10)))
 *levels*)

(vector-push-extend
 '(list (list
         "Introducing Pushed Crates"
         "Push them around")
   (list nil nil
    :south nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil nil nil nil nil nil
    :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    nil nil nil
    :east nil nil nil nil nil nil nil
    :south nil nil nil nil nil nil nil nil nil nil nil)
   (list
    (make-player       6  8)
    (make-wall         2  2)
    (make-slope-es     6  0)
    (make-slope-ws     14 0)
    (make-slope-wn     14 10)
    (make-slope-en     6  10)
    (make-pushed       6  4)
    (make-exit         5  10)))
 *levels*)

(vector-push-extend
 '(list (list
         "Oh, it\'s just a merry-go-round"
         "Remember To Take a Spin")
   (list nil nil
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
   (list
    (make-slope-es      6  0)
    (make-slope-ws      11 0)
    (make-block-timer   2  2  18)
    (make-exit          0  3)
    (make-turnstile-s   3  3)
    (make-player        6  3)
    (make-pass-counter  11 3  2)
    (make-wall          9  4)
    (make-wall          4  5)
    (make-wall          1  6)
    (make-wall          8  6)
    (make-key           3  7)
    (make-wall          3  8)
    (make-slope-en      6  8)
    (make-slope-wn      11 8)
    (make-wall          5  9)))
 *levels*)

(vector-push-extend
 '(list (list
         "Busy Valentine's Day"
         "The Ways Of the Heart")
   (list nil nil
    :east nil nil nil nil nil nil
    :north nil nil nil nil nil nil nil
    :east nil nil
    :north nil nil nil
    :west nil nil nil nil
    :south nil nil nil nil
    :west nil nil nil nil
    :north nil nil nil nil nil nil nil
    :east nil nil
    :north nil nil
    :east nil nil nil nil nil
    :south nil nil nil
    :west nil nil
    :south nil nil nil nil
    :west nil nil nil
    :south nil nil
    :west nil nil
    :south nil nil
    :west nil nil nil
    :north nil nil
    :west nil nil nil
    :north nil nil
    :west nil
    :north nil nil nil nil nil nil
    :east nil nil nil nil
    :south nil nil
    :east nil nil nil
    :south nil nil nil nil nil
    :north nil nil nil nil
    :east nil nil nil nil nil nil
    :north nil
    :west nil nil nil
    :north nil nil nil nil nil nil nil nil
    :south nil nil nil nil nil nil
    :west nil nil nil
    :north nil nil nil nil
    :west nil nil nil nil nil
    :east nil nil
    :south nil nil
    :east nil nil
    :south nil nil
    :east nil nil nil nil nil
    :north nil nil nil
    :east nil nil
    :north nil nil
    :east nil nil nil nil nil nil nil
    :east nil nil nil nil nil)
   (list
    (make-wall        1  0)
    (make-wall        6  1)
    (make-key         7  1)
    (make-slope-ws    10 1)
    (make-wall        13 2)
    (make-wall        3  3)
    (make-wall        4  3)
    (make-wall        8  3)
    (make-wall        9  3)
    (make-wall        2  4)
    (make-key         4  4)
    (make-turnstile-e 5  4)
    (make-turnstile-s 7  4)
    (make-key         8  4)
    (make-wall        10 4)
    (make-slope-es    0  5)
    (make-turnstile-w 2  5)
    (make-wall        6  5)
    (make-turnstile-e 10 5)
    (make-turnstile-e 12 5)
    (make-block-timer 14 5  18)
    (make-wall        2  6)
    (make-wall        10 6)
    (make-exit        16 6)
    (make-wall        2  7)
    (make-wall        10 7)
    (make-wall        0  8)
    (make-wall        3  8)
    (make-wall        9  8)
    (make-slope-wn    14 8)
    (make-wall        4  9)
    (make-wall        8  9)
    (make-wall        1  10)
    (make-wall        5  10)
    (make-wall        7  10)
    (make-wall        11 10)
    (make-turnstile-n 6  11)
    (make-wall        9  12)
    (make-wall        3  13)
    (make-wall        7  14)
    (make-player      0  14)))
 *levels*)

(vector-push-extend
 '(list (list
         "Airlock"
         "Just Follow the Sequence")
   (list nil nil
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
   (list
    (make-wall          0  0)
    (make-wall          1  0)
    (make-wall          2  0)
    (make-wall          3  0)
    (make-wall          4  0)
    (make-wall          5  0)
    (make-wall          6  0)
    (make-wall          7  0)
    (make-wall          8  0)
    (make-wall          9  0)
    (make-wall          10 0)
    (make-wall          11 0)
    (make-wall          12 0)
    (make-wall          13 0)
    (make-wall          14 0)
    (make-wall          15 0)
    (make-wall          16 0)
    (make-wall          17 0)
    (make-wall          18 0)
    (make-wall          0  1)
    (make-block-counter 6  1  1)
    (make-block-counter 10 1  1)
    (make-block-counter 14 1  1)
    (make-key           17 1)
    (make-wall          18 1)
    (make-wall          0  2)
    (make-wall          2  2)
    (make-wall          3  2)
    (make-wall          6  2)
    (make-wall          7  2)
    (make-wall          10 2)
    (make-wall          11 2)
    (make-wall          14 2)
    (make-wall          15 2)
    (make-wall          16 2)
    (make-turnstile-s   17 2)
    (make-wall          18 2)
    (make-wall          0  3)
    (make-pulled-ns     5  3)
    (make-pulled-ns     9  3)
    (make-pulled-ns     13 3)
    (make-wall          18 3)
    (make-wall          0  4)
    (make-wall          1  4)
    (make-wall          2  4)
    (make-wall          3  4)
    (make-wall          4  4)
    (make-wall          6  4)
    (make-wall          7  4)
    (make-wall          8  4)
    (make-wall          10 4)
    (make-wall          11 4)
    (make-wall          12 4)
    (make-wall          14 4)
    (make-wall          15 4)
    (make-wall          16 4)
    (make-wall          18 4)
    (make-exit          0  5)
    (make-player        2  5)
    (make-slope-ws      6  5)
    (make-slope-ws      10 5)
    (make-slope-ws      14 5)
    (make-wall          18 5)
    (make-wall          0  6)
    (make-wall          4  6)
    (make-wall          8  6)
    (make-wall          12 6)
    (make-wall          18 6)
    (make-wall          0 7)
    (make-wall          1 7)
    (make-wall          2 7)
    (make-wall          3 7)
    (make-wall          4 7)
    (make-pulled-n      5 7)
    (make-wall          6 7)
    (make-wall          7 7)
    (make-wall          8 7)
    (make-pulled-n      9 7)
    (make-wall          10 7)
    (make-wall          11 7)
    (make-wall          12 7)
    (make-pulled-n      13 7)
    (make-wall          14 7)
    (make-wall          15 7)
    (make-wall          16 7)
    (make-wall          17 7)
    (make-wall          18 7)))
 *levels*)

(vector-push-extend
 '(list (list
         "Introducing Toggles"
         "Remember To Turn On All the Toggles")
   (list nil nil
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
   (list
    (make-pulled-ensw  3  3)          ; Top line
    (make-pulled-ensw  4  3)
    (make-pulled-ensw  5  3)
    (make-pulled-ensw  9  3)
    (make-pulled-ensw  10 3)
    (make-pulled-ensw  11 3)
    (make-pulled-ensw  3  4)          ; Second line
    (make-toggle       4  4)
    (make-pulled-ensw  5  4)
    (make-pulled-ensw  6  4)
    (make-pulled-ensw  8  4)
    (make-pulled-ensw  9  4)
    (make-toggle       10 4)
    (make-pulled-ensw  11 4)
    (make-pulled-ensw  3  5)          ; Third line
    (make-pulled-ensw  4  5)
    (make-pulled-ensw  5  5)
    (make-pulled-ensw  6  5)
    (make-pulled-ensw  7  5)
    (make-pulled-ensw  8  5)
    (make-pulled-ensw  9  5)
    (make-pulled-ensw  10 5)
    (make-pulled-ensw  11 5)
    (make-pulled-ensw  4  6)          ; Fourth line
    (make-pulled-ensw  5  6)
    (make-pulled-ensw  6  6)
    (make-pulled-ensw  7  6)
    (make-pulled-ensw  8  6)
    (make-pulled-ensw  9  6)
    (make-pulled-ensw  10 6)
    (make-pulled-ensw  5  7)          ; Fifth line
    (make-pulled-ensw  6  7)
    (make-exit         7  7)
    (make-pulled-ensw  8  7)
    (make-pulled-ensw  9  7)
    (make-pulled-ensw  4  8)          ; Mirror fourth line
    (make-pulled-ensw  5  8)
    (make-pulled-ensw  6  8)
    (make-pulled-ensw  7  8)
    (make-pulled-ensw  8  8)
    (make-pulled-ensw  9  8)
    (make-pulled-ensw  10 8)
    (make-pulled-ensw  3  9)          ; Mirrored third line
    (make-pulled-ensw  4  9)
    (make-pulled-ensw  5  9)
    (make-pulled-ensw  6  9)
    (make-pulled-ensw  7  9)
    (make-pulled-ensw  8  9)
    (make-pulled-ensw  9  9)
    (make-pulled-ensw  10 9)
    (make-pulled-ensw  11 9)
    (make-pulled-ensw  3  10)         ; Mirrored second line
    (make-toggle       4  10)
    (make-pulled-ensw  5  10)
    (make-pulled-ensw  6  10)
    (make-pulled-ensw  8  10)
    (make-pulled-ensw  9  10)
    (make-toggle       10 10)
    (make-pulled-ensw  11 10)
    (make-pulled-ensw  3  11)         ; Mirrored top line
    (make-pulled-ensw  4  11)
    (make-pulled-ensw  5  11)
    (make-pulled-ensw  9  11)
    (make-pulled-ensw  10 11)
    (make-pulled-ensw  11 11)
    (make-player       7  14)
    (make-stepper      1  1)          ; Top row
    (make-stepper      2  1)
    (make-stepper      3  1)
    (make-stepper      4  1)
    (make-stepper      5  1)
    (make-stepper      6  1)
    (make-stepper      7  1)
    (make-stepper      8  1)
    (make-stepper      9  1)
    (make-stepper      10 1)
    (make-stepper      11 1)
    (make-stepper      12 1)
    (make-stepper      13 1)
    (make-stepper      1  2)          ; West column
    (make-stepper      1  3)
    (make-stepper      1  4)
    (make-stepper      1  5)
    (make-stepper      1  6)
    (make-stepper      1  7)
    (make-stepper      1  8)
    (make-stepper      1  9)
    (make-stepper      1  10)
    (make-stepper      1  11)
    (make-stepper      1  12)
    (make-stepper      1  13)         ; Bottom row
    (make-stepper      2  13)
    (make-stepper      3  13)
    (make-stepper      4  13)
    (make-stepper      5  13)
    (make-stepper      6  13)
    (make-stepper      7  13)
    (make-stepper      8  13)
    (make-stepper      9  13)
    (make-stepper      10 13)
    (make-stepper      11 13)
    (make-stepper      12 13)
    (make-stepper      13 13)
    (make-stepper      13 2)          ; East column
    (make-stepper      13 3)
    (make-stepper      13 4)
    (make-stepper      13 5)
    (make-stepper      13 6)
    (make-stepper      13 7)
    (make-stepper      13 8)
    (make-stepper      13 9)
    (make-stepper      13 10)
    (make-stepper      13 11)
    (make-stepper      13 12)))
 *levels*)

(vector-push-extend
 '(list (list
         "Oh, All Those Boxes To Pull"
         "Try Not To Get Stuck")
   (list nil nil
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
   (list
    (make-pulled-ensw  2   2)         ; Top pulled line
    (make-pulled-ensw  3   2)
    (make-pulled-ensw  4   2)
    (make-pulled-ensw  5   2)
    (make-pulled-ensw  6   2)
    (make-pulled-ensw  7   2)
    (make-pulled-ensw  8   2)
    (make-pulled-ensw  9   2)
    (make-pulled-ensw  10  2)
    (make-pulled-ensw  11  2)
    (make-pulled-ensw  12  2)
    (make-pulled-ensw  13  2)
    (make-pulled-ensw  14  2)
    (make-pulled-ensw  15  2)
    (make-pulled-ensw  16  2)
    (make-pulled-ensw  2   3)         ; Second pulled line
    (make-pulled-ensw  3   3)
    (make-pulled-ensw  4   3)
    (make-pulled-ensw  5   3)
    (make-pulled-ensw  6   3)
    (make-pulled-ensw  7   3)
    (make-pulled-ensw  8   3)
    (make-pulled-ensw  9   3)
    (make-pulled-ensw  10  3)
    (make-pulled-ensw  11  3)
    (make-pulled-ensw  12  3)
    (make-pulled-ensw  13  3)
    (make-pulled-ensw  14  3)
    (make-pulled-ensw  15  3)
    (make-pulled-ensw  16  3)
    (make-pulled-ensw  2   4)         ; Third pulled line
    (make-pulled-ensw  3   4)
    (make-pulled-ensw  4   4)
    (make-pulled-ensw  5   4)
    (make-pulled-ensw  6   4)
    (make-pulled-ensw  7   4)
    (make-pulled-ensw  8   4)
    (make-pulled-ensw  9   4)
    (make-pulled-ensw  10  4)
    (make-pulled-ensw  11  4)
    (make-pulled-ensw  12  4)
    (make-pulled-ensw  13  4)
    (make-pulled-ensw  14  4)
    (make-pulled-ensw  15  4)
    (make-pulled-ensw  16  4)
    (make-pulled-ensw  2   5)         ; Fourth pulled line
    (make-pulled-ensw  3   5)
    (make-pulled-ensw  4   5)
    (make-pulled-ensw  5   5)
    (make-pulled-ensw  6   5)
    (make-pulled-ensw  7   5)
    (make-pulled-ensw  8   5)
    (make-pulled-ensw  9   5)
    (make-pulled-ensw  10  5)
    (make-pulled-ensw  11  5)
    (make-pulled-ensw  12  5)
    (make-pulled-ensw  13  5)
    (make-pulled-ensw  14  5)
    (make-pulled-ensw  15  5)
    (make-pulled-ensw  16  5)
    (make-pulled-ensw  2   6)         ; Fifth pulled line (3 empty)
    (make-pulled-ensw  3   6)
    (make-pulled-ensw  4   6)
    (make-pulled-ensw  5   6)
    (make-pulled-ensw  6   6)
    (make-pulled-ensw  7   6)
    ;; Three empty
    (make-pulled-ensw  11  6)
    (make-pulled-ensw  12  6)
    (make-pulled-ensw  13  6)
    (make-pulled-ensw  14  6)
    (make-pulled-ensw  15  6)
    (make-pulled-ensw  16  6)
    (make-pulled-ensw  2   7)         ; Sixth pulled line (5 empty)
    (make-pulled-ensw  3   7)
    (make-pulled-ensw  4   7)
    (make-pulled-ensw  5   7)
    (make-pulled-ensw  6   7)
    ;; Five empty
    (make-pulled-ensw  12  7)
    (make-pulled-ensw  13  7)
    (make-pulled-ensw  14  7)
    (make-pulled-ensw  15  7)
    (make-pulled-ensw  16  7)
    (make-pulled-ensw  2   8)         ; Seventh pulled line (7 empty)
    (make-pulled-ensw  3   8)
    (make-pulled-ensw  4   8)
    (make-pulled-ensw  5   8)
    ;; Seven empty
    (make-pulled-ensw  13  8)
    (make-pulled-ensw  14  8)
    (make-pulled-ensw  15  8)
    (make-pulled-ensw  16  8)
    (make-pulled-ensw  2   9)    ; Eight pulled line (7 empty) player's line
    (make-pulled-ensw  3   9)
    (make-pulled-ensw  4   9)
    (make-pulled-ensw  5   9)
    ;; Seven empty
    (make-pulled-ensw  13  9)
    (make-pulled-ensw  14  9)
    (make-pulled-ensw  15  9)
    (make-pulled-ensw  16  9)
    (make-pulled-ensw  2   10)        ; Ninth pulled line (7 empty)
    (make-pulled-ensw  3   10)
    (make-pulled-ensw  4   10)
    (make-pulled-ensw  5   10)
    ;; Seven empty
    (make-pulled-ensw  13  10)
    (make-pulled-ensw  14  10)
    (make-pulled-ensw  15  10)
    (make-pulled-ensw  16  10)
    (make-pulled-ensw  2   11)        ; Tenth pulled line (5 empty)
    (make-pulled-ensw  3   11)
    (make-pulled-ensw  4   11)
    (make-pulled-ensw  5   11)
    (make-pulled-ensw  6   11)
    ;; Five empty
    (make-pulled-ensw  12  11)
    (make-pulled-ensw  13  11)
    (make-pulled-ensw  14  11)
    (make-pulled-ensw  15  11)
    (make-pulled-ensw  16  11)
    (make-pulled-ensw  2   12)        ; Eleventh pulled line (3 empty)
    (make-pulled-ensw  3   12)
    (make-pulled-ensw  4   12)
    (make-pulled-ensw  5   12)
    (make-pulled-ensw  6   12)
    (make-pulled-ensw  7   12)
    ;; Three empty
    (make-pulled-ensw  11  12)
    (make-pulled-ensw  12  12)
    (make-pulled-ensw  13  12)
    (make-pulled-ensw  14  12)
    (make-pulled-ensw  15  12)
    (make-pulled-ensw  16  12)
    (make-pulled-ensw  2   13)        ; Fourth to last pulled line
    (make-pulled-ensw  3   13)
    (make-pulled-ensw  4   13)
    (make-pulled-ensw  5   13)
    (make-pulled-ensw  6   13)
    (make-pulled-ensw  7   13)
    (make-pulled-ensw  8   13)
    (make-pulled-ensw  9   13)
    (make-pulled-ensw  10  13)
    (make-pulled-ensw  11  13)
    (make-pulled-ensw  12  13)
    (make-pulled-ensw  13  13)
    (make-pulled-ensw  14  13)
    (make-pulled-ensw  15  13)
    (make-pulled-ensw  16  13)
    (make-pulled-ensw  2   14)        ; Third to last pulled line
    (make-pulled-ensw  3   14)
    (make-pulled-ensw  4   14)
    (make-pulled-ensw  5   14)
    (make-pulled-ensw  6   14)
    (make-pulled-ensw  7   14)
    (make-pulled-ensw  8   14)
    (make-pulled-ensw  9   14)
    (make-pulled-ensw  10  14)
    (make-pulled-ensw  11  14)
    (make-pulled-ensw  12  14)
    (make-pulled-ensw  13  14)
    (make-pulled-ensw  14  14)
    (make-pulled-ensw  15  14)
    (make-pulled-ensw  16  14)
    (make-pulled-ensw  2   15)        ; Second to Last pulled line
    (make-pulled-ensw  3   15)
    (make-pulled-ensw  4   15)
    (make-pulled-ensw  5   15)
    (make-pulled-ensw  6   15)
    (make-pulled-ensw  7   15)
    (make-pulled-ensw  8   15)
    (make-pulled-ensw  9   15)
    (make-pulled-ensw  10  15)
    (make-pulled-ensw  11  15)
    (make-pulled-ensw  12  15)
    (make-pulled-ensw  13  15)
    (make-pulled-ensw  14  15)
    (make-pulled-ensw  15  15)
    (make-pulled-ensw  16  15)
    (make-pulled-ensw  2   16)        ; Last pulled line
    (make-pulled-ensw  3   16)
    (make-pulled-ensw  4   16)
    (make-pulled-ensw  5   16)
    (make-pulled-ensw  6   16)
    (make-pulled-ensw  7   16)
    (make-pulled-ensw  8   16)
    (make-pulled-ensw  9   16)
    (make-pulled-ensw  10  16)
    (make-pulled-ensw  11  16)
    (make-pulled-ensw  12  16)
    (make-pulled-ensw  13  16)
    (make-pulled-ensw  14  16)
    (make-pulled-ensw  15  16)
    (make-pulled-ensw  16  16)
    (make-exit         0   9)
    (make-exit         18  9)
    (make-player       9   9)))
 *levels*)

(vector-push-extend
 '(list (list
         "Level Name"
         "Level Hint")
   (list nil nil
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
   (list (make-wall        0  0)      ; Top row
    (make-wall        1  0)
    (make-wall        2  0)
    (make-wall        3  0)
    (make-wall        4  0)
    (make-wall        5  0)
    (make-wall        6  0)
    (make-wall        7  0)
    (make-wall        8  0)
    (make-wall        0  1)           ; Second row
    (make-player      1  1)
    (make-wall        8  1)
    (make-wall        9  1)
    (make-wall        10 1)
    (make-wall        11 1)
    (make-wall        12 1)
    (make-wall        13 1)
    (make-wall        14 1)
    (make-wall        15 1)
    (make-wall        0  2)           ; Third row
    (make-wall        15 2)
    (make-wall        0  3)           ; Fourth row
    (make-pulled-ensw 14 3)
    (make-wall        15 3)
    (make-wall        0  4)           ; Fifth row
    (make-wall        3  4)
    (make-wall        4  4)
    (make-pulled-ensw 5  4)
    (make-pulled-ensw 6  4)
    (make-pulled-ensw 7  4)
    (make-wall        8  4)
    (make-wall        9  4)
    (make-pulled-ensw 10 4)
    (make-wall        11 4)
    (make-wall        12 4)
    (make-wall        15 4)
    (make-wall        0  5)           ; Sixth row
    (make-wall        3  5)
    (make-wall        4  5)
    (make-pulled-ensw 5  5)
    (make-pulled-ensw 6  5)
    (make-wall        7  5)
    (make-wall        8  5)
    (make-wall        9  5)
    (make-pulled-ensw 10 5)
    (make-wall        11 5)
    (make-wall        12 5)
    (make-wall        15 5)
    (make-wall        0  6)           ; Seventh row
    (make-wall        3  6)
    (make-wall        4  6)
    (make-pulled-ensw 5  6)
    (make-pulled-ensw 6  6)
    (make-pulled-ensw 7  6)
    (make-pulled-ensw 8  6)
    (make-pulled-ensw 9  6)
    (make-pulled-ensw 10 6)
    (make-wall        11 6)
    (make-wall        12 6)
    (make-wall        15 6)
    (make-wall        0  7)           ; Eight row
    (make-wall        3  7)
    (make-wall        4  7)
    (make-pulled-ensw 5  7)
    (make-pulled-ensw 6  7)
    (make-exit        8  7)
    (make-pulled-ensw 10 7)
    (make-wall        11 7)
    (make-wall        12 7)
    (make-wall        15 7)
    (make-wall        0  8)           ; Ninth row
    (make-wall        3  8)
    (make-wall        4  8)
    (make-pulled-ensw 5  8)
    (make-pulled-ensw 6  8)
    (make-wall        7  8)
    (make-wall        8  8)
    (make-wall        9  8)
    (make-pulled-ensw 10 8)
    (make-wall        11 8)
    (make-wall        12 8)
    (make-wall        15 8)
    (make-wall        0  9)           ; Tenth row
    (make-pulled-ensw 14 9)
    (make-wall        15 9)
    (make-wall        0  10)          ; Eleventh row
    (make-wall        1  10)
    (make-wall        15 10)
    (make-wall        1  11)          ; Last row
    (make-wall        2  11)
    (make-wall        3  11)
    (make-wall        4  11)
    (make-wall        5  11)
    (make-wall        6  11)
    (make-wall        7  11)
    (make-wall        8  11)
    (make-wall        9  11)
    (make-wall        10 11)
    (make-wall        11 11)
    (make-wall        12 11)
    (make-wall        13 11)
    (make-wall        14 11)
    (make-wall        15 11)))
 *levels*)

(vector-push-extend
 '(list (list
         "Three Toggles In Walled Garden"
         "Toggle all the Toggles")
   (list
    :south nil
    :west  nil
    :west
    :north nil nil
    :south nil
    :east  nil
    :north nil
    :south nil nil
    :east  nil
    :east
    :east  nil
    :north
    :north nil
    :north nil nil
    :west  nil nil nil
    :north nil nil nil
    :east  nil nil nil nil nil
    :south nil
    :south nil nil
    :south nil nil nil nil nil nil nil nil
    :north nil nil
    :west  nil
    :east  nil
    :south
    :west  nil
    :west  nil
    :north nil nil
    :south nil nil nil
    :east  nil
    :east  nil
    :north
    :north
    :north nil
    :north nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil
    :south nil nil
    :east  nil
    :north nil
    :north nil nil
    :north nil nil
    :west  nil nil
    :east  nil nil nil nil nil
    :north nil
    :north nil
    :north nil
    :west
    :west  nil
    :west  nil
    :south nil nil nil
    :north nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :west  nil
    :south nil nil
    :north nil nil
    :east  nil nil
    :south nil nil
    :west  nil nil nil
    :south nil
    :north nil nil
    :east  nil nil nil nil
    :south
    :south nil
    :south
    :south
    :south
    :south nil nil
    :west
    :west
    :west  nil
    :west  nil nil
    :north nil nil nil nil
    :east  nil nil
    :south nil nil nil nil nil nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil nil
    :south nil nil nil nil
    :west
    :west
    :west
    :west  nil
    :west
    :west
    :north nil nil
    :east  nil nil nil
    :west  nil nil nil nil nil
    :south
    :east  nil
    :east
    :east
    :east
    :east  nil nil nil
    :north nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :east  nil nil
    :west  nil nil nil
    :south nil nil
    :east  nil nil
    :east
    :east
    :east
    :east  nil
    :east
    :north nil nil nil
    :north
    :north
    :north nil
    :north
    :north nil nil nil nil
    :west  nil nil nil nil nil nil nil nil nil
    :north nil nil nil
    :east  nil nil nil nil nil
    :west  nil nil nil nil nil
    :north
    :east  nil
    :east
    :east
    :east
    :east
    :east
    :east  nil nil
    :west  nil
    :south nil nil nil nil
    :west  nil
    :east  nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil nil nil nil
    :east  nil nil
    :south nil nil
    :west  nil nil nil
    :east  nil nil nil
    :north nil nil nil nil nil
    :west
    :west  nil
    :west  nil nil
    :south nil nil nil nil nil nil
    :west  nil
    :east  nil nil nil
    :north nil nil nil
    :east  nil nil nil nil
    :north
    :north nil
    :north
    :west  nil nil nil nil
    :west
    :west
    :west  nil
    :west
    :west  nil
    :west  nil nil
    :east  nil
    :south nil nil
    :north nil nil nil nil nil nil nil
    :east  nil
    :east
    :east  nil nil
    :east  nil
    :east
    :east
    :east  nil
    :south nil
    :west  nil nil
    :east  nil nil nil nil nil nil nil
    :north nil nil
    :west
    :west  nil
    :west
    :west
    :west  nil nil
    :west
    :west
    :west
    :west
    :west
    :west
    :south
    :south nil
    :south
    :east  nil nil
    :north nil
    :south nil nil nil nil
    :west  nil
    :north
    :north
    :north nil
    :north nil nil
    :north nil nil nil
    :east  nil nil nil nil nil nil nil
    :west  nil nil nil
    :north nil nil
    :east
    :east  nil nil nil
    :east
    :east  nil nil nil
    :south nil nil nil nil nil
    :west  nil nil
    :east  nil nil nil nil nil nil nil
    :north nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :west  nil nil
    :south nil nil nil nil nil nil nil nil
    :north nil nil nil
    :east  nil
    :east  nil nil nil nil nil nil nil
    :east
    :east  nil nil nil nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :south nil nil
    :north nil nil
    :east  nil nil
    :south nil nil nil nil nil nil nil nil nil
    :east  nil nil nil nil
    :north
    :north
    :north
    :north nil
    :north
    :north nil nil
    :west
    :west  nil
    :west  nil nil
    :south nil nil
    :north nil nil nil nil nil nil nil nil nil
    :east  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :east  nil
    :east  nil nil nil nil nil nil nil
    :south nil
    :south nil nil nil nil nil nil nil nil
    :south
    :south nil
    :south
    :south nil
    :south
    :south nil
    :west  nil
    :west
    :west
    :west  nil
    :west
    :west  nil nil
    :north nil nil nil nil nil nil
    :south nil nil nil nil nil nil nil nil
    :east  nil
    :north nil nil nil
    :west  nil nil nil nil
    :north nil nil nil nil nil nil nil
    :west  nil nil nil
    :west
    :west
    :west  nil
    :west  nil
    :south
    :south
    :south nil
    :east  nil
    :north nil nil
    :south nil
    :west  nil nil
    :north
    :north
    :north nil
    :east  nil nil nil
    :west  nil nil nil
    :north nil
    :east
    :east  nil
    :east  nil nil
    :south nil nil nil nil nil nil nil
    :west  nil
    :east  nil nil nil nil nil
    :north nil nil nil
    :west  nil
    :west
    :west
    :west
    :west
    :west  nil nil nil
    :south nil nil nil
    :north nil nil nil nil nil
    :east  nil nil nil nil
    :south nil nil nil nil nil
    :west  nil nil
    :south nil nil
    :east  nil nil nil nil nil nil nil
    :north nil nil
    :west
    :west  nil nil
    :south
    :south
    :south nil nil nil
    :south
    :south nil
    :south nil
    :east
    :east
    :east  nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :east  nil
    :east
    :east
    :east  nil
    :east  nil nil nil nil nil
    :west  nil
    :west  nil
    :west  nil
    :north nil nil nil
    :west  nil
    :east  nil nil nil nil
    :south
    :west  nil nil nil nil nil nil nil nil nil
    :south nil
    :east  nil
    :east  nil
    :south nil
    :east
    :east
    :east  nil nil
    :north nil nil nil
    :south nil nil
    :east  nil nil
    :east  nil nil
    :south nil nil nil)
   (list
    (make-wall        0  0)             ; 0
    (make-wall        1  0)
    (make-wall        2  0)
    (make-wall        3  0)
    (make-wall        4  0)
    (make-wall        5  0)
    (make-wall        6  0)
    (make-wall        7  0)
    (make-wall        8  0)
    (make-wall        9  0)
    (make-wall        10 0)
    (make-wall        11 0)
    (make-wall        12 0)
    (make-wall        0  1)             ; 1
    (make-stepper     1  1)
    (make-stepper     2  1)
    (make-stepper     3  1)
    (make-stepper     4  1)
    (make-stepper     5  1)
    (make-stepper     6  1)
    (make-stepper     7  1)
    (make-stepper     8  1)
    (make-stepper     9  1)
    (make-stepper     10 1)
    (make-stepper     11 1)
    (make-wall        12 1)
    (make-wall        0  2)             ; 2
    (make-stepper     1  2)
    (make-stepper     11 2)
    (make-wall        12 2)
    (make-wall        0  3)             ; 3
    (make-stepper     1  3)
    (make-stepper     11 3)
    (make-wall        12 3)
    (make-wall        0  4)             ; 4
    (make-stepper     1  4)
    (make-pulled-ensw 3  4)
    (make-pulled-ensw 4  4)
    (make-pulled-ensw 5  4)
    (make-pulled-ensw 6  4)
    (make-pulled-ensw 7  4)
    (make-pulled-ensw 8  4)
    (make-pulled-ensw 9  4)
    (make-stepper     11 4)
    (make-wall        12 4)
    (make-wall        0  5)             ; 5
    (make-stepper     1  5)
    (make-pulled-ensw 3  5)
    (make-toggle      4  5)
    (make-pulled-ensw 5  5)
    (make-toggle      6  5)
    (make-pulled-ensw 7  5)
    (make-toggle      8  5)
    (make-pulled-ensw 9  5)
    (make-stepper     11 5)
    (make-wall        12 5)
    (make-wall        0  6)             ; 6
    (make-stepper     1  6)
    (make-pulled-ensw 3  6)
    (make-pulled-ensw 4  6)
    (make-pulled-ensw 5  6)
    (make-pulled-ensw 6  6)
    (make-pulled-ensw 7  6)
    (make-pulled-ensw 8  6)
    (make-pulled-ensw 9  6)
    (make-stepper     11 6)
    (make-wall        12 6)
    (make-wall        0  7)             ; 7
    (make-stepper     1  7)
    (make-stepper     11 7)
    (make-wall        12 7)
    (make-wall        0  8)             ; 8
    (make-stepper     1  8)
    (make-player      10 8)
    (make-stepper     11 8)
    (make-wall        12 8)
    (make-wall        0  9)             ; 9
    (make-stepper     1  9)
    (make-stepper     2  9)
    (make-stepper     3  9)
    (make-stepper     4  9)
    (make-stepper     5  9)
    (make-stepper     6  9)
    (make-stepper     7  9)
    (make-stepper     8  9)
    (make-stepper     9  9)
    (make-stepper     10 9)
    (make-stepper     11 9)
    (make-wall        12 9)
    (make-wall        0  10)            ; Last row
    (make-wall        1  10)
    (make-wall        2  10)
    (make-wall        3  10)
    (make-wall        4  10)
    (make-wall        5  10)
    (make-exit        6  10)
    (make-wall        7  10)
    (make-wall        8  10)
    (make-wall        9  10)
    (make-wall        10 10)
    (make-wall        11 10)
    (make-wall        12 10)))
 *levels*)

(vector-push-extend
 '(list (list
         "Level Name"
         "Level Hint")
   (list
    :west  nil nil nil nil nil
    :north nil nil nil
    :south nil nil nil nil
    :east  nil nil nil nil nil nil nil nil
    :north nil nil
    :west  nil nil nil nil nil nil nil nil nil
    :north nil nil nil
    :south nil nil nil nil
    :east  nil nil nil nil nil nil nil nil
    :north nil nil
    :west  nil nil nil nil nil nil nil
    :north nil nil
    :south nil nil nil
    :east  nil nil nil nil nil nil
    :north nil nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil
    :west  nil nil
    :north nil nil
    :south nil nil nil
    :east  nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :south nil nil nil nil
    :east  nil
    :north nil nil
    :west  nil nil nil nil nil nil nil
    :north nil nil
    :west  nil
    :east  nil nil nil nil nil nil nil
    :north nil nil nil nil nil nil
    :west  nil nil
    :south nil nil nil nil nil nil nil nil nil nil nil
    :east  nil
    :north nil nil
    :west  nil nil nil nil nil nil
    :north nil
    :west  nil nil
    :north nil nil nil nil
    :south nil nil nil nil
    :east  nil nil nil nil nil nil nil
    :south nil nil nil nil
    :east  nil
    :north nil nil
    :west  nil nil nil nil nil nil nil
    :north nil
    :west  nil nil
    :east  nil nil nil nil nil
    :south nil nil nil
    :east  nil nil
    :north nil
    :west  nil nil nil nil nil nil
    :north nil nil
    :west  nil nil
    :north nil nil nil nil
    :south nil nil nil nil nil
    :east  nil nil nil nil nil nil nil
    :south nil nil nil nil
    :east  nil
    :north nil nil nil
    :west  nil nil nil nil nil nil
    :north nil nil nil
    :west
    :east  nil nil nil nil
    :south nil nil nil
    :east  nil nil nil nil
    :north nil
    :west  nil nil nil nil nil nil nil nil
    :north nil nil
    :west  nil
    :north nil nil nil nil nil nil
    :south nil nil nil nil nil nil
    :east  nil nil nil nil nil nil nil
    :south nil nil nil nil
    :east  nil
    :north nil nil
    :west  nil nil nil nil nil nil nil
    :north nil nil
    :west  nil
    :north nil nil
    :west
    :north nil nil nil nil nil nil nil nil nil)
   (list
    (make-wall          0  0)           ; 0
    (make-wall          1  0)
    (make-wall          2  0)
    (make-wall          3  0)
    (make-wall          4  0)
    (make-wall          5  0)
    (make-wall          6  0)
    (make-wall          7  0)
    (make-wall          8  0)
    (make-wall          9  0)
    (make-wall          10 0)
    (make-wall          0  1)           ; 1
    (make-wall          10 1)
    (make-wall          0  2)           ; 2
    (make-wall          10 2)
    (make-wall          0  3)           ; 3
    (make-wall          1  3)
    (make-wall          2  3)
    (make-wall          3  3)
    (make-wall          4  3)
    (make-wall          5  3)
    (make-wall          6  3)
    (make-wall          9  3)
    (make-wall          10 3)
    (make-wall          0  4)           ; 4
    (make-exit          1  4)
    (make-pulled-ensw   2  4)
    (make-pulled-ensw   3  4)
    (make-pulled-ensw   4  4)
    (make-pulled-ensw   5  4)
    (make-wall          6  4)
    (make-wall          10 4)
    (make-wall          0  5)           ; 5
    (make-pulled-ensw   1  5)
    (make-pulled-ensw   2  5)
    (make-pulled-ensw   3  5)
    (make-pulled-ensw   4  5)
    (make-pulled-ensw   5  5)
    (make-wall          6  5)
    (make-wall          10 5)
    (make-wall          0  6)           ; 6
    (make-pulled-ensw   1  6)
    (make-pulled-ensw   2  6)
    (make-pulled-ensw   3  6)
    (make-pulled-ensw   4  6)
    (make-pulled-ensw   5  6)
    (make-wall          6  6)
    (make-wall          10 6)
    (make-wall          0  7)           ; 7
    (make-pulled-ensw   1  7)
    (make-pulled-ensw   2  7)
    (make-pulled-ensw   3  7)
    (make-pulled-ensw   4  7)
    (make-pulled-ensw   5  7)
    (make-wall          6  7)
    (make-wall          10 7)
    (make-wall          0  8)           ; 8
    (make-pulled-ensw   1  8)
    (make-pulled-ensw   2  8)
    (make-pulled-ensw   3  8)
    (make-pulled-ensw   4  8)
    (make-pulled-ensw   5  8)
    (make-wall          6  8)
    (make-wall          10 8)
    (make-wall          0  9)           ; 9
    (make-pulled-ensw   1  9)
    (make-pulled-ensw   2  9)
    (make-pulled-ensw   3  9)
    (make-pulled-ensw   4  9)
    (make-pulled-ensw   5  9)
    (make-wall          6  9)
    (make-wall          10 9)
    (make-wall          0  10)          ; 10
    (make-pulled-ensw   1  10)
    (make-pulled-ensw   2  10)
    (make-pulled-ensw   3  10)
    (make-pulled-ensw   4  10)
    (make-pulled-ensw   5  10)
    (make-pulled-ensw   6  10)
    (make-wall          10 10)
    (make-wall          0  11)          ; 11
    (make-wall          10 11)
    (make-wall          0  12)          ; 12
    (make-wall          9  12)
    (make-wall          10 12)
    (make-wall          0  13)          ; 13
    (make-wall          10 13)
    (make-wall          0  14)          ; 14
    (make-player        6  14)
    (make-wall          10 14)
    (make-wall          0  15)          ; 15
    (make-wall          2  15)
    (make-wall          3  15)
    (make-wall          4  15)
    (make-wall          5  15)
    (make-wall          6  15)
    (make-wall          7  15)
    (make-wall          10 15)
    (make-wall          0  16)          ; 16
    (make-wall          10 16)
    (make-wall          0  17)          ; 17
    (make-wall          1  17)
    (make-wall          2  17)
    (make-wall          3  17)
    (make-wall          4  17)
    (make-wall          5  17)
    (make-wall          6  17)
    (make-wall          7  17)
    (make-wall          8  17)
    (make-wall          9  17)
    (make-wall          10 17)))
 *levels*)

(vector-push-extend
 '(list (list
         "Rubber Band Twist"
         "Just Keep Jumping")
   (list
    :north   nil nil nil nil nil nil
    :action1 nil nil nil nil nil nil nil nil nil nil
    :action1 nil nil nil nil nil nil nil nil nil nil
    :action1 nil nil nil nil nil nil nil nil nil nil
    :action1 nil nil nil nil nil nil nil nil nil nil
    :action1 nil nil nil nil nil nil nil nil nil nil
    :action1 nil nil nil nil nil nil)
   (list
    (make-player       0  8)
    (make-special-jump 0  6)
    (make-vacuum       0  3)
    (make-slope-es     0  0)
    (make-slope-ws     2  0)
    (make-special-jump 2  2)
    (make-vacuum       2  5)
    (make-slope-en     2  8)
    (make-slope-wn     4  8)
    (make-special-jump 4  6)
    (make-vacuum       4  3)
    (make-slope-es     4  0)
    (make-slope-ws     6  0)
    (make-special-jump 6  2)
    (make-vacuum       6  5)
    (make-slope-en     6  8)
    (make-slope-wn     8  8)
    (make-special-jump 8  6)
    (make-vacuum       8  3)
    (make-slope-es     8  0)
    (make-slope-ws     10 0)
    (make-special-jump 10 2)
    (make-vacuum       10 5)
    (make-exit         10 8)))
 *levels*)

(vector-push-extend
 '(list (list
         "Oh, Even More Boxes To Pull"
         "Try Not To Get Stuck, Real Hard This Time")
   (list
    :west  nil nil nil
    :east  nil nil nil nil nil nil
    :south
    :west  nil nil nil nil nil nil nil
    :north nil
    :west  nil
    :east  nil nil nil nil nil nil
    :south nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil nil nil
    :west
    :east  nil nil nil nil nil nil
    :south nil nil nil
    :west  nil nil nil
    :north nil nil nil nil nil nil
    :west
    :east  nil nil nil
    :south nil nil nil nil nil
    :west  nil nil nil
    :north nil nil nil nil nil nil
    :west  nil
    :south nil nil nil nil nil
    :north nil nil nil nil nil
    :east  nil nil
    :south nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :east  nil nil
    :west  nil nil nil
    :north nil nil nil nil
    :east  nil
    :south nil nil
    :east  nil nil nil nil
    :north nil nil
    :south nil nil nil
    :west  nil nil nil
    :north nil nil nil
    :west  nil nil nil nil
    :east  nil nil nil nil nil nil nil nil nil nil nil nil
    :south nil nil nil nil nil nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil
    :east  nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :south nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :west  nil
    :north nil nil nil nil nil
    :west  nil nil nil
    :south nil
    :west  nil nil
    :east  nil nil nil nil nil nil nil nil nil
    :south nil nil nil
    :west  nil nil
    :north nil nil nil
    :west  nil nil nil nil nil nil
    :north nil nil nil
    :west  nil nil nil nil nil nil nil
    :south nil nil
    :west  nil
    :east  nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    :south nil nil nil
    :west  nil nil nil nil nil nil nil nil nil nil nil nil nil
    :north nil nil nil nil
    :west  nil
    :north nil nil
    :west  nil nil
    :south nil
    :west  nil nil
    :east  nil nil nil nil nil nil nil nil
    :north nil
    :west  nil nil
    :south nil
    :west nil nil nil nil nil nil nil)
   (list
    (make-pulled-ensw 2 1)
    (make-pulled-ensw 3 1)
    (make-pulled-ensw 4 1)
    (make-pulled-ensw 5 1)
    (make-pulled-ensw 6 1)
    (make-pulled-ensw 7 1)
    (make-pulled-ensw 8 1)
    (make-pulled-ensw 9 1)
    (make-pulled-ensw 10 1)
    (make-pulled-ensw 11 1)
    (make-pulled-ensw 12 1)
    (make-pulled-ensw 13 1)
    (make-pulled-ensw 14 1)
    (make-pulled-ensw 15 1)
    (make-pulled-ensw 16 1)
    (make-pulled-ensw 17 1)
    (make-pulled-ensw 18 1)
    (make-pulled-ensw 2 2)
    (make-pulled-ensw 3 2)
    (make-pulled-ensw 4 2)
    (make-pulled-ensw 5 2)
    (make-pulled-ensw 6 2)
    (make-pulled-ensw 7 2)
    (make-pulled-ensw 8 2)
    (make-pulled-ensw 9 2)
    (make-pulled-ensw 10 2)
    (make-pulled-ensw 11 2)
    (make-pulled-ensw 12 2)
    (make-pulled-ensw 13 2)
    (make-pulled-ensw 14 2)
    (make-pulled-ensw 15 2)
    (make-pulled-ensw 16 2)
    (make-pulled-ensw 17 2)
    (make-pulled-ensw 18 2)
    (make-pulled-ensw 2 3)
    (make-pulled-ensw 3 3)
    (make-pulled-ensw 4 3)
    (make-pulled-ensw 5 3)
    (make-pulled-ensw 6 3)
    (make-pulled-ensw 7 3)
    (make-pulled-ensw 8 3)
    (make-pulled-ensw 9 3)
    (make-pulled-ensw 10 3)
    (make-pulled-ensw 11 3)
    (make-pulled-ensw 12 3)
    (make-pulled-ensw 13 3)
    (make-pulled-ensw 14 3)
    (make-pulled-ensw 15 3)
    (make-pulled-ensw 16 3)
    (make-pulled-ensw 17 3)
    (make-pulled-ensw 18 3)
    (make-pulled-ensw 2 4)
    (make-pulled-ensw 3 4)
    (make-pulled-ensw 4 4)
    (make-pulled-ensw 5 4)
    (make-pulled-ensw 6 4)
    (make-pulled-ensw 7 4)
    (make-pulled-ensw 8 4)
    (make-pulled-ensw 9 4)
    (make-pulled-ensw 10 4)
    (make-pulled-ensw 11 4)
    (make-pulled-ensw 12 4)
    (make-pulled-ensw 13 4)
    (make-pulled-ensw 14 4)
    (make-pulled-ensw 15 4)
    (make-pulled-ensw 16 4)
    (make-pulled-ensw 17 4)
    (make-pulled-ensw 18 4)
    (make-pulled-ensw 2 5)
    (make-pulled-ensw 3 5)
    (make-pulled-ensw 4 5)
    (make-pulled-ensw 5 5)
    (make-pulled-ensw 6 5)
    (make-pulled-ensw 7 5)
    (make-pulled-ensw 8 5)
    (make-pulled-ensw 9 5)
    (make-pulled-ensw 10 5)
    (make-pulled-ensw 11 5)
    (make-pulled-ensw 12 5)
    (make-pulled-ensw 13 5)
    (make-pulled-ensw 14 5)
    (make-pulled-ensw 15 5)
    (make-pulled-ensw 16 5)
    (make-pulled-ensw 17 5)
    (make-pulled-ensw 18 5)
    (make-pulled-ensw 2 6)
    (make-pulled-ensw 3 6)
    (make-pulled-ensw 4 6)
    (make-pulled-ensw 5 6)
    (make-pulled-ensw 6 6)
    (make-pulled-ensw 7 6)
    (make-pulled-ensw 8 6)
    (make-pulled-ensw 12 6)
    (make-pulled-ensw 13 6)
    (make-pulled-ensw 14 6)
    (make-pulled-ensw 15 6)
    (make-pulled-ensw 16 6)
    (make-pulled-ensw 17 6)
    (make-pulled-ensw 18 6)
    (make-pulled-ensw 2 7)
    (make-pulled-ensw 3 7)
    (make-pulled-ensw 4 7)
    (make-pulled-ensw 5 7)
    (make-pulled-ensw 6 7)
    (make-pulled-ensw 7 7)
    (make-pulled-ensw 13 7)
    (make-pulled-ensw 14 7)
    (make-pulled-ensw 15 7)
    (make-pulled-ensw 16 7)
    (make-pulled-ensw 17 7)
    (make-pulled-ensw 18 7)
    (make-pulled-ensw 2 8)
    (make-pulled-ensw 3 8)
    (make-pulled-ensw 4 8)
    (make-pulled-ensw 5 8)
    (make-pulled-ensw 6 8)
    (make-pulled-ensw 14 8)
    (make-pulled-ensw 15 8)
    (make-pulled-ensw 16 8)
    (make-pulled-ensw 17 8)
    (make-pulled-ensw 18 8)
    (make-pulled-ensw 2 9)
    (make-pulled-ensw 3 9)
    (make-pulled-ensw 4 9)
    (make-pulled-ensw 5 9)
    (make-pulled-ensw 6 9)
    (make-pulled-ensw 14 9)
    (make-pulled-ensw 15 9)
    (make-pulled-ensw 16 9)
    (make-pulled-ensw 17 9)
    (make-pulled-ensw 18 9)
    (make-pulled-ensw 2 10)
    (make-pulled-ensw 3 10)
    (make-pulled-ensw 4 10)
    (make-pulled-ensw 5 10)
    (make-pulled-ensw 6 10)
    (make-pulled-ensw 14 10)
    (make-pulled-ensw 15 10)
    (make-pulled-ensw 16 10)
    (make-pulled-ensw 17 10)
    (make-pulled-ensw 18 10)
    (make-pulled-ensw 2 11)
    (make-pulled-ensw 3 11)
    (make-pulled-ensw 4 11)
    (make-pulled-ensw 5 11)
    (make-pulled-ensw 6 11)
    (make-pulled-ensw 7 11)
    (make-pulled-ensw 13 11)
    (make-pulled-ensw 14 11)
    (make-pulled-ensw 15 11)
    (make-pulled-ensw 16 11)
    (make-pulled-ensw 17 11)
    (make-pulled-ensw 18 11)
    (make-pulled-ensw 2 12)
    (make-pulled-ensw 3 12)
    (make-pulled-ensw 4 12)
    (make-pulled-ensw 5 12)
    (make-pulled-ensw 6 12)
    (make-pulled-ensw 7 12)
    (make-pulled-ensw 8 12)
    (make-pulled-ensw 12 12)
    (make-pulled-ensw 13 12)
    (make-pulled-ensw 14 12)
    (make-pulled-ensw 15 12)
    (make-pulled-ensw 16 12)
    (make-pulled-ensw 17 12)
    (make-pulled-ensw 18 12)
    (make-pulled-ensw 2 13)
    (make-pulled-ensw 3 13)
    (make-pulled-ensw 4 13)
    (make-pulled-ensw 5 13)
    (make-pulled-ensw 6 13)
    (make-pulled-ensw 7 13)
    (make-pulled-ensw 8 13)
    (make-pulled-ensw 9 13)
    (make-pulled-ensw 10 13)
    (make-pulled-ensw 11 13)
    (make-pulled-ensw 12 13)
    (make-pulled-ensw 13 13)
    (make-pulled-ensw 14 13)
    (make-pulled-ensw 15 13)
    (make-pulled-ensw 16 13)
    (make-pulled-ensw 17 13)
    (make-pulled-ensw 18 13)
    (make-pulled-ensw 2 14)
    (make-pulled-ensw 3 14)
    (make-pulled-ensw 4 14)
    (make-pulled-ensw 5 14)
    (make-pulled-ensw 6 14)
    (make-pulled-ensw 7 14)
    (make-pulled-ensw 8 14)
    (make-pulled-ensw 9 14)
    (make-pulled-ensw 10 14)
    (make-pulled-ensw 11 14)
    (make-pulled-ensw 12 14)
    (make-pulled-ensw 13 14)
    (make-pulled-ensw 14 14)
    (make-pulled-ensw 15 14)
    (make-pulled-ensw 16 14)
    (make-pulled-ensw 17 14)
    (make-pulled-ensw 18 14)
    (make-pulled-ensw 2 15)
    (make-pulled-ensw 3 15)
    (make-pulled-ensw 4 15)
    (make-pulled-ensw 5 15)
    (make-pulled-ensw 6 15)
    (make-pulled-ensw 7 15)
    (make-pulled-ensw 8 15)
    (make-pulled-ensw 9 15)
    (make-pulled-ensw 10 15)
    (make-pulled-ensw 11 15)
    (make-pulled-ensw 12 15)
    (make-pulled-ensw 13 15)
    (make-pulled-ensw 14 15)
    (make-pulled-ensw 15 15)
    (make-pulled-ensw 16 15)
    (make-pulled-ensw 17 15)
    (make-pulled-ensw 18 15)
    (make-pulled-ensw 2 16)
    (make-pulled-ensw 3 16)
    (make-pulled-ensw 4 16)
    (make-pulled-ensw 5 16)
    (make-pulled-ensw 6 16)
    (make-pulled-ensw 7 16)
    (make-pulled-ensw 8 16)
    (make-pulled-ensw 9 16)
    (make-pulled-ensw 10 16)
    (make-pulled-ensw 11 16)
    (make-pulled-ensw 12 16)
    (make-pulled-ensw 13 16)
    (make-pulled-ensw 14 16)
    (make-pulled-ensw 15 16)
    (make-pulled-ensw 16 16)
    (make-pulled-ensw 17 16)
    (make-pulled-ensw 18 16)
    (make-pulled-ensw 2 17)
    (make-pulled-ensw 3 17)
    (make-pulled-ensw 4 17)
    (make-pulled-ensw 5 17)
    (make-pulled-ensw 6 17)
    (make-pulled-ensw 7 17)
    (make-pulled-ensw 8 17)
    (make-pulled-ensw 9 17)
    (make-pulled-ensw 10 17)
    (make-pulled-ensw 11 17)
    (make-pulled-ensw 12 17)
    (make-pulled-ensw 13 17)
    (make-pulled-ensw 14 17)
    (make-pulled-ensw 15 17)
    (make-pulled-ensw 16 17)
    (make-pulled-ensw 17 17)
    (make-pulled-ensw 18 17)
    (make-exit   0  9)
    (make-exit   20 9)
    (make-player 10 9)))
 *levels*)

(defun num-levels ()
  (length *levels*))

(defun load-level (index)
  (let ((level (level-get-nth index)))
    (setf *level-par* (level-get-par level))
    (log:debug "Loading level at index ~A" index)
    (eval level)))

