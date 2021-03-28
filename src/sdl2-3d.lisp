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
(in-package :crates2-ui)

;; Crate is drawn as CW x CH pixel shape
(defconstant cw 32)
(defconstant ch 32)
(defconstant iw 1024)
(defconstant ih 1024)
;; Window dimensions
(defconstant screen-width 800)
(defconstant screen-height 600)

(defparameter *crates2-window* :pointer)
(defparameter *crates2-gl-context* :pointer)
;; (defparameter *crates2-renderer* :pointer)
(defparameter *image* :pointer)
(defparameter *txids* :pointer)
;; (defparameter *texture* :pointer)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun make-rect-for-top (tx1 ty1 tx2 ty2 &optional (delta 0.0))
  (let* ((hw  0.5)
         (-hw  (- hw))
         (z    (+ hw delta)))
    (list 0.0  0.0  1.0                 ; normal
          tx1  ty2      tx2  ty2      tx2 ty1       tx1 ty1 ; texture coordinates
          -hw -hw  z   hw  -hw  z   hw  hw  z   -hw  hw  z)))

(defun make-rect-for-north (tx1 ty1 tx2 ty2)
  (let* ((hw  0.5)
         (-hw  (- hw)))
    (list 0.0  1.0  0.0
          tx1 ty2       tx1 ty1       tx2 ty1      tx2 ty2
          -hw hw -hw   -hw  hw  hw    hw  hw  hw   hw  hw -hw)))

(defun make-rect-for-south (tx1 ty1 tx2 ty2)
  (let* ((hw  0.5)
         (-hw  (- hw)))
    (list 0.0  -1.0  0.0
          tx1  ty2       tx2 ty2       tx2 ty1     tx1 ty1
          -hw -hw -hw    hw -hw -hw    hw -hw hw  -hw -hw hw)))

(defun make-rect-for-west (tx1 ty1 tx2 ty2)
  (let* ((hw  0.5)
         (-hw  (- hw)))
    (list -1.0  0.0  0.0
          tx1 ty2        tx1 ty1       tx2 ty1     tx2 ty2
          -hw -hw -hw   -hw -hw hw    -hw  hw hw  -hw  hw -hw)))

(defun make-rect-for-bottom (tx1 ty1 tx2 ty2 &optional (delta 0.0))
  (let* ((hw  0.5)
         (-hw  (- hw))
         (z    (+ -hw delta)))
    (list 0.0  0.0  1.0
          tx1 ty2        tx2 ty2       tx2 ty1      tx1 ty1
         -hw -hw z       hw -hw z      hw  hw z    -hw  hw z)))

(defun make-rect-for-east (tx1 ty1 tx2 ty2)
  (let* ((hw  0.5)
         (-hw  (- hw)))
    (list 1.0  0.0  0.0
          tx1 ty2       tx2 ty2       tx2 ty1     tx1 ty1
          hw -hw -hw    hw  hw -hw    hw  hw hw   hw -hw hw)))

(defun make-rect (index face)
  (let* ((sprites-per-row (floor iw cw))
         (tx1 (float (/ (* cw (mod index sprites-per-row)) iw)))
         (ty1 (float (/ (* ch (floor index sprites-per-row)) iw)))
         (tw  (float (/ cw iw)))
         (th  (float (/ ch ih)))
         (tx2 (+ tx1 tw))
         (ty2 (+ ty1 th)))
    (ecase face
      (:top    (make-rect-for-top    tx1 ty1 tx2 ty2))
      (:bottom (make-rect-for-bottom tx1 ty1 tx2 ty2))
      (:front  (make-rect-for-south  tx1 ty1 tx2 ty2))
      (:back   (make-rect-for-north  tx1 ty1 tx2 ty2))
      (:east   (make-rect-for-east   tx1 ty1 tx2 ty2))
      (:west   (make-rect-for-west   tx1 ty1 tx2 ty2)))))

(defun make-transparent-top (index)
  (let* ((sprites-per-row (floor iw cw))
         (tx1 (float (/ (* cw (mod index sprites-per-row)) iw)))
         (ty1 (float (/ (* ch (floor index sprites-per-row)) iw)))
         (tw  (float (/ cw iw)))
         (th  (float (/ ch ih)))
         (tx2 (+ tx1 tw))
         (ty2 (+ ty1 th)))
    (list
     (make-rect-for-top    tx1 ty1 tx2 ty2 0.08)
     nil
     nil
     nil
     nil
     nil)))

(defun make-transparent-bottom (index)
  (let* ((sprites-per-row (floor iw cw))
         (tx1 (float (/ (* cw (mod index sprites-per-row)) iw)))
         (ty1 (float (/ (* ch (floor index sprites-per-row)) iw)))
         (tw  (float (/ cw iw)))
         (th  (float (/ ch ih)))
         (tx2 (+ tx1 tw))
         (ty2 (+ ty1 th)))
    (list
     nil
     (make-rect-for-bottom tx1 ty1 tx2 ty2 0.08)
     nil
     nil
     nil
     nil)))

(defun make-cube (top bottom front back east west)
  (list
   (if top    (make-rect top    :top)    nil)
   (if bottom (make-rect bottom :bottom) nil)
   (if front  (make-rect front  :front)  nil)
   (if back   (make-rect back   :back)   nil)
   (if east   (make-rect east   :east)   nil)
   (if west   (make-rect west   :west)   nil)))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-body" *visual-hash*) (make-cube nil 482 nil nil nil nil))
  (setf (gethash "gear-00"     *visual-hash*) (make-transparent-bottom 483))
  (setf (gethash "gear-01"     *visual-hash*) (make-transparent-bottom 484))
  (setf (gethash "gear-02"     *visual-hash*) (make-transparent-bottom 485))
  (setf (gethash "gear-03"     *visual-hash*) (make-transparent-bottom 486))
  (setf (gethash "gear-04"     *visual-hash*) (make-transparent-bottom 487))
  (setf (gethash "gear-05"     *visual-hash*) (make-transparent-bottom 488))
  (setf (gethash "gear-06"     *visual-hash*) (make-transparent-bottom 489))
  (setf (gethash "gear-07"     *visual-hash*) (make-transparent-bottom 490))
  ;; WALL
  (setf (gethash "wall-idle-00" *visual-hash*) (make-cube 0 0 0 0 0 0))
  (setf (gethash "wall-idle-01" *visual-hash*) (make-cube 1 1 1 1 1 1))
  (setf (gethash "wall-idle-02" *visual-hash*) (make-cube 2 2 2 2 2 2))
  (setf (gethash "wall-idle-03" *visual-hash*) (make-cube 3 3 3 3 3 3))
  ;; PUSHED
  (setf (gethash "pushed-idle" *visual-hash*) (make-cube 0 0 0 0 0 0))
  ;; BLOCK-TIMER
  (setf (gethash "block-timer-durable" *visual-hash*) (make-cube 0 0 0 0 0 0))
  (setf (gethash "block-timer"         *visual-hash*) (make-cube 5 5 5 5 5 5))
  ;; EXIT
  (setf (gethash "exit-idle"        *visual-hash*) (make-cube 96 96 96 96 96 96)) ; 48 + 48 to old
  (setf (gethash "exit-active-pass" *visual-hash*) (make-cube 97 97 97 97 97 97))
  (setf (gethash "exit-active-fail" *visual-hash*) (make-cube 98 98 98 98 98 98))
  ;; KEY
  (setf (gethash "key-idle-00" *visual-hash*) (make-cube 64 64 64 64 64 64))
  (setf (gethash "key-idle-01" *visual-hash*) (make-cube 65 65 65 65 65 65))
  (setf (gethash "key-idle-02" *visual-hash*) (make-cube 66 66 66 66 66 66))
  (setf (gethash "key-idle-03" *visual-hash*) (make-cube 67 67 67 67 67 67))
  (setf (gethash "key-idle-04" *visual-hash*) (make-cube 68 68 68 68 68 68))
  (setf (gethash "key-idle-05" *visual-hash*) (make-cube 69 69 69 69 69 69))
  (setf (gethash "key-idle-06" *visual-hash*) (make-cube 70 70 70 70 70 70))
  (setf (gethash "key-idle-07" *visual-hash*) (make-cube 71 71 71 71 71 71))
  (setf (gethash "key-idle-08" *visual-hash*) (make-cube 72 72 72 72 72 72))
  (setf (gethash "key-active"  *visual-hash*) (make-cube 73 73 73 73 73 73))
  ;; SPECIAL
  (setf (gethash "special-jump-idle-00" *visual-hash*) (make-cube nil 960 nil nil nil nil))
  (setf (gethash "special-jump-idle-01" *visual-hash*) (make-cube nil 961 nil nil nil nil))
  (setf (gethash "special-jump-idle-02" *visual-hash*) (make-cube nil 962 nil nil nil nil))
  (setf (gethash "special-jump-idle-03" *visual-hash*) (make-cube nil 963 nil nil nil nil))
  (setf (gethash "special-jump-idle-04" *visual-hash*) (make-cube nil 964 nil nil nil nil))
  (setf (gethash "special-jump-idle-05" *visual-hash*) (make-cube nil 965 nil nil nil nil))
  (setf (gethash "special-jump-idle-06" *visual-hash*) (make-cube nil 966 nil nil nil nil))
  (setf (gethash "special-jump-idle-07" *visual-hash*) (make-cube nil 967 nil nil nil nil))
  (setf (gethash "special-jump-idle-08" *visual-hash*) (make-cube nil 968 nil nil nil nil))
  (setf (gethash "special-jump-idle-09" *visual-hash*) (make-cube nil 969 nil nil nil nil))
  (setf (gethash "special-jump-idle-10" *visual-hash*) (make-cube nil 970 nil nil nil nil))
  (setf (gethash "special-jump-idle-11" *visual-hash*) (make-cube nil 971 nil nil nil nil))
  (setf (gethash "special-jump-idle-12" *visual-hash*) (make-cube nil 972 nil nil nil nil))
  (setf (gethash "special-jump-idle-13" *visual-hash*) (make-cube nil 972 nil nil nil nil))
  (setf (gethash "special-jump-active"  *visual-hash*) (make-cube nil 973 nil nil nil nil))
  ;; PLAYER
  (setf (gethash "player-active-00" *visual-hash*) (make-cube 32 32 32 32 32 32))
  (setf (gethash "player-active-01" *visual-hash*) (make-cube 33 33 33 33 33 33))
  (setf (gethash "player-active-02" *visual-hash*) (make-cube 34 34 34 34 34 34))
  (setf (gethash "player-active-03" *visual-hash*) (make-cube 35 35 35 35 35 35))
  (setf (gethash "player-active-04" *visual-hash*) (make-cube 36 36 36 36 36 36))
  (setf (gethash "player-active-05" *visual-hash*) (make-cube 37 37 37 37 37 37))
  (setf (gethash "player-active-06" *visual-hash*) (make-cube 38 38 38 38 38 38))

  (setf (gethash "player-airborne"  *visual-hash*) (let* ((w 1.0)
                                                          (hw (/ 1.0 2))
                                                          (-hw (- hw))
                                                          (tx1 0.0)
                                                          (tx2 (+ tx1 (/ cw iw)))
                                                          (ty1 (/ (* 1.0 ch) ih))
                                                          (ty2 (+ ty1 (/ ch ih))))
                                                     (list

                                                      ;; top
                                                      (list 0.0 0.0 1.0 ; normal
                                                            tx1 ty2 tx2 ty2 tx2 ty1 tx1 ty1 ; texture coordinates
                                                            -hw -hw (+ hw 1.0) hw -hw (+ hw 1.0) hw hw (+ hw 1.0) -hw hw (+ hw 1.0))
                                                      ;; north
                                                      (list 0.0 1.0 0.0
                                                            tx1 ty2 tx1 ty1 tx2 ty1 tx2 ty2
                                                            -hw hw (+ -hw 1.0) -hw hw (+ hw 1.0) hw hw (+ hw 1.0) hw hw (+ -hw 1.0))
                                                      ;; south
                                                      (list 0.0 -1.0 0.0
                                                            tx1 ty2 tx2 ty2 tx2 ty1 tx1 ty1
                                                            -hw -hw (+ -hw 1.0) hw -hw (+ -hw 1.0) hw -hw (+ hw 1.0) -hw -hw (+ hw 1.0))
                                                      ;; west
                                                      (list -1.0  0.0  0.0
                                                            tx1 ty2        tx1 ty1       tx2 ty1     tx2 ty2
                                                            -hw -hw (+ -hw 1.0)   -hw -hw (+ hw 1.0)    -hw  hw (+ hw 1.0)  -hw  hw (+ -hw 1.0))
                                                      ;; bottom
                                                      nil
                                                      ;; east
                                                      (list 1.0  0.0  0.0
                                                            tx1 ty2       tx2 ty2       tx2 ty1     tx1 ty1
                                                            hw -hw (+ -hw 1.0)    hw  hw (+ -hw 1.0)    hw  hw (+ hw 1.0)   hw -hw (+ hw 1.0)))))

  (setf (gethash "player-hidden"    *visual-hash*) (make-cube 15 15 15 15 15 15))
  ;; SLOPES
  (setf (gethash "slope-en"        *visual-hash*) (make-cube 384 nil 0 nil nil 0))
  (setf (gethash "slope-en-active" *visual-hash*) (make-cube 385 nil 0 nil nil 0))
  (setf (gethash "slope-es"        *visual-hash*) (make-cube 416 nil nil 0 nil 0)) ; + 32 to EN
  (setf (gethash "slope-es-active" *visual-hash*) (make-cube 417 nil nil 0 nil 0))
  (setf (gethash "slope-wn"        *visual-hash*) (make-cube 448 nil 0 nil 0 nil)) ; + 32 to ES
  (setf (gethash "slope-wn-active" *visual-hash*) (make-cube 449 nil 0 nil 0 nil))
  (setf (gethash "slope-ws"        *visual-hash*) (make-cube 480 nil nil 0 0 nil)) ; + 32 to WN
  (setf (gethash "slope-ws-active" *visual-hash*) (make-cube 481 nil nil 0 0 nil))
  ;; TURNSTILE
  (setf (gethash "turnstile-e"         *visual-hash*) (make-cube 128 0 0 0 0 0))
  (setf (gethash "turnstile-e-active"  *visual-hash*) (make-cube 129 0 0 0 0 0))
  (setf (gethash "turnstile-e1"        *visual-hash*) (make-cube 160 0 0 0 0 0)) ; + 32 to E
  (setf (gethash "turnstile-e1-active" *visual-hash*) (make-cube 161 0 0 0 0 0))
  (setf (gethash "turnstile-n"         *visual-hash*) (make-cube 192 0 0 0 0 0)) ; + 32 to E1
  (setf (gethash "turnstile-n-active"  *visual-hash*) (make-cube 193 0 0 0 0 0))
  (setf (gethash "turnstile-n1"        *visual-hash*) (make-cube 224 0 0 0 0 0)) ; + 32 to W1
  (setf (gethash "turnstile-n1-active" *visual-hash*) (make-cube 225 0 0 0 0 0))
  (setf (gethash "turnstile-s"         *visual-hash*) (make-cube 256 0 0 0 0 0)) ; + 32 to N
  (setf (gethash "turnstile-s-active"  *visual-hash*) (make-cube 257 0 0 0 0 0))
  (setf (gethash "turnstile-s1"        *visual-hash*) (make-cube 288 0 0 0 0 0)) ; + 32 to N1
  (setf (gethash "turnstile-s1-active" *visual-hash*) (make-cube 289 0 0 0 0 0))
  (setf (gethash "turnstile-w"         *visual-hash*) (make-cube 320 0 0 0 0 0)) ; + 32 to S1
  (setf (gethash "turnstile-w-active"  *visual-hash*) (make-cube 321 0 0 0 0 0))
  (setf (gethash "turnstile-w1"        *visual-hash*) (make-cube 352 0 0 0 0 0)) ; + 32 to W
  (setf (gethash "turnstile-w1-active" *visual-hash*) (make-cube 353 0 0 0 0 0))
  ;; NUMBERS
  (setf (gethash "number-01"        *visual-hash*) (make-transparent-top 99))
  (setf (gethash "number-02"        *visual-hash*) (make-transparent-top 100))
  (setf (gethash "number-03"        *visual-hash*) (make-transparent-top 101))
  (setf (gethash "number-04"        *visual-hash*) (make-transparent-top 102))
  (setf (gethash "number-05"        *visual-hash*) (make-transparent-top 103))
  (setf (gethash "number-06"        *visual-hash*) (make-transparent-top 104))
  (setf (gethash "number-07"        *visual-hash*) (make-transparent-top 105))
  (setf (gethash "number-08"        *visual-hash*) (make-transparent-top 106))
  (setf (gethash "number-09"        *visual-hash*) (make-transparent-top 107))
  (setf (gethash "number-10"        *visual-hash*) (make-transparent-top 108))
  (setf (gethash "number-11"        *visual-hash*) (make-transparent-top 131)) ; Add 32 to start of previous line.
  (setf (gethash "number-12"        *visual-hash*) (make-transparent-top 132))
  (setf (gethash "number-13"        *visual-hash*) (make-transparent-top 133))
  (setf (gethash "number-14"        *visual-hash*) (make-transparent-top 134))
  (setf (gethash "number-15"        *visual-hash*) (make-transparent-top 135))
  (setf (gethash "number-16"        *visual-hash*) (make-transparent-top 136))
  (setf (gethash "number-17"        *visual-hash*) (make-transparent-top 137))
  (setf (gethash "number-18"        *visual-hash*) (make-transparent-top 138))
  (setf (gethash "number-19"        *visual-hash*) (make-transparent-top 139))
  (setf (gethash "number-20"        *visual-hash*) (make-transparent-top 140))
  (setf (gethash "number-21"        *visual-hash*) (make-transparent-top 163)) ; Add 32 to start of previous line.
  (setf (gethash "number-22"        *visual-hash*) (make-transparent-top 164))
  (setf (gethash "number-23"        *visual-hash*) (make-transparent-top 165))
  (setf (gethash "number-24"        *visual-hash*) (make-transparent-top 166))
  (setf (gethash "number-25"        *visual-hash*) (make-transparent-top 167))
  (setf (gethash "number-26"        *visual-hash*) (make-transparent-top 168))
  (setf (gethash "number-27"        *visual-hash*) (make-transparent-top 169))
  (setf (gethash "number-28"        *visual-hash*) (make-transparent-top 170))
  (setf (gethash "number-29"        *visual-hash*) (make-transparent-top 171))
  (setf (gethash "number-30"        *visual-hash*) (make-transparent-top 172))
  (setf (gethash "number-31"        *visual-hash*) (make-transparent-top 195)) ; Add 32 to start of previous line.
  (setf (gethash "number-32"        *visual-hash*) (make-transparent-top 196))
  (setf (gethash "number-33"        *visual-hash*) (make-transparent-top 197))
  (setf (gethash "number-34"        *visual-hash*) (make-transparent-top 198))
  (setf (gethash "number-35"        *visual-hash*) (make-transparent-top 199))
  (setf (gethash "number-36"        *visual-hash*) (make-transparent-top 200))
  (setf (gethash "number-37"        *visual-hash*) (make-transparent-top 201))
  (setf (gethash "number-38"        *visual-hash*) (make-transparent-top 202))
  (setf (gethash "number-39"        *visual-hash*) (make-transparent-top 203))
  (setf (gethash "number-40"        *visual-hash*) (make-transparent-top 204))
  (setf (gethash "number-41"        *visual-hash*) (make-transparent-top 227)) ; Add 32 to start of previous line.
  (setf (gethash "number-42"        *visual-hash*) (make-transparent-top 228))
  (setf (gethash "number-43"        *visual-hash*) (make-transparent-top 229))
  (setf (gethash "number-44"        *visual-hash*) (make-transparent-top 230))
  (setf (gethash "number-45"        *visual-hash*) (make-transparent-top 231))
  (setf (gethash "number-46"        *visual-hash*) (make-transparent-top 232))
  (setf (gethash "number-47"        *visual-hash*) (make-transparent-top 233))
  (setf (gethash "number-48"        *visual-hash*) (make-transparent-top 234))
  (setf (gethash "number-49"        *visual-hash*) (make-transparent-top 235))
  (setf (gethash "number-50"        *visual-hash*) (make-transparent-top 236))
  (setf (gethash "number-51"        *visual-hash*) (make-transparent-top 259)) ; Add 32 to start of previous line.
  (setf (gethash "number-52"        *visual-hash*) (make-transparent-top 260))
  (setf (gethash "number-53"        *visual-hash*) (make-transparent-top 261))
  (setf (gethash "number-54"        *visual-hash*) (make-transparent-top 262))
  (setf (gethash "number-55"        *visual-hash*) (make-transparent-top 263))
  (setf (gethash "number-56"        *visual-hash*) (make-transparent-top 264))
  (setf (gethash "number-57"        *visual-hash*) (make-transparent-top 265))
  (setf (gethash "number-58"        *visual-hash*) (make-transparent-top 266))
  (setf (gethash "number-59"        *visual-hash*) (make-transparent-top 267))
  (setf (gethash "number-60"        *visual-hash*) (make-transparent-top 268))
  (setf (gethash "number-61"        *visual-hash*) (make-transparent-top 291)) ; Add 32 to start of previous line.
  (setf (gethash "number-62"        *visual-hash*) (make-transparent-top 292))
  (setf (gethash "number-63"        *visual-hash*) (make-transparent-top 293))
  (setf (gethash "number-64"        *visual-hash*) (make-transparent-top 294))
  (setf (gethash "number-65"        *visual-hash*) (make-transparent-top 295))
  (setf (gethash "number-66"        *visual-hash*) (make-transparent-top 296))
  (setf (gethash "number-67"        *visual-hash*) (make-transparent-top 297))
  (setf (gethash "number-68"        *visual-hash*) (make-transparent-top 298))
  (setf (gethash "number-69"        *visual-hash*) (make-transparent-top 299))
  (setf (gethash "number-70"        *visual-hash*) (make-transparent-top 300))
  (setf (gethash "number-71"        *visual-hash*) (make-transparent-top 323)) ; Add 32 to start of previous line.
  (setf (gethash "number-72"        *visual-hash*) (make-transparent-top 324))
  (setf (gethash "number-73"        *visual-hash*) (make-transparent-top 325))
  (setf (gethash "number-74"        *visual-hash*) (make-transparent-top 326))
  (setf (gethash "number-75"        *visual-hash*) (make-transparent-top 327))
  (setf (gethash "number-76"        *visual-hash*) (make-transparent-top 328))
  (setf (gethash "number-77"        *visual-hash*) (make-transparent-top 329))
  (setf (gethash "number-78"        *visual-hash*) (make-transparent-top 330))
  (setf (gethash "number-79"        *visual-hash*) (make-transparent-top 331))
  (setf (gethash "number-80"        *visual-hash*) (make-transparent-top 332))
  (setf (gethash "number-81"        *visual-hash*) (make-transparent-top 355)) ; Add 32 to start of previous line.
  (setf (gethash "number-82"        *visual-hash*) (make-transparent-top 356))
  (setf (gethash "number-83"        *visual-hash*) (make-transparent-top 357))
  (setf (gethash "number-84"        *visual-hash*) (make-transparent-top 358))
  (setf (gethash "number-85"        *visual-hash*) (make-transparent-top 359))
  (setf (gethash "number-86"        *visual-hash*) (make-transparent-top 360))
  (setf (gethash "number-87"        *visual-hash*) (make-transparent-top 361))
  (setf (gethash "number-88"        *visual-hash*) (make-transparent-top 362))
  (setf (gethash "number-89"        *visual-hash*) (make-transparent-top 363))
  (setf (gethash "number-90"        *visual-hash*) (make-transparent-top 364))
  (setf (gethash "number-91"        *visual-hash*) (make-transparent-top 387)) ; Add 32 to start of previous line.
  (setf (gethash "number-92"        *visual-hash*) (make-transparent-top 388))
  (setf (gethash "number-93"        *visual-hash*) (make-transparent-top 389))
  (setf (gethash "number-94"        *visual-hash*) (make-transparent-top 390))
  (setf (gethash "number-95"        *visual-hash*) (make-transparent-top 391))
  (setf (gethash "number-96"        *visual-hash*) (make-transparent-top 392))
  (setf (gethash "number-97"        *visual-hash*) (make-transparent-top 393))
  (setf (gethash "number-98"        *visual-hash*) (make-transparent-top 394))
  (setf (gethash "number-99"        *visual-hash*) (make-transparent-top 395))
  (setf (gethash "number-100"       *visual-hash*) (make-transparent-top 396))
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter"    *visual-hash*) (make-cube 0 nil 0 0 0 0))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter"     *visual-hash*) (make-cube nil 4 nil nil nil nil))
  ;; PASS-TIMER
  (setf (gethash "pass-timer"       *visual-hash*) (make-cube nil 4 nil nil nil nil))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) (make-cube 6 6 6 6 6 6))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) (make-cube 11 nil nil nil nil nil))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) (make-cube 7 nil nil nil nil nil))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) (make-cube 15 nil nil nil nil nil))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) (make-cube 13 nil nil nil nil nil))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) (make-cube 9 nil nil nil nil nil))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) (make-cube 15 nil nil nil nil nil))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) (make-cube 12 nil nil nil nil nil))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) (make-cube 8 nil nil nil nil nil))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) (make-cube 15 nil nil nil nil nil))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) (make-cube 14 nil nil nil nil nil))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) (make-cube 10 nil nil nil nil nil))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) (make-cube 15 nil nil nil nil nil))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) (make-cube nil 74 nil nil nil nil))
  (setf (gethash "stepper-active"             *visual-hash*) (make-cube nil 75 nil nil nil nil))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) (make-cube 39 39 39 39 39 39))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) (make-cube 40 nil nil nil nil nil))
  (setf (gethash "toggle-east-off"            *visual-hash*) (make-cube 50 nil nil nil nil nil))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) (make-cube 42 nil nil nil nil nil))
  (setf (gethash "toggle-west-off"            *visual-hash*) (make-cube 50 nil nil nil nil nil))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) (make-cube 41 nil nil nil nil nil))
  (setf (gethash "toggle-north-off"           *visual-hash*) (make-cube 50 nil nil nil nil nil))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) (make-cube 43 nil nil nil nil nil))
  (setf (gethash "toggle-south-off"           *visual-hash*) (make-cube 50 nil nil nil nil nil))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) (make-cube 76 76 76 76 76 76))
  (setf (gethash "bomb"          *visual-hash*) (make-cube 77 77 77 77 77 77))
  ;; ring 1
  (setf (gethash "bomb-ring-1"   *visual-hash*) (list
                                                 (let* ((hw 0.5)
                                                        (-hw (- hw))
                                                        (tx1 0.0)
                                                        (tx2 (+ tx1 (/ cw iw)))
                                                        (ty1 (/ (* 16.0 ch) ih))
                                                        (ty2 (+ ty1 (/ ch ih))))
                                                   (list 0.0 0.0 1.0 ; normal
                                                         tx1 ty2 tx2 ty2 tx2 ty1 tx1 ty1 ; texture coordinates
                                                         -hw -hw 0.0 hw -hw 0.0 hw hw 0.0 -hw hw 0.0))
                                                 nil
                                                 nil
                                                 nil
                                                 nil
                                                 nil)) ; start at index 256 (y slot 16); size 1 slot
  ;; ring 2
  (setf (gethash "bomb-ring-2"   *visual-hash*) (list
                                                 (let* ((hw 1.5)
                                                        (-hw (- hw))
                                                        (tx1 0.0)
                                                        (tx2 (+ tx1 (/ (* 3 cw) iw)))
                                                        (ty1 (/ (* 17.0 ch) ih))
                                                        (ty2 (+ ty1 (/ (* 3 ch) ih))))
                                                   (list 0.0 0.0 1.0 ; normal
                                                         tx1 ty2 tx2 ty2 tx2 ty1 tx1 ty1 ; texture coordinates
                                                         -hw -hw 0.0 hw -hw 0.0 hw hw 0.0 -hw hw 0.0))
                                                 nil
                                                 nil
                                                 nil
                                                 nil
                                                 nil)) ; start at y slot 17; size 3x3 slots
  ;; ring 3
  (setf (gethash "bomb-ring-3"   *visual-hash*) (list
                                                 (let* ((hw 2.5)
                                                        (-hw (- hw))
                                                        (tx1 0.0)
                                                        (tx2 (+ tx1 (/ (* 5 cw) iw)))
                                                        (ty1 (/ (* 20.0 ch) ih))
                                                        (ty2 (+ ty1 (/ (* 5 ch) ih))))
                                                   (list 0.0 0.0 1.0 ; normal
                                                         tx1 ty2 tx2 ty2 tx2 ty1 tx1 ty1 ; texture coordinates
                                                         -hw -hw 0.0 hw -hw 0.0 hw hw 0.0 -hw hw 0.0))
                                                 nil
                                                 nil
                                                 nil
                                                 nil
                                                 nil)) ; size 5x5 slots
  ;; ring 4
  (setf (gethash "bomb-ring-4"   *visual-hash*) (list
                                                 (let* ((hw 2.5)
                                                        (-hw (- hw))
                                                        (tx1 0.0)
                                                        (tx2 (+ tx1 (/ (* 5 cw) iw)))
                                                        (ty1 (/ (* 25.0 ch) ih))
                                                        (ty2 (+ ty1 (/ (* 5 ch) ih))))
                                                   (list 0.0 0.0 1.0 ; normal
                                                         tx1 ty2 tx2 ty2 tx2 ty1 tx1 ty1 ; texture coordinates
                                                         -hw -hw 0.0 hw -hw 0.0 hw hw 0.0 -hw hw 0.0))
                                                 nil
                                                 nil
                                                 nil
                                                 nil
                                                 nil))) ; size 5x5 slots

(defparameter *fake-input* nil)

(defun check-error (&optional msg)
  (let ((err (glgeterror)))
    (unless (=  err +GL-NO-ERROR+)
      (error "GL ERROR: ~A (~A)~%" (gluErrorString err) msg))))

(defun ui-render-face (face)
  ;; TODO this could be made more efficient by using foreign
  ;; arrays and function(s) operating on them.
  (let* ((nx  (nth 0   face))
         (ny  (nth 1   face))
         (nz  (nth 2   face))

         (t1  (nth 3   face))
         (t2  (nth 4   face))
         (t3  (nth 5   face))
         (t4  (nth 6   face))
         (t5  (nth 7   face))
         (t6  (nth 8   face))
         (t7  (nth 9   face))
         (t8  (nth 10  face))

         (v1  (nth 11  face))
         (v2  (nth 12  face))
         (v3  (nth 13  face))
         (v4  (nth 14  face))
         (v5  (nth 15  face))
         (v6  (nth 16  face))
         (v7  (nth 17  face))
         (v8  (nth 18  face))
         (v9  (nth 19  face))
         (v10 (nth 20  face))
         (v11 (nth 21  face))
         (v12 (nth 22  face)))
    (glnormal3f nx  ny  nz)
    (gltexcoord2f t1 t2) (glvertex3f v1  v2  v3)
    (gltexcoord2f t3 t4) (glvertex3f v4  v5  v6)
    (gltexcoord2f t5 t6) (glvertex3f v7  v8  v9)
    (gltexcoord2f t7 t8) (glvertex3f v10 v11 v12)))

(defun ui-render-cube (faces)
  ;; top bottom front back east west
  (let ((top    (first  faces))
        (bottom (second faces))
        (south  (third  faces))
        (north  (fourth faces))
        (east   (fifth  faces))
        (west   (sixth  faces)))
    (glbegin +GL-QUADS+)
    ;; Top north south west east bottom
    (when top
      (ui-render-face top))

    ;; North face
    (when north
      (ui-render-face north))

    ;; South face
    (when south
      (ui-render-face south))

    ;; West face
    (when west
      (ui-render-face west))

    ;; East face
    (when east
      (ui-render-face east))

    ;; Bottom face (inverted)
    (when bottom
      (ui-render-face bottom))

    (glend)
    (check-error)))

(defun ui-render (level step)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (check-error)
    (glclearcolor 0.0 0.0 0.0 1.0)
    (glclear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
    (glenable +GL-TEXTURE-2D+)
    (glbindtexture +GL-TEXTURE-2D+ (mem-aref *txids* :uint32 0))
    (glenable +GL-TEXTURE-2D+)
    (check-error "X")
    (glenable  +GL-DEPTH-TEST+)
    (gldisable +GL-CULL-FACE+)
    (gldisable +GL-ALPHA-TEST+)
    ;; (gltranslatef 5.0 5.0 -5.0)
    (check-error)

    (glcolor4f   1.0 1.0 1.0 1.0)
    (cffi:with-foreign-objects ((rect1 '(:struct sdl-rect))
                                (rect2 '(:struct sdl-rect))
                                (rect1pointer :pointer)
                                (rect2pointer :pointer)
                                (nullpointer :pointer))
      (setf nullpointer (null-pointer))
      (setf rect1pointer (mem-aptr rect1 '(:struct sdl-rect) 0))
      (setf rect2pointer (mem-aptr rect2 '(:struct sdl-rect) 0))
      (loop for crate in level
            do (progn
                 (let* ((x (if (and (= step 0) (typep crate 'crates2:moving)) (tail-x crate) (crate-x crate)))
                        (y (if (and (= step 0) (typep crate 'crates2:moving)) (tail-y crate) (crate-y crate)))
                        (z (if (and (= step 0) (typep crate 'crates2:moving)) (tail-z crate) (crate-z crate)))
                        (vids (visual crate)))
                   (loop for vid in vids
                         do
                            (let ((viv (gethash vid *visual-hash*)))
                              (when viv
                                (glpushmatrix)
                                (gltranslatef (float x) (- (float y)) (float z))
                                (ui-render-cube viv)
                                (glpopmatrix)))))))
      (sdl-gl-swapwindow *crates2-window*)
      (check-error))))

(defun ui-init ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (sdl-init +SDL-INIT-VIDEO+)
    ;; (sdl-gl-setattribute :SDL-GL-CONTEXT-MAJOR-VERSION 2)
    ;; (sdl-gl-setattribute :SDL-GL-CONTEXT-MINOR-VERSION 0)
    ;; (sdl-gl-setattribute :SDL-GL-CONTEXT-PROFILE-MASK (foreign-enum-value 'sdl-glprofile :SDL-GL-CONTEXT-PROFILE-CORE))
    ;; (sdl-gl-setattribute :SDL-GL-DOUBLEBUFFER 1)
    (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height (logior (foreign-enum-value 'sdl-windowflags :SDL-WINDOW-OPENGL) (foreign-enum-value 'sdl-windowflags :SDL-WINDOW-SHOWN))))
    ;; (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height 2))
    ;; (sdl-gl-setswapinterval 1)
    (setf *crates2-gl-context* (sdl-gl-createcontext *crates2-window*))
    (glewinit)
    (check-error "1")
    (glenable +GL-TEXTURE-2D+)
    (gldisable +GL-LIGHTING+)
    ;; (gldisable +GL-BLEND+)
    (glenable +GL-BLEND+)
    (glblendfunc +GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
    (glmatrixmode +GL-PROJECTION+)
    (check-error "2")
    (glloadidentity)
    (check-error "3")
    (glviewport 0 0 screen-width screen-height)
    (check-error "4")
    ;; (gluortho2d 0.0d0 (coerce screen-width 'double-float) 0.0d0 (coerce screen-height 'double-float))
    (gluperspective 45.0d0 (/ (coerce screen-width 'double-float) screen-height) 0.1d0 100.0d0)
    (glulookat 11.0d0 -26.0d0 20.0d0   10.0d0 -10.0d0 0.0d0   0.0d0 0.0d0 1.0d0)
    (check-error "5")
    (glmatrixmode +GL-MODELVIEW+)
    (check-error "6")
    (glloadidentity)
    (setf *image* (img-load "etc/assets/texture/texture32.png"))
    (setf *txids* (foreign-alloc :uint32 :count 1))
    (glgentextures 1 *txids*)
    (glbindtexture +GL-TEXTURE-2D+ (mem-aref *txids* :uint32 0))
    (glenable +GL-TEXTURE-2D+)
    (with-foreign-slots ((pixels) *image* (:struct sdl-surface))
      (glteximage2d +GL-TEXTURE-2D+ 0 +GL-RGBA+ 1024 1024 0 +GL-RGBA+ +GL-UNSIGNED-BYTE+ pixels))
    (gltexparameteri +GL-TEXTURE-2D+ +GL-TEXTURE-MIN-FILTER+ +GL-LINEAR+)
    (check-error "7")
    ;; (setf *texture* (sdl-createtexturefromsurface
    ;;                  *crates2-renderer*
    ;;                  *image*))
    ))

(defun ui-delete ()
    (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
      ;; (sdl-destroytexture *texture*)
      (sdl-freesurface *image*)
      ;; (sdl-destroyrenderer *crates2-renderer*)
      (sdl-destroywindow *crates2-window*)
      (sdl-quit)))

(defun ui-read-input ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (let ((result nil))
      (with-foreign-objects ((event '(:union sdl-event)))
        (loop while (/= (sdl-pollevent event) 0)
              do
                 (cffi:with-foreign-slots ((type) event (:union sdl-event))
                   (cond ((eq type :SDL-KEYDOWN) (cffi:with-foreign-slots ((keysym) event sdl-keyboardevent)
                                                   (let* ((scancode (getf keysym 'scancode))
                                                          (modstate (sdl-getmodstate))
                                                          (shift    (/= (logand modstate +KMOD-SHIFT+) 0)))
                                                     (cond ((eq scancode :SDL-SCANCODE-LEFT)   (setf result (if shift :prev :west)))
                                                           ((eq scancode :SDL-SCANCODE-RIGHT)  (setf result (if shift :next :east)))
                                                           ((eq scancode :SDL-SCANCODE-UP)     (setf result :north))
                                                           ((eq scancode :SDL-SCANCODE-DOWN)   (setf result :south))
                                                           ((eq scancode :SDL-SCANCODE-R)      (setf result :restart))
                                                           ((eq scancode :SDL-SCANCODE-SPACE)  (setf result :action1))
                                                           ((eq scancode :SDL-SCANCODE-B)      (setf result :back))
                                                           ((eq scancode :SDL-SCANCODE-ESCAPE) (setf result :back))))))
                         ((eq type :SDL-QUIT) (setf result :back))))))
      result)))