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
(defconstant cw 64)
(defconstant ch 64)
(defconstant iw 2048)
(defconstant ih 2048)
;; Window dimensions
(defconstant screen-width 800)
(defconstant screen-height 600)

(defparameter *ui-level-number* -123)
(defparameter *ui-program*      "")
(defparameter *ui-par*          -123)

(defparameter *crates2-window* :pointer)
(defparameter *crates2-gl-context* :pointer)
(defparameter *texture-dimensions* (list 0 0))
(defparameter *image* :pointer)
(defparameter *image-array* :pointer)
(defparameter *txids* :pointer)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun make-rect-for-top (tx1 ty1 tx2 ty2 hw delta)
  (let* ((-hw  (- hw))
         (z    (+ hw delta)))
    (list 0.0  0.0  1.0                 ; normal
          tx1  ty2      tx2  ty2      tx2 ty1       tx1 ty1 ; texture coordinates
          -hw -hw  z   hw  -hw  z   hw  hw  z   -hw  hw  z)))

(defun make-rect-for-north (tx1 ty1 tx2 ty2 hw)
  (let* ((-hw  (- hw)))
    (list 0.0  1.0  0.0
          tx1 ty2       tx1 ty1       tx2 ty1      tx2 ty2
          -hw hw -hw   -hw  hw  hw    hw  hw  hw   hw  hw -hw)))

(defun make-rect-for-south (tx1 ty1 tx2 ty2 hw)
  (let* ((-hw  (- hw)))
    (list 0.0  -1.0  0.0
          tx1  ty2       tx2 ty2       tx2 ty1     tx1 ty1
          -hw -hw -hw    hw -hw -hw    hw -hw hw  -hw -hw hw)))

(defun make-rect-for-west (tx1 ty1 tx2 ty2 hw)
  (let* ((-hw  (- hw)))
    (list -1.0  0.0  0.0
          tx1 ty2        tx1 ty1       tx2 ty1     tx2 ty2
          -hw -hw -hw   -hw -hw hw    -hw  hw hw  -hw  hw -hw)))

(defun make-rect-for-bottom (tx1 ty1 tx2 ty2 hw delta)
  (let* ((-hw  (- hw))
         (z    (+ -hw delta)))
    (list 0.0  0.0  1.0
          tx1 ty2        tx2 ty2       tx2 ty1      tx1 ty1
         -hw -hw z       hw -hw z      hw  hw z    -hw  hw z)))

(defun make-rect-for-east (tx1 ty1 tx2 ty2 hw)
  (let* ((-hw  (- hw)))
    (list 1.0  0.0  0.0
          tx1 ty2       tx2 ty2       tx2 ty1     tx1 ty1
          hw -hw -hw    hw  hw -hw    hw  hw hw   hw -hw hw)))

(defun make-rect (index face &optional (hw 0.5))
  (let* ((sprites-per-row (coerce (floor iw cw) 'double-float))
         (tx1 (coerce (/ (* cw (mod index sprites-per-row)) iw) 'double-float))
         (ty1 (coerce (/ (* ch (floor index sprites-per-row)) iw) 'double-float))
         (tw  (coerce (/ cw iw) 'double-float))
         (th  (coerce (/ ch ih) 'double-float))
         (tx2 (coerce (+ tx1 tw) 'double-float))
         (ty2 (coerce (+ ty1 th) 'double-float)))
    (ecase face
      (:top    (make-rect-for-top    tx1 ty1 tx2 ty2 hw 0.0))
      (:bottom (make-rect-for-bottom tx1 ty1 tx2 ty2 hw 0.0))
      (:front  (make-rect-for-south  tx1 ty1 tx2 ty2 hw))
      (:back   (make-rect-for-north  tx1 ty1 tx2 ty2 hw))
      (:east   (make-rect-for-east   tx1 ty1 tx2 ty2 hw))
      (:west   (make-rect-for-west   tx1 ty1 tx2 ty2 hw)))))

(defun make-transparent-top (index &optional (z 0.08))
  (let* ((iwd (coerce iw 'double-float))
         (ihd (coerce ih 'double-float))
         (cwd (coerce cw 'double-float))
         (chd (coerce ch 'double-float))
         (sprites-per-row (coerce (floor iwd cwd) 'double-float))
         (tx1 (coerce (/ (* cwd (coerce (mod index sprites-per-row) 'double-float)) iwd) 'double-float))
         (ty1 (coerce (/ (* chd (coerce (floor index sprites-per-row) 'double-float)) iwd) 'double-float))
         (tw  (coerce (/ cwd iwd) 'double-float))
         (th  (coerce (/ chd ihd) 'double-float))
         (tx2 (coerce (+ tx1 tw) 'double-float))
         (ty2 (coerce (+ ty1 th) 'double-float)))
    (list
     (make-rect-for-top    tx1 ty1 tx2 ty2 0.5 z)
     nil
     nil
     nil
     nil
     nil)))

(defun make-transparent-bottom (index &optional (z 0.08))
  (let* ((sprites-per-row (coerce (floor iw cw) 'double-float))
         (tx1 (coerce (/ (* cw (mod index sprites-per-row)) iw) 'double-float))
         (ty1 (coerce (/ (* ch (floor index sprites-per-row)) iw) 'double-float))
         (tw  (coerce (/ cw iw) 'double-float))
         (th  (coerce (/ ch ih) 'double-float))
         (tx2 (coerce (+ tx1 tw) 'double-float))
         (ty2 (coerce (+ ty1 th) 'double-float)))
    (list
     nil
     (make-rect-for-bottom tx1 ty1 tx2 ty2 0.5 z)
     nil
     nil
     nil
     nil)))

(defun make-cube (top bottom front back east west &optional (hw 0.5))
  (list
   (if top    (make-rect top    :top    hw) nil)
   (if bottom (make-rect bottom :bottom hw) nil)
   (if front  (make-rect front  :front  hw) nil)
   (if back   (make-rect back   :back   hw) nil)
   (if east   (make-rect east   :east   hw) nil)
   (if west   (make-rect west   :west   hw) nil)))

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
  (setf (gethash "wall-idle-00" *visual-hash*) (make-cube 0 nil 0 0 0 0))
  (setf (gethash "wall-idle-01" *visual-hash*) (make-cube 1 nil 1 1 1 1))
  (setf (gethash "wall-idle-02" *visual-hash*) (make-cube 2 nil 2 2 2 2))
  (setf (gethash "wall-idle-03" *visual-hash*) (make-cube 3 nil 3 3 3 3))
  ;; PUSHED
  (setf (gethash "pushed-idle" *visual-hash*) (make-cube 1016 1016 1016 1016 1016 1016 0.39))
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
  (setf (gethash "player-active-00" *visual-hash*) (make-cube 1017 nil 1017 1017 1017 1017 0.39))
  (setf (gethash "player-active-01" *visual-hash*) (make-cube 1018 nil 1018 1018 1018 1018 0.39))
  (setf (gethash "player-active-02" *visual-hash*) (make-cube 1019 nil 1019 1019 1019 1019 0.39))
  (setf (gethash "player-active-03" *visual-hash*) (make-cube 1020 nil 1020 1020 1020 1020 0.39))
  (setf (gethash "player-active-04" *visual-hash*) (make-cube 1021 nil 1021 1021 1021 1021 0.39))
  (setf (gethash "player-active-05" *visual-hash*) (make-cube 1022 nil 1022 1022 1022 1022 0.39))
  (setf (gethash "player-active-06" *visual-hash*) (make-cube 1023 nil 1023 1023 1023 1023 0.39))
  (setf (gethash "player-airborne"  *visual-hash*) (let* ((w 1.0)
                                                          (hw (/ 1.0 2))
                                                          (-hw (- hw))
                                                          (tx1 0.0d0)
                                                          (tx2 (coerce (+ tx1 (/ cw iw)) 'double-float))
                                                          (ty1 (coerce (/ (* 1.0 ch) ih) 'double-float))
                                                          (ty2 (coerce (+ ty1 (/ ch ih)) 'double-float)))
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

  (setf (gethash "player-hidden"    *visual-hash*) (make-cube nil nil nil nil nil nil))
  ;; AUTOMATON
  (setf (gethash "automaton-idle"            *visual-hash*) (make-cube 991 nil 991 991 991 991))
  (setf (gethash "automaton-programming"     *visual-hash*) (make-cube 989 nil 989 989 989 989))
  (setf (gethash "automaton-executing"       *visual-hash*) (make-cube 990 nil 990 990 990 990))
  (setf (gethash "automaton-executing-hover" *visual-hash*) (make-cube 990 nil 990 990 990 990))
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
  ;; BOTTOM NUMBERS
  (setf (gethash "number-bottom-01"  *visual-hash*) (make-transparent-bottom 99))
  (setf (gethash "number-bottom-02"  *visual-hash*) (make-transparent-bottom 100))
  (setf (gethash "number-bottom-03"  *visual-hash*) (make-transparent-bottom 101))
  (setf (gethash "number-bottom-04"  *visual-hash*) (make-transparent-bottom 102))
  (setf (gethash "number-bottom-05"  *visual-hash*) (make-transparent-bottom 103))
  (setf (gethash "number-bottom-06"  *visual-hash*) (make-transparent-bottom 104))
  (setf (gethash "number-bottom-07"  *visual-hash*) (make-transparent-bottom 105))
  (setf (gethash "number-bottom-08"  *visual-hash*) (make-transparent-bottom 106))
  (setf (gethash "number-bottom-09"  *visual-hash*) (make-transparent-bottom 107))
  (setf (gethash "number-bottom-10"  *visual-hash*) (make-transparent-bottom 108))
  (setf (gethash "number-bottom-11"  *visual-hash*) (make-transparent-bottom 131)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-12"  *visual-hash*) (make-transparent-bottom 132))
  (setf (gethash "number-bottom-13"  *visual-hash*) (make-transparent-bottom 133))
  (setf (gethash "number-bottom-14"  *visual-hash*) (make-transparent-bottom 134))
  (setf (gethash "number-bottom-15"  *visual-hash*) (make-transparent-bottom 135))
  (setf (gethash "number-bottom-16"  *visual-hash*) (make-transparent-bottom 136))
  (setf (gethash "number-bottom-17"  *visual-hash*) (make-transparent-bottom 137))
  (setf (gethash "number-bottom-18"  *visual-hash*) (make-transparent-bottom 138))
  (setf (gethash "number-bottom-19"  *visual-hash*) (make-transparent-bottom 139))
  (setf (gethash "number-bottom-20"  *visual-hash*) (make-transparent-bottom 140))
  (setf (gethash "number-bottom-21"  *visual-hash*) (make-transparent-bottom 163)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-22"  *visual-hash*) (make-transparent-bottom 164))
  (setf (gethash "number-bottom-23"  *visual-hash*) (make-transparent-bottom 165))
  (setf (gethash "number-bottom-24"  *visual-hash*) (make-transparent-bottom 166))
  (setf (gethash "number-bottom-25"  *visual-hash*) (make-transparent-bottom 167))
  (setf (gethash "number-bottom-26"  *visual-hash*) (make-transparent-bottom 168))
  (setf (gethash "number-bottom-27"  *visual-hash*) (make-transparent-bottom 169))
  (setf (gethash "number-bottom-28"  *visual-hash*) (make-transparent-bottom 170))
  (setf (gethash "number-bottom-29"  *visual-hash*) (make-transparent-bottom 171))
  (setf (gethash "number-bottom-30"  *visual-hash*) (make-transparent-bottom 172))
  (setf (gethash "number-bottom-31"  *visual-hash*) (make-transparent-bottom 195)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-32"  *visual-hash*) (make-transparent-bottom 196))
  (setf (gethash "number-bottom-33"  *visual-hash*) (make-transparent-bottom 197))
  (setf (gethash "number-bottom-34"  *visual-hash*) (make-transparent-bottom 198))
  (setf (gethash "number-bottom-35"  *visual-hash*) (make-transparent-bottom 199))
  (setf (gethash "number-bottom-36"  *visual-hash*) (make-transparent-bottom 200))
  (setf (gethash "number-bottom-37"  *visual-hash*) (make-transparent-bottom 201))
  (setf (gethash "number-bottom-38"  *visual-hash*) (make-transparent-bottom 202))
  (setf (gethash "number-bottom-39"  *visual-hash*) (make-transparent-bottom 203))
  (setf (gethash "number-bottom-40"  *visual-hash*) (make-transparent-bottom 204))
  (setf (gethash "number-bottom-41"  *visual-hash*) (make-transparent-bottom 227)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-42"  *visual-hash*) (make-transparent-bottom 228))
  (setf (gethash "number-bottom-43"  *visual-hash*) (make-transparent-bottom 229))
  (setf (gethash "number-bottom-44"  *visual-hash*) (make-transparent-bottom 230))
  (setf (gethash "number-bottom-45"  *visual-hash*) (make-transparent-bottom 231))
  (setf (gethash "number-bottom-46"  *visual-hash*) (make-transparent-bottom 232))
  (setf (gethash "number-bottom-47"  *visual-hash*) (make-transparent-bottom 233))
  (setf (gethash "number-bottom-48"  *visual-hash*) (make-transparent-bottom 234))
  (setf (gethash "number-bottom-49"  *visual-hash*) (make-transparent-bottom 235))
  (setf (gethash "number-bottom-50"  *visual-hash*) (make-transparent-bottom 236))
  (setf (gethash "number-bottom-51"  *visual-hash*) (make-transparent-bottom 259)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-52"  *visual-hash*) (make-transparent-bottom 260))
  (setf (gethash "number-bottom-53"  *visual-hash*) (make-transparent-bottom 261))
  (setf (gethash "number-bottom-54"  *visual-hash*) (make-transparent-bottom 262))
  (setf (gethash "number-bottom-55"  *visual-hash*) (make-transparent-bottom 263))
  (setf (gethash "number-bottom-56"  *visual-hash*) (make-transparent-bottom 264))
  (setf (gethash "number-bottom-57"  *visual-hash*) (make-transparent-bottom 265))
  (setf (gethash "number-bottom-58"  *visual-hash*) (make-transparent-bottom 266))
  (setf (gethash "number-bottom-59"  *visual-hash*) (make-transparent-bottom 267))
  (setf (gethash "number-bottom-60"  *visual-hash*) (make-transparent-bottom 268))
  (setf (gethash "number-bottom-61"  *visual-hash*) (make-transparent-bottom 291)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-62"  *visual-hash*) (make-transparent-bottom 292))
  (setf (gethash "number-bottom-63"  *visual-hash*) (make-transparent-bottom 293))
  (setf (gethash "number-bottom-64"  *visual-hash*) (make-transparent-bottom 294))
  (setf (gethash "number-bottom-65"  *visual-hash*) (make-transparent-bottom 295))
  (setf (gethash "number-bottom-66"  *visual-hash*) (make-transparent-bottom 296))
  (setf (gethash "number-bottom-67"  *visual-hash*) (make-transparent-bottom 297))
  (setf (gethash "number-bottom-68"  *visual-hash*) (make-transparent-bottom 298))
  (setf (gethash "number-bottom-69"  *visual-hash*) (make-transparent-bottom 299))
  (setf (gethash "number-bottom-70"  *visual-hash*) (make-transparent-bottom 300))
  (setf (gethash "number-bottom-71"  *visual-hash*) (make-transparent-bottom 323)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-72"  *visual-hash*) (make-transparent-bottom 324))
  (setf (gethash "number-bottom-73"  *visual-hash*) (make-transparent-bottom 325))
  (setf (gethash "number-bottom-74"  *visual-hash*) (make-transparent-bottom 326))
  (setf (gethash "number-bottom-75"  *visual-hash*) (make-transparent-bottom 327))
  (setf (gethash "number-bottom-76"  *visual-hash*) (make-transparent-bottom 328))
  (setf (gethash "number-bottom-77"  *visual-hash*) (make-transparent-bottom 329))
  (setf (gethash "number-bottom-78"  *visual-hash*) (make-transparent-bottom 330))
  (setf (gethash "number-bottom-79"  *visual-hash*) (make-transparent-bottom 331))
  (setf (gethash "number-bottom-80"  *visual-hash*) (make-transparent-bottom 332))
  (setf (gethash "number-bottom-81"  *visual-hash*) (make-transparent-bottom 355)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-82"  *visual-hash*) (make-transparent-bottom 356))
  (setf (gethash "number-bottom-83"  *visual-hash*) (make-transparent-bottom 357))
  (setf (gethash "number-bottom-84"  *visual-hash*) (make-transparent-bottom 358))
  (setf (gethash "number-bottom-85"  *visual-hash*) (make-transparent-bottom 359))
  (setf (gethash "number-bottom-86"  *visual-hash*) (make-transparent-bottom 360))
  (setf (gethash "number-bottom-87"  *visual-hash*) (make-transparent-bottom 361))
  (setf (gethash "number-bottom-88"  *visual-hash*) (make-transparent-bottom 362))
  (setf (gethash "number-bottom-89"  *visual-hash*) (make-transparent-bottom 363))
  (setf (gethash "number-bottom-90"  *visual-hash*) (make-transparent-bottom 364))
  (setf (gethash "number-bottom-91"  *visual-hash*) (make-transparent-bottom 387)) ; Add 32 to start of previous line.
  (setf (gethash "number-bottom-92"  *visual-hash*) (make-transparent-bottom 388))
  (setf (gethash "number-bottom-93"  *visual-hash*) (make-transparent-bottom 389))
  (setf (gethash "number-bottom-94"  *visual-hash*) (make-transparent-bottom 390))
  (setf (gethash "number-bottom-95"  *visual-hash*) (make-transparent-bottom 391))
  (setf (gethash "number-bottom-96"  *visual-hash*) (make-transparent-bottom 392))
  (setf (gethash "number-bottom-97"  *visual-hash*) (make-transparent-bottom 393))
  (setf (gethash "number-bottom-98"  *visual-hash*) (make-transparent-bottom 394))
  (setf (gethash "number-bottom-99"  *visual-hash*) (make-transparent-bottom 395))
  (setf (gethash "number-bottom-100" *visual-hash*) (make-transparent-bottom 396))
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter"    *visual-hash*) (make-cube 0 nil 0 0 0 0))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter"     *visual-hash*) (make-cube nil 4 nil nil nil nil))
  ;; PASS-TIMER
  (setf (gethash "pass-timer"       *visual-hash*) (make-cube nil 4 nil nil nil nil))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) (make-cube 1016 nil 1016 1016 1016 1016))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) (make-transparent-top 11))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) (make-transparent-top 7))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) (make-cube nil nil nil nil nil nil))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) (make-transparent-top 13))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) (make-transparent-top 9))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) (make-cube nil nil nil nil nil nil))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) (make-transparent-top 12))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) (make-transparent-top 8))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) (make-cube nil nil nil nil nil nil))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) (make-transparent-top 14))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) (make-transparent-top 10))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) (make-cube nil nil nil nil nil nil))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) (make-transparent-top 74))
  (setf (gethash "stepper-active"             *visual-hash*) (make-transparent-top 75))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) (make-cube 39 nil 1015 1015 1015 1015))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) (make-transparent-top 40))
  (setf (gethash "toggle-east-off"            *visual-hash*) (make-transparent-top 50))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) (make-transparent-top 42))
  (setf (gethash "toggle-west-off"            *visual-hash*) (make-transparent-top 50))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) (make-transparent-top 41))
  (setf (gethash "toggle-north-off"           *visual-hash*) (make-transparent-top 50))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) (make-transparent-top 43))
  (setf (gethash "toggle-south-off"           *visual-hash*) (make-transparent-top 50))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) (make-cube 76 76 76 76 76 76))
  (setf (gethash "bomb"          *visual-hash*) (make-cube 77 77 77 77 77 77))
  ;; ring 1
  (setf (gethash "bomb-ring-1"   *visual-hash*) (list
                                                 (let* ((hw 0.5)
                                                        (-hw (- hw))
                                                        (tx1 0.0d0)
                                                        (tx2 (coerce (+ tx1 (/ cw iw)) 'double-float))
                                                        (ty1 (coerce (/ (* 16.0 ch) ih) 'double-float))
                                                        (ty2 (coerce (+ ty1 (/ ch ih)) 'double-float)))
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
                                                        (tx1 0.0d0)
                                                        (tx2 (coerce (+ tx1 (/ (* 3 cw) iw)) 'double-float))
                                                        (ty1 (coerce (/ (* 17.0 ch) ih) 'double-float))
                                                        (ty2 (coerce (+ ty1 (/ (* 3 ch) ih)) 'double-float)))
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
                                                        (tx1 0.0d0)
                                                        (tx2 (coerce (+ tx1 (/ (* 5 cw) iw)) 'double-float))
                                                        (ty1 (coerce (/ (* 20.0 ch) ih) 'double-float))
                                                        (ty2 (coerce (+ ty1 (/ (* 5 ch) ih)) 'double-float)))
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
                                                        (tx1 0.0d0)
                                                        (tx2 (coerce (+ tx1 (/ (* 5 cw) iw)) 'double-float))
                                                        (ty1 (coerce (/ (* 25.0 ch) ih) 'double-float))
                                                        (ty2 (coerce (+ ty1 (/ (* 5 ch) ih)) 'double-float)))
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
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (let ((err (glgeterror)))
      (unless (=  err +GL-NO-ERROR+)
        (error "GL ERROR: ~A (~A)~%" (gluErrorString err) msg)))))

(defun ui-render-face (face)
  ;; TODO this could be made more efficient by using foreign
  ;; arrays and function(s) operating on them.
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
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
      (gltexcoord2d t1 t2) (glvertex3f v1  v2  v3)
      (gltexcoord2d t3 t4) (glvertex3f v4  v5  v6)
      (gltexcoord2d t5 t6) (glvertex3f v7  v8  v9)
      (gltexcoord2d t7 t8) (glvertex3f v10 v11 v12))))

(defun ui-render-cube (faces)
  ;; top bottom front back east west
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
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

      (glend))))

(defun enter-ortho ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (glpushattrib (logior +GL-DEPTH-BUFFER-BIT+ +GL-ENABLE-BIT+))
    (glpushmatrix)
    (glloadidentity)
    (glmatrixmode +GL-PROJECTION+)
    (glpushmatrix)
    (glloadidentity)
    (glortho 0.0d0 (coerce screen-width 'double-float) 0.0d0 (coerce screen-height 'double-float) -1.0d0 1.0d0)
    (gldisable +GL-DEPTH-TEST+)
    (glEnable +GL-BLEND+)))

(defun leave-ortho ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (gldisable +GL-BLEND+)
    (glpopmatrix)
    (glmatrixmode +GL-MODELVIEW+)
    (glpopmatrix)
    (glpopattrib)))

(defun ui-render-impl (level step)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (ensure-text-texture crates2:*level-number* (crates2:num-levels) (car crates2:*infos*) (cadr crates2:*infos*) (crates2:current-par))
    (glclearcolor 0.0 0.0 0.0 1.0)
    (glclear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
    (glenable +GL-TEXTURE-2D+)
    (glbindtexture +GL-TEXTURE-2D+ (mem-aref *txids* :uint32 0))
    (glenable +GL-TEXTURE-2D+)
    (glenable  +GL-DEPTH-TEST+)
    (gldisable +GL-CULL-FACE+)
    (gldisable +GL-ALPHA-TEST+)
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
                 (let* ((x (if (and (= step 0) (typep crate 'crates2:moving)) (crates2:tail-x crate) (crates2:crate-x crate)))
                        (y (if (and (= step 0) (typep crate 'crates2:moving)) (crates2:tail-y crate) (crates2:crate-y crate)))
                        (z (if (and (= step 0) (typep crate 'crates2:moving)) (crates2:tail-z crate) (crates2:crate-z crate)))
                        (vids (crates2:visual crate)))
                   (glenable +GL-DEPTH-TEST+)
                   (loop for vid in vids
                         for i from 0
                         do
                            (let ((viv (gethash vid *visual-hash*)))
                              (when viv
                                (when (= i 1)
                                  (gldisable +GL-DEPTH-TEST+))
                                (glpushmatrix)
                                (gltranslatef (float x) (- (float y)) (float z))
                                (ui-render-cube viv)
                                (glpopmatrix)))))))
      ;; Text
      (glbindtexture +GL-TEXTURE-2D+ (mem-aref *txids* :uint32 1))
      (enter-ortho)
      (ui-update-hint-time)
      (let* ((w  (float (car  *texture-dimensions*)))
             (h  (float (cadr *texture-dimensions*)))
             (d  (- (float (ceiling (* (ui-ease *ui-hint-time*) h))) h))
             (d2 (+ d h)))
        (glbegin +GL-QUADS+)
        (gltexcoord2d 0.0d0 1.0d0) (glvertex3f 0.0  d   0.0)
        (gltexcoord2d 1.0d0 1.0d0) (glvertex3f w    d   0.0)
        (gltexcoord2d 1.0d0 0.0d0) (glvertex3f w    d2  0.0)
        (gltexcoord2d 0.0d0 0.0d0) (glvertex3f 0.0  d2  0.0)
        (glend))
      (leave-ortho)
      (sdl-gl-swapwindow *crates2-window*)
      (check-error))))

(defun ui-render (level step)
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (ui-render-impl level step))
  #+ccl
  (ui-render-impl level step))

(defun ui-look-at (x y m minx miny maxx maxy)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (glmatrixmode +GL-PROJECTION+)
    (glHint +GL-PERSPECTIVE-CORRECTION-HINT+ +GL-NICEST+)
    (glloadidentity)
    (glviewport 0 0 screen-width screen-height)
    (gluperspective 45.0d0 (/ (coerce screen-width 'double-float) screen-height) 0.1d0 80.0d0)
    (let* ((xd (coerce x 'double-float))
           (yd (- (coerce y 'double-float)))
           (ex xd)
           (ey (coerce (- (- maxy) 6) 'double-float))
           (ez (* (coerce (- maxx minx) 'double-float) 1.5)))
      (glulookat ex          ey          ez
                 xd          yd          1.0d0
                 0.0d0       0.0d0       1.0d0))
    (glmatrixmode +GL-MODELVIEW+)))

(defun ensure-text-texture (level-number num-levels level-name level-hint par)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (when (and num-levels (or (/= *ui-par* par)
                              (/= *ui-level-number* level-number)
                              (string/= crates2:*program* *ui-program*)))
      (with-surface (text-surface)
        (with-foreign-objects ((nullpointer :pointer))
          (setf nullpointer (null-pointer))
          (with-foreign-string (text (ui-sdl2-format-info-message crates2:*program* level-number num-levels level-name level-hint par))
            (setf text-surface (sdl-convertsurfaceformat
                                (ttf-renderutf8-blended-wrapped *IBMPlexMono-Bold* text (list 143 125 94 255) screen-width)
                                +SDL-PIXELFORMAT-RGBA+ 0))
            (glbindtexture +GL-TEXTURE-2D+ (mem-aref *txids* :uint32 1))
            (with-foreign-slots ((pixels w h) text-surface (:struct sdl-surface))
              (setf *texture-dimensions* (list w h))
              (glteximage2d +GL-TEXTURE-2D+ 0 +GL-RGBA+ w h 0 +GL-RGBA+ +GL-UNSIGNED-BYTE+ pixels))
            (glgeneratemipmap +GL-TEXTURE-2D+))))
      (setf *ui-program* crates2:*program*
            *ui-level-number* level-number
            *ui-par* par))))

(defun ui-init-impl (options)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (ui-common-init)
    (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height (logior (foreign-enum-value 'sdl-windowflags :SDL-WINDOW-OPENGL) (foreign-enum-value 'sdl-windowflags :SDL-WINDOW-SHOWN))))
    (setf *crates2-gl-context* (sdl-gl-createcontext *crates2-window*))
    (ui-sdl2-load-font)
    (when (getf options :fullscreen)
      (sdl-setwindowfullscreen *crates2-window* +SDL-TRUE+))
    (glewinit)
    (format t "OpenGL version is ~A~%" (glgetstring +GL-VERSION+))
    (glenable +GL-TEXTURE-2D+)
    (gldisable +GL-LIGHTING+)
    (glenable +GL-BLEND+)
    (glblendfunc +GL-SRC-ALPHA+ +GL-ONE-MINUS-SRC-ALPHA+)
    (ui-look-at 10 10 10 0 0 0 0)
    (glmatrixmode +GL-MODELVIEW+)
    (glloadidentity)
    (setf *image-array* (foreign-alloc :uint8 :count (length *texture64.png*) :initial-contents *texture64.png*))
    (setf *image* (img-load-rw (sdl-rwfromconstmem *image-array* (length *texture64.png*)) -1))
    (setf *txids* (foreign-alloc :uint32 :count 2))
    (glgentextures 2 *txids*)
    (glbindtexture +GL-TEXTURE-2D+ (mem-aref *txids* :uint32 0))
    (glenable +GL-TEXTURE-2D+)
    (gltexparameteri +GL-TEXTURE-2D+ +GL-TEXTURE-MIN-FILTER+ +GL-LINEAR-MIPMAP-LINEAR+)
    (gltexparameteri +GL-TEXTURE-2D+ +GL-TEXTURE-MAG-FILTER+ +GL-LINEAR+)
    (gltexparameteri +GL-TEXTURE-2D+ +GL-GENERATE-MIPMAP+ +GL-TRUE+)
    (with-foreign-slots ((pixels) *image* (:struct sdl-surface))
      (glteximage2d +GL-TEXTURE-2D+ 0 +GL-RGBA+ iw ih 0 +GL-RGBA+ +GL-UNSIGNED-BYTE+ pixels))
    (glgeneratemipmap +GL-TEXTURE-2D+)
    (check-error)))

(defun ui-init (options)
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (ui-init-impl options))
  #+ccl
  (ui-init-impl options))

(defun ui-delete-impl ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (ui-common-delete)
    ;; (sdl-destroytexture *texture*)
    (ttf-closefont *IBMPlexMono-Bold*)
    (foreign-free *IBMPlexMono-Bold-array* )
    (setf *IBMPlexMono-Bold-array* (null-pointer))
    (ttf-quit)
    (ui-close-audio)
    (sdl-freesurface *image*)
    (foreign-free *image-array*)
    (setf *image-array* (null-pointer))
    (sdl-destroywindow *crates2-window*)
    (sdl-quit)))

(defun ui-delete ()
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (ui-delete-impl))
  #+ccl
  (ui-delete-impl))

(defun ui-read-input-impl ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
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

(defun ui-read-input ()
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (ui-read-input-impl))
  #+ccl
  (ui-read-input-impl))