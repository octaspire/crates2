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
;; Window dimensions
(defconstant screen-width 800)
(defconstant screen-height 600)

(defparameter *crates2-window* :pointer)
(defparameter *crates2-gl-context* :pointer)
;; (defparameter *crates2-renderer* :pointer)
(defparameter *image* :pointer)
;; (defparameter *texture* :pointer)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun make-rect (index)
  (let* ((sprites-per-row (floor iw cw))
         (x (* cw (mod index sprites-per-row)))
         (y (* ch (floor index sprites-per-row))))
    (list x y cw ch)))

(defun make-cube (top bottom front back east west)
  (list
    (if top    (make-rect top)    nil)
    (if bottom (make-rect bottom) nil)
    (if front  (make-rect front)  nil)
    (if back   (make-rect back)   nil)
    (if east   (make-rect east)   nil)
    (if west   (make-rect west)   nil)))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-body" *visual-hash*) (make-cube nil 482 nil nil nil nil))
  (setf (gethash "gear-00"     *visual-hash*) (make-cube nil 483 nil nil nil nil))
  (setf (gethash "gear-01"     *visual-hash*) (make-cube nil 484 nil nil nil nil))
  (setf (gethash "gear-02"     *visual-hash*) (make-cube nil 485 nil nil nil nil))
  (setf (gethash "gear-03"     *visual-hash*) (make-cube nil 486 nil nil nil nil))
  (setf (gethash "gear-04"     *visual-hash*) (make-cube nil 487 nil nil nil nil))
  (setf (gethash "gear-05"     *visual-hash*) (make-cube nil 488 nil nil nil nil))
  (setf (gethash "gear-06"     *visual-hash*) (make-cube nil 489 nil nil nil nil))
  (setf (gethash "gear-07"     *visual-hash*) (make-cube nil 490 nil nil nil nil))
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
  ;; PLAYER
  (setf (gethash "player-active-00" *visual-hash*) (make-cube 32 32 32 32 32 32))
  (setf (gethash "player-active-01" *visual-hash*) (make-cube 33 33 33 33 33 33))
  (setf (gethash "player-active-02" *visual-hash*) (make-cube 34 34 34 34 34 34))
  (setf (gethash "player-active-03" *visual-hash*) (make-cube 35 35 35 35 35 35))
  (setf (gethash "player-active-04" *visual-hash*) (make-cube 36 36 36 36 36 36))
  (setf (gethash "player-active-05" *visual-hash*) (make-cube 37 37 37 37 37 37))
  (setf (gethash "player-active-06" *visual-hash*) (make-cube 38 38 38 38 38 38))
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
  (setf (gethash "number-01"        *visual-hash*) (make-cube 99 nil nil nil nil nil))
  (setf (gethash "number-02"        *visual-hash*) (make-cube 100 nil nil nil nil nil))
  (setf (gethash "number-03"        *visual-hash*) (make-cube 101 nil nil nil nil nil))
  (setf (gethash "number-04"        *visual-hash*) (make-cube 102 nil nil nil nil nil))
  (setf (gethash "number-05"        *visual-hash*) (make-cube 103 nil nil nil nil nil))
  (setf (gethash "number-06"        *visual-hash*) (make-cube 104 nil nil nil nil nil))
  (setf (gethash "number-07"        *visual-hash*) (make-cube 105 nil nil nil nil nil))
  (setf (gethash "number-08"        *visual-hash*) (make-cube 106 nil nil nil nil nil))
  (setf (gethash "number-09"        *visual-hash*) (make-cube 107 nil nil nil nil nil))
  (setf (gethash "number-10"        *visual-hash*) (make-cube 108 nil nil nil nil nil))
  (setf (gethash "number-11"        *visual-hash*) (make-cube 131 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-12"        *visual-hash*) (make-cube 132 nil nil nil nil nil))
  (setf (gethash "number-13"        *visual-hash*) (make-cube 133 nil nil nil nil nil))
  (setf (gethash "number-14"        *visual-hash*) (make-cube 134 nil nil nil nil nil))
  (setf (gethash "number-15"        *visual-hash*) (make-cube 135 nil nil nil nil nil))
  (setf (gethash "number-16"        *visual-hash*) (make-cube 136 nil nil nil nil nil))
  (setf (gethash "number-17"        *visual-hash*) (make-cube 137 nil nil nil nil nil))
  (setf (gethash "number-18"        *visual-hash*) (make-cube 138 nil nil nil nil nil))
  (setf (gethash "number-19"        *visual-hash*) (make-cube 139 nil nil nil nil nil))
  (setf (gethash "number-20"        *visual-hash*) (make-cube 140 nil nil nil nil nil))
  (setf (gethash "number-21"        *visual-hash*) (make-cube 163 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-22"        *visual-hash*) (make-cube 164 nil nil nil nil nil))
  (setf (gethash "number-23"        *visual-hash*) (make-cube 165 nil nil nil nil nil))
  (setf (gethash "number-24"        *visual-hash*) (make-cube 166 nil nil nil nil nil))
  (setf (gethash "number-25"        *visual-hash*) (make-cube 167 nil nil nil nil nil))
  (setf (gethash "number-26"        *visual-hash*) (make-cube 168 nil nil nil nil nil))
  (setf (gethash "number-27"        *visual-hash*) (make-cube 169 nil nil nil nil nil))
  (setf (gethash "number-28"        *visual-hash*) (make-cube 170 nil nil nil nil nil))
  (setf (gethash "number-29"        *visual-hash*) (make-cube 171 nil nil nil nil nil))
  (setf (gethash "number-30"        *visual-hash*) (make-cube 172 nil nil nil nil nil))
  (setf (gethash "number-31"        *visual-hash*) (make-cube 195 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-32"        *visual-hash*) (make-cube 196 nil nil nil nil nil))
  (setf (gethash "number-33"        *visual-hash*) (make-cube 197 nil nil nil nil nil))
  (setf (gethash "number-34"        *visual-hash*) (make-cube 198 nil nil nil nil nil))
  (setf (gethash "number-35"        *visual-hash*) (make-cube 199 nil nil nil nil nil))
  (setf (gethash "number-36"        *visual-hash*) (make-cube 200 nil nil nil nil nil))
  (setf (gethash "number-37"        *visual-hash*) (make-cube 201 nil nil nil nil nil))
  (setf (gethash "number-38"        *visual-hash*) (make-cube 202 nil nil nil nil nil))
  (setf (gethash "number-39"        *visual-hash*) (make-cube 203 nil nil nil nil nil))
  (setf (gethash "number-40"        *visual-hash*) (make-cube 204 nil nil nil nil nil))
  (setf (gethash "number-41"        *visual-hash*) (make-cube 227 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-42"        *visual-hash*) (make-cube 228 nil nil nil nil nil))
  (setf (gethash "number-43"        *visual-hash*) (make-cube 229 nil nil nil nil nil))
  (setf (gethash "number-44"        *visual-hash*) (make-cube 230 nil nil nil nil nil))
  (setf (gethash "number-45"        *visual-hash*) (make-cube 231 nil nil nil nil nil))
  (setf (gethash "number-46"        *visual-hash*) (make-cube 232 nil nil nil nil nil))
  (setf (gethash "number-47"        *visual-hash*) (make-cube 233 nil nil nil nil nil))
  (setf (gethash "number-48"        *visual-hash*) (make-cube 234 nil nil nil nil nil))
  (setf (gethash "number-49"        *visual-hash*) (make-cube 235 nil nil nil nil nil))
  (setf (gethash "number-50"        *visual-hash*) (make-cube 236 nil nil nil nil nil))
  (setf (gethash "number-51"        *visual-hash*) (make-cube 259 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-52"        *visual-hash*) (make-cube 260 nil nil nil nil nil))
  (setf (gethash "number-53"        *visual-hash*) (make-cube 261 nil nil nil nil nil))
  (setf (gethash "number-54"        *visual-hash*) (make-cube 262 nil nil nil nil nil))
  (setf (gethash "number-55"        *visual-hash*) (make-cube 263 nil nil nil nil nil))
  (setf (gethash "number-56"        *visual-hash*) (make-cube 264 nil nil nil nil nil))
  (setf (gethash "number-57"        *visual-hash*) (make-cube 265 nil nil nil nil nil))
  (setf (gethash "number-58"        *visual-hash*) (make-cube 266 nil nil nil nil nil))
  (setf (gethash "number-59"        *visual-hash*) (make-cube 267 nil nil nil nil nil))
  (setf (gethash "number-60"        *visual-hash*) (make-cube 268 nil nil nil nil nil))
  (setf (gethash "number-61"        *visual-hash*) (make-cube 291 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-62"        *visual-hash*) (make-cube 292 nil nil nil nil nil))
  (setf (gethash "number-63"        *visual-hash*) (make-cube 293 nil nil nil nil nil))
  (setf (gethash "number-64"        *visual-hash*) (make-cube 294 nil nil nil nil nil))
  (setf (gethash "number-65"        *visual-hash*) (make-cube 295 nil nil nil nil nil))
  (setf (gethash "number-66"        *visual-hash*) (make-cube 296 nil nil nil nil nil))
  (setf (gethash "number-67"        *visual-hash*) (make-cube 297 nil nil nil nil nil))
  (setf (gethash "number-68"        *visual-hash*) (make-cube 298 nil nil nil nil nil))
  (setf (gethash "number-69"        *visual-hash*) (make-cube 299 nil nil nil nil nil))
  (setf (gethash "number-70"        *visual-hash*) (make-cube 300 nil nil nil nil nil))
  (setf (gethash "number-71"        *visual-hash*) (make-cube 323 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-72"        *visual-hash*) (make-cube 324 nil nil nil nil nil))
  (setf (gethash "number-73"        *visual-hash*) (make-cube 325 nil nil nil nil nil))
  (setf (gethash "number-74"        *visual-hash*) (make-cube 326 nil nil nil nil nil))
  (setf (gethash "number-75"        *visual-hash*) (make-cube 327 nil nil nil nil nil))
  (setf (gethash "number-76"        *visual-hash*) (make-cube 328 nil nil nil nil nil))
  (setf (gethash "number-77"        *visual-hash*) (make-cube 329 nil nil nil nil nil))
  (setf (gethash "number-78"        *visual-hash*) (make-cube 330 nil nil nil nil nil))
  (setf (gethash "number-79"        *visual-hash*) (make-cube 331 nil nil nil nil nil))
  (setf (gethash "number-80"        *visual-hash*) (make-cube 332 nil nil nil nil nil))
  (setf (gethash "number-81"        *visual-hash*) (make-cube 355 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-82"        *visual-hash*) (make-cube 356 nil nil nil nil nil))
  (setf (gethash "number-83"        *visual-hash*) (make-cube 357 nil nil nil nil nil))
  (setf (gethash "number-84"        *visual-hash*) (make-cube 358 nil nil nil nil nil))
  (setf (gethash "number-85"        *visual-hash*) (make-cube 359 nil nil nil nil nil))
  (setf (gethash "number-86"        *visual-hash*) (make-cube 360 nil nil nil nil nil))
  (setf (gethash "number-87"        *visual-hash*) (make-cube 361 nil nil nil nil nil))
  (setf (gethash "number-88"        *visual-hash*) (make-cube 362 nil nil nil nil nil))
  (setf (gethash "number-89"        *visual-hash*) (make-cube 363 nil nil nil nil nil))
  (setf (gethash "number-90"        *visual-hash*) (make-cube 364 nil nil nil nil nil))
  (setf (gethash "number-91"        *visual-hash*) (make-cube 387 nil nil nil nil nil)) ; Add 32 to start of previous line.
  (setf (gethash "number-92"        *visual-hash*) (make-cube 388 nil nil nil nil nil))
  (setf (gethash "number-93"        *visual-hash*) (make-cube 389 nil nil nil nil nil))
  (setf (gethash "number-94"        *visual-hash*) (make-cube 390 nil nil nil nil nil))
  (setf (gethash "number-95"        *visual-hash*) (make-cube 391 nil nil nil nil nil))
  (setf (gethash "number-96"        *visual-hash*) (make-cube 392 nil nil nil nil nil))
  (setf (gethash "number-97"        *visual-hash*) (make-cube 393 nil nil nil nil nil))
  (setf (gethash "number-98"        *visual-hash*) (make-cube 394 nil nil nil nil nil))
  (setf (gethash "number-99"        *visual-hash*) (make-cube 395 nil nil nil nil nil))
  (setf (gethash "number-100"       *visual-hash*) (make-cube 396 nil nil nil nil nil))
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
                                                 (list 0 (* 16 ch) (* 1 cw) (* 1 ch))
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil))) ; start at index 256 (y slot 16); size 1 slot
  ;; ring 2
  (setf (gethash "bomb-ring-2"   *visual-hash*) (list
                                                 (list 0 (* 17 ch) (* 3 cw) (* 3 ch))
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil))) ; start at y slot 17; size 3x3 slots
  ;; ring 3
  (setf (gethash "bomb-ring-3"   *visual-hash*) (list
                                                 (list 0 (* 20 ch) (* 5 cw) (* 5 ch))
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil))) ; size 5x5 slots
  ;; ring 4
  (setf (gethash "bomb-ring-4"   *visual-hash*) (list
                                                 (list 0 (* 25 ch) (* 5 cw) (* 5 ch))
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)
                                                 (list nil nil nil nil nil nil)))) ; size 5x5 slots

(defparameter *fake-input* nil)

(defun draw-face (index x y z)
  )

(defun check-error (&optional msg)
  (let ((err (glgeterror)))
    (unless (=  err +GL-NO-ERROR+)
      (error "GL ERROR: ~A (~A)~%" (gluErrorString err) msg))))

(defun ui-render-cube (faces)
  (glbegin +GL-QUADS+)
  (gltexcoord2f 0.0   1.0) (glvertex3f -0.5  -0.5  0.5)
  (gltexcoord2f 0.125 1.0) (glvertex3f  0.5  -0.5  0.5)
  (gltexcoord2f 0.125 0.0) (glvertex3f  0.5   0.5  0.5)
  (gltexcoord2f 0.0   0.0) (glvertex3f -0.5   0.5  0.5)
  (glend)
  (check-error))

(defun ui-render (level step)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    ;; (sdl-setrenderdrawcolor *crates2-renderer* #x00 #x00 #x00 #xFF)
    (check-error)
    (glclearcolor 0.0 0.0 0.0 1.0)
    (glclear (logior +GL-COLOR-BUFFER-BIT+ +GL-DEPTH-BUFFER-BIT+))
    (glenable +GL-TEXTURE-2D+)
    (gldisable +GL-DEPTH-TEST+)
    (gldisable +GL-CULL-FACE+)
    (gldisable +GL-ALPHA-TEST+)
    ;; (gltranslatef 5.0 5.0 -5.0)
    (check-error)

    (glcolor4f   1.0 1.0 1.0 1.0)
    ;; (sdl-renderclear *crates2-renderer*)
    ;; (sdl-setrenderdrawcolor *crates2-renderer* #xBA #x16 #x0C #xFF)
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
                                (glpopmatrix)

                                ;; (let* ((rx (first viv))
                                ;;        (ry (second viv))
                                ;;        (vivw (third viv))
                                ;;        (vivh (fourth viv))
                                ;;        (dx (floor (- cw vivw) 2))
                                ;;        (dy (floor (- ch vivh) 2))
                                ;;        (finy (+ (* y ch) dy))
                                ;;        (deltax (+ (* x cw) dx)))
                                ;;   (set-rect rect1 rx ry vivw vivh)
                                ;;   (set-rect rect2 deltax finy vivw vivh)
                                ;;   (glpushmatrix)
                                ;;   (gltranslatef deltax finy z)
                                ;;   (ui-render-cube viv)
                                ;;   (glpopmatrix)
                                ;;   ;; (sdl-rendercopy *crates2-renderer* *texture* rect1pointer rect2pointer)
                                ;;   )
                                ))))))
      ;; (sdl-renderpresent *crates2-renderer*)
      (sdl-gl-swapwindow *crates2-window*)
      (check-error))))

(defun ui-init ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (sdl-init +SDL-INIT-VIDEO+)
    ;; (sdl-gl-setattribute :SDL-GL-CONTEXT-MAJOR-VERSION 3)
    ;; (sdl-gl-setattribute :SDL-GL-CONTEXT-MINOR-VERSION 1)
    ;; (sdl-gl-setattribute :SDL-GL-CONTEXT-PROFILE-MASK (foreign-enum-value 'sdl-glprofile :SDL-GL-CONTEXT-PROFILE-CORE))
    ;; (sdl-gl-setattribute :SDL-GL-DOUBLEBUFFER 1)
    (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height (logior (foreign-enum-value 'sdl-windowflags :SDL-WINDOW-OPENGL) (foreign-enum-value 'sdl-windowflags :SDL-WINDOW-SHOWN))))
    ;; (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height 2))
    ;; (sdl-gl-setswapinterval 1)
    (setf *crates2-gl-context* (sdl-gl-createcontext *crates2-window*))
    (glewinit)
    (format t "========~A=========~%" *crates2-gl-context*)
    (check-error "1")
    (glenable +GL-TEXTURE-2D+)
    (glmatrixmode +GL-PROJECTION+)
    (check-error "2")
    (glloadidentity)
    (check-error "3")
    (glviewport 0 0 screen-width screen-height)
    (check-error "4")
    ;; (gluortho2d 0.0d0 (coerce screen-width 'double-float) 0.0d0 (coerce screen-height 'double-float))
    ;; (gluperspective 45.0d0 (/ (coerce screen-width 'double-float) screen-height) 0.1d0 100.0d0)
    (gluperspective 45.0d0 (/ (coerce screen-width 'double-float) screen-height) 0.1d0 100.0d0)
    (glulookat 0.0d0 0.0d0 50.0d0   0.0d0 0.0d0 0.0d0   0.0d0 1.0d0 0.0d0)
    (check-error "5")
    (glmatrixmode +GL-MODELVIEW+)
    (check-error "6")
    (glloadidentity)
    ;; (setf *crates2-renderer* (sdl-createrenderer *crates2-window* -1 +SDL_RENDERER_SOFTWARE+))
    (setf *image* (img-load "etc/assets/texture/texture32.png"))
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
                                                   (let ((scancode (getf keysym 'scancode)))
                                                     (cond ((eq scancode :SDL-SCANCODE-LEFT)   (setf result :west))
                                                           ((eq scancode :SDL-SCANCODE-RIGHT)  (setf result :east))
                                                           ((eq scancode :SDL-SCANCODE-UP)     (setf result :north))
                                                           ((eq scancode :SDL-SCANCODE-DOWN)   (setf result :south))
                                                           ((eq scancode :SDL-SCANCODE-R)      (setf result :restart))
                                                           ((eq scancode :SDL-SCANCODE-B)      (setf result :back))
                                                           ((eq scancode :SDL-SCANCODE-ESCAPE) (setf result :back))))))
                         ((eq type :SDL-QUIT) (setf result :back))))))
      result)))