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
(defparameter *crates2-renderer* :pointer)
(defparameter *image* :pointer)
(defparameter *texture* :pointer)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defparameter *look-at-x* 0)
(defparameter *look-at-y* 0)

(defun make-rect (index)
  (let* ((sprites-per-row (floor iw cw))
         (x (* cw (mod index sprites-per-row)))
         (y (* ch (floor index sprites-per-row))))
    (list x y cw ch)))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-body" *visual-hash*) (make-rect 482))
  (setf (gethash "gear-00"     *visual-hash*) (make-rect 483))
  (setf (gethash "gear-01"     *visual-hash*) (make-rect 484))
  (setf (gethash "gear-02"     *visual-hash*) (make-rect 485))
  (setf (gethash "gear-03"     *visual-hash*) (make-rect 486))
  (setf (gethash "gear-04"     *visual-hash*) (make-rect 487))
  (setf (gethash "gear-05"     *visual-hash*) (make-rect 488))
  (setf (gethash "gear-06"     *visual-hash*) (make-rect 489))
  (setf (gethash "gear-07"     *visual-hash*) (make-rect 490))
  ;; WALL
  (setf (gethash "wall-idle-00" *visual-hash*) (make-rect 0))
  (setf (gethash "wall-idle-01" *visual-hash*) (make-rect 1))
  (setf (gethash "wall-idle-02" *visual-hash*) (make-rect 2))
  (setf (gethash "wall-idle-03" *visual-hash*) (make-rect 3))
  ;; PUSHED
  (setf (gethash "pushed-idle" *visual-hash*) (make-rect 0))
  ;; BLOCK-TIMER
  (setf (gethash "block-timer-durable" *visual-hash*) (make-rect 0))
  (setf (gethash "block-timer"         *visual-hash*) (make-rect 5))
  ;; EXIT
  (setf (gethash "exit-idle"        *visual-hash*) (make-rect 96)) ; 48 + 48 to old
  (setf (gethash "exit-active-pass" *visual-hash*) (make-rect 97))
  (setf (gethash "exit-active-fail" *visual-hash*) (make-rect 98))
  ;; KEY
  (setf (gethash "key-idle-00" *visual-hash*) (make-rect 64))
  (setf (gethash "key-idle-01" *visual-hash*) (make-rect 65))
  (setf (gethash "key-idle-02" *visual-hash*) (make-rect 66))
  (setf (gethash "key-idle-03" *visual-hash*) (make-rect 67))
  (setf (gethash "key-idle-04" *visual-hash*) (make-rect 68))
  (setf (gethash "key-idle-05" *visual-hash*) (make-rect 69))
  (setf (gethash "key-idle-06" *visual-hash*) (make-rect 70))
  (setf (gethash "key-idle-07" *visual-hash*) (make-rect 71))
  (setf (gethash "key-idle-08" *visual-hash*) (make-rect 72))
  (setf (gethash "key-active"  *visual-hash*) (make-rect 73))
  ;; SPECIAL
  (setf (gethash "special-jump-idle-00" *visual-hash*) (make-rect 960))
  (setf (gethash "special-jump-idle-01" *visual-hash*) (make-rect 961))
  (setf (gethash "special-jump-idle-02" *visual-hash*) (make-rect 962))
  (setf (gethash "special-jump-idle-03" *visual-hash*) (make-rect 963))
  (setf (gethash "special-jump-idle-04" *visual-hash*) (make-rect 964))
  (setf (gethash "special-jump-idle-05" *visual-hash*) (make-rect 965))
  (setf (gethash "special-jump-idle-06" *visual-hash*) (make-rect 966))
  (setf (gethash "special-jump-idle-07" *visual-hash*) (make-rect 967))
  (setf (gethash "special-jump-idle-08" *visual-hash*) (make-rect 968))
  (setf (gethash "special-jump-idle-09" *visual-hash*) (make-rect 969))
  (setf (gethash "special-jump-idle-10" *visual-hash*) (make-rect 970))
  (setf (gethash "special-jump-idle-11" *visual-hash*) (make-rect 971))
  (setf (gethash "special-jump-idle-12" *visual-hash*) (make-rect 972))
  (setf (gethash "special-jump-idle-13" *visual-hash*) (make-rect 972))
  (setf (gethash "special-jump-active"  *visual-hash*) (make-rect 973))
  ;; PLAYER
  (setf (gethash "player-active-00" *visual-hash*) (make-rect 32))
  (setf (gethash "player-active-01" *visual-hash*) (make-rect 33))
  (setf (gethash "player-active-02" *visual-hash*) (make-rect 34))
  (setf (gethash "player-active-03" *visual-hash*) (make-rect 35))
  (setf (gethash "player-active-04" *visual-hash*) (make-rect 36))
  (setf (gethash "player-active-05" *visual-hash*) (make-rect 37))
  (setf (gethash "player-active-06" *visual-hash*) (make-rect 38))
  (setf (gethash "player-airborne"  *visual-hash*) (list (floor (* 0.9619140625 iw)) 0 (floor (* 1.2 cw)) (floor (* 1.2 ch))))
  (setf (gethash "player-hidden"    *visual-hash*) (make-rect 15))
  ;; SLOPES
  (setf (gethash "slope-en"        *visual-hash*) (make-rect 384))
  (setf (gethash "slope-en-active" *visual-hash*) (make-rect 385))
  (setf (gethash "slope-es"        *visual-hash*) (make-rect 416)) ; + 32 to EN
  (setf (gethash "slope-es-active" *visual-hash*) (make-rect 417))
  (setf (gethash "slope-wn"        *visual-hash*) (make-rect 448)) ; + 32 to ES
  (setf (gethash "slope-wn-active" *visual-hash*) (make-rect 449))
  (setf (gethash "slope-ws"        *visual-hash*) (make-rect 480)) ; + 32 to WN
  (setf (gethash "slope-ws-active" *visual-hash*) (make-rect 481))
  ;; TURNSTILE
  (setf (gethash "turnstile-e"         *visual-hash*) (make-rect 128))
  (setf (gethash "turnstile-e-active"  *visual-hash*) (make-rect 129))
  (setf (gethash "turnstile-e1"        *visual-hash*) (make-rect 160)) ; + 32 to E
  (setf (gethash "turnstile-e1-active" *visual-hash*) (make-rect 161))
  (setf (gethash "turnstile-n"         *visual-hash*) (make-rect 192)) ; + 32 to E1
  (setf (gethash "turnstile-n-active"  *visual-hash*) (make-rect 193))
  (setf (gethash "turnstile-n1"        *visual-hash*) (make-rect 224)) ; + 32 to W1
  (setf (gethash "turnstile-n1-active" *visual-hash*) (make-rect 225))
  (setf (gethash "turnstile-s"         *visual-hash*) (make-rect 256)) ; + 32 to N
  (setf (gethash "turnstile-s-active"  *visual-hash*) (make-rect 257))
  (setf (gethash "turnstile-s1"        *visual-hash*) (make-rect 288)) ; + 32 to N1
  (setf (gethash "turnstile-s1-active" *visual-hash*) (make-rect 289))
  (setf (gethash "turnstile-w"         *visual-hash*) (make-rect 320)) ; + 32 to S1
  (setf (gethash "turnstile-w-active"  *visual-hash*) (make-rect 321))
  (setf (gethash "turnstile-w1"        *visual-hash*) (make-rect 352)) ; + 32 to W
  (setf (gethash "turnstile-w1-active" *visual-hash*) (make-rect 353))
  ;; NUMBERS
  (setf (gethash "number-01"        *visual-hash*) (make-rect 99))
  (setf (gethash "number-02"        *visual-hash*) (make-rect 100))
  (setf (gethash "number-03"        *visual-hash*) (make-rect 101))
  (setf (gethash "number-04"        *visual-hash*) (make-rect 102))
  (setf (gethash "number-05"        *visual-hash*) (make-rect 103))
  (setf (gethash "number-06"        *visual-hash*) (make-rect 104))
  (setf (gethash "number-07"        *visual-hash*) (make-rect 105))
  (setf (gethash "number-08"        *visual-hash*) (make-rect 106))
  (setf (gethash "number-09"        *visual-hash*) (make-rect 107))
  (setf (gethash "number-10"        *visual-hash*) (make-rect 108))
  (setf (gethash "number-11"        *visual-hash*) (make-rect 131)) ; Add 32 to start of previous line.
  (setf (gethash "number-12"        *visual-hash*) (make-rect 132))
  (setf (gethash "number-13"        *visual-hash*) (make-rect 133))
  (setf (gethash "number-14"        *visual-hash*) (make-rect 134))
  (setf (gethash "number-15"        *visual-hash*) (make-rect 135))
  (setf (gethash "number-16"        *visual-hash*) (make-rect 136))
  (setf (gethash "number-17"        *visual-hash*) (make-rect 137))
  (setf (gethash "number-18"        *visual-hash*) (make-rect 138))
  (setf (gethash "number-19"        *visual-hash*) (make-rect 139))
  (setf (gethash "number-20"        *visual-hash*) (make-rect 140))
  (setf (gethash "number-21"        *visual-hash*) (make-rect 163)) ; Add 32 to start of previous line.
  (setf (gethash "number-22"        *visual-hash*) (make-rect 164))
  (setf (gethash "number-23"        *visual-hash*) (make-rect 165))
  (setf (gethash "number-24"        *visual-hash*) (make-rect 166))
  (setf (gethash "number-25"        *visual-hash*) (make-rect 167))
  (setf (gethash "number-26"        *visual-hash*) (make-rect 168))
  (setf (gethash "number-27"        *visual-hash*) (make-rect 169))
  (setf (gethash "number-28"        *visual-hash*) (make-rect 170))
  (setf (gethash "number-29"        *visual-hash*) (make-rect 171))
  (setf (gethash "number-30"        *visual-hash*) (make-rect 172))
  (setf (gethash "number-31"        *visual-hash*) (make-rect 195)) ; Add 32 to start of previous line.
  (setf (gethash "number-32"        *visual-hash*) (make-rect 196))
  (setf (gethash "number-33"        *visual-hash*) (make-rect 197))
  (setf (gethash "number-34"        *visual-hash*) (make-rect 198))
  (setf (gethash "number-35"        *visual-hash*) (make-rect 199))
  (setf (gethash "number-36"        *visual-hash*) (make-rect 200))
  (setf (gethash "number-37"        *visual-hash*) (make-rect 201))
  (setf (gethash "number-38"        *visual-hash*) (make-rect 202))
  (setf (gethash "number-39"        *visual-hash*) (make-rect 203))
  (setf (gethash "number-40"        *visual-hash*) (make-rect 204))
  (setf (gethash "number-41"        *visual-hash*) (make-rect 227)) ; Add 32 to start of previous line.
  (setf (gethash "number-42"        *visual-hash*) (make-rect 228))
  (setf (gethash "number-43"        *visual-hash*) (make-rect 229))
  (setf (gethash "number-44"        *visual-hash*) (make-rect 230))
  (setf (gethash "number-45"        *visual-hash*) (make-rect 231))
  (setf (gethash "number-46"        *visual-hash*) (make-rect 232))
  (setf (gethash "number-47"        *visual-hash*) (make-rect 233))
  (setf (gethash "number-48"        *visual-hash*) (make-rect 234))
  (setf (gethash "number-49"        *visual-hash*) (make-rect 235))
  (setf (gethash "number-50"        *visual-hash*) (make-rect 236))
  (setf (gethash "number-51"        *visual-hash*) (make-rect 259)) ; Add 32 to start of previous line.
  (setf (gethash "number-52"        *visual-hash*) (make-rect 260))
  (setf (gethash "number-53"        *visual-hash*) (make-rect 261))
  (setf (gethash "number-54"        *visual-hash*) (make-rect 262))
  (setf (gethash "number-55"        *visual-hash*) (make-rect 263))
  (setf (gethash "number-56"        *visual-hash*) (make-rect 264))
  (setf (gethash "number-57"        *visual-hash*) (make-rect 265))
  (setf (gethash "number-58"        *visual-hash*) (make-rect 266))
  (setf (gethash "number-59"        *visual-hash*) (make-rect 267))
  (setf (gethash "number-60"        *visual-hash*) (make-rect 268))
  (setf (gethash "number-61"        *visual-hash*) (make-rect 291)) ; Add 32 to start of previous line.
  (setf (gethash "number-62"        *visual-hash*) (make-rect 292))
  (setf (gethash "number-63"        *visual-hash*) (make-rect 293))
  (setf (gethash "number-64"        *visual-hash*) (make-rect 294))
  (setf (gethash "number-65"        *visual-hash*) (make-rect 295))
  (setf (gethash "number-66"        *visual-hash*) (make-rect 296))
  (setf (gethash "number-67"        *visual-hash*) (make-rect 297))
  (setf (gethash "number-68"        *visual-hash*) (make-rect 298))
  (setf (gethash "number-69"        *visual-hash*) (make-rect 299))
  (setf (gethash "number-70"        *visual-hash*) (make-rect 300))
  (setf (gethash "number-71"        *visual-hash*) (make-rect 323)) ; Add 32 to start of previous line.
  (setf (gethash "number-72"        *visual-hash*) (make-rect 324))
  (setf (gethash "number-73"        *visual-hash*) (make-rect 325))
  (setf (gethash "number-74"        *visual-hash*) (make-rect 326))
  (setf (gethash "number-75"        *visual-hash*) (make-rect 327))
  (setf (gethash "number-76"        *visual-hash*) (make-rect 328))
  (setf (gethash "number-77"        *visual-hash*) (make-rect 329))
  (setf (gethash "number-78"        *visual-hash*) (make-rect 330))
  (setf (gethash "number-79"        *visual-hash*) (make-rect 331))
  (setf (gethash "number-80"        *visual-hash*) (make-rect 332))
  (setf (gethash "number-81"        *visual-hash*) (make-rect 355)) ; Add 32 to start of previous line.
  (setf (gethash "number-82"        *visual-hash*) (make-rect 356))
  (setf (gethash "number-83"        *visual-hash*) (make-rect 357))
  (setf (gethash "number-84"        *visual-hash*) (make-rect 358))
  (setf (gethash "number-85"        *visual-hash*) (make-rect 359))
  (setf (gethash "number-86"        *visual-hash*) (make-rect 360))
  (setf (gethash "number-87"        *visual-hash*) (make-rect 361))
  (setf (gethash "number-88"        *visual-hash*) (make-rect 362))
  (setf (gethash "number-89"        *visual-hash*) (make-rect 363))
  (setf (gethash "number-90"        *visual-hash*) (make-rect 364))
  (setf (gethash "number-91"        *visual-hash*) (make-rect 387)) ; Add 32 to start of previous line.
  (setf (gethash "number-92"        *visual-hash*) (make-rect 388))
  (setf (gethash "number-93"        *visual-hash*) (make-rect 389))
  (setf (gethash "number-94"        *visual-hash*) (make-rect 390))
  (setf (gethash "number-95"        *visual-hash*) (make-rect 391))
  (setf (gethash "number-96"        *visual-hash*) (make-rect 392))
  (setf (gethash "number-97"        *visual-hash*) (make-rect 393))
  (setf (gethash "number-98"        *visual-hash*) (make-rect 394))
  (setf (gethash "number-99"        *visual-hash*) (make-rect 395))
  (setf (gethash "number-100"       *visual-hash*) (make-rect 396))
  ;; BOTTOM NUMBERS
  (loop for i from 1 to 100
        do (setf (gethash (format nil "number-bottom-~2,'0d" i) *visual-hash*) (make-rect (+ i 98))))
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter"    *visual-hash*) (make-rect 0))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter"     *visual-hash*) (make-rect 4))
  ;; PASS-TIMER
  (setf (gethash "pass-timer"       *visual-hash*) (make-rect 4))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) (make-rect 6))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) (make-rect 11))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) (make-rect 7))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) (make-rect 15))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) (make-rect 13))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) (make-rect 9))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) (make-rect 15))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) (make-rect 12))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) (make-rect 8))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) (make-rect 15))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) (make-rect 14))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) (make-rect 10))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) (make-rect 15))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) (make-rect 74))
  (setf (gethash "stepper-active"             *visual-hash*) (make-rect 75))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) (make-rect 39))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) (make-rect 40))
  (setf (gethash "toggle-east-off"            *visual-hash*) (make-rect 50))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) (make-rect 42))
  (setf (gethash "toggle-west-off"            *visual-hash*) (make-rect 50))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) (make-rect 41))
  (setf (gethash "toggle-north-off"           *visual-hash*) (make-rect 50))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) (make-rect 43))
  (setf (gethash "toggle-south-off"           *visual-hash*) (make-rect 50))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) (make-rect 76))
  (setf (gethash "bomb"          *visual-hash*) (make-rect 77))
  ;; ring 1
  (setf (gethash "bomb-ring-1"   *visual-hash*) (list 0 (* 16 ch) (* 1 cw) (* 1 ch))) ; start at index 256 (y slot 16); size 1 slot
  ;; ring 2
  (setf (gethash "bomb-ring-2"   *visual-hash*) (list 0 (* 17 ch) (* 3 cw) (* 3 ch))) ; start at y slot 17; size 3x3 slots
  ;; ring 3
  (setf (gethash "bomb-ring-3"   *visual-hash*) (list 0 (* 20 ch) (* 5 cw) (* 5 ch))) ; size 5x5 slots
  ;; ring 4
  (setf (gethash "bomb-ring-4"   *visual-hash*) (list 0 (* 25 ch) (* 5 cw) (* 5 ch)))) ; size 5x5 slots

(defparameter *fake-input* nil)

(defun ui-render (level step)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (sdl-setrenderdrawcolor *crates2-renderer* #x00 #x00 #x00 #xFF)
    (sdl-renderclear *crates2-renderer*)
    (sdl-setrenderdrawcolor *crates2-renderer* #xBA #x16 #x0C #xFF)
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
                                (let* ((rx (first viv))
                                       (ry (second viv))
                                       (vivw (third viv))
                                       (vivh (fourth viv))
                                       (dx (floor (- cw vivw) 2))
                                       (dy (floor (- ch vivh) 2))
                                       (finy (+ (* y ch) dy))
                                       (deltax (+ (* x cw) dx)))
                                  (set-rect rect1 rx ry vivw vivh)
                                  (set-rect rect2 (+ deltax *look-at-x*) (+ finy *look-at-y*) vivw vivh)
                                  (sdl-rendercopy *crates2-renderer* *texture* rect1pointer rect2pointer))))))))
      (sdl-renderpresent *crates2-renderer*))))

(defun ui-look-at (x y m minx miny maxx maxy)
  (setf *look-at-x* (* (+ (floor (- (- *level-width* maxx) minx) 2) 2) cw))
  (setf *look-at-y* (* (1- (floor (- (- *level-height* maxy) miny) 2)) ch)))

(defun ui-init (options)
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (sdl-init +SDL-INIT-VIDEO+)
    (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height 0))
    (when (getf options :fullscreen)
      (sdl-setwindowfullscreen *crates2-window* +SDL-TRUE+))
    (setf *crates2-renderer* (sdl-createrenderer *crates2-window* -1 +SDL_RENDERER_SOFTWARE+))
    (setf *image* (img-load "etc/assets/texture/texture32.png"))
    (setf *texture* (sdl-createtexturefromsurface
                     *crates2-renderer*
                     *image*))))

(defun ui-delete ()
    (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
      (sdl-destroytexture *texture*)
      (sdl-freesurface *image*)
      (sdl-destroyrenderer *crates2-renderer*)
      (sdl-destroywindow *crates2-window*)
      (sdl-quit)))
