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
(defconstant iw 512)
;; Window dimensions
(defconstant screen-width 800)
(defconstant screen-height 600)

(defparameter *crates2-window* :pointer)
(defparameter *crates2-renderer* :pointer)
(defparameter *image* :pointer)
(defparameter *texture* :pointer)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun make-rect (index)
  (let* ((sprites-per-row (floor iw cw))
         (x (* cw (mod index sprites-per-row)))
         (y (* ch (floor index sprites-per-row))))
    (list x y cw ch)))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-idle" *visual-hash*) (make-rect 0))
  (setf (gethash "vacuum-full" *visual-hash*) (make-rect 0))
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
  (setf (gethash "exit-idle"        *visual-hash*) (make-rect 48))
  (setf (gethash "exit-active-pass" *visual-hash*) (make-rect 49))
  (setf (gethash "exit-active-fail" *visual-hash*) (make-rect 50))
  ;; KEY
  (setf (gethash "key-idle-00" *visual-hash*) (make-rect 32))
  (setf (gethash "key-idle-01" *visual-hash*) (make-rect 33))
  (setf (gethash "key-idle-02" *visual-hash*) (make-rect 34))
  (setf (gethash "key-idle-03" *visual-hash*) (make-rect 35))
  (setf (gethash "key-idle-04" *visual-hash*) (make-rect 36))
  (setf (gethash "key-idle-05" *visual-hash*) (make-rect 37))
  (setf (gethash "key-idle-06" *visual-hash*) (make-rect 38))
  (setf (gethash "key-idle-07" *visual-hash*) (make-rect 39))
  (setf (gethash "key-idle-08" *visual-hash*) (make-rect 40))
  (setf (gethash "key-active"  *visual-hash*) (make-rect 41))
  ;; PLAYER
  (setf (gethash "player-active-00" *visual-hash*) (make-rect 16))
  (setf (gethash "player-active-01" *visual-hash*) (make-rect 17))
  (setf (gethash "player-active-02" *visual-hash*) (make-rect 18))
  (setf (gethash "player-active-03" *visual-hash*) (make-rect 19))
  (setf (gethash "player-active-04" *visual-hash*) (make-rect 20))
  (setf (gethash "player-active-05" *visual-hash*) (make-rect 21))
  (setf (gethash "player-active-06" *visual-hash*) (make-rect 22))
  (setf (gethash "player-hidden"    *visual-hash*) (make-rect 15))
  ;; SLOPES
  (setf (gethash "slope-en"        *visual-hash*) (make-rect 192))
  (setf (gethash "slope-en-active" *visual-hash*) (make-rect 193))
  (setf (gethash "slope-es"        *visual-hash*) (make-rect 208))
  (setf (gethash "slope-es-active" *visual-hash*) (make-rect 209))
  (setf (gethash "slope-wn"        *visual-hash*) (make-rect 224))
  (setf (gethash "slope-wn-active" *visual-hash*) (make-rect 225))
  (setf (gethash "slope-ws"        *visual-hash*) (make-rect 240))
  (setf (gethash "slope-ws-active" *visual-hash*) (make-rect 241))
  ;; TURNSTILE
  (setf (gethash "turnstile-e1"        *visual-hash*) (make-rect 80))
  (setf (gethash "turnstile-e1-active" *visual-hash*) (make-rect 81))
  (setf (gethash "turnstile-w1"        *visual-hash*) (make-rect 176))
  (setf (gethash "turnstile-w1-active" *visual-hash*) (make-rect 177))
  (setf (gethash "turnstile-n1"        *visual-hash*) (make-rect 112))
  (setf (gethash "turnstile-n1-active" *visual-hash*) (make-rect 113))
  (setf (gethash "turnstile-s1"        *visual-hash*) (make-rect 144))
  (setf (gethash "turnstile-s1-active" *visual-hash*) (make-rect 145))
  (setf (gethash "turnstile-e"         *visual-hash*) (make-rect 64))
  (setf (gethash "turnstile-e-active"  *visual-hash*) (make-rect 65))
  (setf (gethash "turnstile-w"         *visual-hash*) (make-rect 160))
  (setf (gethash "turnstile-w-active"  *visual-hash*) (make-rect 161))
  (setf (gethash "turnstile-n"         *visual-hash*) (make-rect 96))
  (setf (gethash "turnstile-n-active"  *visual-hash*) (make-rect 97))
  (setf (gethash "turnstile-s"         *visual-hash*) (make-rect 128))
  (setf (gethash "turnstile-s-active"  *visual-hash*) (make-rect 129))
  ;; NUMBERS
  (setf (gethash "number-01"        *visual-hash*) (make-rect 51))
  (setf (gethash "number-02"        *visual-hash*) (make-rect 52))
  (setf (gethash "number-03"        *visual-hash*) (make-rect 53))
  (setf (gethash "number-04"        *visual-hash*) (make-rect 54))
  (setf (gethash "number-05"        *visual-hash*) (make-rect 55))
  (setf (gethash "number-06"        *visual-hash*) (make-rect 56))
  (setf (gethash "number-07"        *visual-hash*) (make-rect 57))
  (setf (gethash "number-08"        *visual-hash*) (make-rect 58))
  (setf (gethash "number-09"        *visual-hash*) (make-rect 59))
  (setf (gethash "number-10"        *visual-hash*) (make-rect 60))
  (setf (gethash "number-11"        *visual-hash*) (make-rect 67)) ; Add 16 to start of previous line.
  (setf (gethash "number-12"        *visual-hash*) (make-rect 68))
  (setf (gethash "number-13"        *visual-hash*) (make-rect 69))
  (setf (gethash "number-14"        *visual-hash*) (make-rect 70))
  (setf (gethash "number-15"        *visual-hash*) (make-rect 71))
  (setf (gethash "number-16"        *visual-hash*) (make-rect 72))
  (setf (gethash "number-17"        *visual-hash*) (make-rect 73))
  (setf (gethash "number-18"        *visual-hash*) (make-rect 74))
  (setf (gethash "number-19"        *visual-hash*) (make-rect 75))
  (setf (gethash "number-20"        *visual-hash*) (make-rect 76))
  (setf (gethash "number-21"        *visual-hash*) (make-rect 92)) ; Add 16 to start of previous line.
  (setf (gethash "number-22"        *visual-hash*) (make-rect 93))
  (setf (gethash "number-23"        *visual-hash*) (make-rect 94))
  (setf (gethash "number-24"        *visual-hash*) (make-rect 95))
  (setf (gethash "number-25"        *visual-hash*) (make-rect 96))
  (setf (gethash "number-26"        *visual-hash*) (make-rect 97))
  (setf (gethash "number-27"        *visual-hash*) (make-rect 98))
  (setf (gethash "number-28"        *visual-hash*) (make-rect 99))
  (setf (gethash "number-29"        *visual-hash*) (make-rect 100))
  (setf (gethash "number-30"        *visual-hash*) (make-rect 101))
  (setf (gethash "number-31"        *visual-hash*) (make-rect 108)) ; Add 16 to start of previous line.
  (setf (gethash "number-32"        *visual-hash*) (make-rect 109))
  (setf (gethash "number-33"        *visual-hash*) (make-rect 110))
  (setf (gethash "number-34"        *visual-hash*) (make-rect 111))
  (setf (gethash "number-35"        *visual-hash*) (make-rect 112))
  (setf (gethash "number-36"        *visual-hash*) (make-rect 113))
  (setf (gethash "number-37"        *visual-hash*) (make-rect 114))
  (setf (gethash "number-38"        *visual-hash*) (make-rect 115))
  (setf (gethash "number-39"        *visual-hash*) (make-rect 116))
  (setf (gethash "number-40"        *visual-hash*) (make-rect 117))
  (setf (gethash "number-41"        *visual-hash*) (make-rect 124)) ; Add 16 to start of previous line.
  (setf (gethash "number-42"        *visual-hash*) (make-rect 125))
  (setf (gethash "number-43"        *visual-hash*) (make-rect 126))
  (setf (gethash "number-44"        *visual-hash*) (make-rect 127))
  (setf (gethash "number-45"        *visual-hash*) (make-rect 128))
  (setf (gethash "number-46"        *visual-hash*) (make-rect 129))
  (setf (gethash "number-47"        *visual-hash*) (make-rect 130))
  (setf (gethash "number-48"        *visual-hash*) (make-rect 131))
  (setf (gethash "number-49"        *visual-hash*) (make-rect 132))
  (setf (gethash "number-50"        *visual-hash*) (make-rect 133))
  (setf (gethash "number-51"        *visual-hash*) (make-rect 140)) ; Add 16 to start of previous line.
  (setf (gethash "number-52"        *visual-hash*) (make-rect 141))
  (setf (gethash "number-53"        *visual-hash*) (make-rect 142))
  (setf (gethash "number-54"        *visual-hash*) (make-rect 143))
  (setf (gethash "number-55"        *visual-hash*) (make-rect 144))
  (setf (gethash "number-56"        *visual-hash*) (make-rect 145))
  (setf (gethash "number-57"        *visual-hash*) (make-rect 146))
  (setf (gethash "number-58"        *visual-hash*) (make-rect 147))
  (setf (gethash "number-59"        *visual-hash*) (make-rect 148))
  (setf (gethash "number-60"        *visual-hash*) (make-rect 149))
  (setf (gethash "number-61"        *visual-hash*) (make-rect 156)) ; Add 16 to start of previous line.
  (setf (gethash "number-62"        *visual-hash*) (make-rect 157))
  (setf (gethash "number-63"        *visual-hash*) (make-rect 158))
  (setf (gethash "number-64"        *visual-hash*) (make-rect 159))
  (setf (gethash "number-65"        *visual-hash*) (make-rect 160))
  (setf (gethash "number-66"        *visual-hash*) (make-rect 161))
  (setf (gethash "number-67"        *visual-hash*) (make-rect 162))
  (setf (gethash "number-68"        *visual-hash*) (make-rect 163))
  (setf (gethash "number-69"        *visual-hash*) (make-rect 164))
  (setf (gethash "number-70"        *visual-hash*) (make-rect 165))
  (setf (gethash "number-71"        *visual-hash*) (make-rect 172)) ; Add 16 to start of previous line.
  (setf (gethash "number-72"        *visual-hash*) (make-rect 173))
  (setf (gethash "number-73"        *visual-hash*) (make-rect 174))
  (setf (gethash "number-74"        *visual-hash*) (make-rect 175))
  (setf (gethash "number-75"        *visual-hash*) (make-rect 176))
  (setf (gethash "number-76"        *visual-hash*) (make-rect 177))
  (setf (gethash "number-77"        *visual-hash*) (make-rect 178))
  (setf (gethash "number-78"        *visual-hash*) (make-rect 179))
  (setf (gethash "number-79"        *visual-hash*) (make-rect 180))
  (setf (gethash "number-80"        *visual-hash*) (make-rect 181))
  (setf (gethash "number-81"        *visual-hash*) (make-rect 188)) ; Add 16 to start of previous line.
  (setf (gethash "number-82"        *visual-hash*) (make-rect 189))
  (setf (gethash "number-83"        *visual-hash*) (make-rect 190))
  (setf (gethash "number-84"        *visual-hash*) (make-rect 191))
  (setf (gethash "number-85"        *visual-hash*) (make-rect 192))
  (setf (gethash "number-86"        *visual-hash*) (make-rect 193))
  (setf (gethash "number-87"        *visual-hash*) (make-rect 194))
  (setf (gethash "number-88"        *visual-hash*) (make-rect 195))
  (setf (gethash "number-89"        *visual-hash*) (make-rect 196))
  (setf (gethash "number-90"        *visual-hash*) (make-rect 197))
  (setf (gethash "number-91"        *visual-hash*) (make-rect 204)) ; Add 16 to start of previous line.
  (setf (gethash "number-92"        *visual-hash*) (make-rect 205))
  (setf (gethash "number-93"        *visual-hash*) (make-rect 206))
  (setf (gethash "number-94"        *visual-hash*) (make-rect 207))
  (setf (gethash "number-95"        *visual-hash*) (make-rect 208))
  (setf (gethash "number-96"        *visual-hash*) (make-rect 209))
  (setf (gethash "number-97"        *visual-hash*) (make-rect 210))
  (setf (gethash "number-98"        *visual-hash*) (make-rect 211))
  (setf (gethash "number-99"        *visual-hash*) (make-rect 212))
  (setf (gethash "number-100"       *visual-hash*) (make-rect 213))
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
  (setf (gethash "stepper-idle"               *visual-hash*) (make-rect 42))
  (setf (gethash "stepper-active"             *visual-hash*) (make-rect 43))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) (make-rect 23))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) (make-rect 24))
  (setf (gethash "toggle-east-off"            *visual-hash*) (make-rect 28))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) (make-rect 26))
  (setf (gethash "toggle-west-off"            *visual-hash*) (make-rect 28))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) (make-rect 25))
  (setf (gethash "toggle-north-off"           *visual-hash*) (make-rect 28))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) (make-rect 27))
  (setf (gethash "toggle-south-off"           *visual-hash*) (make-rect 28))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) (make-rect 0))
  (setf (gethash "bomb"          *visual-hash*) (make-rect 0))
  ;; ring 1
  (setf (gethash "bomb-ring-1"   *visual-hash*) (make-rect 0))
  ;; ring 2
  (setf (gethash "bomb-ring-2"   *visual-hash*) (make-rect 0)))

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
                                  (set-rect rect2 deltax finy vivw vivh)
                                  (sdl-rendercopy *crates2-renderer* *texture* rect1pointer rect2pointer))))))))
      (sdl-renderpresent *crates2-renderer*))))

(defun ui-init ()
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (sdl-init +SDL-INIT-VIDEO+)
    (setf *crates2-window* (sdl-createwindow "Crates 2" 0 0 screen-width screen-height 0))
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