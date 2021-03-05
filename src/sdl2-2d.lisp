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
  (setf (gethash "block-timer"         *visual-hash*) (make-rect 0))
  ;; COUNT
  (setf (gethash "count-01"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-02"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-03"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-04"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-05"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-06"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-07"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-08"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-09"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-10"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-11"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-12"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-13"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-14"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-15"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-16"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-17"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-18"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-19"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-20"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-21"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-22"            *visual-hash*) (make-rect 0))
  (setf (gethash "count-23"            *visual-hash*) (make-rect 0))
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
  (setf (gethash "player-hidden"    *visual-hash*) (make-rect 27))
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
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter-10" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-09" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-08" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-07" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-06" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-05" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-04" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-03" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-02" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-01" *visual-hash*) (make-rect 0))
  (setf (gethash "block-counter-00" *visual-hash*) (make-rect 0))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter-10" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-09" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-08" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-07" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-06" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-05" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-04" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-03" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-02" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-01" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-counter-00" *visual-hash*) (make-rect 0))
  ;; PASS-TIMER
  (setf (gethash "pass-timer-10" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-09" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-08" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-07" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-06" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-05" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-04" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-03" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-02" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-01" *visual-hash*) (make-rect 0))
  (setf (gethash "pass-timer-00" *visual-hash*) (make-rect 0))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) (make-rect 0))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) (make-rect 0))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) (make-rect 0))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) (make-rect 0))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) (make-rect 0))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) (make-rect 0))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) (make-rect 0))
  (setf (gethash "stepper-active"             *visual-hash*) (make-rect 0))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) (make-rect 0))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) (make-rect 0))
  (setf (gethash "toggle-east-off"            *visual-hash*) (make-rect 0))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) (make-rect 0))
  (setf (gethash "toggle-west-off"            *visual-hash*) (make-rect 0))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) (make-rect 0))
  (setf (gethash "toggle-north-off"           *visual-hash*) (make-rect 0))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) (make-rect 0))
  (setf (gethash "toggle-south-off"           *visual-hash*) (make-rect 0))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) (make-rect 0))
  (setf (gethash "bomb"          *visual-hash*) (make-rect 0))
  ;; ring 1
  (setf (gethash "bomb-ring-1"   *visual-hash*) (make-rect 0))
  ;; ring 2
  (setf (gethash "bomb-ring-2"   *visual-hash*) (make-rect 0)))

(defparameter *fake-input* nil)

(defparameter *last-input* nil)

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
                 (let* ((x (if (and (= step 0) (typep crate 'moving)) (tail-x crate) (crate-x crate)))
                        (y (if (and (= step 0) (typep crate 'moving)) (tail-y crate) (crate-y crate)))
                        (z (if (and (= step 0) (typep crate 'moving)) (tail-z crate) (crate-z crate)))
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

(defun ui-maybe-read-input ()
  (let ((player (find-first-crate-of-type 'player)))
    (if (and player (movingp player))
        nil                    ; No input while player moves, in textual mode.
        (setf *last-input* (ui-read-input)))))

(defun ui-input ()
  (if *level*
      (if *test-run*
          (let ((input (car *fake-input*)))
            (setf *fake-input* (cdr *fake-input*))
            (setf *last-input* input)
            input)
          (ui-maybe-read-input))
      nil))
