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

;; Crate is drawn as CW x CH pixel shape
(defconstant cw 16)
(defconstant ch 16)
;; Window dimensions
(defconstant screen-width 600)
(defconstant screen-height 480)

(defparameter *crates2-window* nil)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-idle" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "vacuum-full" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; WALL
  (setf (gethash "wall-idle" *visual-hash*) (sdl2:make-rect 0 336 cw ch))
  ;; PUSHED
  (setf (gethash "pushed-idle" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; BLOCK-TIMER
  (setf (gethash "block-timer-durable" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-timer"         *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; COUNT
  (setf (gethash "count-01"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-02"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-03"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-04"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-05"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-06"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-07"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-08"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-09"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-10"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-11"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-12"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-13"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-14"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-15"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-16"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-17"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-18"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-19"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-20"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-21"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-22"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "count-23"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; EXIT
  (setf (gethash "exit-idle" *visual-hash*) (sdl2:make-rect 0 16 cw ch))
  (setf (gethash "exit-active" *visual-hash*) (sdl2:make-rect 0 32 cw ch))
  ;; KEY
  (setf (gethash "key-idle" *visual-hash*) (sdl2:make-rect 0 64 cw ch))
  ;; PLAYER
  (setf (gethash "player-active" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "player-hidden" *visual-hash*) (sdl2:make-rect cw 0 cw ch))
  ;; SLOPES
  (setf (gethash "slope-en" *visual-hash*) (sdl2:make-rect 0 480 cw ch))
  (setf (gethash "slope-es" *visual-hash*) (sdl2:make-rect 0 496 cw ch))
  (setf (gethash "slope-wn" *visual-hash*)(sdl2:make-rect 0 512 cw ch))
  (setf (gethash "slope-ws" *visual-hash*)(sdl2:make-rect 0 528 cw ch))
  ;; TURNSTILE
  (setf (gethash "turnstile-e1" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-w1" *visual-hash*)(sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-n1" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-s1" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-e" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-w" *visual-hash*)(sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-n" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "turnstile-s" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter-10" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-09" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-08" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-07" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-06" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-05" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-04" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-03" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-02" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-01" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "block-counter-00" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter-10" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-09" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-08" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-07" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-06" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-05" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-04" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-03" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-02" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-01" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-counter-00" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; PASS-TIMER
  (setf (gethash "pass-timer-10" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-09" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-08" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-07" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-06" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-05" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-04" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-03" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-02" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-01" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pass-timer-00" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) (sdl2:make-rect 0 80 cw ch))
  (setf (gethash "stepper-active"             *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "toggle-east-off"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "toggle-west-off"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "toggle-north-off"           *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "toggle-south-off"           *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  (setf (gethash "bomb"          *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; ring 1
  (setf (gethash "bomb-ring-1"   *visual-hash*) (sdl2:make-rect 0 0 cw ch))
  ;; ring 2
  (setf (gethash "bomb-ring-2"   *visual-hash*) (sdl2:make-rect 0 0 cw ch)))

(defparameter *fake-input* nil)

(defparameter *last-input* nil)

(defparameter *image* nil)

(defun ui-render (level step)
  (let ((screen-surface (sdl2:get-window-surface *crates2-window*)))
    (sdl2:fill-rect screen-surface nil (sdl2:map-rgb (sdl2:surface-format screen-surface) 0 0 0))
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
                              (let* ((rx (sdl2:rect-x viv))
                                     (ry (sdl2:rect-y viv))
                                     (vivh (sdl2:rect-height viv))
                                     (vivw (sdl2:rect-width viv))
                                     (dx (truncate (/ (- cw vivw) 2)))
                                     (dy (truncate (/ (- ch vivh) 2)))
                                     (finx (* x cw))
                                     (finy (truncate (+ (* y ch) dy ry)))
                                     (deltax (truncate (+ finx dx)))
                                     (target-rect (sdl2:make-rect deltax finy vivw vivh)))
                                (sdl2:blit-surface *image* viv screen-surface target-rect))))))))
    (sdl2:update-window *crates2-window*)))

(defun ui-init ()
  (sdl2:init :video)
  (setf *crates2-window* (sdl2:create-window :title "Crates 2" :w screen-width :h screen-height))
  (setf *image* (sdl2:load-bmp "entities.bmp")))

(defun ui-delete ()
  (sdl2:destroy-window *crates2-window*)
  (sdl2:quit))

(defun ui-read-input ()
  (sdl2:with-event-loop (:method :poll)
    (:quit () :back)
    (:keydown
     (:keysym keysym)
     (case (sdl2:scancode keysym)
       (:scancode-w :north)
       (:scancode-s :shout)
       (:scancode-a :west)
       (:scancode-d :east)
       (:scancode-r :restart)
       (:scancode-b :back)))))

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
