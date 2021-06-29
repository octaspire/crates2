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

(defconstant +UI-HINT-DELAY+    -0.5)
(defconstant +UI-VOLUME-MAX+     128)
(defconstant +UI-VOLUME-HIGH+    (- +UI-VOLUME-MAX+ (floor +UI-VOLUME-MAX+ 4)))
(defconstant +UI-VOLUME-MIDDLE+  (- +UI-VOLUME-MAX+ (floor +UI-VOLUME-MAX+ 2)))
(defconstant +UI-VOLUME-LOW+     (- +UI-VOLUME-MIDDLE+ (floor +UI-VOLUME-MAX+ 4)))
(defconstant +UI-VOLUME-MIN+     0)
(defconstant +UI-JOYSTICK-AXIS-NOISE+ 30000)
(defparameter *ui-hint-time*     +UI-HINT-DELAY+)
(defparameter *ui-music-volume*  +UI-VOLUME-MIDDLE+)
(defparameter *ui-effect-volume* +UI-VOLUME-LOW+)
(defparameter *controller*       nil)
(defparameter *music*                     :pointer)
(defparameter *music-array*               :pointer)
(defparameter *snd-special*               :pointer)
(defparameter *snd-special-array*         :pointer)
(defparameter *snd-hit-wall*              :pointer)
(defparameter *snd-hit-wall-array*        :pointer)
(defparameter *snd-explosion*             :pointer)
(defparameter *snd-explosion-array*       :pointer)
(defparameter *snd-bomb-on*               :pointer)
(defparameter *snd-bomb-on-array*         :pointer)
(defparameter *snd-exit-error*            :pointer)
(defparameter *snd-exit-error-array*      :pointer)
(defparameter *snd-exit-ok*               :pointer)
(defparameter *snd-exit-ok-array*         :pointer)
(defparameter *snd-key-collect*           :pointer)
(defparameter *snd-key-collect-array*     :pointer)
(defparameter *snd-hit-counter*           :pointer)
(defparameter *snd-hit-counter-array*     :pointer)
(defparameter *snd-pulled-activate*       :pointer)
(defparameter *snd-pulled-activate-array* :pointer)
(defparameter *snd-redirect*              :pointer)
(defparameter *snd-redirect-array*        :pointer)
(defparameter *snd-slope*                 :pointer)
(defparameter *snd-slope-array*           :pointer)
(defparameter *snd-hit-vacuum*            :pointer)
(defparameter *snd-hit-vacuum-array*      :pointer)
(defparameter *snd-hit-automaton*         :pointer)
(defparameter *snd-hit-automaton-array*   :pointer)
(defparameter *snd-automaton-key*         :pointer)
(defparameter *snd-automaton-key-array*   :pointer)
(defparameter *IBMPlexMono-Bold*          :pointer)
(defparameter *IBMPlexMono-Bold-array*    :pointer)

(defun ui-common-init ()
  (when (< (sdl-init (logior +SDL-INIT-VIDEO+ +SDL-INIT-GAMECONTROLLER+)) 0)
    (error "SDL Init failed"))
  (when (and (> (sdl-numjoysticks) 0) (sdl-isgamecontroller 0))
    (format t "~%~%Opening controller~%~%")
    (setf *controller* (sdl-gamecontrolleropen 0)))
  (img-init +IMG-INIT-PNG+)
  (ui-init-audio)
  (unless (ttf-init)
    (error "TTF Init failed")))

(defun ui-common-delete ()
  (when *controller*
    (sdl-gamecontrollerclose *controller*)
    (setf *controller* nil)))

(defun ui-init-audio ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (mix-init +MIX-INIT-OGG+)
    (mix-openaudio +MIX-DEFAULT-FREQUENCY+ +MIX-DEFAULT-FORMAT+ 2 2048)
    (mix-volume -1 *ui-effect-volume*)
    (mix-volumemusic *ui-music-volume*)
    ;; Music
    (setf *music-array* (foreign-alloc :uint8 :count (length *ending.ogg*) :initial-contents *ending.ogg*))
    (setf *music* (mix-loadmus-rw (sdl-rwfromconstmem *music-array* (length *ending.ogg*)) -1))
    ;; Special sound effect
    (setf *snd-special-array* (foreign-alloc :uint8 :count (length *special.wav*) :initial-contents *special.wav*))
    (setf *snd-special* (mix-loadwav-rw (sdl-rwfromconstmem *snd-special-array* (length *special.wav*)) -1))
    ;; Hit wall sound effect
    (setf *snd-hit-wall-array* (foreign-alloc :uint8 :count (length *hit-wall.wav*) :initial-contents *hit-wall.wav*))
    (setf *snd-hit-wall* (mix-loadwav-rw (sdl-rwfromconstmem *snd-hit-wall-array* (length *hit-wall.wav*)) -1))
    ;; Explosion sound effect
    (setf *snd-explosion-array* (foreign-alloc :uint8 :count (length *explosion.wav*) :initial-contents *explosion.wav*))
    (setf *snd-explosion* (mix-loadwav-rw (sdl-rwfromconstmem *snd-explosion-array* (length *explosion.wav*)) -1))
    ;; Bomb on sound effect
    (setf *snd-bomb-on-array* (foreign-alloc :uint8 :count (length *bomb-on.wav*) :initial-contents *bomb-on.wav*))
    (setf *snd-bomb-on* (mix-loadwav-rw (sdl-rwfromconstmem *snd-bomb-on-array* (length *bomb-on.wav*)) -1))
    ;; Exit error sound effect
    (setf *snd-exit-error-array* (foreign-alloc :uint8 :count (length *exit-error.wav*) :initial-contents *exit-error.wav*))
    (setf *snd-exit-error* (mix-loadwav-rw (sdl-rwfromconstmem *snd-exit-error-array* (length *exit-error.wav*)) -1))
    ;; Exit OK sound effect
    (setf *snd-exit-ok-array* (foreign-alloc :uint8 :count (length *exit-ok.wav*) :initial-contents *exit-ok.wav*))
    (setf *snd-exit-ok* (mix-loadwav-rw (sdl-rwfromconstmem *snd-exit-ok-array* (length *exit-ok.wav*)) -1))
    ;; Key collect sound effect
    (setf *snd-key-collect-array* (foreign-alloc :uint8 :count (length *key-collect.wav*) :initial-contents *key-collect.wav*))
    (setf *snd-key-collect* (mix-loadwav-rw (sdl-rwfromconstmem *snd-key-collect-array* (length *key-collect.wav*)) -1))
    ;; Hit counter sound effect
    (setf *snd-hit-counter-array* (foreign-alloc :uint8 :count (length *hit-counter.wav*) :initial-contents *hit-counter.wav*))
    (setf *snd-hit-counter* (mix-loadwav-rw (sdl-rwfromconstmem *snd-hit-counter-array* (length *hit-counter.wav*)) -1))
    ;; Pulled activate sound effect
    (setf *snd-pulled-activate-array* (foreign-alloc :uint8 :count (length *pulled-activate.wav*) :initial-contents *pulled-activate.wav*))
    (setf *snd-pulled-activate* (mix-loadwav-rw (sdl-rwfromconstmem *snd-pulled-activate-array* (length *pulled-activate.wav*)) -1))
    ;; Redirect sound effect
    (setf *snd-redirect-array* (foreign-alloc :uint8 :count (length *redirect.wav*) :initial-contents *redirect.wav*))
    (setf *snd-redirect* (mix-loadwav-rw (sdl-rwfromconstmem *snd-redirect-array* (length *redirect.wav*)) -1))
    ;; Slope sound effect
    (setf *snd-slope-array* (foreign-alloc :uint8 :count (length *slope.wav*) :initial-contents *slope.wav*))
    (setf *snd-slope* (mix-loadwav-rw (sdl-rwfromconstmem *snd-slope-array* (length *slope.wav*)) -1))
    ;; Hit vacuum sound effect
    (setf *snd-hit-vacuum-array* (foreign-alloc :uint8 :count (length *hit-vacuum.wav*) :initial-contents *hit-vacuum.wav*))
    (setf *snd-hit-vacuum* (mix-loadwav-rw (sdl-rwfromconstmem *snd-hit-vacuum-array* (length *hit-vacuum.wav*)) -1))
    ;; Hit automaton sound effect
    (setf *snd-hit-automaton-array* (foreign-alloc :uint8 :count (length *hit-automaton.wav*) :initial-contents *hit-automaton.wav*))
    (setf *snd-hit-automaton* (mix-loadwav-rw (sdl-rwfromconstmem *snd-hit-automaton-array* (length *hit-automaton.wav*)) -1))
    ;; Automaton key sound effect
    (setf *snd-automaton-key-array* (foreign-alloc :uint8 :count (length *automaton-key.wav*) :initial-contents *automaton-key.wav*))
    (setf *snd-automaton-key* (mix-loadwav-rw (sdl-rwfromconstmem *snd-automaton-key-array* (length *automaton-key.wav*)) -1))
    (mix-playmusic *music* -1)))

(defun ui-close-audio ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (mix-freemusic *music*)
    (foreign-free *music-array*)
    (setf *music-array* (null-pointer))

    (mix-freechunk *snd-special*)
    (foreign-free *snd-special-array*)
    (setf *snd-special-array* (null-pointer))

    (mix-freechunk *snd-hit-wall*)
    (foreign-free *snd-hit-wall-array*)
    (setf *snd-hit-wall-array* (null-pointer))

    (mix-freechunk *snd-explosion*)
    (foreign-free *snd-explosion-array*)
    (setf *snd-explosion-array* (null-pointer))

    (mix-freechunk *snd-bomb-on*)
    (foreign-free *snd-bomb-on-array*)
    (setf *snd-bomb-on-array* (null-pointer))

    (mix-freechunk *snd-exit-error*)
    (foreign-free *snd-exit-error-array*)
    (setf *snd-exit-error-array* (null-pointer))

    (mix-freechunk *snd-exit-ok*)
    (foreign-free *snd-exit-ok-array*)
    (setf *snd-exit-ok-array* (null-pointer))

    (mix-freechunk *snd-key-collect*)
    (foreign-free *snd-key-collect-array*)
    (setf *snd-key-collect-array* (null-pointer))

    (mix-freechunk *snd-hit-counter*)
    (foreign-free *snd-hit-counter-array*)
    (setf *snd-hit-counter-array* (null-pointer))

    (mix-freechunk *snd-pulled-activate*)
    (foreign-free *snd-pulled-activate-array*)
    (setf *snd-pulled-activate-array* (null-pointer))

    (mix-freechunk *snd-redirect*)
    (foreign-free *snd-redirect-array*)
    (setf *snd-redirect-array* (null-pointer))

    (mix-freechunk *snd-slope*)
    (foreign-free *snd-slope-array*)
    (setf *snd-slope-array* (null-pointer))

    (mix-freechunk *snd-hit-vacuum*)
    (foreign-free *snd-hit-vacuum-array*)
    (setf *snd-hit-vacuum-array* (null-pointer))

    (mix-freechunk *snd-hit-automaton*)
    (foreign-free *snd-hit-automaton-array*)
    (setf *snd-hit-automaton-array* (null-pointer))

    (mix-freechunk *snd-automaton-key*)
    (foreign-free *snd-automaton-key-array*)
    (setf *snd-automaton-key-array* (null-pointer))

    (mix-quit)))

(defun ui-play-sound (id)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (ecase id
      (:explosion       (mix-playchanneltimed -1 *snd-explosion*       0 -1))
      (:hit-wall        (mix-playchanneltimed -1 *snd-hit-wall*        0 -1))
      (:bomb-on         (mix-playchanneltimed -1 *snd-bomb-on*         0 -1))
      (:exit-error      (mix-playchanneltimed -1 *snd-exit-error*      0 -1))
      (:exit-ok         (mix-playchanneltimed -1 *snd-exit-ok*         0 -1))
      (:key-collect     (mix-playchanneltimed -1 *snd-key-collect*     0 -1))
      (:hit-counter     (mix-playchanneltimed -1 *snd-hit-counter*     0 -1))
      (:pulled-activate (mix-playchanneltimed -1 *snd-pulled-activate* 0 -1))
      (:redirect        (mix-playchanneltimed -1 *snd-redirect*        0 -1))
      (:slope           (mix-playchanneltimed -1 *snd-slope*           0 -1))
      (:hit-vacuum      (mix-playchanneltimed -1 *snd-hit-vacuum*      0 -1))
      (:hit-automaton   (mix-playchanneltimed -1 *snd-hit-automaton*   0 -1))
      (:automaton-key   (mix-playchanneltimed -1 *snd-automaton-key*   0 -1))
      (:special         (mix-playchanneltimed -1 *snd-special*         0 -1)))))

(defun ui-on-level-changed ()
  (setf *ui-hint-time* +UI-HINT-DELAY+))

(defun ui-sdl2-format-info-message (program level-number num-levels level-name level-hint)
  (if (> (length level-hint) 0)
      (if (> (length program) 0)
          (format nil "Program \"~A\"~C~A" program #\return level-hint)
          (format nil "Level ~A/~A \"~A\"~C~A" (1+ level-number) num-levels level-name #\return level-hint))
      (if (> (length program) 0)
          (format nil "Program \"~A\"" program)
          (format nil "Level ~A/~A \"~A\"" (1+ level-number) num-levels level-name))))

(defun ui-update-hint-time ()
  (when (< *ui-hint-time* 1.0)
    (setf *ui-hint-time* (+ *ui-hint-time* 0.05))
    (when (> *ui-hint-time* 1.0)
      (setf *ui-hint-time* 1.0))))

(defun ui-ease (p)
  (if (< p 0)
      0
      ;; Based on Robert Penner's BackEaseOut easing function
      (let* ((f  (- 1 p))
             (f3 (* f f f))
             (s  (sin (* f PI))))
        (- 1 (- f3 (* f s))))))

(defun ui-sdl2-load-font ()
  (setf *IBMPlexMono-Bold-array* (foreign-alloc :uint8 :count (length *IBMPlexMono-Bold.ttf*) :initial-contents *IBMPlexMono-Bold.ttf*))
  (setf *IBMPlexMono-Bold* (ttf-openfontrw (sdl-rwfromconstmem *IBMPlexMono-Bold-array* (length *IBMPlexMono-Bold.ttf*)) -1 16)))

(defun ui-read-input-impl-keydown (event previous)
  (cffi:with-foreign-slots ((keysym) event sdl-keyboardevent)
    (let* ((scancode (getf keysym 'scancode))
           (modstate (sdl-getmodstate))
           (shift    (/= (logand modstate +KMOD-SHIFT+) 0)))
      (cond ((eq scancode :SDL-SCANCODE-LEFT)      (if shift :prev :west))
            ((eq scancode :SDL-SCANCODE-RIGHT)     (if shift :next :east))
            ((eq scancode :SDL-SCANCODE-UP)        :north)
            ((eq scancode :SDL-SCANCODE-DOWN)      :south)
            ((eq scancode :SDL-SCANCODE-R)         :restart)
            ((eq scancode :SDL-SCANCODE-SPACE)     :action1)
            ((eq scancode :SDL-SCANCODE-O)         :action2)
            ((eq scancode :SDL-SCANCODE-X)         :action3)
            ((eq scancode :SDL-SCANCODE-BACKSPACE) :backspace)
            ((eq scancode :SDL-SCANCODE-B)         :back)
            ((eq scancode :SDL-SCANCODE-ESCAPE)    :back)
            (t previous)))))

(defparameter *controller-button-plist*
  '((:SDL-CONTROLLER-BUTTON-A          . :action1)
    (:SDL-CONTROLLER-BUTTON-B          . :action2)
    (:SDL-CONTROLLER-BUTTON-X          . :action3)
    (:SDL-CONTROLLER-BUTTON-Y          . :backspace)
    (:SDL-CONTROLLER-BUTTON-MISC1      . :restart)
    (:SDL-CONTROLLER-BUTTON-BACK       . :prev)
    (:SDL-CONTROLLER-BUTTON-START      . :next)
    (:SDL-CONTROLLER-BUTTON-DPAD-UP    . :north)
    (:SDL-CONTROLLER-BUTTON-DPAD-DOWN  . :south)
    (:SDL-CONTROLLER-BUTTON-DPAD-LEFT  . :west)
    (:SDL-CONTROLLER-BUTTON-DPAD-RIGHT . :east)))

(defun ui-helper-is-controller-axis-activated (axis)
  (let ((value (sdl-gamecontrollergetaxis *controller* axis)))
    (if (>= (abs value) +UI-JOYSTICK-AXIS-NOISE+)
        value
        nil)))

(defun ui-read-input-poll-controller (result)
  (unless *controller*
    (return-from ui-read-input-poll-controller result))
  (when (or (ui-helper-is-controller-axis-activated :SDL-CONTROLLER-AXIS-TRIGGERLEFT)
            (ui-helper-is-controller-axis-activated :SDL-CONTROLLER-AXIS-TRIGGERRIGHT))
    (return-from ui-read-input-poll-controller :action1))
  (let ((leftx  (ui-helper-is-controller-axis-activated :SDL-CONTROLLER-AXIS-LEFTX))
        (rightx (ui-helper-is-controller-axis-activated :SDL-CONTROLLER-AXIS-RIGHTX))
        (lefty  (ui-helper-is-controller-axis-activated :SDL-CONTROLLER-AXIS-LEFTY))
        (righty (ui-helper-is-controller-axis-activated :SDL-CONTROLLER-AXIS-RIGHTY)))
    (when leftx  (return-from ui-read-input-poll-controller (if (< leftx  0) :west  :east)))
    (when rightx (return-from ui-read-input-poll-controller (if (< rightx 0) :west  :east)))
    (when lefty  (return-from ui-read-input-poll-controller (if (< lefty 0)  :north :south)))
    (when righty (return-from ui-read-input-poll-controller (if (< righty 0) :north :south))))
  (loop for cell in *controller-button-plist*
        do (when (/= (sdl-gamecontrollergetbutton *controller* (car cell)) 0)
             (return-from ui-read-input-poll-controller (cdr cell))))
  result)

(defun ui-read-input-impl ()
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (let ((result nil))
      (with-foreign-objects ((event '(:union sdl-event)))
        (loop while (/= (sdl-pollevent event) 0)
              do
                 (cffi:with-foreign-slots ((type jhat cbutton) event (:union sdl-event))
                   ;; (format t "~%TYPE IS ~A~%" type)
                   (cond ((eq type :SDL-KEYDOWN) (setf result (ui-read-input-impl-keydown       event   result)))
                         ((eq type :SDL-QUIT)    (setf result :back)))))
        (setf result (ui-read-input-poll-controller result)))
      result)))

(defun ui-read-input ()
  #+sbcl
  (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
    (ui-read-input-impl))
  #+ccl
  (ui-read-input-impl))