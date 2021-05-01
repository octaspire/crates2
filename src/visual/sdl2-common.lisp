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

(defconstant +UI-HINT-DELAY+ -5)
(defparameter *ui-hint-time* +UI-HINT-DELAY+)

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
  (ttf-openfont "etc/assets/font/IBM/Plex/IBMPlexMono-Bold.ttf" 16))

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
                                                     (cond ((eq scancode :SDL-SCANCODE-LEFT)      (setf result (if shift :prev :west)))
                                                           ((eq scancode :SDL-SCANCODE-RIGHT)     (setf result (if shift :next :east)))
                                                           ((eq scancode :SDL-SCANCODE-UP)        (setf result :north))
                                                           ((eq scancode :SDL-SCANCODE-DOWN)      (setf result :south))
                                                           ((eq scancode :SDL-SCANCODE-R)         (setf result :restart))
                                                           ((eq scancode :SDL-SCANCODE-SPACE)     (setf result :action1))
                                                           ((eq scancode :SDL-SCANCODE-O)         (setf result :action2))
                                                           ((eq scancode :SDL-SCANCODE-X)         (setf result :action3))
                                                           ((eq scancode :SDL-SCANCODE-BACKSPACE) (setf result :backspace))
                                                           ((eq scancode :SDL-SCANCODE-B)         (setf result :back))
                                                           ((eq scancode :SDL-SCANCODE-ESCAPE)    (setf result :back))))))
                         ((eq type :SDL-QUIT) (setf result :back))))))
      result)))