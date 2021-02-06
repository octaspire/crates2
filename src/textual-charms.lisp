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

(defparameter *crates2-window* nil)

(defun ui-init ()
  (setf *crates2-window* (cl-charms/low-level:initscr))
  (cl-charms:disable-echoing)
  (cl-charms:enable-raw-input)
  ;; (cl-charms/low-level:raw)
  (cl-charms/low-level:curs-set 0)
  ;; Don't block on getch
  (cl-charms/low-level:timeout 0)
  (cl-charms/low-level:keypad *crates2-window* cl-charms/low-level:TRUE)
  (cl-charms/low-level:keypad cl-charms/low-level:*stdscr* cl-charms/low-level:TRUE))

(defun ui-delete ()
  (cl-charms/low-level:endwin))

(defun ui-read-input ()
  (let ((c (cl-charms/low-level:wgetch *crates2-window*)))
    (log:debug "Input is ~A" c)
    (cond
      ((or (= c (char-code #\w))
           (= c cl-charms/low-level:KEY_UP)) :north)
      ((or (= c (char-code #\s))
           (= c cl-charms/low-level:KEY_DOWN)) :south)
      ((or (= c (char-code #\a))
           (= c cl-charms/low-level:KEY_LEFT)) :west)
      ((or (= c (char-code #\d))
           (= c cl-charms/low-level:KEY_RIGHT)) :east)
      ((or (= c (char-code #\q))
           ;; Escape
           (= c 27)) :back)
      ((= c (char-code #\r)) :restart)
      (t nil))))

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

(defun ui-render (level step)
  (multiple-value-bind (lines x-axis bar) (ui-generate level step)
    (cl-charms/low-level:clear)
    (cl-charms/low-level:mvaddstr 0 0 x-axis)
    (cl-charms/low-level:mvaddstr 1 0 (format nil "  +~A+ Level ~A~%" bar *level-number*))
    (loop for line across lines
          for y from 0
          do (if (= (mod y ch) 0)
                 (cl-charms/low-level:mvaddstr (+ 2 y) 0 (format nil "~2d|~A|" (floor y ch) line))
                 (cl-charms/low-level:mvaddstr (+ 2 y) 0 (format nil "  |~A|" line)))
             (if (= y 0)
                 (cl-charms/low-level:mvaddstr (+ 3 y) 0 (format nil " Input: ~@[~A~]" *last-input*))
                 (when (= y 1)
                   (cl-charms/low-level:mvaddstr (+ 3 y) 0 (format nil " #updates: ~A~%" *update-counter*))))
          finally (cl-charms/low-level:mvaddstr (+ 3 y) 0 (format nil "  +~A+" bar)))
    (cl-charms/low-level:refresh))
  (setf *last-input* nil))
