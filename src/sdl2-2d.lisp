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
(defconstant cw 24)
(defconstant ch 24)
;; Window dimensions
(defconstant screen-width 800)
(defconstant screen-height 600)

(defparameter *crates2-window* nil)
(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-idle" *visual-hash*) #("+----+" "|suck|" "+----+"))
  (setf (gethash "vacuum-full" *visual-hash*) #("+----+" "|SUCK|" "+----+"))
  ;; WALL
  (setf (gethash "wall-idle" *visual-hash*) #("######" "######" "######"))
  ;; PUSHED
  (setf (gethash "pushed-idle" *visual-hash*) #("+----+" "|PUSH|" "+----+"))
  ;; BLOCK-TIMER
  (setf (gethash "block-timer-durable" *visual-hash*) #("+----+" "BTXX  " "+----+"))
  (setf (gethash "block-timer"         *visual-hash*) #("+----+" "BT    " "+----+"))
  ;; COUNT
  (setf (gethash "count-01"            *visual-hash*) #("      " "    01" "      "))
  (setf (gethash "count-02"            *visual-hash*) #("      " "    02" "      "))
  (setf (gethash "count-03"            *visual-hash*) #("      " "    03" "      "))
  (setf (gethash "count-04"            *visual-hash*) #("      " "    04" "      "))
  (setf (gethash "count-05"            *visual-hash*) #("      " "    05" "      "))
  (setf (gethash "count-06"            *visual-hash*) #("      " "    06" "      "))
  (setf (gethash "count-07"            *visual-hash*) #("      " "    07" "      "))
  (setf (gethash "count-08"            *visual-hash*) #("      " "    08" "      "))
  (setf (gethash "count-09"            *visual-hash*) #("      " "    09" "      "))
  (setf (gethash "count-10"            *visual-hash*) #("      " "    10" "      "))
  (setf (gethash "count-11"            *visual-hash*) #("      " "    11" "      "))
  (setf (gethash "count-12"            *visual-hash*) #("      " "    12" "      "))
  (setf (gethash "count-13"            *visual-hash*) #("      " "    13" "      "))
  (setf (gethash "count-14"            *visual-hash*) #("      " "    14" "      "))
  (setf (gethash "count-15"            *visual-hash*) #("      " "    15" "      "))
  (setf (gethash "count-16"            *visual-hash*) #("      " "    16" "      "))
  (setf (gethash "count-17"            *visual-hash*) #("      " "    17" "      "))
  (setf (gethash "count-18"            *visual-hash*) #("      " "    18" "      "))
  (setf (gethash "count-19"            *visual-hash*) #("      " "    19" "      "))
  (setf (gethash "count-20"            *visual-hash*) #("      " "    20" "      "))
  (setf (gethash "count-21"            *visual-hash*) #("      " "    21" "      "))
  (setf (gethash "count-22"            *visual-hash*) #("      " "    22" "      "))
  (setf (gethash "count-23"            *visual-hash*) #("      " "    23" "      "))
  ;; EXIT
  (setf (gethash "exit-idle" *visual-hash*) #("+----+" "|exit|" "+----+"))
  (setf (gethash "exit-active" *visual-hash*) #("+----+" "|EXIT|" "+----+"))
  ;; KEY
  (setf (gethash "key-idle" *visual-hash*) #("+----+" " KEY  " "+----+"))
  ;; PLAYER
  (setf (gethash "player-active" *visual-hash*) #(" .--. " " |  | " " `--' "))
  (setf (gethash "player-hidden" *visual-hash*) #("      " "      " "      "))
  ;; SLOPES
  (setf (gethash "slope-en" *visual-hash*) #("+--+  " "|   \\ " "+----+"))
  (setf (gethash "slope-es" *visual-hash*) #("+----+" "|   / " "+--+  "))
  (setf (gethash "slope-wn" *visual-hash*) #("  +--+" " /   |" "+----+"))
  (setf (gethash "slope-ws" *visual-hash*) #("+----+" " \\   |" "  +--+"))
  ;; TURNSTILE
  (setf (gethash "turnstile-e1" *visual-hash*) #("+----+" "---->1" "+----+"))
  (setf (gethash "turnstile-w1" *visual-hash*) #("+----+" "1<----" "+----+"))
  (setf (gethash "turnstile-n1" *visual-hash*) #("+1111+" "^^^^^^" "+||||+"))
  (setf (gethash "turnstile-s1" *visual-hash*) #("+||||+" "\\/\\/\\/" "+1111+"))
  (setf (gethash "turnstile-e" *visual-hash*) #("+----+" "----->" "+----+"))
  (setf (gethash "turnstile-w" *visual-hash*) #("+----+" "<-----" "+----+"))
  (setf (gethash "turnstile-n" *visual-hash*) #("+^^^^+" "||||||" "+||||+"))
  (setf (gethash "turnstile-s" *visual-hash*) #("+....+" "||||||" "\\/\\/\\/"))
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter-10" *visual-hash*) #("+----+" "BCXX10" "+----+"))
  (setf (gethash "block-counter-09" *visual-hash*) #("+----+" "BCXX09" "+----+"))
  (setf (gethash "block-counter-08" *visual-hash*) #("+----+" "BCXX08" "+----+"))
  (setf (gethash "block-counter-07" *visual-hash*) #("+----+" "BCXX07" "+----+"))
  (setf (gethash "block-counter-06" *visual-hash*) #("+----+" "BCXX06" "+----+"))
  (setf (gethash "block-counter-05" *visual-hash*) #("+----+" "BCXX05" "+----+"))
  (setf (gethash "block-counter-04" *visual-hash*) #("+----+" "BCXX04" "+----+"))
  (setf (gethash "block-counter-03" *visual-hash*) #("+----+" "BCXX03" "+----+"))
  (setf (gethash "block-counter-02" *visual-hash*) #("+----+" "BCXX02" "+----+"))
  (setf (gethash "block-counter-01" *visual-hash*) #("+----+" "BCXX01" "+----+"))
  (setf (gethash "block-counter-00" *visual-hash*) #("+----+" "BCXX00" "+----+"))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter-10" *visual-hash*) #("+----+" "PC  10" "+----+"))
  (setf (gethash "pass-counter-09" *visual-hash*) #("+----+" "PC  09" "+----+"))
  (setf (gethash "pass-counter-08" *visual-hash*) #("+----+" "PC  08" "+----+"))
  (setf (gethash "pass-counter-07" *visual-hash*) #("+----+" "PC  07" "+----+"))
  (setf (gethash "pass-counter-06" *visual-hash*) #("+----+" "PC  06" "+----+"))
  (setf (gethash "pass-counter-05" *visual-hash*) #("+----+" "PC  05" "+----+"))
  (setf (gethash "pass-counter-04" *visual-hash*) #("+----+" "PC  04" "+----+"))
  (setf (gethash "pass-counter-03" *visual-hash*) #("+----+" "PC  03" "+----+"))
  (setf (gethash "pass-counter-02" *visual-hash*) #("+----+" "PC  02" "+----+"))
  (setf (gethash "pass-counter-01" *visual-hash*) #("+----+" "PC  01" "+----+"))
  (setf (gethash "pass-counter-00" *visual-hash*) #("+----+" "PC  00" "+----+"))
  ;; PASS-TIMER
  (setf (gethash "pass-timer-10" *visual-hash*) #("+----+" "PT  10" "+----+"))
  (setf (gethash "pass-timer-09" *visual-hash*) #("+----+" "PT  09" "+----+"))
  (setf (gethash "pass-timer-08" *visual-hash*) #("+----+" "PT  08" "+----+"))
  (setf (gethash "pass-timer-07" *visual-hash*) #("+----+" "PT  07" "+----+"))
  (setf (gethash "pass-timer-06" *visual-hash*) #("+----+" "PT  06" "+----+"))
  (setf (gethash "pass-timer-05" *visual-hash*) #("+----+" "PT  05" "+----+"))
  (setf (gethash "pass-timer-04" *visual-hash*) #("+----+" "PT  04" "+----+"))
  (setf (gethash "pass-timer-03" *visual-hash*) #("+----+" "PT  03" "+----+"))
  (setf (gethash "pass-timer-02" *visual-hash*) #("+----+" "PT  02" "+----+"))
  (setf (gethash "pass-timer-01" *visual-hash*) #("+----+" "PT  01" "+----+"))
  (setf (gethash "pass-timer-00" *visual-hash*) #("+----+" "PT  00" "+----+"))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) #(".-  -." "|    |" "`-  -'"))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) #("      " "    o " "      "))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) #("      " "    = " "      "))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) #("      " "      " "      "))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) #("      " " o    " "      "))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) #("      " " =    " "      "))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) #("      " "      " "      "))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) #("  oo  " "      " "      "))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) #("  ==  " "      " "      "))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) #("  --  " "      " "      "))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) #("      " "      " "  oo  "))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) #("      " "      " "  ==  "))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) #("      " "      " "  --  "))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) #("      " "   +  " "      "))
  (setf (gethash "stepper-active"             *visual-hash*) #("      " "   o  " "      "))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) #("+-  -+" "|    |" "+-  -+"))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) #("      " "    o " "      "))
  (setf (gethash "toggle-east-off"            *visual-hash*) #("      " "    | " "      "))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) #("      " " o    " "      "))
  (setf (gethash "toggle-west-off"            *visual-hash*) #("      " " |    " "      "))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) #("  oo  " "      " "      "))
  (setf (gethash "toggle-north-off"           *visual-hash*) #("  __  " "      " "      "))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) #("      " "      " "  oo  "))
  (setf (gethash "toggle-south-off"           *visual-hash*) #("      " "      " "  ''  "))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) #("+----+" "BOMP  " "+----+"))
  (setf (gethash "bomb"          *visual-hash*) #("+----+" "BOMX  " "+----+"))
  ;; ring 1
  ;; ******************
  ;; ******************
  ;; ******************
  ;; ******      ******
  ;; ******      ******
  ;; ******      ******
  ;; ******************
  ;; ******************
  ;; ******************
  (setf (gethash "bomb-ring-1"   *visual-hash*) #("******************" "******************" "******************" "******      ******" "******      ******" "******      ******" "******************" "******************" "******************"))
  ;; ring 2
  ;; ##############################
  ;; ##############################
  ;; ##############################
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ##############################
  ;; ##############################
  ;; ##############################
  (setf (gethash "bomb-ring-2"   *visual-hash*) #("##############################" "##############################" "##############################" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "##############################" "##############################" "##############################")))

(defparameter *fake-input* nil)

(defparameter *last-input* nil)

(defun ui-generate (level step)
  (let ((lines (empty-level))
        (x-axis (x-axis *level-width*))
        (bar (format nil "~v@{~A~:*~}" (* cw *level-width*) #\-)))
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
                              (let* ((vivh (length viv))
                                     (dy (truncate (/ (- ch vivh) 2))))
                                (loop for liney from 0 to (- vivh 1)
                                      do (let* ((str (aref viv liney))
                                                (vivw (length str))
                                                (dx (truncate (/ (- cw vivw) 2)))
                                                (finy (truncate (+ (* y ch) dy liney))))
                                           (when (>= finy 0)
                                             (let* ((line (aref lines finy))
                                                    (finx (* x cw))
                                                    (deltax (truncate (+ finx dx))))
                                               (setf line (replace-substr-at-transparent-whitespace line deltax str)))))))))))))
    (values lines x-axis bar)))

(defun ui-init ()
  (setf *crates2-window* (sdl2:init))
  
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
