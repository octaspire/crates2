;; Octaspire Crates 2 - Puzzle Game
;; Copyright 2020 octaspire.com
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

;; Crate is drawn as CW x CH character shape
(defconstant cw 6)
(defconstant ch 3)

(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-idle" *visual-hash*) #("+----+" "|suck|" "+----+"))
  (setf (gethash "vacuum-full" *visual-hash*) #("+----+" "|SUCK|" "+----+"))
  ;; WALL
  (setf (gethash "wall-idle" *visual-hash*) #("+----+" "|XXXX|" "+----+"))
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
  (setf (gethash "player-active" *visual-hash*) #("+----+" "|oooo|" "+----+"))
  (setf (gethash "player-hidden" *visual-hash*) #("      " "      " "      "))
  ;; SLOPES
  (setf (gethash "slope-en" *visual-hash*) #("+--+  " "|   \\ " "+----+"))
  (setf (gethash "slope-es" *visual-hash*) #("+----+" "|   / " "+--+  "))
  (setf (gethash "slope-wn" *visual-hash*) #("  +--+" " /   |" "+----+"))
  (setf (gethash "slope-ws" *visual-hash*) #("+----+" " \\   |" "  +--+"))
  ;; TURNSTILE
  (setf (gethash "turnstile-e1" *visual-hash*) #("+----+" ">>>>>1" "+----+"))
  (setf (gethash "turnstile-w1" *visual-hash*) #("+----+" "<<<<<1" "+----+"))
  (setf (gethash "turnstile-n1" *visual-hash*) #("+1111+" "^^^^^^" "+^^^^+"))
  (setf (gethash "turnstile-s1" *visual-hash*) #("+....+" "......" "+1111+"))
  (setf (gethash "turnstile-e" *visual-hash*) #("+>>>>+" ">>>>>>" "+>>>>+"))
  (setf (gethash "turnstile-w" *visual-hash*) #("+<<<<+" "<<<<<<" "+<<<<+"))
  (setf (gethash "turnstile-n" *visual-hash*) #("+^^^^+" "^^^^^^" "+^^^^+"))
  (setf (gethash "turnstile-s" *visual-hash*) #("+....+" "......" "+....+"))
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
  (setf (gethash "pulled-idle"               *visual-hash*) #("-- ---" "      " "-- ---"))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) #("      " "     o" "      "))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) #("      " "     +" "      "))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) #("      " "     |" "      "))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) #("      " "o     " "      "))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) #("      " "+     " "      "))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) #("      " "|     " "      "))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) #("  o   " "      " "      "))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) #("  +   " "      " "      "))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) #("  -   " "      " "      "))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) #("      " "      " "  o   "))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) #("      " "      " "  +   "))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) #("      " "      " "  -   ")))

(defun empty-line ()
  (let* ((w *level-width*)
         (maxi (- w 1))
         (cs (make-array cw :element-type 'character :fill-pointer cw :initial-element #\Space :adjustable nil))
         (s (string "")))
    (loop for i from 0 to maxi
          do (setf s (concatenate 'string s cs)))
    s))

(defun empty-level ()
  (let* ((w (* ch *level-height*))
         (maxi (- w 1))
         (a (make-array w)))
    (loop for i from 0 to maxi
          do (setf (aref a i) (empty-line)))
    a))

(defparameter *fake-input* nil)

(defparameter *last-input* nil)

(defun ui-read-input ()
  (format t "Input: ")
  (finish-output)
  (let ((c (read-char)))
    (case c
      (#\w :north)
      (#\s :south)
      (#\a :west)
      (#\d :east)
      (#\q :back)
      (#\r :restart)
      (otherwise nil))))

(defun ui-maybe-read-input ()
  (let ((player (find-first-crate-of-type 'player)))
    (if (and player (movingp player))
        nil                    ; No input while player moves, in textual mode.
        (setf *last-input* (ui-read-input)))))

(defun ui-input ()
  (if *test-run*
      (let ((input (car *fake-input*)))
        (setf *fake-input* (cdr *fake-input*))
        (setf *last-input* input)
        input)
      (ui-maybe-read-input)))

(defun x-axis (length)
  (let ((result ""))
    (loop for i from 0 to (- length 1)
          do (setf result
                   (concatenate 'string result (format nil "~vd" cw i))))
    result))

(defun ui-render (level)
  (let ((lines (empty-level))
        (x-axis (x-axis *level-width*))
        (bar (format nil "~v@{~A~:*~}" (* cw *level-width*) #\-)))
    (loop for crate in level
          do (progn
               (let* ((x (crate-x crate))
                      (y (crate-y crate))
                      (z (crate-z crate))
                      (vids (visual crate)))
                 (loop for vid in vids
                       do
                          (let ((viv (gethash vid *visual-hash*)))
                            (when viv
                              (loop for liney from 0 to (- ch 1)
                                    do (let ((str (aref viv liney))
                                             (line (aref lines (+ (* y ch) liney)))
                                             (deltax (* x cw)))
                                         (setf line (replace-substr-at-transparent-whitespace line deltax str))))))))))
    (format t "~%  ~A~%" x-axis)
    (format t "  +~A+ Level ~A~%" bar *level-number*)
    (loop for line across lines
          for y from 0
          do (if (= (mod y ch) 0)
                 (format t "~2d|~A|" (floor y ch) line)
                 (format t "  |~A|" line))
             (if (= y 0)
                 (format t " Input: ~@[~A~]~%" *last-input*)
                 (if (= y 1)
                     (format t " #updates: ~A~%" *update-counter*)
                     (format t "~%"))))
    (format t "  +~A+~%" bar))
  (setf *last-input* nil))
