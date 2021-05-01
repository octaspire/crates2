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

(defun ui-init (options)
  ;; Nothing to do.
  )

(defun ui-delete ()
  ;; Nothing to do.
  )

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

(defun ui-render (level step)
  (multiple-value-bind (lines x-axis bar) (ui-generate level step)
    (format t "~%  ~A~%" x-axis)
    (if (> (length *program*) 0)
        (format t "  +~A+ Program \"~A\"~%" bar *program*)
        (format t "  +~A+ Level ~A ~A [~A]~%" bar *level-number* (car *infos*) (cadr *infos*)))
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
