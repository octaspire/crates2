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

(defun empty-line ()
  (let* ((w *level-width*)
         (maxi (- w 1))
         (s (string "")))
    (loop for i from 0 to maxi
          do (setf s (concatenate 'string s "   ")))
    s))

(defun empty-level ()
  (let* ((w (* 2 *level-height*))
         (maxi (- w 1))
         (a (make-array w)))
    (loop for i from 0 to maxi
          do (setf (aref a i) (empty-line)))
    a))

(defparameter *fake-input*
  (list nil :east nil nil nil nil nil nil nil nil nil nil nil nil nil nil ; 0
        :west nil nil nil nil nil nil nil ; 1
        :west nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil ; 2
        :west))                           ; 3

(defparameter *last-fake-input* nil)

(defun ui-input ()
  (let ((input (car *fake-input*)))
    (setf *fake-input* (cdr *fake-input*))
    (setf *last-fake-input* input)
    input))

(defun x-axis (length)
  (let ((result ""))
    (loop for i from 0 to (- length 1)
          do (setf result
                   (concatenate 'string result (format nil "~3d" i))))
    result))

(defun ui-render (level)
  (let ((lines (empty-level))
        (x-axis (x-axis *level-width*))
        (bar (format nil "~v@{~A~:*~}" (* 3 *level-width*) #\-)))
    (loop for crate in level
          do (progn
               (let* ((x (crate-x crate))
                      (y (crate-y crate))
                      (z (crate-z crate))
                      (v (visual crate))
                      (v1 (subseq v 0 3))
                      (v2 (subseq v 3 6))
                      (line1 (aref lines (* 2 y)))
                      (line2 (aref lines (+ (* 2 y) 1))))
                 (when v
                   (setf (aref line1 (* 3 x)) (coerce (subseq v1 0 1) 'character))
                   (setf (aref line1 (+ (* 3 x) 1)) (coerce (subseq v1 1 2) 'character))
                   (setf (aref line1 (+ (* 3 x) 2)) (coerce (subseq v1 2 3) 'character))
                   (setf (aref line2 (* 3 x)) (coerce (subseq v2 0 1) 'character))
                   (setf (aref line2 (+ (* 3 x) 1)) (coerce (subseq v2 1 2) 'character))
                   (setf (aref line2 (+ (* 3 x) 2)) (coerce (subseq v2 2 3) 'character))))))
    (format t "~%  ~A~%" x-axis)
    (format t "  +~A+ Level ~A~%" bar *level-number*)
    (loop for line across lines
          for y from 0
          do (if (= (mod y 2) 0)
                 (format t "~2d|~A|" (floor y 2) line)
                 (format t "  |~A|" line))
             (if (= y 0)
                 (format t " Input: ~@[~A~]~%" *last-fake-input*)
                 (if (= y 1)
                     (format t " #updates: ~A~%" *update-counter*)
                     (format t "~%"))))
    (format t "  +~A+~%" bar))
  (setf *last-fake-input* nil))
