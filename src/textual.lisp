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
          do (setf s (concatenate 'string s " ")))
    s))

(defun empty-level ()
  (let* ((maxi (- *level-height* 1))
         (a (make-array *level-height*)))
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
          do (let ((num (mod i 10)))
               (setf result (concatenate 'string result (write-to-string num)))))
    result))

(defun ui-render (level)
  (let ((lines (empty-level))
        (x-axis (x-axis *level-width*))
        (bar (format nil "~v@{~A~:*~}" *level-width* #\-)))
    (loop for crate in level
          do (progn
               (let* ((x (crate-x crate))
                      (y (crate-y crate))
                      (z (crate-z crate))
                      (v (visual crate))
                      (line (aref lines y)))
                 (when v
                   (setf (aref line x) v)))))
    (format t "~%  ~A~%" x-axis)
    (format t " +~A+ Level ~A~%" bar *level-number*)
    (loop for line across lines
          for y from 0
          do (format t "~A|~A|" (mod y 10) line)
             (if (= y 0)
                 (format t " Input: ~@[~A~]~%" *last-fake-input*)
                 (if (= y 1)
                     (format t " #updates: ~A~%" *update-counter*)
                     (format t "~%"))))
    (format t " +~A+~%" bar))
  (setf *last-fake-input* nil))
