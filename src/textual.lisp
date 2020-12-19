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

(defun ui-render (level)
  (let ((lines (empty-level))
        (bar (format nil "~v@{~A~:*~}" *level-width* #\-)))
    (loop for crate in level
          do (progn
               (let* ((x (x crate))
                      (y (y crate))
                      (z (z crate))
                      (v (visual crate))
                      (line (aref lines y)))
                 (when v
                   (setf (aref line x) v)))))
    (format t "~%+~A+~%" bar)
    (loop for line across lines
          do (format t "|~A|~%" line))
    (format t "+~A+~%" bar)))