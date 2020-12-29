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

;; Methods

(defmethod update ((self list))
  (let ((level (get-current-level)))
    (when (runningp)
      (attach-created)
      (loop for crate in level
            do (update crate))
      (purge-lamented))))

(defmethod render ((self list))
  (when (runningp)
    (let ((level (get-current-level)))
      (loop for crate in level
            do (render crate)))))

;; Functions

(defun contains-keys-p ()
  (find-if #'(lambda (crate)
               (eq (type-of crate) 'key)) *level*))

(defun purge-lamented ()
  (setf *level* (remove-if #'(lambda (crate)
                               (let ((type (type-of crate)))
                                 (unless (eq type 'player)
                                   (lamentedp crate)))) *level*)))

(defun attach-created ()
  (let ((l (last *level*)))
    (loop for crate in *created*
          do (push crate (cdr l))
          (setf l crate)))
  (setf *created* nil))

(defun request-attaching (crate)
  (setf *created* (cons crate *created*)))
