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
  (when (runningp)
      (attach-created)
      (loop for crate in self
            do (update crate))
      (attach-and-update-new-updatables)
      (remove-marked-updatables)
      (purge-lamented)))

(defmethod render ((self list))
  (when (runningp)
    (let ((level (get-current-level)))
      (loop for crate in level
            do (render crate)))))

;; Functions

(defun contains-keys-p ()
  (find-if #'(lambda (crate)
               (eq (type-of crate) 'key)) *level*))

(defun contains-off-toggles-p ()
  (find-if #'(lambda (crate)
               (and (eq (type-of crate) 'toggle)
                    (not (toggle-on-p crate)))) *level*))

(defun find-first-crate-of-type (type)
  (find-if #'(lambda (crate)
               (subtypep (type-of crate) type)) *level*))

(defun purge-lamented ()
  (setf *level* (remove-if #'(lambda (crate)
                               (let ((type (type-of crate)))
                                 (unless (eq type 'player)
                                   (lamentedp crate)))) *level*)))

(defun attach-created ()
  (let ((l (last *level*)))
    (loop for crate in *created*
          do (push crate (cdr l))
             (setf l crate)
             (when (typep crate 'crates2:updatable)
               (crates2:add-updatable crate))))
  (setf *created* nil))

(defun attach-and-update-new-updatables ()
  (loop for crate in *level-updatables-addlist*
        do (update crate))
  (setf *level-updatables* (append *level-updatables* *level-updatables-addlist*))
  (setf *level-updatables-addlist* nil))

(defun remove-marked-updatables ()
  (setf *level-updatables*
        (delete-if
         #'(lambda (crate) (find crate *level-updatables-removelist*))
         *level-updatables*))
  (setf *level-updatables-removelist* nil))

(defun request-attaching (crate)
  (setf *created* (cons crate *created*)))
