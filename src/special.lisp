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

;; Constructors

(defun make-special-jump (x y)
  (make-instance 'special-jump :x x :y y :z 0))

;; Methods

(defmethod visual ((self special-jump))
  (ecase (crate-state self)
    (:idle
     (list (format nil "special-jump-idle-~2,'0d" (crate-frame self))))
    (:active (list "special-jump-active"))
    (:lamented (list "special-jump-active"))))

(defmethod update ((self special-jump))
  (ecase (crate-state self)
    (:idle
     (let ((frame (1+ (crate-frame self))))
       (setf (crate-frame self) (mod frame 14))))
    (:active
     (let ((step (special-jump-active-step self)))
       (if (> step 0)
           (setf (special-jump-active-step self) (1- step))
           (progn
             (attach-to self (special-jump-target self))
             (lament self)))))
    (:lamented nil))
  (call-next-method))

(defmethod collide ((self special-jump) (target player))
  (ecase (crate-state self)
    (:idle
     (setf (velocity target) (on-which-side-i-am self target))
     (setf (crate-state self) :active)
     (setf (special-jump-active-step self) 1)
     (setf (special-jump-target self) target)
     (crates2-ui:ui-play-sound :special))
    (:active
     (setf (velocity target) (on-which-side-i-am self target)))
    (:lamented nil)))

(defmethod attach-to ((self special-jump) (target moving))
  (setf (moving-specials target) (cons self (moving-specials target))))
