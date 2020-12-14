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

(defparameter *running* t)
(defparameter *level* (list))
(defparameter *level-width* 32)
(defparameter *level-height* 24)

;; Methods

(defmethod update ((obj list))
  (when *running* (loop for crate in *level*
                        do (update crate)))
  *running*)

(defmethod render ((obj list))
  (loop for crate in *level*
        do (render crate)))
