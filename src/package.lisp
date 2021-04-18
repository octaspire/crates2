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
(defpackage :crates2
  (:use
   :common-lisp)
  (:export #:main
           #:player
           #:moving
           #:movingp
           #:crate-x
           #:crate-y
           #:crate-z
           #:visual
           #:tail-x
           #:tail-y
           #:tail-z
           #:*test-run*
           #:find-first-crate-of-type
           #:*level*
           #:*level-width*
           #:*level-height*
           #:*level-number*
           #:*infos*
           #:*update-counter*
           #:replace-substr-at-transparent-whitespace))

(defpackage :crates2-ui
  (:use
   :common-lisp
   :crates2)
  (:export #:ui-init
           #:ui-read-input
           #:init-visual-hash
           #:ui-render
           #:ui-look-at
           #:ui-delete))
