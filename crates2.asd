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
(asdf:defsystem "crates2"
  :depends-on (:alexandria :unix-opts :parse-float)
  :serial t
  :components ((:module src
                :components
                ((:file "package")
                 (:file "utils")
                 (:file "classes")
                 (:file "crate")
                 (:file "moving")
                 (:file "simple-crates")
                 (:file "exit")
                 (:file "player")
                 (:file "slopes")
                 (:file "turnstiles")
                 (:file "block-timer")
                 (:file "block-counter")
                 (:file "vacuum")
                 (:file "level")
                 (:file "textual")
                 (:file "levels")
                 (:file "main"))))
  :build-operation program-op
  :build-pathname "crates2"
  :entry-point "crates2:main")
