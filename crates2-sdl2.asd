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
(asdf:defsystem "crates2-sdl2"
  :depends-on (:alexandria
               :str
               :unix-opts
               :parse-float
               :trivial-garbage
               :cffi
               :cffi-libffi
               :trivial-features
               :log4cl)
  :serial t
  :components ((:module common
                :components ((:file "package")))
               (:module generated
                :depends-on ("common")
                :components ((:file "ending")
                             (:file "special")
                             (:file "hit-wall")
                             (:file "explosion")
                             (:file "bomb-on")
                             (:file "exit-error")
                             (:file "exit-ok")
                             (:file "key-collect")
                             (:file "hit-counter")
                             (:file "pulled-activate")
                             (:file "redirect")
                             (:file "slope")))
               (:module src
                :depends-on ("common" "generated")
                :components
                        ((:file "classes")
                         (:file "utils")
                         (:file "crate")
                         (:file "moving")
                         (:file "simple-crates")
                         (:file "exit")
                         (:file "key")
                         (:file "special")
                         (:file "pulled")
                         (:file "toggle")
                         (:file "player")
                         (:file "automaton")
                         (:file "slopes")
                         (:file "turnstiles")
                         (:file "bomb")
                         (:file "block-timer")
                         (:file "block-counter")
                         (:file "pass-counter")
                         (:file "pass-timer")
                         (:file "vacuum")
                         (:file "stepper")
                         (:file "level")
                         (:file "ffi/octaspire-cl-sdl2")
                         (:file "visual/sdl2-2d")
                         (:file "visual/sdl2-common")
                         (:file "levels")
                         (:file "main"))))
  :build-operation program-op
  :build-pathname "crates2-sdl2"
  :entry-point "crates2:main")
