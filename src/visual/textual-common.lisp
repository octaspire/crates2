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
(in-package :crates2-ui)

;; Crate is drawn as CW x CH character shape
(defconstant cw 6)
(defconstant ch 3)

(defparameter *look-at-x* 0)
(defparameter *look-at-y* 0)

(defun ui-play-sound (id))

(defun ui-on-level-changed ())

(defun ui-look-at (x y m minx miny maxx maxy)
  (setf *look-at-x* (floor (- (- *level-width* maxx) minx) 2))
  (setf *look-at-y* (* (1- (floor (- (- *level-height* maxy) miny) 2)) ch)))

(defparameter *visual-hash* (make-hash-table :test 'equal))

(defun init-visual-hash ()
  ;; VACUUM
  (setf (gethash "vacuum-body" *visual-hash*) #("+----+" "|    |" "+----+"))
  (setf (gethash "gear-00"     *visual-hash*) #("      " "   |  " "      "))
  (setf (gethash "gear-01"     *visual-hash*) #("      " "   /  " "      "))
  (setf (gethash "gear-02"     *visual-hash*) #("      " "   -  " "      "))
  (setf (gethash "gear-03"     *visual-hash*) #("      " "   \\  " "      "))
  (setf (gethash "gear-04"     *visual-hash*) #("      " "   |  " "      "))
  (setf (gethash "gear-05"     *visual-hash*) #("      " "   /  " "      "))
  (setf (gethash "gear-06"     *visual-hash*) #("      " "   -  " "      "))
  (setf (gethash "gear-07"     *visual-hash*) #("      " "   \\  " "      "))
  ;; WALL
  (setf (gethash "wall-idle-00" *visual-hash*) #("#0####" "######" "######"))
  (setf (gethash "wall-idle-01" *visual-hash*) #("#1####" "######" "######"))
  (setf (gethash "wall-idle-02" *visual-hash*) #("#2####" "######" "######"))
  (setf (gethash "wall-idle-03" *visual-hash*) #("#3####" "######" "######"))
  ;; PUSHED
  (setf (gethash "pushed-idle" *visual-hash*) #("+----+" "|PUSH|" "+----+"))
  ;; BLOCK-TIMER
  (setf (gethash "block-timer-durable" *visual-hash*) #("+----+" "BTXX  " "+----+"))
  (setf (gethash "block-timer"         *visual-hash*) #("+----+" "BT    " "+----+"))
  ;; COUNT
  (setf (gethash "count-01"            *visual-hash*) #("      " "    01" "      "))
  (setf (gethash "count-02"            *visual-hash*) #("      " "    02" "      "))
  (setf (gethash "count-03"            *visual-hash*) #("      " "    03" "      "))
  (setf (gethash "count-04"            *visual-hash*) #("      " "    04" "      "))
  (setf (gethash "count-05"            *visual-hash*) #("      " "    05" "      "))
  (setf (gethash "count-06"            *visual-hash*) #("      " "    06" "      "))
  (setf (gethash "count-07"            *visual-hash*) #("      " "    07" "      "))
  (setf (gethash "count-08"            *visual-hash*) #("      " "    08" "      "))
  (setf (gethash "count-09"            *visual-hash*) #("      " "    09" "      "))
  (setf (gethash "count-10"            *visual-hash*) #("      " "    10" "      "))
  (setf (gethash "count-11"            *visual-hash*) #("      " "    11" "      "))
  (setf (gethash "count-12"            *visual-hash*) #("      " "    12" "      "))
  (setf (gethash "count-13"            *visual-hash*) #("      " "    13" "      "))
  (setf (gethash "count-14"            *visual-hash*) #("      " "    14" "      "))
  (setf (gethash "count-15"            *visual-hash*) #("      " "    15" "      "))
  (setf (gethash "count-16"            *visual-hash*) #("      " "    16" "      "))
  (setf (gethash "count-17"            *visual-hash*) #("      " "    17" "      "))
  (setf (gethash "count-18"            *visual-hash*) #("      " "    18" "      "))
  (setf (gethash "count-19"            *visual-hash*) #("      " "    19" "      "))
  (setf (gethash "count-20"            *visual-hash*) #("      " "    20" "      "))
  (setf (gethash "count-21"            *visual-hash*) #("      " "    21" "      "))
  (setf (gethash "count-22"            *visual-hash*) #("      " "    22" "      "))
  (setf (gethash "count-23"            *visual-hash*) #("      " "    23" "      "))
  ;; EXIT
  (setf (gethash "exit-idle"        *visual-hash*) #("+----+" "|exit|" "+----+"))
  (setf (gethash "exit-active-pass" *visual-hash*) #("+----+" "|EXIT|" "+----+"))
  (setf (gethash "exit-active-fail" *visual-hash*) #("+----+" "|FAIL|" "+----+"))
  ;; KEY
  (setf (gethash "key-idle-00" *visual-hash*) #("+----+" " KEY  " "+----+"))
  (setf (gethash "key-idle-01" *visual-hash*) #("+----+" " KEY  " "+----+"))
  (setf (gethash "key-idle-02" *visual-hash*) #("+----+" " KEY  " "+----+"))
  (setf (gethash "key-idle-03" *visual-hash*) #("+----+" " KEY  " "+----+"))
  (setf (gethash "key-idle-04" *visual-hash*) #("+----+" " key  " "+----+"))
  (setf (gethash "key-idle-05" *visual-hash*) #("+----+" " key  " "+----+"))
  (setf (gethash "key-idle-06" *visual-hash*) #("+----+" " key  " "+----+"))
  (setf (gethash "key-idle-07" *visual-hash*) #("+----+" " KEY  " "+----+"))
  (setf (gethash "key-idle-08" *visual-hash*) #("+----+" " KEY  " "+----+"))
  (setf (gethash "key-active"  *visual-hash*) #("+----+" " !!!  " "+----+"))
  ;; SPECIAL
  (setf (gethash "special-jump-idle-00" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-01" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-02" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-03" *visual-hash*) #("      " "  ::  " "      "))
  (setf (gethash "special-jump-idle-04" *visual-hash*) #("      " "  ::  " "      "))
  (setf (gethash "special-jump-idle-05" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-06" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-07" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-08" *visual-hash*) #("      " "  ::  " "      "))
  (setf (gethash "special-jump-idle-09" *visual-hash*) #("      " "  ::  " "      "))
  (setf (gethash "special-jump-idle-10" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-11" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-12" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-idle-13" *visual-hash*) #("      " "  **  " "      "))
  (setf (gethash "special-jump-active"  *visual-hash*) #("      " "  OO  " "      "))
  ;; PLAYER
  (setf (gethash "player-active-00" *visual-hash*) #(" .--. " " |XX| " " `--' "))
  (setf (gethash "player-active-01" *visual-hash*) #(" .--. " " |xx| " " `--' "))
  (setf (gethash "player-active-02" *visual-hash*) #(" .--. " " |**| " " `--' "))
  (setf (gethash "player-active-03" *visual-hash*) #(" .--. " " |..| " " `--' "))
  (setf (gethash "player-active-04" *visual-hash*) #(" .--. " " |..| " " `--' "))
  (setf (gethash "player-active-05" *visual-hash*) #(" .--. " " |**| " " `--' "))
  (setf (gethash "player-active-06" *visual-hash*) #(" .--. " " |xx| " " `--' "))
  (setf (gethash "player-airborne"  *visual-hash*) #(".----." "|xxxx|" "`----'"))
  (setf (gethash "player-hidden" *visual-hash*) #("      " "      " "      "))
  ;; AUTOMATON
  (setf (gethash "automaton-idle"            *visual-hash*) #(" .--. " "AUTOM." " `--' "))
  (setf (gethash "automaton-programming"     *visual-hash*) #(" .--. " "AUTOM!" " `--' "))
  (setf (gethash "automaton-executing"       *visual-hash*) #(" .--. " "!!!!!!" " `--' "))
  (setf (gethash "automaton-executing-hover" *visual-hash*) #("      " "      " "      "))
  ;; SLOPES
  (setf (gethash "slope-en"        *visual-hash*) #("+--+  " "|   \\ " "+----+"))
  (setf (gethash "slope-en-active" *visual-hash*) #("o--o  " "|   \\ " "o----o"))
  (setf (gethash "slope-es"        *visual-hash*) #("+----+" "|   / " "+--+  "))
  (setf (gethash "slope-es-active" *visual-hash*) #("o----o" "|   / " "o--o  "))
  (setf (gethash "slope-wn"        *visual-hash*) #("  +--+" " /   |" "+----+"))
  (setf (gethash "slope-wn-active" *visual-hash*) #("  o--o" " /   |" "o----o"))
  (setf (gethash "slope-ws"        *visual-hash*) #("+----+" " \\   |" "  +--+"))
  (setf (gethash "slope-ws-active" *visual-hash*) #("o----o" " \\   |" "  o--o"))
  ;; TURNSTILE
  (setf (gethash "turnstile-e1"        *visual-hash*) #("+----+" "---->1" "+----+"))
  (setf (gethash "turnstile-e1-active" *visual-hash*) #("+----+" "---->!" "+----+"))
  (setf (gethash "turnstile-w1"        *visual-hash*) #("+----+" "1<----" "+----+"))
  (setf (gethash "turnstile-w1-active" *visual-hash*) #("+----+" "!<----" "+----+"))
  (setf (gethash "turnstile-n1"        *visual-hash*) #("+1111+" "|^^^^|" "+||||+"))
  (setf (gethash "turnstile-n1-active" *visual-hash*) #("+!!!!+" "|^^^^|" "+^^^^+"))
  (setf (gethash "turnstile-s1"        *visual-hash*) #("+||||+" "\\/\\/\\/" "+1111+"))
  (setf (gethash "turnstile-s1-active" *visual-hash*) #("+\\/\\/+" "\\/\\/\\/" "+!!!!+"))
  (setf (gethash "turnstile-e"         *visual-hash*) #("+----+" "----->" "+----+"))
  (setf (gethash "turnstile-e-active"  *visual-hash*) #("+----+" ">>>>>>" "+----+"))
  (setf (gethash "turnstile-w"         *visual-hash*) #("+----+" "<-----" "+----+"))
  (setf (gethash "turnstile-w-active"  *visual-hash*) #("+----+" "<<<<<<" "+----+"))
  (setf (gethash "turnstile-n"         *visual-hash*) #("+^^^^+" "||||||" "+||||+"))
  (setf (gethash "turnstile-n-active"  *visual-hash*) #("+^^^^+" "|^^^^|" "+^^^^+"))
  (setf (gethash "turnstile-s"         *visual-hash*) #("+....+" "||||||" "\\/\\/\\/"))
  (setf (gethash "turnstile-s-active"  *visual-hash*) #("+\\/\\/+" "|\\/\\/|" "\\/\\/\\/"))
  ;; NUMBERS
  (setf (gethash "number-01"        *visual-hash*) #("      " "    01" "      "))
  (setf (gethash "number-02"        *visual-hash*) #("      " "    02" "      "))
  (setf (gethash "number-03"        *visual-hash*) #("      " "    03" "      "))
  (setf (gethash "number-04"        *visual-hash*) #("      " "    04" "      "))
  (setf (gethash "number-05"        *visual-hash*) #("      " "    05" "      "))
  (setf (gethash "number-06"        *visual-hash*) #("      " "    06" "      "))
  (setf (gethash "number-07"        *visual-hash*) #("      " "    07" "      "))
  (setf (gethash "number-08"        *visual-hash*) #("      " "    08" "      "))
  (setf (gethash "number-09"        *visual-hash*) #("      " "    09" "      "))
  (setf (gethash "number-10"        *visual-hash*) #("      " "    10" "      "))
  (setf (gethash "number-11"        *visual-hash*) #("      " "    11" "      "))
  (setf (gethash "number-12"        *visual-hash*) #("      " "    12" "      "))
  (setf (gethash "number-13"        *visual-hash*) #("      " "    13" "      "))
  (setf (gethash "number-14"        *visual-hash*) #("      " "    14" "      "))
  (setf (gethash "number-15"        *visual-hash*) #("      " "    15" "      "))
  (setf (gethash "number-16"        *visual-hash*) #("      " "    16" "      "))
  (setf (gethash "number-17"        *visual-hash*) #("      " "    17" "      "))
  (setf (gethash "number-18"        *visual-hash*) #("      " "    18" "      "))
  (setf (gethash "number-19"        *visual-hash*) #("      " "    19" "      "))
  (setf (gethash "number-20"        *visual-hash*) #("      " "    20" "      "))
  (setf (gethash "number-21"        *visual-hash*) #("      " "    21" "      "))
  (setf (gethash "number-22"        *visual-hash*) #("      " "    22" "      "))
  (setf (gethash "number-23"        *visual-hash*) #("      " "    23" "      "))
  (setf (gethash "number-24"        *visual-hash*) #("      " "    24" "      "))
  (setf (gethash "number-25"        *visual-hash*) #("      " "    25" "      "))
  (setf (gethash "number-26"        *visual-hash*) #("      " "    26" "      "))
  (setf (gethash "number-27"        *visual-hash*) #("      " "    27" "      "))
  (setf (gethash "number-28"        *visual-hash*) #("      " "    28" "      "))
  (setf (gethash "number-29"        *visual-hash*) #("      " "    29" "      "))
  (setf (gethash "number-30"        *visual-hash*) #("      " "    30" "      "))
  (setf (gethash "number-31"        *visual-hash*) #("      " "    31" "      "))
  (setf (gethash "number-32"        *visual-hash*) #("      " "    32" "      "))
  (setf (gethash "number-33"        *visual-hash*) #("      " "    33" "      "))
  (setf (gethash "number-34"        *visual-hash*) #("      " "    34" "      "))
  (setf (gethash "number-35"        *visual-hash*) #("      " "    35" "      "))
  (setf (gethash "number-36"        *visual-hash*) #("      " "    36" "      "))
  (setf (gethash "number-37"        *visual-hash*) #("      " "    37" "      "))
  (setf (gethash "number-38"        *visual-hash*) #("      " "    38" "      "))
  (setf (gethash "number-39"        *visual-hash*) #("      " "    39" "      "))
  (setf (gethash "number-40"        *visual-hash*) #("      " "    40" "      "))
  (setf (gethash "number-41"        *visual-hash*) #("      " "    41" "      "))
  (setf (gethash "number-42"        *visual-hash*) #("      " "    42" "      "))
  (setf (gethash "number-43"        *visual-hash*) #("      " "    43" "      "))
  (setf (gethash "number-44"        *visual-hash*) #("      " "    44" "      "))
  (setf (gethash "number-45"        *visual-hash*) #("      " "    45" "      "))
  (setf (gethash "number-46"        *visual-hash*) #("      " "    46" "      "))
  (setf (gethash "number-47"        *visual-hash*) #("      " "    47" "      "))
  (setf (gethash "number-48"        *visual-hash*) #("      " "    48" "      "))
  (setf (gethash "number-49"        *visual-hash*) #("      " "    49" "      "))
  (setf (gethash "number-50"        *visual-hash*) #("      " "    50" "      "))
  (setf (gethash "number-51"        *visual-hash*) #("      " "    51" "      "))
  (setf (gethash "number-52"        *visual-hash*) #("      " "    52" "      "))
  (setf (gethash "number-53"        *visual-hash*) #("      " "    53" "      "))
  (setf (gethash "number-54"        *visual-hash*) #("      " "    54" "      "))
  (setf (gethash "number-55"        *visual-hash*) #("      " "    55" "      "))
  (setf (gethash "number-56"        *visual-hash*) #("      " "    56" "      "))
  (setf (gethash "number-57"        *visual-hash*) #("      " "    57" "      "))
  (setf (gethash "number-58"        *visual-hash*) #("      " "    58" "      "))
  (setf (gethash "number-59"        *visual-hash*) #("      " "    59" "      "))
  (setf (gethash "number-60"        *visual-hash*) #("      " "    60" "      "))
  (setf (gethash "number-61"        *visual-hash*) #("      " "    61" "      "))
  (setf (gethash "number-62"        *visual-hash*) #("      " "    62" "      "))
  (setf (gethash "number-63"        *visual-hash*) #("      " "    63" "      "))
  (setf (gethash "number-64"        *visual-hash*) #("      " "    64" "      "))
  (setf (gethash "number-65"        *visual-hash*) #("      " "    65" "      "))
  (setf (gethash "number-66"        *visual-hash*) #("      " "    66" "      "))
  (setf (gethash "number-67"        *visual-hash*) #("      " "    67" "      "))
  (setf (gethash "number-68"        *visual-hash*) #("      " "    68" "      "))
  (setf (gethash "number-69"        *visual-hash*) #("      " "    69" "      "))
  (setf (gethash "number-70"        *visual-hash*) #("      " "    70" "      "))
  (setf (gethash "number-71"        *visual-hash*) #("      " "    71" "      "))
  (setf (gethash "number-72"        *visual-hash*) #("      " "    72" "      "))
  (setf (gethash "number-73"        *visual-hash*) #("      " "    73" "      "))
  (setf (gethash "number-74"        *visual-hash*) #("      " "    74" "      "))
  (setf (gethash "number-75"        *visual-hash*) #("      " "    75" "      "))
  (setf (gethash "number-76"        *visual-hash*) #("      " "    76" "      "))
  (setf (gethash "number-77"        *visual-hash*) #("      " "    77" "      "))
  (setf (gethash "number-78"        *visual-hash*) #("      " "    78" "      "))
  (setf (gethash "number-79"        *visual-hash*) #("      " "    79" "      "))
  (setf (gethash "number-80"        *visual-hash*) #("      " "    80" "      "))
  (setf (gethash "number-81"        *visual-hash*) #("      " "    81" "      "))
  (setf (gethash "number-82"        *visual-hash*) #("      " "    82" "      "))
  (setf (gethash "number-83"        *visual-hash*) #("      " "    83" "      "))
  (setf (gethash "number-84"        *visual-hash*) #("      " "    84" "      "))
  (setf (gethash "number-85"        *visual-hash*) #("      " "    85" "      "))
  (setf (gethash "number-86"        *visual-hash*) #("      " "    86" "      "))
  (setf (gethash "number-87"        *visual-hash*) #("      " "    87" "      "))
  (setf (gethash "number-88"        *visual-hash*) #("      " "    88" "      "))
  (setf (gethash "number-89"        *visual-hash*) #("      " "    89" "      "))
  (setf (gethash "number-90"        *visual-hash*) #("      " "    90" "      "))
  (setf (gethash "number-91"        *visual-hash*) #("      " "    91" "      "))
  (setf (gethash "number-92"        *visual-hash*) #("      " "    92" "      "))
  (setf (gethash "number-93"        *visual-hash*) #("      " "    93" "      "))
  (setf (gethash "number-94"        *visual-hash*) #("      " "    94" "      "))
  (setf (gethash "number-95"        *visual-hash*) #("      " "    95" "      "))
  (setf (gethash "number-96"        *visual-hash*) #("      " "    96" "      "))
  (setf (gethash "number-97"        *visual-hash*) #("      " "    97" "      "))
  (setf (gethash "number-98"        *visual-hash*) #("      " "    98" "      "))
  (setf (gethash "number-99"        *visual-hash*) #("      " "    99" "      "))
  (setf (gethash "number-100"       *visual-hash*) #("      " "    100" "      "))
  ;; BOTTOM NUMBERS
  (loop for i from 1 to 100
        do (setf (gethash (format nil "number-bottom-~2,'0d" i) *visual-hash*) (vector "      " (format nil "   ~3,' d" i) "      ")))
  ;; BLOCK-COUNTER
  (setf (gethash "block-counter"   *visual-hash*) #("+----+" "BCXX  " "+----+"))
  ;; PASS-COUNTER
  (setf (gethash "pass-counter"    *visual-hash*) #("+----+" "PC    " "+----+"))
  ;; PASS-TIMER
  (setf (gethash "pass-timer"      *visual-hash*) #("+----+" "PT    " "+----+"))
  ;; PULLED
  (setf (gethash "pulled-idle"               *visual-hash*) #(".-  -." "|    |" "`-  -'"))
  ;; east
  (setf (gethash "pulled-east-handle-active" *visual-hash*) #("      " "    o " "      "))
  (setf (gethash "pulled-east-handle-idle"   *visual-hash*) #("      " "    = " "      "))
  (setf (gethash "pulled-east-no-handle"     *visual-hash*) #("      " "      " "      "))
  ;; west
  (setf (gethash "pulled-west-handle-active" *visual-hash*) #("      " " o    " "      "))
  (setf (gethash "pulled-west-handle-idle"   *visual-hash*) #("      " " =    " "      "))
  (setf (gethash "pulled-west-no-handle"     *visual-hash*) #("      " "      " "      "))
  ;; north
  (setf (gethash "pulled-north-handle-active" *visual-hash*) #("  oo  " "      " "      "))
  (setf (gethash "pulled-north-handle-idle"   *visual-hash*) #("  ==  " "      " "      "))
  (setf (gethash "pulled-north-no-handle"     *visual-hash*) #("  --  " "      " "      "))
  ;; south
  (setf (gethash "pulled-south-handle-active" *visual-hash*) #("      " "      " "  oo  "))
  (setf (gethash "pulled-south-handle-idle"   *visual-hash*) #("      " "      " "  ==  "))
  (setf (gethash "pulled-south-no-handle"     *visual-hash*) #("      " "      " "  --  "))
  ;; STEPPER
  (setf (gethash "stepper-idle"               *visual-hash*) #("      " "   +  " "      "))
  (setf (gethash "stepper-active"             *visual-hash*) #("      " "   o  " "      "))
  ;; TOGGLE
  (setf (gethash "toggle-idle"                *visual-hash*) #("+-  -+" "|    |" "+-  -+"))
  ;; east
  (setf (gethash "toggle-east-on"             *visual-hash*) #("      " "    o " "      "))
  (setf (gethash "toggle-east-off"            *visual-hash*) #("      " "    | " "      "))
  ;; west
  (setf (gethash "toggle-west-on"             *visual-hash*) #("      " " o    " "      "))
  (setf (gethash "toggle-west-off"            *visual-hash*) #("      " " |    " "      "))
  ;; north
  (setf (gethash "toggle-north-on"            *visual-hash*) #("  oo  " "      " "      "))
  (setf (gethash "toggle-north-off"           *visual-hash*) #("  __  " "      " "      "))
  ;; south
  (setf (gethash "toggle-south-on"            *visual-hash*) #("      " "      " "  oo  "))
  (setf (gethash "toggle-south-off"           *visual-hash*) #("      " "      " "  ''  "))
  ;; BOMB
  (setf (gethash "bomb-durable"  *visual-hash*) #("+----+" "BOMP  " "+----+"))
  (setf (gethash "bomb"          *visual-hash*) #("+----+" "BOMX  " "+----+"))
  ;; ring 1
  ;;    ************ 
  ;;   **************
  ;;   ****      ****
  ;;   ****      ****
  ;;   ****      ****
  ;;   **************
  ;;    ************ 
  (setf (gethash "bomb-ring-1"   *visual-hash*) #("  **************  " "  **************  " "  ****      ****  " "  ****      ****  " "  ****      ****  " "  **************  " "  **************  "))
  ;; ring 2
  ;;  **************** 
  ;; ******************
  ;; ******************
  ;; ******      ******
  ;; ******      ******
  ;; ******      ******
  ;; ******************
  ;; ******************
  ;;  **************** 
  (setf (gethash "bomb-ring-2"   *visual-hash*) #(" **************** " "******************" "******************" "******      ******" "******      ******" "******      ******" "******************" "******************" " **************** "))
  ;; ring 3
  ;;  ############################
  ;; ##############################
  ;; ##############################
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ######                  ######
  ;; ##############################
  ;; ##############################
  ;;  ############################ 
  (setf (gethash "bomb-ring-3"   *visual-hash*) #(" ############################ " "##############################" "##############################" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "######                  ######" "##############################" "##############################" " ############################ "))
  ;; ring 4
  ;;  ############################ 
  ;; ##############################
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ####                      ####
  ;; ##############################
  ;;  ############################ 
  (setf (gethash "bomb-ring-4"   *visual-hash*) #(" ############################ " "##############################" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "####                      ####" "##############################" " ############################ ")))

(defun empty-line ()
  (let* ((w *level-width*)
         (maxi (- w 1))
         (cs (make-array cw :element-type 'character :fill-pointer cw :initial-element #\Space :adjustable nil))
         (s (string "")))
    (loop for i from 0 to maxi
          do (setf s (concatenate 'string s cs)))
    s))

(defun empty-level ()
  (let* ((w (* ch *level-height*))
         (maxi (- w 1))
         (a (make-array w)))
    (loop for i from 0 to maxi
          do (setf (aref a i) (empty-line)))
    a))

(defparameter *fake-input* nil)

(defparameter *last-input* nil)

(defun x-axis (length)
  (let ((result ""))
    (loop for i from 0 to (- length 1)
          do (setf result
                   (concatenate 'string result (format nil "~vd" cw i))))
    result))

(defun ui-generate (level step)
  (let ((lines (empty-level))
        (x-axis (x-axis *level-width*))
        (bar (format nil "~v@{~A~:*~}" (* cw *level-width*) #\-)))
    (loop for crate in level
          do (progn
               (let* ((x (if (and (= step 0) (typep crate 'moving)) (tail-x crate) (crate-x crate)))
                      (y (if (and (= step 0) (typep crate 'moving)) (tail-y crate) (crate-y crate)))
                      (z (if (and (= step 0) (typep crate 'moving)) (tail-z crate) (crate-z crate)))
                      (vids (visual crate)))
                 (loop for vid in vids
                       do
                          (let ((viv (gethash vid *visual-hash*)))
                            (when viv
                              (let* ((vivh (length viv))
                                     (dy (truncate (/ (- ch vivh) 2))))
                                (loop for liney from 0 to (- vivh 1)
                                      do (let* ((str (aref viv liney))
                                                (vivw (length str))
                                                (dx (truncate (/ (- cw vivw) 2)))
                                                (finy (+ (truncate (+ (* y ch) dy liney)) *look-at-y*)))
                                           (when (>= finy 0)
                                             (let* ((line (aref lines finy))
                                                    (finx (* (+ x *look-at-x*) cw))
                                                    (deltax (truncate (+ finx dx))))
                                               (setf line (replace-substr-at-transparent-whitespace line deltax str)))))))))))))
    (values lines x-axis bar)))
