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
(in-package :crates2)

(defparameter *verbose* 0)
(defparameter *version-major* 0)
(defparameter *version-minor* 1)
(defparameter *version-patch* 0)
(defparameter *errors* nil)
(defparameter *update-counter* 0)
(defparameter *input* nil)
(defparameter *level-number* 0)
(defparameter *running* t)
(defparameter *level* nil)
(defparameter *created* nil)
(defparameter *next-level* nil)
(defparameter *level-width* 20)
(defparameter *level-height* 20)
(defparameter *frame-duration-default* 0.25) ; Not zeroed in test mode.
(defparameter *frame-duration* *frame-duration-default*) ; Zeroed in test mode.
(defparameter *test-run* nil)
(defparameter *test-run-max-updates* 3000)

(defun verbose-parser (x)
  (setf *verbose* (parse-integer x)))

(defun test-parser (x)
  (autoplay-parser x)
  (setf *frame-duration* 0))

(defun autoplay-parser (x)
  (setf *test-run* t)
  (let ((num (parse-integer x)))
    (setf *level-number* num)
    (setf *next-level* nil)))

(defun get-current-level()
  (unless *level*
    (load-next-level))
  *level*)

(defun runningp ()
  *running*)

(defun running (value)
  (setf *running* value))

(opts:define-opts
  (:name :help
   :description "Show this usage information and quit"
   :short #\h
   :long "help")
  (:name :verbose
   :description "Make verbose"
   :short #\v
   :long "verbose"
   :arg-parser #'verbose-parser)
  (:name :version
   :description "Show version information"
   :long "version")
  (:name :test
   :description "Do a test run starting from the given level.
This is similar to 'autoplay' but runs without delays, i.e. too fast
to see what happens."
   :long "test"
   :arg-parser #'test-parser)
  (:name :autoplay
   :description "Do a autoplay run starting from the given level.
This is similar to 'test' but runs much slower."
   :long "autoplay"
   :arg-parser #'autoplay-parser)
  (:name :log-input
   :description "Log and show user input after quitting game."
   :long "log-input")
  (:name :fullscreen
   :description "Run in fullscreen mode"
   :long "fullscreen"))

(defun dbg (fmt &rest args)
  (when (> *verbose* 0)
    (format t fmt args)))

(defun run (options)
  (unless *errors*
    (init-visual-hash)
    (request-next-level)
    (let ((str (make-array 2048 :element-type 'character :fill-pointer 0 :adjustable t))
          (log-input (getf options :log-input))
          (half-frame-duration (* 0.5 *frame-duration*)))
      (with-output-to-string (s str)
        (loop while (and (runningp)
                         (or (not *test-run*)
                             (< *update-counter* *test-run-max-updates*)))
              do (setf *input* nil)
                 (ui-render *level* 0)
                 (sleep half-frame-duration)
                 (ui-render *level* 1)
                 (let ((input (ui-input)))
                   (when log-input
                     (format s (if (keywordp input) "~%~S " "~S " ) input))
                   (when input
                     (setf *input* (cons input *input*))
                     (case input
                       (:back    (running nil))
                       (:restart (setf *next-level* *level-number*)))))
                 (unless *next-level*
                   (update *level*))
                 (when *next-level*
                   (load-next-level)
                   (trivial-garbage:gc :full t)
                   (when log-input
                     (format s "~%----------LEVEL ~A----------~%" *level-number*)))
                 (incf *update-counter*)
                 (sleep half-frame-duration))
        (when (>= *update-counter* *test-run-max-updates*)
          (format t "~%CRATES2: WARNING EXECUTION STOPPED ON TOO LARGE UPDATE COUNT~%"))
        (setf str (nstring-downcase str))
        (when log-input
          (format t "~%~A~%" str))))))

(defun usage ()
  (opts:describe
   :prefix "Puzzle game"
   :usage-of "crates2"))

(defun version ()
  (format t "crates ~A.~A.~A~%" *version-major* *version-minor* *version-patch*))

(defun unknown-option (condition)
  (format t "Error: option '~A' is unknown~%" (opts:option condition))
  (setf *errors* t)
  (invoke-restart 'opts:skip-option))

(defun parser-error (condition)
  (format t "Argument Parse Error: ~A~%" (opts:option condition))
  (setf *errors* t)
  (invoke-restart 'opts:skip-option))

(defmacro cond-option (options &rest clauses)
  (alexandria:with-gensyms
      (option value opts-not-empty)
    `(let ((,opts-not-empty (or ,options (list nil nil))))
       (loop for (,option ,value) on ,opts-not-empty by #'cddr
             do (case ,option ,@clauses)))))

(defun load-next-level ()
  (if (and *test-run* (>= *next-level* *num-levels*))
      (running nil)
      (let ((level-number (mod *next-level* *num-levels*)))
        (setf *next-level* nil)
        (setf *fake-input* nil)
        (setf *level-number* level-number)
        (format t "LEVEL ~A~%" *level-number*)
        (setf *level* nil)
        (let ((loaded (load-level *level-number*)))
          (setf *level* (cadr loaded))
          (setf *fake-input* (car loaded))))))

(defun request-next-level ()
  ;; Don't override previous request, if present.
  (unless *next-level*
    (setf *next-level* (+ *level-number* 1))))

(defun request-restart-level ()
  (format t "RESTART~%")
  (setf *next-level* *level-number*))

(defun request-previous-level ()
  (setf *next-level* (- *level-number* 1)))

(defun main ()
  (let ((options (handler-case
                     (handler-bind ((opts:arg-parser-failed #'parser-error)
                                    (opts:unknown-option    #'unknown-option))
                       (opts:get-opts)))))
    (cond-option options
                 (:help (usage))
                 (:version (version))
                 (otherwise (run options)))))
