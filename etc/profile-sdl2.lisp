(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)))
(require :sb-sprof)
(ql:quickload :crates2-sdl2 :silent t)
(sb-sprof:start-profiling)
(crates2:main)
(sb-sprof:report :type :flat)
(quit)
