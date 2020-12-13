#!/bin/bash
# Octaspire Crates 2 - Puzzle Game
# Copyright 2020 octaspire.com
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
EMACS=emacs
EMACSCLIENT=emacsclient
EMACS_SERVER_ON=$(lsof -c emacs | grep server)
PROGRAM="(progn (slime)                                    \
         (while (not (slime-connected-p)) (sleep-for 0.5)) \
         (slime-repl-eval-string                           \
           \"(progn (ql:quickload :crates2)                \
                    (in-package :crates2))\"))"

if [ -z "${EMACS_SERVER_ON}" ]
then
    ${EMACS} --eval "$PROGRAM" &
else
    ${EMACSCLIENT} --eval "$PROGRAM" > /dev/null
fi
