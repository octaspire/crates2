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
LISP    ?= sbcl
EVAL    ?= "--eval"
level   ?= 0

all: crates2-text crates2-charms

.PHONY: slime clean help test

crates2-text: Makefile crates2-text.asd src/*.lisp etc/*.*
	@$(LISP) $(EVAL) "(progn (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) \
                                 (ql:quickload :crates2-text)                                  \
                                 (asdf:make :crates2-text)                                     \
                                 (quit))"

crates2-charms: Makefile crates2-charms.asd src/*.lisp etc/*.*
	@$(LISP) $(EVAL) "(progn (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) \
                                 (ql:quickload :crates2-charms)                                \
                                 (asdf:make :crates2-charms)                                   \
                                 (quit))"

slime:
	@etc/slime.sh &

run: crates2-text crates2-charms
	@etc/run.sh

play: crates2-text crates2-charms
	@etc/autoplay.sh $(level)

install: crates2-text crates2-charms
	@etc/install.sh

clean:
	@rm -f crates2-text crates2-charms expected.txt.bz2 expected.txt got.txt

test: crates2-text
	@etc/test.sh

help:
	@echo 'Usage:'
	@echo '  make <target>'
	@echo ''
	@echo 'Targets:'
	@echo '  all             build standalone binaries crates2 text, charms, SDL and OpenGL (default target)'
	@echo '  crates2-text    build standalone binary executable for crates2 text mode'
	@echo '  crates2-charms  build standalone binary executable for crates2 charms (ncurses) mode'
	@echo '  run             build standalone binary and run it'
	@echo '  play            build standalone binary and autoplay it from given level'
	@echo '  install         build standalone binary and install it'
	@echo '  slime           start Emacs/slime (if needed) with crates2 loaded'
	@echo '  clean           remove build artifacts'
	@echo '  test            build and do a play test'
	@echo '  help            show this help'
