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
LISP  ?= sbcl
EVAL  ?= "--eval"
LOAD  ?= "--load
SOURCES = $(wildcard src/*.lisp)

.PHONY: slime clean help

crates2: Makefile crates2.asd $(SOURCES)
	@$(LISP) $(EVAL) '(ql:quickload :crates2)' \
                 $(EVAL) '(asdf:make :crates2)'    \
                 $(EVAL) '(quit)'

slime:
	@etc/slime.sh &

run: crates2
	@./crates2

clean:
	@rm -f crates2

help:
	@echo "Usage:"
	@echo "  make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  crates2  build standalone binary executable for crates2 (default target)"
	@echo "  run      build standalone binary and run it"
	@echo "  slime    start Emacs/slime (if needed) with crates2 loaded"
	@echo "  clean    remove build artifacts"
	@echo "  help     show this help"