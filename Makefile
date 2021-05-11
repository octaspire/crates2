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
LISP      ?= sbcl
EVAL      ?= --eval
FLAGS     ?= --noinform --noprint
level     ?= 0
FFIDIR    ?= src/ffi/
VISUALDIR ?= src/visual/
CLHEXDUMP ?= etc/script/cl-hexdump.sh
GENERATED ?= generated/ending.lisp          \
             generated/special.lisp         \
             generated/hit-wall.lisp        \
             generated/explosion.lisp       \
             generated/bomb-on.lisp         \
             generated/exit-error.lisp      \
             generated/exit-ok.lisp         \
             generated/key-collect.lisp     \
             generated/hit-counter.lisp     \
             generated/pulled-activate.lisp \
             generated/redirect.lisp        \
             generated/slope.lisp           \
             generated/hit-vacuum.lisp      \
             generated/hit-automaton.lisp   \
             generated/automaton-key.lisp   \
             generated/texture32.lisp       \
             generated/texture64.lisp       \
             generated/IBMPlexMono-Bold.lisp

all: $(GENERATED) crates2-text crates2-charms crates2-sdl2 crates2-sdl2-opengl

.PHONY: slime clean help test

generated/ending.lisp: etc/assets/sound/music/ending.ogg Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/special.lisp: etc/assets/sound/effect/special.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/hit-wall.lisp: etc/assets/sound/effect/hit-wall.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/explosion.lisp: etc/assets/sound/effect/explosion.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/bomb-on.lisp: etc/assets/sound/effect/bomb-on.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/exit-error.lisp: etc/assets/sound/effect/exit-error.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/exit-ok.lisp: etc/assets/sound/effect/exit-ok.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/key-collect.lisp: etc/assets/sound/effect/key-collect.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/hit-counter.lisp: etc/assets/sound/effect/hit-counter.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/pulled-activate.lisp: etc/assets/sound/effect/pulled-activate.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/redirect.lisp: etc/assets/sound/effect/redirect.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/slope.lisp: etc/assets/sound/effect/slope.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/hit-vacuum.lisp: etc/assets/sound/effect/hit-vacuum.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/hit-automaton.lisp: etc/assets/sound/effect/hit-automaton.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/automaton-key.lisp: etc/assets/sound/effect/automaton-key.wav Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/texture32.lisp: etc/assets/texture/texture32.png Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/texture64.lisp: etc/assets/texture/texture64.png Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

generated/IBMPlexMono-Bold.lisp: etc/assets/font/IBM/Plex/IBMPlexMono-Bold.ttf Makefile $(CLHEXDUMP)
	@$(CLHEXDUMP) $< $@

crates2-text: Makefile crates2-text.asd src/*.lisp $(VISUALDIR)textual-common.lisp $(VISUALDIR)textual-plain.lisp etc/*.*
	@$(LISP) $(FLAGS) $(EVAL) "(progn (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) \
                                          (ql:quickload :crates2-text :silent t)                        \
                                          (asdf:make :crates2-text)                                     \
                                          (quit))"

crates2-charms: Makefile crates2-charms.asd src/*.lisp $(VISUALDIR)textual-common.lisp $(VISUALDIR)textual-charms.lisp etc/*.*
	@$(LISP) $(FLAGS) $(EVAL) "(progn (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) \
                                          (ql:quickload :crates2-charms :silent t)                      \
                                          (asdf:make :crates2-charms)                                   \
                                          (quit))"

crates2-sdl2: $(GENERATED) Makefile crates2-sdl2.asd src/*.lisp $(FFIDIR)octaspire-cl-sdl2.lisp $(VISUALDIR)sdl2-common.lisp $(VISUALDIR)sdl2-2d.lisp etc/assets/font/IBM/Plex/IBMPlexMono-Bold.ttf etc/assets/texture/texture32.png
	@$(LISP) $(FLAGS) $(EVAL) "(progn (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) \
                                          (ql:quickload :crates2-sdl2 :silent t)                        \
                                          (asdf:make :crates2-sdl2)                                     \
                                          (quit))"

crates2-sdl2-opengl: $(GENERATED) Makefile crates2-sdl2-opengl.asd src/*.lisp $(FFIDIR)octaspire-cl-sdl2.lisp $(VISUALDIR)sdl2-common.lisp $(VISUALDIR)sdl2-3d.lisp etc/assets/font/IBM/Plex/IBMPlexMono-Bold.ttf etc/assets/texture/texture32.png
	@$(LISP) $(FLAGS) $(EVAL) "(progn (declaim (optimize (speed 0) (space 0) (safety 3) (debug 3))) \
                                          (ql:quickload :crates2-sdl2-opengl :silent t)                 \
                                          (asdf:make :crates2-sdl2-opengl)                              \
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
	@rm -f generated/*.lisp crates2-text crates2-charms crates2-sdl2 crates2-sdl2-opengl expected.txt.bz2 expected.txt got.txt

test: crates2-text
	@etc/test.sh

help:
	@echo 'Usage:'
	@echo '  make <target>'
	@echo ''
	@echo 'Targets:'
	@echo '  all                 build standalone binaries crates2 text, charms, SDL and OpenGL (default target)'
	@echo '  crates2-text        build standalone binary executable for crates2 text mode'
	@echo '  crates2-charms      build standalone binary executable for crates2 charms (ncurses) mode'
	@echo '  crates2-sdl2        build standalone binary executable for crates2 SDL2 (2D) mode'
	@echo '  crates2-sdl2-opengl build standalone binary executable for crates2 SDL2 (3D) mode'
	@echo '  run                 build standalone binary and run it'
	@echo '  play                build standalone binary and autoplay it from given level'
	@echo '  install             build standalone binary and install it'
	@echo '  slime               start Emacs/slime (if needed) with crates2 loaded'
	@echo '  clean               remove build artifacts'
	@echo '  test                build and do a play test'
	@echo '  help                show this help'
