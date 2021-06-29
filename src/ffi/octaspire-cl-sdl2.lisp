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
(defpackage :crates2-ui
  (:use
   :common-lisp
   :alexandria
   :parse-float
   :trivial-garbage
   :cffi
   :log4cl))

(in-package :crates2-ui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (define-foreign-library libsdl2
        (:darwin (:or (:framework "SDL2") (:default "libSDL2")))
      (:unix (:or "libSDL2-2.0.so")))

    (define-foreign-library libsdl2-image
        (:darwin (:or (:framework "SDL2_image") (:default "libSDL2_image")))
      (:unix (:or "libSDL2_image-2.0.so")))

    (define-foreign-library libsdl2-ttf
        (:darwin (:or (:framework "SDL2_ttf") (:default "libSDL2_ttf")))
      (:unix (:or "libSDL2_ttf-2.0.so")))

    (define-foreign-library libsdl2-mixer
        (:darwin (:or (:framework "SDL2_mixer") (:default "libSDL2_mixer")))
      (:unix (:or "libSDL2_mixer-2.0.so")))

    (define-foreign-library libgl
        (:darwin (:or (:framework "GL") (:default "libGL")))
      (:unix (:or "libGL.so")))

    (define-foreign-library libglu
        (:darwin (:or (:framework "GLU") (:default "libGLU")))
      (:unix (:or "libGLU.so")))

    (define-foreign-library libglew
        (:darwin (:or (:framework "GLEW") (:default "libGLEW")))
      (:unix (:or "libGLEW.so")))

    (cffi:load-foreign-library 'libsdl2)
    (cffi:load-foreign-library 'libsdl2-image)
    (cffi:load-foreign-library 'libsdl2-ttf)
    (cffi:load-foreign-library 'libsdl2-mixer)
    (cffi:load-foreign-library 'libgl)
    (cffi:load-foreign-library 'libglu)
    (cffi:load-foreign-library 'libglew)))

(defcfun "SDL_Init" :int
  (flags :long))

;; Declared in include/SDL_render.h
(defconstant +SDL-RENDERER-SOFTWARE+      #x0001)
(defconstant +SDL-RENDERER-ACCELERATED+   #x0002)
(defconstant +SDL-RENDERER-PRESENTVSYNC+  #x0004)
(defconstant +SDL-RENDERER-TARGETTEXTURE+ #x0008)



;; Declared in include/SDL_audio.h
(defconstant +AUDIO-U8+     #x0008)
(defconstant +AUDIO-S8+     #x8008)
(defconstant +AUDIO-U16LSB+ #x0010)
(defconstant +AUDIO-S16LSB+ #x8010)
(defconstant +AUDIO-U16MSB+ #x1010)
(defconstant +AUDIO-S16MSB+ #x9010)
(defconstant +AUDIO-U16+    +AUDIO-U16LSB+)
(defconstant +AUDIO-S16+    +AUDIO-S16LSB+)

(defconstant +AUDIO-S32LSB+ #x8020)
(defconstant +AUDIO-S32MSB+ #x9020)
(defconstant +AUDIO-S32+    +AUDIO-S16LSB+)

(defconstant +AUDIO-F32LSB+ #x8120)
(defconstant +AUDIO-F32MSB+ #x9120)
(defconstant +AUDIO-F32+    +AUDIO-F32LSB+)

#+little-endian
(progn
  (defconstant +AUDIO-U16SYS+ +AUDIO-U16LSB+)
  (defconstant +AUDIO-S16SYS+ +AUDIO-S16LSB+)
  (defconstant +AUDIO-S32SYS+ +AUDIO-S32LSB+)
  (defconstant +AUDIO-F32SYS+ +AUDIO-F32LSB+))

#+big-endian
(progn
  (defconstant +AUDIO-U16SYS+ +AUDIO-U16MSB+)
  (defconstant +AUDIO-S16SYS+ +AUDIO-S16MSB+)
  (defconstant +AUDIO-S32SYS+ +AUDIO-S32MSB+)
  (defconstant +AUDIO-F32SYS+ +AUDIO-F32MSB+))

;; Declared in include/SDL_stdinc.h
(defconstant +SDL-FALSE+ #x0)
(defconstant +SDL-TRUE+  #x1)

;; Declared in include/SDL.h
(defconstant +SDL-INIT-TIMER+                 #x001)
(defconstant +SDL-INIT-AUDIO+                 #x010)
(defconstant +SDL-INIT-VIDEO+                 #x020)
(defconstant +SDL-INIT-JOYSTICK+              #x200)
(defconstant +SDL-INIT-HAPTIC+                #x1000)
(defconstant +SDL-INIT-GAMECONTROLLER+        #x2000)
(defconstant +SDL-INIT-EVENTS+                #x4000)
(defconstant +SDL-INIT-SENSOR+                #x8000)
(defconstant +SDL-INIT-NOPARACHUTE+           #x100000)
(defconstant +SDL-INIT-EVERYTHING+            (logior +SDL-INIT-TIMER+
                                                      +SDL-INIT-AUDIO+
                                                      +SDL-INIT-VIDEO+
                                                      +SDL-INIT-EVENTS+
                                                      +SDL-INIT-JOYSTICK+
                                                      +SDL-INIT-HAPTIC+
                                                      +SDL-INIT-GAMECONTROLLER+
                                                      +SDL-INIT-SENSOR+))
(defconstant +SDL-TEXTEDITINGEVENT-TEXT-SIZE+ 32)
(defconstant +SDL-TEXTINPUTEVENT-TEXT-SIZE+   32)

;; Declared in include/SDL_rect.h
(defcstruct sdl-rect
  "SDL Rectangle structure."
  (x :int)
  (y :int)
  (w :int)
  (h :int))

(defun set-rect (result rx ry rw rh)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (with-foreign-slots ((x y w h) result (:struct sdl-rect))
      (setf x rx)
      (setf y ry)
      (setf w rw)
      (setf h rh)
      result)
    result))

;; Declared in include/SDL_pixels.h
(defcstruct (sdl-color :class sdl-color-type)
  "SDL Color structure."
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defmethod cffi:translate-into-foreign-memory ((value list) (type sdl-color-type) p)
  (with-foreign-slots ((r g b a) p (:struct sdl-color))
    (setf r (nth 0 value))
    (setf g (nth 1 value))
    (setf b (nth 2 value))
    (setf a (nth 3 value))))

(defmethod cffi:translate-from-foreign (p (type sdl-color-type))
  (with-foreign-slots ((r g b a) p (:struct sdl-color))
    (list r g b a)))

;; Declared in include/SDL_events.h
(defcenum sdl-eventtype
  "Types of events."
  (:SDL-FIRSTEVENT        0)
  ;; Application
  (:SDL-QUIT              #x100)
  (:SDL-APP-TERMINATING)
  (:SDL-APP-LOWMEMORY)
  (:SDL-APP-WILLENTERBACKGROUND)
  (:SDL-APP-DIDENTERBACKGROUND)
  (:SDL-APP-WILLENTERFOREGROUND)
  (:SDL-APP-DIDENTERFOREGROUND)
  (:SDL-APP-LOCALECHANGED)
  ;; Display
  (:SDL-DISPLAYEVENT  #x150)
  ;; Window
  (:SDL-WINDOWEVENT   #x200)
  (:SDL-SYSWMEVENT)
  ;; Keyboard
  (:SDL-KEYDOWN       #x300)
  (:SDL-KEYUP)
  (:SDL-TEXTEDITING)
  (:SDL-TEXTINPUT)
  (:SDL-KEYMAPCHANGED)
  ;; Mouse
  (:SDL-MOUSEMOTION   #x400)
  (:SDL-MOUSEBUTTONDOWN)
  (:SDL-MOUSEBUTTONUP)
  (:SDL-MOUSEWHEEL)
  ;; Joystick
  (:SDL-JOYAXISMOTION #x600)
  (:SDL-JOYBALLMOTION)
  (:SDL-JOYHATMOTION)
  (:SDL-JOYBUTTONDOWN)
  (:SDL-JOYBUTTONUP)
  (:SDL-JOYDEVICEADDED)
  (:SDL-JOYDEVICEREMOVED)
  ;; Game controller
  (:SDL-CONTROLLERAXISMOTION #x650)
  (:SDL-CONTROLLERBUTTONDOWN)
  (:SDL-CONTROLLERBUTTONUP)
  (:SDL-CONTROLLERDEVICEADDED)
  (:SDL-CONTROLLERDEVICEREMOVED)
  (:SDL-CONTROLLERDEVICEREMAPPED)
  (:SDL-CONTROLLERTOUCHPADDOWN)
  (:SDL-CONTROLLERTOUCHPADMOTION)
  (:SDL-CONTROLLERTOUCHPADUP)
  (:SDL-CONTROLLERSENSORUPDATE)
  ;; Touch
  (:SDL-FINGERDOWN #x700)
  (:SDL-FINGERUP)
  (:SDL-FINGERMOTION)
  ;; Gesture
  (:SDL-DOLLARGESTURE #x800)
  (:SDL-DOLLARDRECORD)
  (:SDL-MULTIGESTURE)
  ;; Clipboard
  (:SDL-CLIPBOARDUPDATE #x900)
  ;; Drag and Drop
  (:SDL-DROPFILE        #x1000)
  (:SDL-DROPTEXT)
  (:SDL-DROPBEGIN)
  (:SDL-DROPCOMPLETE)
  ;; Audio hotplug
  (:SDL-AUDIODEVICEADDED #x1100)
  (:SDL-AUDIODEVICEREMOVED)
  ;; Sensor
  (:SDL-SENSORUPDATE     #x1200)
  ;; Render
  (:SDL-RENDER-TARGETS-RESET #x2000)
  (:SDL-RENDER-DEVICE-RESET)
  ;; User
  (:SDL-USEREVENT #x8000)
  ;; Last event
  (:SDL-LASTEVENT #xFFFF))

;; Declared in include/SDL_scancode.h
(defcenum sdl-scancode
  "SDL keyboard scancode enumeration."
  (:SDL-SCANCODE-UNKNOWN 0)
  ;; Usage page 0x07 (USB keyboard page).
  (:SDL-SCANCODE-A 4)
  (:SDL-SCANCODE-B)
  (:SDL-SCANCODE-C)
  (:SDL-SCANCODE-D)
  (:SDL-SCANCODE-E)
  (:SDL-SCANCODE-F)
  (:SDL-SCANCODE-G)
  (:SDL-SCANCODE-H)
  (:SDL-SCANCODE-I)
  (:SDL-SCANCODE-J)
  (:SDL-SCANCODE-K)
  (:SDL-SCANCODE-L)
  (:SDL-SCANCODE-M)
  (:SDL-SCANCODE-N)
  (:SDL-SCANCODE-O)
  (:SDL-SCANCODE-P)
  (:SDL-SCANCODE-Q)
  (:SDL-SCANCODE-R)
  (:SDL-SCANCODE-S)
  (:SDL-SCANCODE-T)
  (:SDL-SCANCODE-U)
  (:SDL-SCANCODE-V)
  (:SDL-SCANCODE-W)
  (:SDL-SCANCODE-X)
  (:SDL-SCANCODE-Y)
  (:SDL-SCANCODE-Z)                     ; 29

  (:SDL-SCANCODE-1)                     ; 30
  (:SDL-SCANCODE-2)
  (:SDL-SCANCODE-3)
  (:SDL-SCANCODE-4)
  (:SDL-SCANCODE-5)
  (:SDL-SCANCODE-6)
  (:SDL-SCANCODE-7)
  (:SDL-SCANCODE-8)
  (:SDL-SCANCODE-9)
  (:SDL-SCANCODE-0)                     ; 39

  (:SDL-SCANCODE-RETURN)                ; 40
  (:SDL-SCANCODE-ESCAPE)
  (:SDL-SCANCODE-BACKSPACE)
  (:SDL-SCANCODE-TAB)
  (:SDL-SCANCODE-SPACE)                 ; 44

  (:SDL-SCANCODE-MINUS)
  (:SDL-SCANCODE-EQUALS)
  (:SDL-SCANCODE-LEFTBRACKET)
  (:SDL-SCANCODE-RIGHTBRACKET)
  (:SDL-SCANCODE-BACKSLASH)
  (:SDL-SCANCODE-NONUSHASH)
  (:SDL-SCANCODE-SEMICOLON)
  (:SDL-SCANCODE-APOSTROPHE)
  (:SDL-SCANCODE-GRAVE)                 ; 53
  (:SDL-SCANCODE-COMMA)
  (:SDL-SCANCODE-PERIOD)
  (:SDL-SCANCODE-SLASH)
  (:SDL-SCANCODE-CAPSLOCK)
  (:SDL-SCANCODE-F1)
  (:SDL-SCANCODE-F2)
  (:SDL-SCANCODE-F3)
  (:SDL-SCANCODE-F4)
  (:SDL-SCANCODE-F5)
  (:SDL-SCANCODE-F6)
  (:SDL-SCANCODE-F7)
  (:SDL-SCANCODE-F8)
  (:SDL-SCANCODE-F9)
  (:SDL-SCANCODE-F10)
  (:SDL-SCANCODE-F11)
  (:SDL-SCANCODE-F12)                   ; 69

  (:SDL-SCANCODE-PRINTSCREEN)           ; 70
  (:SDL-SCANCODE-SCROLLLOCK)
  (:SDL-SCANCODE-PAUSE)
  (:SDL-SCANCODE-INSERT)
  (:SDL-SCANCODE-HOME)
  (:SDL-SCANCODE-PAGEUP)
  (:SDL-SCANCODE-DELETE)
  (:SDL-SCANCODE-END)
  (:SDL-SCANCODE-PAGEDOWN)
  (:SDL-SCANCODE-RIGHT)
  (:SDL-SCANCODE-LEFT)
  (:SDL-SCANCODE-DOWN)
  (:SDL-SCANCODE-UP)                    ; 82

  (:SDL-SCANCODE-NUMLOCKCLEAR)          ; 83
  (:SDL-SCANCODE-KP-DIVIDE)
  (:SDL-SCANCODE-KP-MULTIPLY)
  (:SDL-SCANCODE-KP-MINUS)
  (:SDL-SCANCODE-KP-PLUS)
  (:SDL-SCANCODE-KP-ENTER)
  (:SDL-SCANCODE-KP-1)                  ; 89
  (:SDL-SCANCODE-KP-2)
  (:SDL-SCANCODE-KP-3)
  (:SDL-SCANCODE-KP-4)
  (:SDL-SCANCODE-KP-5)
  (:SDL-SCANCODE-KP-6)
  (:SDL-SCANCODE-KP-7)
  (:SDL-SCANCODE-KP-8)
  (:SDL-SCANCODE-KP-9)
  (:SDL-SCANCODE-KP-0)
  (:SDL-SCANCODE-KP-PERIOD)             ; 99

  (:SDL-SCANCODE-NONUSBACKSLASH)        ; 100
  (:SDL-SCANCODE-APPLICATION)
  (:SDL-SCANCODE-POWER)
  (:SDL-SCANCODE-KP-EQUALS)             ; 103
  (:SDL-SCANCODE-F13)
  (:SDL-SCANCODE-F14)
  (:SDL-SCANCODE-F15)
  (:SDL-SCANCODE-F16)
  (:SDL-SCANCODE-F17)
  (:SDL-SCANCODE-F18)
  (:SDL-SCANCODE-F19)
  (:SDL-SCANCODE-F20)
  (:SDL-SCANCODE-F21)
  (:SDL-SCANCODE-F22)
  (:SDL-SCANCODE-F23)
  (:SDL-SCANCODE-F24)
  (:SDL-SCANCODE-EXECUTE)               ; 116
  (:SDL-SCANCODE-HELP)
  (:SDL-SCANCODE-MENU)
  (:SDL-SCANCODE-SELECT)
  (:SDL-SCANCODE-STOP)
  (:SDL-SCANCODE-AGAIN)
  (:SDL-SCANCODE-UNDO)
  (:SDL-SCANCODE-CUT)
  (:SDL-SCANCODE-COPY)
  (:SDL-SCANCODE-PASTE)
  (:SDL-SCANCODE-FIND)
  (:SDL-SCANCODE-MUTE)
  (:SDL-SCANCODE-VOLUMEUP)
  (:SDL-SCANCODE-VOLUMEDOWN)            ; 129
  ;; Three skipped in SDL header.
  (:SDL-SCANCODE-KP-COMMA 133)
  (:SDL-SCANCODE-KP-EQUALSAS400)
  (:SDL-SCANCODE-KP-INTERNATIONAL1)     ; 135
  (:SDL-SCANCODE-KP-INTERNATIONAL2)
  (:SDL-SCANCODE-KP-INTERNATIONAL3)
  (:SDL-SCANCODE-KP-INTERNATIONAL4)
  (:SDL-SCANCODE-KP-INTERNATIONAL5)
  (:SDL-SCANCODE-KP-INTERNATIONAL6)
  (:SDL-SCANCODE-KP-INTERNATIONAL7)
  (:SDL-SCANCODE-KP-INTERNATIONAL8)
  (:SDL-SCANCODE-KP-INTERNATIONAL9)
  (:SDL-SCANCODE-LANG1)                 ; 144
  (:SDL-SCANCODE-LANG2)
  (:SDL-SCANCODE-LANG3)
  (:SDL-SCANCODE-LANG4)
  (:SDL-SCANCODE-LANG5)
  (:SDL-SCANCODE-LANG6)
  (:SDL-SCANCODE-LANG7)
  (:SDL-SCANCODE-LANG8)
  (:SDL-SCANCODE-LANG9)                 ; 152

  (:SDL-SCANCODE-ALTERASE)              ; 153 Erase-Eaze
  (:SDL-SCANCODE-SYSREQ)
  (:SDL-SCANCODE-CANCEL)
  (:SDL-SCANCODE-CLEAR)
  (:SDL-SCANCODE-PRIOR)
  (:SDL-SCANCODE-RETURN2)
  (:SDL-SCANCODE-SEPARATOR)
  (:SDL-SCANCODE-OUT)
  (:SDL-SCANCODE-OPER)
  (:SDL-SCANCODE-CLEARAGAIN)
  (:SDL-SCANCODE-CRSEL)
  (:SDL-SCANCODE-EXSEL)                 ; 164

  (:SDL-SCANCODE-KP-00)                 ; 176
  (:SDL-SCANCODE-KP-000)
  (:SDL-SCANCODE-THOUSANDSSEPARATOR)
  (:SDL-SCANCODE-DECIMALSEPARATOR)
  (:SDL-SCANCODE-CURRENCYUNIT)
  (:SDL-SCANCODE-CURRENCYSUBUNIT)
  (:SDL-SCANCODE-KP-LEFTPAREN)
  (:SDL-SCANCODE-KP-RIGHTPAREN)         ; 183
  (:SDL-SCANCODE-KP-LEFTBRACE)
  (:SDL-SCANCODE-KP-RIGHTBRACE)
  (:SDL-SCANCODE-KP-TAB)                ; 186
  (:SDL-SCANCODE-KP-BACKSPACE)
  (:SDL-SCANCODE-KP-A)
  (:SDL-SCANCODE-KP-B)
  (:SDL-SCANCODE-KP-C)
  (:SDL-SCANCODE-KP-D)
  (:SDL-SCANCODE-KP-E)
  (:SDL-SCANCODE-KP-F)
  (:SDL-SCANCODE-KP-XOR)
  (:SDL-SCANCODE-KP-POWER)
  (:SDL-SCANCODE-KP-PERCENT)
  (:SDL-SCANCODE-KP-LESS)
  (:SDL-SCANCODE-KP-GREATER)
  (:SDL-SCANCODE-KP-AMPERSAND)
  (:SDL-SCANCODE-KP-DBLAMPERSAND)       ; 200
  (:SDL-SCANCODE-KP-VERICALBAR)
  (:SDL-SCANCODE-KP-COLON)
  (:SDL-SCANCODE-KP-HASH)
  (:SDL-SCANCODE-KP-SPACE)
  (:SDL-SCANCODE-KP-AT)
  (:SDL-SCANCODE-KP-EXCLAM)
  (:SDL-SCANCODE-KP-MEMSTORE)
  (:SDL-SCANCODE-KP-MEMRECALL)
  (:SDL-SCANCODE-KP-MEMCLEAR)
  (:SDL-SCANCODE-KP-MEMADD)
  (:SDL-SCANCODE-KP-MEMSUBTRACT)
  (:SDL-SCANCODE-KP-MEMMULTIPLY)
  (:SDL-SCANCODE-KP-MEMDIVIDE)
  (:SDL-SCANCODE-KP-PLUSMINUS)
  (:SDL-SCANCODE-KP-CLEAR)
  (:SDL-SCANCODE-KP-CLEARENTRY)
  (:SDL-SCANCODE-KP-BINARY)
  (:SDL-SCANCODE-KP-OCTAL)
  (:SDL-SCANCODE-KP-DECIMAL)
  (:SDL-SCANCODE-KP-HEXADECIMAL)       ; 221
  ;; Jump over two numbers in SDL2 source
  (:SDL-SCANCODE-LCTRL 224)             ; 224
  (:SDL-SCANCODE-LSHIFT)
  (:SDL-SCANCODE-LALT)
  (:SDL-SCANCODE-LGUI)
  (:SDL-SCANCODE-RCTRL)
  (:SDL-SCANCODE-RSHIFT)
  (:SDL-SCANCODE-RALT)
  (:SDL-SCANCODE-RGUI)                  ; 231
  ;; Jump
  (:SDL-SCANCODE-MODE 257)              ; 257

  ;; Usage page 0x07

  (:SDL-SCANCODE-AUDIONEXT 258)         ; 258
  (:SDL-SCANCODE-AUDIOPREV)
  (:SDL-SCANCODE-AUDIOSTOP)
  (:SDL-SCANCODE-AUDIOPLAY)
  (:SDL-SCANCODE-AUDIOMUTE)
  (:SDL-SCANCODE-MEDIASELECT)
  (:SDL-SCANCODE-WWW)
  (:SDL-SCANCODE-MAIL)
  (:SDL-SCANCODE-CALCULATOR)
  (:SDL-SCANCODE-COMPUTER)
  (:SDL-SCANCODE-AC-SEARCH)
  (:SDL-SCANCODE-AC-HOME)
  (:SDL-SCANCODE-AC-BACK)
  (:SDL-SCANCODE-AC-FORWARD)
  (:SDL-SCANCODE-AC-STOP)
  (:SDL-SCANCODE-AC-REFRESH)
  (:SDL-SCANCODE-AC-BOOKMARKS)          ; 274

  (:SDL-SCANCODE-BRIGHTNESSDOWN)        ; 275
  (:SDL-SCANCODE-BRIGHTNESSUP)
  (:SDL-SCANCODE-DISPLAYSWITCH)
  (:SDL-SCANCODE-KBDILLUMTOGGLE)
  (:SDL-SCANCODE-KBDILLUMDOWN)
  (:SDL-SCANCODE-KBDILLUMUP)
  (:SDL-SCANCODE-EJECT)
  (:SDL-SCANCODE-SLEEP)

  (:SDL-SCANCODE-APP1)                  ; 283
  (:SDL-SCANCODE-APP2)                  ; 284

  ;; Usage page 0x0C

  (:SDL-SCANCODE-AUDIOREWIND)           ; 285
  (:SDL-SCANCODE-AUDIOFASTFORWARD)      ; 286

  (:SDL-NUM-SCANCODES 512)              ; 512, for array bounds.
  )


;; Declared in include/SDL_keycode.h
(defctype sdl-keycode :int32)

;; Declared in include/SDL_keyboard.h
(defcstruct sdl-keysym
  "Used in key events."
  (scancode sdl-scancode)
  (sym      sdl-keycode)
  (mod      :uint16)
  (unused   :uint32))

;; Declared in include/SDL_events.h
(defcstruct sdl-commonevent
  "Fields shared by all events."
  (type      :uint32)
  (timestamp :uint32))

(defcstruct sdl-displayevent
  "Display state change event (event.display)."
  (type      :uint32)
  (timestamp :uint32)
  (display   :uint32)
  (event     :uint8)
  (padding1  :uint8)
  (padding2  :uint8)
  (padding3  :uint8)
  (data1     :int32))

(defcstruct sdl-windowevent
  "Window state change event (event.window)."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  (event     :uint8)
  (padding1  :uint8)
  (padding2  :uint8)
  (padding3  :uint8)
  (data1     :int32)
  (data2     :int32))

(defcstruct sdl-keyboardevent
  "Keyboard button event (event.key)."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  (state     :uint8)
  (repeat    :uint8)
  (padding2  :uint8)
  (padding3  :uint8)
  (keysym    (:struct sdl-keysym)))

(defcstruct sdl-texteditingevent
  "Keyboard text editing event."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  ;; TODO magic value - constant doesn't work alone.
  (text      :char :count 32)           ; +SDL-TEXTEDITINGEVENT-TEXT-SIZE+
  (start     :int32)
  (length    :int32))

(defcstruct sdl-textinputevent
  "Keyboard text input event."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  ;; TODO magic value - constant doesn't work alone.
  (text      :char :count 32)           ; +SDL-TEXTINPUTEVENT-TEXT-SIZE+
  )

(defcstruct sdl-mousemotionevent
  "Mouse motion event."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  (which     :uint32)
  (state     :uint32)
  (x         :int32)
  (y         :int32)
  (xrel      :int32)
  (yrel      :int32))

(defcstruct sdl-mousebuttonevent
  "Mouse button event."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  (which     :uint32)
  (button    :uint8)
  (state     :uint8)
  (clicks    :uint8)
  (padding1  :uint8)
  (x         :int32)
  (y         :int32))

(defcstruct sdl-mousewheelevent
  "Mouse wheel event."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  (which     :uint32)
  (x         :int32)
  (y         :int32)
  (direction :uint32))

;; Declared in include/SDL_joystick.h
(defctype sdl-joystickid :int32)


(defcstruct sdl-joyaxisevent
  "Joystick axis event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (axis      :uint8)
  (padding1  :uint8)
  (padding2  :uint8)
  (padding3  :uint8)
  (value     :int16)
  (padding4  :uint16))

(defcstruct sdl-joyballevent
  "Joystick trackball motion event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (ball      :uint8)
  (padding1  :uint8)
  (padding2  :uint8)
  (padding3  :uint8)
  (xrel      :int16)
  (yrel      :int16))

(defcstruct sdl-joyhatevent
  "Joystick hat position change event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (hat       :uint8)
  (value     :uint8)
  (padding1  :uint8)
  (padding2  :uint8))

(defcstruct sdl-joybuttonevent
  "Joystick button event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (button    :uint8)
  (state     :uint8)
  (padding1  :uint8)
  (padding2  :uint8))

(defcstruct sdl-joydeviceevent
  "Joystick device event."
  (type      :uint32)
  (timestamp :uint32)
  (which     :int32))

(defcstruct sdl-controlleraxisevent
  "Game controller axis motion event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (axis      :uint8)
  (padding1  :uint8)
  (padding2  :uint8)
  (padding3  :uint8)
  (value     :int16)
  (padding4  :uint16))

(defcstruct sdl-controllerbuttonevent
  "Game controller button event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (button    :uint8)
  (state     :uint8)
  (padding1  :uint8)
  (padding2  :uint8))

(defcstruct sdl-controllerdeviceevent
  "Game controller device event."
  (type      :uint32)
  (timestamp :uint32)
  (which     :int32))

(defcstruct sdl-controllertouchpadevent
  "Game controller touchpad event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (touchpad  :int32)
  (finger    :int32)
  (x         :float)
  (y         :float)
  (pressure  :float))

(defcstruct sdl-controllersensorevent
  "Game controller sensor event."
  (type      :uint32)
  (timestamp :uint32)
  (which     sdl-joystickid)
  (sensor    :int32)
  (data      :float :count 3))

(defcstruct sdl-audiodeviceevent
  "Audio device event."
  (type      :uint32)
  (timestamp :uint32)
  (which     :uint32)
  (iscapture :uint8)
  (padding1  :uint8)
  (padding2  :uint8)
  (padding3  :uint8))

(defcstruct sdl-sensorevent
  "Sensor event."
  (type      :uint32)
  (timestamp :uint32)
  (which     :int32)
  (data      :float :count 6))

(defcstruct sdl-quitevent
  "Quit requested event."
  (type      :uint32)
  (timestamp :uint32))

(defcstruct sdl-userevent
  "User defined event."
  (type      :uint32)
  (timestamp :uint32)
  (windowID  :uint32)
  (code      :int32)
  (data1     :pointer)
  (data2     :pointer))

(defcstruct sdl-syswmevent
  "Video driver dependent system event, disabled by default."
  (type      :uint32)
  (timestamp :uint32)
  (msg       :pointer)                  ; SDL_SysWMmsg*
  )

;; Declared in include/SDL_touch.h
(defctype sdl-touchid  :int64)
(defctype sdl-fingerid :int64)

;; declared in include/SDL_gesture.h
(defctype sdl-gestureid :int64)

(defcstruct sdl-touchfingerevent
  "Touch finger event."
  (type       :uint32)
  (timestamp  :uint32)
  (touchId    sdl-touchid)
  (fingerId   sdl-fingerid)
  (x          :float)
  (y          :float)
  (dx         :float)
  (dy         :float)
  (pressure   :float)
  (windowID   :uint32))

(defcstruct sdl-multigestureevent
  "Multiple finger gesture event."
  (type       :uint32)
  (timestamp  :uint32)
  (touchId    sdl-touchid)
  (dTheta     :float)
  (dDist      :float)
  (x          :float)
  (y          :float)
  (numFingers :uint16)
  (padding    :uint16))

(defcstruct sdl-dollargestureevent
  "Dollar gesture event."
  (type       :uint32)
  (timestamp  :uint32)
  (touchId    sdl-touchid)
  (gestureId  sdl-gestureid)
  (numFingers :uint32)
  (err        :float)                   ; error in SDL
  (x          :float)
  (y          :float))

(defcstruct sdl-dropevent
  "Dollar gesture event."
  (type       :uint32)
  (timestamp  :uint32)
  (file       :pointer)                 ; char*
  (windowID   :uint32))

;; Declared in SDL_surface.h
(defcstruct sdl-surface
  "Collection of pixels."
  (flags        :uint32)
  (format       :pointer)
  (w            :int)
  (h            :int)
  (pitch        :int)
  (pixels       :pointer)
  (userdata     :pointer)
  (locked       :int)
  (list-blitmap :pointer)
  (clip-rect    (:struct sdl-rect))
  (map          :pointer)
  (refcount     :int))

(defcunion sdl-event
  "General event structure (union)."
  (type      sdl-eventtype)
  (common    (:struct sdl-commonevent))
  (display   (:struct sdl-displayevent))
  (window    (:struct sdl-windowevent))
  (key       (:struct sdl-keyboardevent))
  (edit      (:struct sdl-texteditingevent))
  (text      (:struct sdl-textinputevent))
  (motion    (:struct sdl-mousemotionevent))
  (button    (:struct sdl-mousebuttonevent))
  (wheel     (:struct sdl-mousewheelevent))
  (jaxis     (:struct sdl-joyaxisevent))
  (jball     (:struct sdl-joyballevent))
  (jhat      (:struct sdl-joyhatevent))
  (jbutton   (:struct sdl-joybuttonevent))
  (jdevice   (:struct sdl-joydeviceevent))
  (caxis     (:struct sdl-controlleraxisevent))
  (cbutton   (:struct sdl-controllerbuttonevent))
  (cdevice   (:struct sdl-controllerdeviceevent))
  (ctouchpad (:struct sdl-controllertouchpadevent))
  (csensor   (:struct sdl-controllersensorevent))
  (adevice   (:struct sdl-audiodeviceevent))
  (sensor    (:struct sdl-sensorevent))
  (quit      (:struct sdl-quitevent))
  (user      (:struct sdl-userevent))
  (syswm     (:struct sdl-syswmevent))
  (tfinger   (:struct sdl-touchfingerevent))
  (mgesture  (:struct sdl-multigestureevent))
  (dgesture  (:struct sdl-dollargestureevent))
  (drop      (:struct sdl-dropevent))
  (padding   :uint8 :count 56))

;; Maybe all FORMAT-SDL-* using PRINT-OBJECT
(defun format-sdl-keysym (keysym)
  (format nil
          "scancode=~A sym=~A mod=~A"
          (getf keysym 'scancode)
          (getf keysym 'sym)
          (getf keysym 'mod)))

(defun format-sdl-commonevent (event name)
  (with-foreign-slots ((type timestamp) event (:struct sdl-commonevent))
    (format nil
            "Event '~A' type=~A timestamp=~A"
            name
            type
            timestamp)))

(defun format-sdl-quitevent (event)
  (with-foreign-slots ((type timestamp) event (:struct sdl-quitevent))
    (format nil
            "Event 'QuitEvent' type=~A timestamp=~A"
            type
            timestamp)))

(defun format-sdl-displayevent (event)
  (with-foreign-slots ((type timestamp display event data1) event (:struct sdl-displayevent))
    (format nil
            "Event 'DisplayEvent' type=~A timestamp=~A display=~A event=~A data1=~A"
            type
            timestamp
            display
            event
            data1)))

(defun format-sdl-windowevent (event)
  (with-foreign-slots ((type timestamp windowID data1 data2) event (:struct sdl-windowevent))
    (format nil
            "Event 'WindowEvent' type=~A timestamp=~A windowID=~A event=~A data1=~A data2=~A"
            type
            timestamp
            windowID
            event
            data1
            data2)))

(defun format-sdl-syswmevent (event)
  (with-foreign-slots ((type timestamp msg) event (:struct sdl-syswmevent))
    (format nil
            "Event 'SysWMEvent' type=~A timestamp=~A msg=~A"
            type
            timestamp
            msg)))

(defun format-sdl-keyboardevent (event name)
  (with-foreign-slots ((type timestamp windowID state repeat keysym) event (:struct sdl-keyboardevent))
      (format nil
              "Event '~A' type=~A timestamp=~A windowID=~A state=~A repeat=~A keysym=~A"
              name
              type
              timestamp
              windowID
              state
              repeat
              (format-sdl-keysym keysym))))

(defun format-sdl-texteditingevent (event)
  (with-foreign-slots ((type timestamp windowID text start length) event (:struct sdl-texteditingevent))
    (format nil
            "Event 'TextEditingEvent' type=~A timestamp=~A windowID=~A text=~A start=~A length=~A"
            type
            timestamp
            windowID
            text
            start
            length)))

(defun format-sdl-mousemotionevent (event)
  (with-foreign-slots ((type timestamp windowID which state x y xrel yrel) event (:struct sdl-mousemotionevent))
    (format nil
            "Event 'MouseMotionEvent' type=~A timestamp=~A windowID=~A which=~A state=~A x=~A y=~A xrel=~A yrel=~A"
            type
            timestamp
            windowID
            which
            state
            x
            y
            xrel
            yrel)))

(defun format-sdl-event (event)
  (with-foreign-slots ((type) event (:union sdl-event))
    (case type
      (:SDL-FIRSTEVENT               (format-sdl-commonevent event "first"))
      ;; Application
      (:SDL-QUIT                     (format-sdl-quitevent event))
      (:SDL-APP-TERMINATING          (format-sdl-commonevent event "terminating"))
      (:SDL-APP-LOWMEMORY            (format-sdl-commonevent event "lowmemory"))
      (:SDL-APP-LOCALECHANGED        (format-sdl-commonevent event "localechanged"))
      ;; Display
      (:SDL-DISPLAYEVENT             (format-sdl-displayevent event))
      ;; Window
      (:SDL-WINDOWEVENT              (format-sdl-windowevent event))
      (:SDL-SYSWMEVENT               (format-sdl-syswmevent event))
      ;; Keyboard
      (:SDL-KEYDOWN                  (format-sdl-keyboardevent event "KeyDown"))
      (:SDL-KEYUP                    (format-sdl-keyboardevent event "KeyUp"))
      (:SDL-TEXTEDITING              (format-sdl-texteditingevent event))
      (:SDL-TEXTINPUT                (format nil "TYPE: TextInput"))
      (:SDL-KEYMAPCHANGED            (format nil "TYPE: KeymapChanged"))
      ;; Mouse
      (:SDL-MOUSEMOTION              (format-sdl-mousemotionevent event))
      (:SDL-MOUSEBUTTONDOWN          (format nil "TYPE: MouseButtonDown"))
      (:SDL-MOUSEBUTTONUP            (format nil "TYPE: MouseButtonUp"))
      (:SDL-MOUSEWHEEL               (format nil "TYPE: MouseWheel"))
      ;; Joystick
      (:SDL-JOYAXISMOTION            (format nil "TYPE: JoyAxisMotion"))
      (:SDL-JOYBALLMOTION            (format nil "TYPE: JoyBallMotion"))
      (:SDL-JOYHATMOTION             (format nil "TYPE: JoyHatMotion"))
      (:SDL-JOYBUTTONDOWN            (format nil "TYPE: JoyButtonDown"))
      (:SDL-JOYBUTTONUP              (format nil "TYPE: JoyButtonUp"))
      (:SDL-JOYDEVICEADDED           (format nil "TYPE: JoyDeviceAdded"))
      (:SDL-JOYDEVICEREMOVED         (format nil "TYPE: JoyDeviceRemoved"))
      ;; Game controller
      (:SDL-CONTROLLERAXISMOTION     (format nil "TYPE: ControllerAxisMotion"))
      (:SDL-CONTROLLERBUTTONDOWN     (format nil "TYPE: ControllerButtonDown"))
      (:SDL-CONTROLLERBUTTONUP       (format nil "TYPE: ControllerButtonUp"))
      (:SDL-CONTROLLERDEVICEADDED    (format nil "TYPE: ControllerDeviceAdded"))
      (:SDL-CONTROLLERDEVICEREMOVED  (format nil "TYPE: ControllerDeviceRemoved"))
      (:SDL-CONTROLLERDEVICEREMAPPED (format nil "TYPE: ControllerDeviceMapped"))
      (:SDL-CONTROLLERTOUCHPADDOWN   (format nil "TYPE: ControllerTouchpadDown"))
      (:SDL-CONTROLLERTOUCHPADMOTION (format nil "TYPE: ControllerTouchpadMotion"))
      (:SDL-CONTROLLERTOUCHPADUP     (format nil "TYPE: ControllerTouchpadUp"))
      (:SDL-CONTROLLERSENSORUPDATE   (format nil "TYPE: ControllerSensorUpdate"))
      ;; Touch
      (:SDL-FINGERDOWN               (format nil "TYPE: FingerDown"))
      (:SDL-FINGERUP                 (format nil "TYPE: FingerUp"))
      (:SDL-FINGERMOTION             (format nil "TYPE: FingerMotion"))
      ;; Gesture
      (:SDL-DOLLARGESTURE            (format nil "TYPE: DollarGesture"))
      (:SDL-DOLLARDRECORD            (format nil "TYPE: DollarRecord"))
      (:SDL-MULTIGESTURE             (format nil "TYPE: MultiGesture"))
      ;; Clipboard
      (:SDL-CLIPBOARDUPDATE          (format nil "TYPE: ClipboardUpdate"))
      ;; Drag and Drop
      (:SDL-DROPFILE                 (format nil "TYPE: DropFile"))
      (:SDL-DROPTEXT                 (format nil "TYPE: DropText"))
      (:SDL-DROPBEGIN                (format nil "TYPE: DropBegin"))
      (:SDL-DROPCOMPLETE             (format nil "TYPE: DropComplete"))
      ;; Audio hotplug
      (:SDL-AUDIODEVICEADDED         (format nil "TYPE: AudioDeviceAdded"))
      (:SDL-AUDIODEVICEREMOVED       (format nil "TYPE: AudioDeviceRemoved"))
      ;; Sensor
      (:SDL-SENSORUPDATE             (format nil "TYPE: SensorUpdate"))
      ;; Render
      (:SDL-RENDER-TARGETS-RESET     (format nil "TYPE: RenderTargetsReset"))
      (:SDL-RENDER-DEVICE-RESET      (format nil "TYPE: RenderDeviceReset"))
      ;; User
      (:SDL-USEREVENT                (format nil "TYPE: UserEvent"))
      ;; Last event
      (:SDL-LASTEVENT                (format nil "TYPE: LastEvent")))))

;; Declared in include/SDL_video.h
(eval (let ((sdl-win-fullscr #x01))
        `(defcenum sdl-windowflags
           "Flags for a window"
           (:SDL-WINDOW-FULLSCREEN    ,sdl-win-fullscr)
           (:SDL-WINDOW-OPENGL        #x02)
           (:SDL-WINDOW-SHOWN         #x04)
           (:SDL-WINDOW-HIDDEN        #x08)
           (:SDL-WINDOW-BORDERLESS    #x10)
           (:SDL-WINDOW-RESIZABLE     #x20)
           (:SDL-WINDOW-MINIMIZED     #x40)
           (:SDL-WINDOW-MAXIMIZED     #x80)
           (:SDL-WINDOW-INPUT-GRABBED #x100)
           (:SDL-WINDOW-INPUT-FOCUS   #x200)
           (:SDL-WINDOW-MOUSE-FOCUS   #x400)
           (:SDL-WINDOW-FULLSCREEN-DESKTOP ,(logior sdl-win-fullscr #x1000))
           (:SDL-WINDOW-FOREIGN       #x800)
           (:SDL-WINDOW-ALLOW-HIGHDPI #x2000)
           (:SDL-WINDOW-MOUSE-CAPTURE #x4000)
           (:SDL-WINDOW-ALWAYS-ON-TOP #x8000)
           (:SDL-WINDOW-SKIP-TASKBAR  #x10000)
           (:SDL-WINDOW-UTILITY       #x20000)
           (:SDL-WINDOW-TOOLTIP       #x40000)
           (:SDL-WINDOW-POPUP-MENU    #x80000)
           (:SDL-WINDOW-VULKAN        #x10000000)
           (:SDL-WINDOW-METAL         #x20000000))))

(defcfun "SDL_GL_CreateContext" :pointer
  (window :pointer))

(defcfun "SDL_GL_SetSwapInterval" :int
  (interval :int))

(defcfun "SDL_CreateWindow" :pointer
  (title (:string :encoding :utf-8))
  (x :int)
  (y :int)
  (w :int)
  (flags :uint32)
  (h :int))

;; Declared in include/SDL_video.h
(defcfun "SDL_SetWindowFullscreen" :int
  (window :pointer)
  (flags  :uint32))

;; Defined in SDL_keycode.h
(defconstant +KMOD-NONE+     #x0)
(defconstant +KMOD-LSHIFT+   #x1)
(defconstant +KMOD-RSHIFT+   #x2)
(defconstant +KMOD-LCTRL+    #x40)
(defconstant +KMOD-RCTRL+    #x80)
(defconstant +KMOD-LALT+     #x100)
(defconstant +KMOD-RALT+     #x200)
(defconstant +KMOD-LGUI+     #x400)
(defconstant +KMOD-RGUI+     #x800)
(defconstant +KMOD-NUM+      #x1000)
(defconstant +KMOD-CAPS+     #x2000)
(defconstant +KMOD-MODE+     #x4000)
(defconstant +KMOD-RESERVED+ #x8000)
(defconstant +KMOD-CTRL+     (logior +KMOD-LCTRL+  +KMOD-RCTRL+))
(defconstant +KMOD-SHIFT+    (logior +KMOD-LSHIFT+ +KMOD-RSHIFT+))
(defconstant +KMOD-ALT+      (logior +KMOD-LALT+   +KMOD-RALT+))
(defconstant +KMOD-GUI+      (logior +KMOD-LGUI+   +KMOD-RGUI+))

(defcfun "SDL_GetModState" :uint32)     ; SDL_Keymod in C

;; OpenGL

;; declared in GL/gl.h
(defconstant +GL-FALSE+               #x0)
(defconstant +GL-TRUE+                #x1)
(defconstant +GL-CURRENT-BIT+         #x1)
(defconstant +GL-POINT-BIT+           #x2)
(defconstant +GL-LINE-BIT+            #x4)
(defconstant +GL-POLYGON-BIT+         #x8)
(defconstant +GL-POLYGON-STIPPLE-BIT+ #x10)
(defconstant +GL-PIXEL-MODE-BIT+      #x20)
(defconstant +GL-LIGHTING-BIT+        #x40)
(defconstant +GL-FOG-BIT+             #x80)
(defconstant +GL-DEPTH-BUFFER-BIT+    #x100)
(defconstant +GL-ACCUM-BUFFER-BIT+    #x200)
(defconstant +GL-STENCIL-BUFFER-BIT+  #x400)
(defconstant +GL-VIEWPORT-BIT+        #x800)
(defconstant +GL-TRANSFORM-BIT+       #x1000)
(defconstant +GL-ENABLE-BIT+          #x2000)
(defconstant +GL-COLOR-BUFFER-BIT+    #x4000)
(defconstant +GL-EVAL-BIT+            #x10000)
(defconstant +GL-LIST-BIT+            #x20000)
(defconstant +GL-TEXTURE-BIT+         #x40000)
(defconstant +GL-SCISSOR-BIT+         #x80000)
(defconstant +GL-ALL-ATTRIB-BITS+     #xFFFFFFFF)

(defconstant +GL-TEXTURE-2D+          #x0DE1)
(defconstant +GL-DEPTH-TEST+          #x0B71)
(defconstant +GL-DEPTH-TEST+          #x0B71)
(defconstant +GL-CULL-FACE+           #x0B44)
(defconstant +GL-ALPHA-TEST+          #x0BC0)

(defconstant +GL-NO-ERROR+            #x0)
(defconstant +GL-INVALID-ENUM+        #x500)
(defconstant +GL-INVALID-VALUE+       #x501)
(defconstant +GL-INVALID-OPERATION+   #x502)
(defconstant +GL-STACK-OVERFLOW+      #x503)
(defconstant +GL-STACK-UNDERFLOW+     #x504)
(defconstant +GL-OUT-OF-MEMORY+       #x505)

(defconstant +GL-BYTE+                #x1400)
(defconstant +GL-UNSIGNED-BYTE+       #x1401)

(defconstant +GL-RGB+                 #x1907)
(defconstant +GL-RGBA+                #x1908)

(defconstant +GL-LIGHTING+            #x0B50)

(defconstant +GL-BLEND+               #x0BE2)

(defcfun "glewInit" :uint)

(defcfun "glGetError" :uint32)           ; GLenum for real
(defcfun "gluErrorString" (:string :encoding :utf-8)
  (error :uint32)) ;; GLenum for real

(defcfun "glGetString" (:string :encoding :utf-8)
  (name :uint32))

(defcfun "glClear" :void
  (mask :uint32))

(defcfun "glClearColor" :void
  (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defcfun "glDisable" :void
  (cap :uint32))

(defcfun "glEnable" :void
  (cap :uint32))

(defcfun "glBlendFunc" :void
  (sfactor :uint32)
  (dfactor :uint32))

(defcfun "glGenTextures" :void
  (n        :uint32)
  (textures :pointer))

(defcfun "glBindTexture" :void
  (target   :uint32)
  (texture  :uint32))

(defcfun "glTranslatef" :void
  (x :float)
  (y :float)
  (z :float))

(defcfun "glViewport" :void
  (x :int32)
  (y :int32)
  (w :uint32)
  (h :uint32))

(defcfun "glFlush" :void)

;; GLenum for real. Defined in include/GL/gl.h
(defconstant +GL-MODELVIEW+  #x1700)
(defconstant +GL-PROJECTION+ #x1701)
(defconstant +GL-TEXTURE+    #x1702)
(defconstant +GL-COLOR+      #x1800)

(defcfun "glMatrixMode" :void
  (mode :uint))

(defcfun "glLoadIdentity" :void)

(defcfun "gluPerspective" :void
  (fovy   :double)
  (aspect :double)
  (znear  :double)
  (zfar   :double))

(defcfun "gluOrtho2D" :void
  (left   :double)
  (rightt :double)
  (bottom :double)
  (top    :double))

(defcfun "glColor3f" :void
  (red   :float)
  (green :float)
  (blue  :float))

(defcfun "glColor4f" :void
  (red   :float)
  (green :float)
  (blue  :float)
  (alpha :float))

(defcfun "glVertex3f" :void
  (x :float)
  (y :float)
  (z :float))

(defcfun "glNormal3f" :void
  (nx :float)
  (ny :float)
  (nz :float))

(defcfun "glTexCoord2f" :void
  (cs :float)
  (ct :float))

(defcfun "glTexCoord2d" :void
  (cs :double)
  (ct :double))

(defcfun "glOrtho" :void
  (left   :double)
  (right  :double)
  (bottom :double)
  (top    :double)
  (znear  :double)
  (zfar   :double))

;; See
;; https://www.khronos.org/opengl/wiki/OpenGL_Type
;; for size of GLbitfield
(defcfun "glPushAttrib" :void
  (mask   :uint32))

(defcfun "glPopAttrib" :void)

(defcfun "gluLookAt" :void
  (eyeX    :double)
  (eyeY    :double)
  (eyeZ    :double)
  (centerX :double)
  (centerY :double)
  (centerZ :double)
  (upX     :double)
  (upY     :double)
  (upZ     :double))

(defcfun "glTexImage2D" :void
  (target         :uint32)
  (level          :int32)
  (internalformat :int32)
  (width          :uint32)
  (height         :uint32)
  (border         :int32)
  (format         :uint32)
  (type           :uint32)
  (pixels         :pointer))

(defcfun "glTexParameteri" :void
  (texture :uint32)
  (pname   :uint32)
  (param   :int32))

(defcfun "glPushMatrix" :void)
(defcfun "glPopMatrix"  :void)

;; GLenum for real. Defined in include/GL/gl.h
(defconstant +GL-POINTS+              #x0)
(defconstant +GL-LINES+               #x1)
(defconstant +GL-LINE-LOOP+           #x2)
(defconstant +GL-LINE-STRIP+          #x3)
(defconstant +GL-TRIANGLES+           #x4)
(defconstant +GL-TRIANGLE-STRIP+      #x5)
(defconstant +GL-TRIANGLE-FAN+        #x6)
(defconstant +GL-QUADS+               #x7)
(defconstant +GL-QUAD-STRIP+          #x8)
(defconstant +GL-POLYGON+             #x9)

;; Blending.
(defconstant +GL-BLEND+               #x0BE2)
(defconstant +GL-BLEND-SRC+           #x0BE1)
(defconstant +GL-BLEND-DST+           #x0BE0)
(defconstant +GL-ZERO+                #x0)
(defconstant +GL-ONE+                 #x1)
(defconstant +GL-SRC-COLOR+           #x0300)
(defconstant +GL-ONE-MINUS-SRC-COLOR+ #x0301)
(defconstant +GL-SRC-ALPHA+           #x0302)
(defconstant +GL-ONE-MINUS-SRC-ALPHA+ #x0303)
(defconstant +GL-DST-ALPHA+           #x0304)
(defconstant +GL-ONE-MINUS-DST-ALPHA+ #x0305)
(defconstant +GL-DST-COLOR+           #x0306)
(defconstant +GL-ONE-MINUS-DST-COLOR+ #x0307)
(defconstant +GL-SRC-ALPHA-SATURATE+  #x0308)

;; Utility

(defconstant +GL-VERSION+ #x1F02)

;; Hints
(defconstant +GL-PERSPECTIVE-CORRECTION-HINT+ #x0C50)
(defconstant +GL-POINT-SMOOTH-HINT+           #x0C51)
(defconstant +GL-LINE-SMOOTH-HINT+            #x0C52)
(defconstant +GL-POLYGON-SMOOTH-HINT+         #x0C53)
(defconstant +GL-FOG-HINT+                    #x0C54)
(defconstant +GL-DONT-CARE+                   #x1100)
(defconstant +GL-FASTEST+                     #x1101)
(defconstant +GL-NICEST+                      #x1102)

;; Texture mapping
(defconstant +GL-TEXTURE-MAG-FILTER+          #x2800)
(defconstant +GL-TEXTURE-MIN-FILTER+          #x2801)
(defconstant +GL-LINEAR+                      #x2601)
(defconstant +GL-NEAREST+                     #x2600)
;; ...
(defconstant +GL-LINEAR-MIPMAP-LINEAR+        #x2703)

;; Defined in include/GL/glext.h
(defconstant +GL-GENERATE-MIPMAP+ #x8191)

(defcfun "glBegin" :void
  (mode :uint))

(defcfun "glGenerateMipmap" :void
  (target :uint32))

(defcfun "glEnd" :void)

(defcfun "glHint" :void
  (target :uint32)
  (mode   :uint32))





(defcfun "SDL_LockSurface" :int
  (surface :pointer))

(defcfun "SDL_UnlockSurface" :void
  (surface :pointer))





;; Declared in include/SDL_joystick.h
(defcfun "SDL_NumJoysticks" :int)

(defcfun "SDL_JoystickOpen" :pointer
  (device-index :int))

(defcfun "SDL_JoystickClose" :void
  (joystick :pointer))





;; Declared in include/SDL_stdinc.h
(defcenum sdl-bool
  "Boolean type"
  (:SDL-FALSE 0)
  (:SDL-TRUE))

;; Declared in include/SDL_gamecontroller.h
(defcenum sdl-gamecontrollerbutton
  "Available buttons from a controller"
  (:SDL-CONTROLLER-BUTTON-INVALID -1)
  (:SDL-CONTROLLER-BUTTON-A        0)
  (:SDL-CONTROLLER-BUTTON-B)
  (:SDL-CONTROLLER-BUTTON-X)
  (:SDL-CONTROLLER-BUTTON-Y)
  (:SDL-CONTROLLER-BUTTON-BACK)
  (:SDL-CONTROLLER-BUTTON-GUIDE)
  (:SDL-CONTROLLER-BUTTON-START)
  (:SDL-CONTROLLER-BUTTON-LEFTSTICK)
  (:SDL-CONTROLLER-BUTTON-RIGHTSTICK)
  (:SDL-CONTROLLER-BUTTON-LEFTSHOULDER)
  (:SDL-CONTROLLER-BUTTON-RIGHTHOULDER)
  (:SDL-CONTROLLER-BUTTON-DPAD-UP)
  (:SDL-CONTROLLER-BUTTON-DPAD-DOWN)
  (:SDL-CONTROLLER-BUTTON-DPAD-LEFT)
  (:SDL-CONTROLLER-BUTTON-DPAD-RIGHT)
  (:SDL-CONTROLLER-BUTTON-MISC1)
  (:SDL-CONTROLLER-BUTTON-PADDLE1)
  (:SDL-CONTROLLER-BUTTON-PADDLE2)
  (:SDL-CONTROLLER-BUTTON-PADDLE3)
  (:SDL-CONTROLLER-BUTTON-PADDLE4)
  (:SDL-CONTROLLER-BUTTON-TOUCHPAD)
  (:SDL-CONTROLLER-BUTTON-MAX))

(defcenum sdl-gamecontrolleraxis
  "Available axes from controller"
  (:SDL-CONTROLLER-AXIS-INVALID -1)
  (:SDL-CONTROLLER-AXIS-LEFTX    0)
  (:SDL-CONTROLLER-AXIS-LEFTY)
  (:SDL-CONTROLLER-AXIS-RIGHTX)
  (:SDL-CONTROLLER-AXIS-RIGHTY)
  (:SDL-CONTROLLER-AXIS-TRIGGERLEFT)
  (:SDL-CONTROLLER-AXIS-TRIGGERRIGHT)
  (:SDL-CONTROLLER-AXIS-MAX))

(defcfun "SDL_IsGameController" sdl-bool
  (joystick_index :int))

(defcfun "SDL_GameControllerOpen" :pointer
  (joystick_index :int))

(defcfun "SDL_GameControllerClose" :void
  (gamecontroller :pointer))

(defcfun "SDL_GameControllerGetButton" :uint8
  (gamecontroller :pointer)
  (button         sdl-gamecontrollerbutton))

(defcfun "SDL_GameControllerGetAxis" :int16
  (gamecontroller :pointer)
  (axis           sdl-gamecontrolleraxis))





;; Declared in include/SDL_render.h
(defcfun "SDL_CreateRenderer" :pointer
  (window :pointer)
  (index  :int)
  (flags  :uint32))

(defcfun "SDL_SetRenderDrawColor" :int
  (renderer :pointer)
  (r  :uint8)
  (g  :uint8)
  (b  :uint8)
  (a  :uint8))

(defcfun "SDL_RenderDrawRect" :int
  (renderer :pointer)
  (rect     :pointer))                     ; const SDL_Rect*

(defcfun "SDL_RenderClear" :int
  (renderer :pointer))

(defcfun "SDL_RenderPresent" :void
  (renderer :pointer))

;; TODO this is only for testing and is NOT PORTABLE
;; FIX it by calculating the correct value for the machine
;; Declared in SDL_pixels.h
(defconstant +SDL-PIXELFORMAT-RGBA+    376840196)

(defcfun "SDL_ConvertSurfaceFormat" :pointer
  (src          :pointer)
  (pixel-format :uint32)
  (flags        :uint32))

(defcfun "SDL_CreateTextureFromSurface" :pointer
  (renderer :pointer)
  (surface  :pointer))

(defcfun "SDL_DestroyTexture" :void
  (texture :pointer))

(defcfun "SDL_RenderCopy" :void
  (renderer :pointer)
  (texture :pointer)
  (srcrect :pointer)                    ; const SDL_Rect*
  (dstrect :pointer))                   ; const SDL_Rect*

(defcfun "SDL_DestroyRenderer" :void
  (renderer :pointer))

(defmacro with-renderer ((&rest args) &body body)
  "Anaphoric macro that introduces an anaphor RENDERER to the forms in BODY."
  `(with-foreign-objects ((renderer :pointer))
     (unwind-protect
          (progn
            (setf renderer (sdl-createrenderer ,@args))
            (unless renderer
              (error "WITH-RENDERER failed to create RENDERER."))
            ,@body)
       (sdl-destroyrenderer renderer))))





;; Declared in include/SDL_surface.h
(defcfun "SDL_UpperBlit" :int
  (src     :pointer)
  (srcrect :pointer)
  (dst     :pointer)
  (dstrect :pointer))

(defcfun "SDL_FreeSurface" :void
  (surface  :pointer))

(defcfun "SDL_GetWindowSurface" :pointer
  (window  :pointer))

;; Declared in include/SDL_video.h
(defcfun "SDL_UpdateWindowSurface" :int
  (window  :pointer))

;; Declared in include/SDL_timer.h
(defcfun "SDL_Delay" :void
  (delay :uint32))

;; Declared in include/SDL_video.h
(defcfun "SDL_DestroyWindow" :void
  (window :pointer))

(defcfun "SDL_PollEvent" :int
  (event :pointer))

;; Declared in include/SDL.h
(defcfun "SDL_Quit" :void)

(defmacro with-init ((&rest flags) &body body)
  `(progn
     (unless (sdl-init ,@flags)
       (error "SDL Init failed"))
     (unwind-protect
          ,@body
       (sdl-quit))))

;; Defined in src/video/SDL_fillrect.c
(defcfun "SDL_FillRect" :int
  (dst   :pointer)
  (rect  :pointer)                         ; const *
  (color :uint32))

;; Declared in SDL_rwops.h
(defcfun "SDL_RWFromFile" :pointer
  (file (:string :encoding :utf-8))
  (mode (:string :encoding :utf-8)))

(defcfun "SDL_RWFromMem" :pointer
  (mem  :pointer)
  (size :int))

(defcfun "SDL_RWFromConstMem" :pointer
  (mem  :pointer)
  (size :int))


;;; SDL_Image

;; Declared in SDL_image.h
(defconstant +IMG-INIT-JPG+    #x1)
(defconstant +IMG-INIT-PNG+    #x2)
(defconstant +IMG-INIT-TIF+    #x4)
(defconstant +IMG-INIT-WEBP+   #x8)

(defcfun "IMG_Init" :int
  (flags :int))

(defcfun "IMG_Quit" :void)

(defcfun "IMG_Load" :pointer
  (file (:string :encoding :utf-8)))

(defcfun "IMG_Load_RW" :pointer
  (src     :pointer)
  (freesrc :int))

(defmacro with-img ((&rest flags) &body body)
  `(progn
     (unless (img-init ,@flags)
       (error "IMG Init failed"))
     (unwind-protect
          ,@body
       (img-quit))))





;;; SDL_TTF

;; Declared in SDL_ttf.h
(defcfun "TTF_Init" :int)

(defcfun "TTF_Quit" :void)

(defmacro with-ttf (&body body)
  `(progn
     (unless (ttf-init)
       (error "TTF Init failed"))
     (unwind-protect
          ,@body
       (ttf-quit))))

(defcfun "TTF_OpenFont" :pointer
  (file (:string :encoding :utf-8))
  (ptsize :int))

(defcfun "TTF_OpenFontRW" :pointer
  (src     :pointer)
  (freesrc :int)
  (ptsize  :int))

(defcfun "TTF_CloseFont" :void
  (font :pointer))

(defcfun "TTF_RenderUTF8_Solid" :pointer
  (font :pointer)
  (text (:string :encoding :utf-8))
  (fg (:struct sdl-color)))             ; fg is passed by value

(defcfun "TTF_RenderUTF8_Blended" :pointer
  (font :pointer)
  (text (:string :encoding :utf-8))
  (fg (:struct sdl-color)))             ; fg is passed by value

(defcfun "TTF_RenderUTF8_Blended_Wrapped" :pointer
  (font       :pointer)
  (text       (:string :encoding :utf-8))
  (fg         (:struct sdl-color))      ; fg is passed by value
  (wrapLength :uint32))

(defun sdl-ext-get-texture-dimensions (texture)
  (trivial-main-thread:with-body-in-main-thread (:blocking t)
    (cffi:with-foreign-objects ((w  :int)
                                (h  :int)
                                (pw :pointer)
                                (ph :pointer))
      (setf pw (cffi:mem-aptr w :int 0))
      (setf ph (cffi:mem-aptr h :int 0))
      (cffi:foreign-funcall "SDL_QueryTexture" :pointer texture :pointer (null-pointer) :pointer (null-pointer) :pointer pw :pointer ph)
      (list (cffi:convert-from-foreign (mem-aref w :int) :int)
            (cffi:convert-from-foreign (mem-aref h :int) :int)))))



;;; SDL_mixer

;; Declared in SDL_mixer.h

;; Enumeration in C, but here separate constants.
(defconstant +MIX-INIT-FLAC+ #x1)
(defconstant +MIX-INIT-MOD+  #x2)
(defconstant +MIX-INIT-MP3+  #x8)
(defconstant +MIX-INIT-OGG+  #x10)
(defconstant +MIX-INIT-MID+  #x20)
(defconstant +MIX-INIT-OPUS+ #x40)
(defconstant +MIX-DEFAULT-FREQUENCY+ 22050)

#+little-endian
(defconstant +MIX-DEFAULT-FORMAT+ +AUDIO-S16LSB+)

#+big-endian
(defconstant +MIX-DEFAULT-FORMAT+ +AUDIO-S16SB+)

(defcfun "Mix_Init" :int
  (flags :int))

(defcfun "Mix_Quit" :void)

(defcfun "Mix_OpenAudio" :int
  (frequency :int)
  (format    :uint16)
  (channels  :int)
  (chunksize :int))

(defcfun "Mix_CloseAudio" :void)

(defcfun "Mix_LoadWAV_RW" :pointer
  (src     :pointer)
  (freesrc :int))

(defcfun "Mix_LoadMUS" :pointer
  (file (:string :encoding :utf-8)))

(defcfun "Mix_LoadMUS_RW" :pointer
  (src     :pointer)
  (freesrc :int))

(defcfun "Mix_PlayMusic" :int
  (music :pointer)
  (loops :int))

(defcfun "Mix_PlayChannelTimed" :int
  (channel :int)
  (chunk   :pointer)
  (loops   :int)
  (ticks   :int))

(defcfun "Mix_FreeChunk" :void
  (chunk :pointer))

(defcfun "Mix_FreeMusic" :void
  (music :pointer))

(defcfun "Mix_Volume" :int
  (channel :int)
  (volume  :int))

(defcfun "Mix_VolumeMusic" :int
  (volume  :int))

(defmacro with-mix ((&rest flags) &body body)
  `(progn
     (unless (mix-init ,@flags)
       (error "Mix Init failed"))
     (unwind-protect
          ,@body
       (mix-quit))))

(defmacro with-audio ((&rest flags) &body body)
  `(progn
     (unless (mix-openaudio ,@flags)
       (error "Audio Init failed"))
     (unwind-protect
          ,@body
       (mix-closeaudio))))


;;; OpenGL

(defcenum sdl-glattr
  "OpenGL configuration attributes"
  (:SDL-GL-RED-SIZE 0)
  (:SDL-GL-GREEN-SIZE)
  (:SDL-GL-BLUE-SIZE)
  (:SDL-GL-ALPHA-SIZE)
  (:SDL-GL-BUFFER-SIZE)
  (:SDL-GL-DOUBLEBUFFER)
  (:SDL-GL-DEPTH-SIZE)
  (:SDL-GL-STENCIL-SIZE)
  (:SDL-GL-ACCUM-RED-SIZE)
  (:SDL-GL-ACCUM-GREEN-SIZE)
  (:SDL-GL-ACCUM-BLUE-SIZE)
  (:SDL-GL-ACCUM-ALPHA-SIZE)
  (:SDL-GL-STEREO)
  (:SDL-GL-MULTISAMPLEBUFFERS)
  (:SDL-GL-MULTISAMPLESAMPLES)
  (:SDL-GL-ACCELERATED-VISUAL)
  (:SDL-GL-RETAINED-BACKING)
  (:SDL-GL-CONTEXT-MAJOR-VERSION)
  (:SDL-GL-CONTEXT-MINOR-VERSION)
  (:SDL-GL-CONTEXT-EGL)
  (:SDL-GL-CONTEXT-FLAGS)
  (:SDL-GL-CONTEXT-PROFILE-MASK)
  (:SDL-GL-SHARE-WITH-CURRENT-CONTEXT)
  (:SDL-GL-FRAMEBUFFER_SRGB_CAPABLE)
  (:SDL-GL-CONTEXT-RELEASE-BEHAVIOR)
  (:SDL-GL-CONTEXT-RESET-NOTIFICATION)
  (:SDL-GL-CONTEXT-NO-ERROR))

(defcenum sdl-glprofile
  "Profile mask"
  (:SDL-GL-CONTEXT-PROFILE-CORE          #x01)
  (:SDL-GL-CONTEXT-PROFILE-COMPATIBILITY #x02)
  (:SDL-GL-CONTEXT-PROFILE-ES            #x04))

(defcfun "SDL_GL_SetAttribute" :int
  (attr  sdl-glattr)
  (value :int))

(defcfun "SDL_GL_SwapWindow" :void
  (window :pointer))

;; Helpers

(defmacro with-window ((&rest args) &body body)
  "Anaphoric macro that introduces an anaphor WINDOW to the forms in BODY."
  `(with-foreign-objects ((window :pointer))
     (unwind-protect
          (progn
            (setf window (sdl-createwindow ,@args))
            (unless window
              (error "WITH-WINDOW failed to create a window."))
            ,@body)
       (sdl-destroywindow window))))

(defmacro with-surface ((name) &body body)
  "Anaphoric macro that introduces the named anaphor to the forms in BODY."
  `(with-foreign-objects ((,name :pointer))
     (unwind-protect
          (progn
            ,@body)
       (sdl-freesurface ,name))))

(defmacro with-texture-from-img ((name filename) &body body)
  "Anaphoric macro that loads and introduces a texture from FILENAME named
according to NAME to the forms in BODY."
  (alexandria:with-gensyms (img)
    `(with-foreign-objects ((,name :pointer))
       (unwind-protect
            (progn
              (with-surface (,img)
                (setf ,img (img-load ,filename))
                (setf ,name (sdl-createtexturefromsurface
                             renderer ;; Captured from the calling environment
                             ,img)))
              ,@body)
         (sdl-destroytexture ,name)))))
