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
(ql:quickload :cffi)        ; For using foreign libraries.
(ql:quickload :cffi-libffi) ; For passing structures by value to foreign functions.

(defpackage :octaspire-cl-sdl2
  (:use
   :common-lisp
   :cffi))

(in-package :octaspire-cl-sdl2)

(define-foreign-library libsdl2
    (:darwin (:or "libSDL2-2.0.0.dylib")))

(define-foreign-library libsdl2-image
    (:darwin (:or "libSDL2_image-2.0.0.dylib")))

(define-foreign-library libsdl2-ttf
    (:darwin (:or "libSDL2_ttf-2.0.0.dylib")))

(use-foreign-library libsdl2)
(use-foreign-library libsdl2-image)
(use-foreign-library libsdl2-ttf)

(defcfun "SDL_Init" :int
  (flags :long))

;; Declared in include/SDL.h
(defconstant +SDL-INIT-TIMER+    #x001)
(defconstant +SDL-INIT-AUDIO+    #x010)
(defconstant +SDL-INIT-VIDEO+    #x020)
(defconstant +SDL-INIT-JOYSTICK+ #x200)
(defconstant +SDL-TEXTEDITINGEVENT-TEXT-SIZE+ 32)
(defconstant +SDL-TEXTINPUTEVENT-TEXT-SIZE+   32)

;; Declared in include/SDL_rect.h
(defcstruct sdl-rect
  "SDL Rectangle structure."
  (x :int)
  (y :int)
  (w :int)
  (h :int))

;; Declared in include/SDL_pixels.h
(defcstruct sdl-color
  "SDL Color structure."
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defun sdl-ext-color (red green blue alpha)
  (with-foreign-object (color '(:struct sdl-color))
    (with-foreign-slots ((r g b a) color (:struct sdl-color))
      (setf r red)
      (setf g green)
      (setf b blue)
      (setf a alpha)
      color)
    color))

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
(defcfun "SDL_CreateWindow" :pointer
  (title (:string :encoding :utf-8))
  (x :int)
  (y :int)
  (w :int)
  (flags :uint32)
  (h :int))

;; Declared in include/SDL_surface.h
(defcfun "SDL_UpperBlit" :int
  (src     :pointer)
  (srcrect :pointer)
  (dst     :pointer)
  (dstrect :pointer))

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

(defcfun "TTF_RenderUTF8_Solid" :pointer
  (font :pointer)
  (text (:string :encoding :utf-8))
  (fg (:pointer (:struct sdl-color))))





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





(defparameter *running* t)

(defun handle-event (event)
  (format t "~A~%" (format-sdl-event event))
  (with-foreign-slots ((type) event (:union sdl-event))
    (when (eq type :SDL-KEYDOWN)
      (setf *running* nil))))

(sb-int:with-float-traps-masked (:invalid :inexact :overflow) ; Prevent crash in macOS Big Sur
  (with-init (+SDL-INIT-VIDEO+)
    (with-img (+IMG-INIT-PNG+)
      (with-ttf
          (with-window ("octaspire" 10 10 400 400 0)
            (with-foreign-objects ((winsurface :pointer)
                                   (txtsurface :pointer)
                                   (image :pointer)
                                   (font :pointer)
                                   (textcolor '(:struct sdl-color))
                                   (nullpointer :pointer))
              (setf winsurface (sdl-getwindowsurface window))
              (setf image (img-load "../../assets/texture/texture.png"))
              (setf font (ttf-openfont "../../assets/font/IBM/Plex/IBMPlexMono-Bold.ttf" 20))
              (setf textcolor (sdl-ext-color #xFF #xFF #xFF #xFF))
              (with-foreign-string (text "Just some text here!")
                (setf txtsurface (ttf-renderutf8-solid font text textcolor)))
              (setf nullpointer (null-pointer))
              (unless image
                (error "Image is NULL"))
              (loop while *running*
                    do
                       (with-foreign-object (event '(:union sdl-event))
                         (loop while (/= (sdl-pollevent event) 0)
                               do
                                  (handle-event event)))
                       (sdl-upperblit image nullpointer winsurface nullpointer)
                       (sdl-upperblit txtsurface nullpointer winsurface nullpointer)
                       (sdl-updatewindowsurface window)
                       (sdl-delay 100))))))))
