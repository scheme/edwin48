#| -*-Scheme-*-

$Id: key-w32.scm,v 1.8 2007/01/05 21:19:23 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Windows Keys
;;; Package: (edwin win32-keys)


(define (initialize-package!)
  (set! end (make-special-key 'END 0))
  unspecific)

(define end)

(define (win32-make-special-key keysym bucky-bits)
  (cond ((vk-code->name keysym)
	 => (lambda (name)
	      (make-special-key name bucky-bits)))
	(else #F)))

(define (vk-code->name vk-code)
  (vector-ref win32-key-translation-vector vk-code))

;; This constructs a vector mapping VK_* codes (integers 0..255) to
;; special key names (symbols).  It doesn not include keys that are
;; affected by the Win32 API TranslateMessage, which are: any printing
;; character, backspace, enter, escape, tab

(define win32-key-translation-vector
  (let ((v (make-vector 256 #f)))
    (for-each (lambda (def)
		(if (not (null? (cddr def)))
		    (vector-set! v (second def) (third def))))
	      '(;;VK_name          code    special-key name
		(VK_LBUTTON        #x01)
		(VK_RBUTTON        #x02)
		(VK_CANCEL         #x03)
		(VK_MBUTTON        #x04)

		(VK_BACK           #x08)
		(VK_TAB            #x09)

		(VK_CLEAR          #x0C)
		(VK_RETURN         #x0D)

		(VK_SHIFT          #x10)
		(VK_CONTROL        #x11)
		(VK_MENU           #x12)
		(VK_PAUSE          #x13    stop)
		(VK_CAPITAL        #x14)

		(VK_ESCAPE         #x1B)

		(VK_SPACE          #x20)
		(VK_PRIOR          #x21    prior)
		(VK_NEXT           #x22    next)
		(VK_END            #x23    end)
		(VK_HOME           #x24    home)
		(VK_LEFT           #x25    left)
		(VK_UP             #x26    up)
		(VK_RIGHT          #x27    right)
		(VK_DOWN           #x28    down)
		(VK_SELECT         #x29    select)
		(VK_PRINT          #x2A    print)
		(VK_EXECUTE        #x2B)
		(VK_SNAPSHOT       #x2C)
		(VK_INSERT         #x2D    insertchar)
		(VK_DELETE         #x2E    deletechar)
		(VK_HELP           #x2F)

		(VK_NUMPAD0        #x60)
		(VK_NUMPAD1        #x61)
		(VK_NUMPAD2        #x62)
		(VK_NUMPAD3        #x63)
		(VK_NUMPAD4        #x64)
		(VK_NUMPAD5        #x65)
		(VK_NUMPAD6        #x66)
		(VK_NUMPAD7        #x67)
		(VK_NUMPAD8        #x68)
		(VK_NUMPAD9        #x69)
		(VK_MULTIPLY       #x6A)
		(VK_ADD            #x6B)
		(VK_SEPARATOR      #x6C)
		(VK_SUBTRACT       #x6D)
		(VK_DECIMAL        #x6E)
		(VK_DIVIDE         #x6F)
		(VK_F1             #x70    f1)
		(VK_F2             #x71    f2)
		(VK_F3             #x72    f3)
		(VK_F4             #x73    f4)
		(VK_F5             #x74    f5)
		(VK_F6             #x75    f6)
		(VK_F7             #x76    f7)
		(VK_F8             #x77    f8)
		(VK_F9             #x78    f9)
		(VK_F10            #x79    f10)
		(VK_F11            #x7A    f11)
		(VK_F12            #x7B    f12)
		(VK_F13            #x7C    f13)
		(VK_F14            #x7D    f14)
		(VK_F15            #x7E    f15)
		(VK_F16            #x7F    f16)
		(VK_F17            #x80    f17)
		(VK_F18            #x81    f18)
		(VK_F19            #x82    f19)
		(VK_F20            #x83    f20)
		(VK_F21            #x84    f21)
		(VK_F22            #x85    f22)
		(VK_F23            #x86    f23)
		(VK_F24            #x87    f24)

		(VK_NUMLOCK        #x90)
		(VK_SCROLL         #x91)

		(VK_LSHIFT         #xA0)
		(VK_RSHIFT         #xA1)
		(VK_LCONTROL       #xA2)
		(VK_RCONTROL       #xA3)
		(VK_LMENU          #xA4)
		(VK_RMENU          #xA5)

		(VK_ATTN           #xF6)
		(VK_CRSEL          #xF7)
		(VK_EXSEL          #xF8)
		(VK_EREOF          #xF9)
		(VK_PLAY           #xFA)
		(VK_ZOOM           #xFB)
		(VK_NONAME         #xFC)
		(VK_PA1            #xFD)
		(VK_OEM_CLEAR      #xFE)))
    v))