;; Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
;;
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0
;; (http://opensource.org/licenses/eclipse-1.0.php) which can be
;; found in the file epl-v10.html at the root of this distribution.
;;
;; By using this software in any fashion, you are agreeing to be bound
;; by the terms of this license.
;;
;; You must not remove this notice, or any other, from this software.

(ns dunaj.host.int
  "Fast unchecked primitive `int` operations and constants
  for performance critical loops.

  Offers workarounds around implicit `int` to `long` conversions
  and autoboxing.

  NOTE: This is a somewhat controversial experiment, but I've found
  it very practical."
  {:authors ["Jozef Wagner"]
   :categories ["Primary" "Comparison" "Operations"
                "Bitwise" "Numbers" "ASCII"]}
  (:api bare)
  (:require [clojure.core :refer [. fn]]
            [clojure.bootstrap :refer [deftype defmacro defn v1]]
            [dunaj.type :refer [Any]]
            [dunaj.boolean :refer [Boolean]]))


;;;; Public API

(deftype Int
  "Host Integer type."
  {:added v1
   :see '[dunaj.math/Integer]
   :category "Primary"}
  java.lang.Integer)

;;; aliases for common integer numbers

(defmacro i-1
  "Primitive int constant of value -1."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/IM1)

(defmacro i0
  "Primitive int constant of value 0."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I00)

(defmacro i1
  "Primitive int constant of value 1."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I01)

(defmacro i2
  "Primitive int constant of value 2."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I02)

(defmacro i3
  "Primitive int constant of value 3."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I03)

(defmacro i4
  "Primitive int constant of value 4."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I04)

(defmacro i5
  "Primitive int constant of value 5."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I05)

(defmacro i6
  "Primitive int constant of value 6."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I06)

(defmacro i7
  "Primitive int constant of value 7."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I07)

(defmacro i8
  "Primitive int constant of value 8."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I08)

(defmacro i9
  "Primitive int constant of value 9."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I09)

(defmacro i10
  "Primitive int constant of value 10."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I10)

(defmacro i11
  "Primitive int constant of value 11."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I11)

(defmacro i12
  "Primitive int constant of value 12."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I12)

(defmacro i13
  "Primitive int constant of value 13."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I13)

(defmacro i14
  "Primitive int constant of value 14."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I14)

(defmacro i15
  "Primitive int constant of value 15."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I15)

(defmacro i16
  "Primitive int constant of value 16."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I16)

(defmacro i17
  "Primitive int constant of value 17."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I17)

(defmacro i18
  "Primitive int constant of value 18."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I18)

(defmacro i19
  "Primitive int constant of value 19."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I19)

(defmacro i20
  "Primitive int constant of value 20."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I20)

(defmacro i21
  "Primitive int constant of value 21."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I21)

(defmacro i22
  "Primitive int constant of value 22."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I22)

(defmacro i23
  "Primitive int constant of value 23."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I23)

(defmacro i24
  "Primitive int constant of value 24."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I24)

(defmacro i25
  "Primitive int constant of value 25."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I25)

(defmacro i26
  "Primitive int constant of value 26."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I26)

(defmacro i27
  "Primitive int constant of value 27."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I27)

(defmacro i28
  "Primitive int constant of value 28."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I28)

(defmacro i29
  "Primitive int constant of value 29."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I29)

(defmacro i30
  "Primitive int constant of value 30."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I30)

(defmacro i31
  "Primitive int constant of value 31."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I31)

(defmacro i32
  "Primitive int constant of value 32."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/I32)

(defmacro ixFF
  "Primitive int constant of value 255."
  {:added v1
   :category "Numbers"}
  []
  `dunaj.lang.Int/IFF)

;;; primitive int aliases for ascii codes

(defmacro iNUL
  "Primitive int constant of value 0x00."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/NUL)

(defmacro iSOH
  "Primitive int constant of value 0x01."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SOH)

(defmacro iSTX
  "Primitive int constant of value 0x02."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/STX)

(defmacro iETX
  "Primitive int constant of value 0x03."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ETX)

(defmacro iEOT
  "Primitive int constant of value 0x04."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/EOT)

(defmacro iENQ
  "Primitive int constant of value 0x05."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ENQ)

(defmacro iACK
  "Primitive int constant of value 0x06."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ACK)

(defmacro iBEL
  "Primitive int constant of value 0x07."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/BEL)

(defmacro iBS
  "Primitive int constant of value 0x08."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/BS)

(defmacro iHT
  "Primitive int constant of value 0x09."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/HT)

(defmacro iLF
  "Primitive int constant of value 0x0A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/LF)

(defmacro iVT
  "Primitive int constant of value 0x0B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/VT)

(defmacro iFF
  "Primitive int constant of value 0x0C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/IFF)

(defmacro iCR
  "Primitive int constant of value 0x0D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CR)

(defmacro iSO
  "Primitive int constant of value 0x0E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SO)

(defmacro iSI
  "Primitive int constant of value 0x0F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SI)

(defmacro iDLE
  "Primitive int constant of value 0x10."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DLE)

(defmacro iDC1
  "Primitive int constant of value 0x11."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DC1)

(defmacro iDC2
  "Primitive int constant of value 0x12."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DC2)

(defmacro iDC3
  "Primitive int constant of value 0x13."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DC3)

(defmacro iDC4
  "Primitive int constant of value 0x14."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DC4)

(defmacro iNAK
  "Primitive int constant of value 0x15."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/NAK)

(defmacro iSYN
  "Primitive int constant of value 0x16."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SYN)

(defmacro iETB
  "Primitive int constant of value 0x17."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ETB)

(defmacro iCAN
  "Primitive int constant of value 0x18."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAN)

(defmacro iEM
  "Primitive int constant of value 0x19."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/EM)

(defmacro iSUB
  "Primitive int constant of value 0x1A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SUB)

(defmacro iESC
  "Primitive int constant of value 0x1B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ESC)

(defmacro iFS
  "Primitive int constant of value 0x1C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/FS)

(defmacro iGS
  "Primitive int constant of value 0x1D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/GS)

(defmacro iRS
  "Primitive int constant of value 0x1E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/RS)

(defmacro iUS
  "Primitive int constant of value 0x1F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/US)


(defmacro iSPACE
  "Primitive int constant of value 0x20."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SPACE)

(defmacro iBANG
  "Primitive int constant of value 0x21."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/BANG)

(defmacro iQUOTE
  "Primitive int constant of value 0x22."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/QUOTE)

(defmacro iHASH
  "Primitive int constant of value 0x23."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/HASH)

(defmacro iDOLLAR
  "Primitive int constant of value 0x24."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DOLLAR)

(defmacro iPERCENT
  "Primitive int constant of value 0x25."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/PERCENT)

(defmacro iAMP
  "Primitive int constant of value 0x26."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/AMP)

(defmacro iAPOS
  "Primitive int constant of value 0x27."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/APOS)

(defmacro iLPAR
  "Primitive int constant of value 0x28."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/LPAR)

(defmacro iRPAR
  "Primitive int constant of value 0x29."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/RPAR)

(defmacro iSTAR
  "Primitive int constant of value 0x2A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/STAR)

(defmacro iPLUS
  "Primitive int constant of value 0x2B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/PLUS)

(defmacro iCOMMA
  "Primitive int constant of value 0x2C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/COMMA)

(defmacro iMINUS
  "Primitive int constant of value 0x2D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/MINUS)

(defmacro iDOT
  "Primitive int constant of value 0x2E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DOT)

(defmacro iSLASH
  "Primitive int constant of value 0x2F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SLASH)

(defmacro iZERO
  "Primitive int constant of value 0x30."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ZERO)

(defmacro iONE
  "Primitive int constant of value 0x31."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ONE)

(defmacro iTWO
  "Primitive int constant of value 0x32."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/TWO)

(defmacro iTHREE
  "Primitive int constant of value 0x33."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/THREE)

(defmacro iFOUR
  "Primitive int constant of value 0x34."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/FOUR)

(defmacro iFIVE
  "Primitive int constant of value 0x35."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/FIVE)

(defmacro iSIX
  "Primitive int constant of value 0x36."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SIX)

(defmacro iSEVEN
  "Primitive int constant of value 0x37."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SEVEN)

(defmacro iEIGHT
  "Primitive int constant of value 0x38."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/EIGHT)

(defmacro iNINE
  "Primitive int constant of value 0x39."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/NINE)

(defmacro iCOLON
  "Primitive int constant of value 0x3A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/COLON)

(defmacro iSEMICOLON
  "Primitive int constant of value 0x3B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SEMICOLON)

(defmacro iLT
  "Primitive int constant of value 0x3C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/LT)

(defmacro iEQ
  "Primitive int constant of value 0x3D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/EQ)

(defmacro iGT
  "Primitive int constant of value 0x3E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/GT)

(defmacro iQMARK
  "Primitive int constant of value 0x3F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/QMARK)

(defmacro iAT
  "Primitive int constant of value 0x40."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/AT)

(defmacro iCAPITAL_A
  "Primitive int constant of value 0x41."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_A)

(defmacro iCAPITAL_B
  "Primitive int constant of value 0x42."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_B)

(defmacro iCAPITAL_C
  "Primitive int constant of value 0x43."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_C)

(defmacro iCAPITAL_D
  "Primitive int constant of value 0x44."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_D)

(defmacro iCAPITAL_E
  "Primitive int constant of value 0x45."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_E)

(defmacro iCAPITAL_F
  "Primitive int constant of value 0x46."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_F)

(defmacro iCAPITAL_G
  "Primitive int constant of value 0x47."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_G)

(defmacro iCAPITAL_H
  "Primitive int constant of value 0x48."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_H)

(defmacro iCAPITAL_I
  "Primitive int constant of value 0x49."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_I)

(defmacro iCAPITAL_J
  "Primitive int constant of value 0x4A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_J)

(defmacro iCAPITAL_K
  "Primitive int constant of value 0x4B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_K)

(defmacro iCAPITAL_L
  "Primitive int constant of value 0x4C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_L)

(defmacro iCAPITAL_M
  "Primitive int constant of value 0x4D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_M)

(defmacro iCAPITAL_N
  "Primitive int constant of value 0x4E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_N)

(defmacro iCAPITAL_O
  "Primitive int constant of value 0x4F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_O)

(defmacro iCAPITAL_P
  "Primitive int constant of value 0x50."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_P)

(defmacro iCAPITAL_Q
  "Primitive int constant of value 0x51."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_Q)

(defmacro iCAPITAL_R
  "Primitive int constant of value 0x52."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_R)

(defmacro iCAPITAL_S
  "Primitive int constant of value 0x53."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_S)

(defmacro iCAPITAL_T
  "Primitive int constant of value 0x54."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_T)

(defmacro iCAPITAL_U
  "Primitive int constant of value 0x55."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_U)

(defmacro iCAPITAL_V
  "Primitive int constant of value 0x56."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_V)

(defmacro iCAPITAL_W
  "Primitive int constant of value 0x57."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_W)

(defmacro iCAPITAL_X
  "Primitive int constant of value 0x58."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_X)

(defmacro iCAPITAL_Y
  "Primitive int constant of value 0x59."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_Y)

(defmacro iCAPITAL_Z
  "Primitive int constant of value 0x5A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/CAPITAL_Z)

(defmacro iLBRACKET
  "Primitive int constant of value 0x5B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/LBRACKET)

(defmacro iBACKSLASH
  "Primitive int constant of value 0x5C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/BACKSLASH)

(defmacro iRBRACKET
  "Primitive int constant of value 0x5D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/RBRACKET)

(defmacro iARROWHEAD
  "Primitive int constant of value 0x5E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/ARROWHEAD)

(defmacro iUNDERSCORE
  "Primitive int constant of value 0x5F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/UNDERSCORE)

(defmacro iBACKQUOTE
  "Primitive int constant of value 0x60."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/BACKQUOTE)

(defmacro iSMALL_A
  "Primitive int constant of value 0x61."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_A)

(defmacro iSMALL_B
  "Primitive int constant of value 0x62."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_B)

(defmacro iSMALL_C
  "Primitive int constant of value 0x63."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_C)

(defmacro iSMALL_D
  "Primitive int constant of value 0x64."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_D)

(defmacro iSMALL_E
  "Primitive int constant of value 0x65."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_E)

(defmacro iSMALL_F
  "Primitive int constant of value 0x66."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_F)

(defmacro iSMALL_G
  "Primitive int constant of value 0x67."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_G)

(defmacro iSMALL_H
  "Primitive int constant of value 0x68."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_H)

(defmacro iSMALL_I
  "Primitive int constant of value 0x69."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_I)

(defmacro iSMALL_J
  "Primitive int constant of value 0x6A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_J)

(defmacro iSMALL_K
  "Primitive int constant of value 0x6B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_K)

(defmacro iSMALL_L
  "Primitive int constant of value 0x6C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_L)

(defmacro iSMALL_M
  "Primitive int constant of value 0x6D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_M)

(defmacro iSMALL_N
  "Primitive int constant of value 0x6E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_N)

(defmacro iSMALL_O
  "Primitive int constant of value 0x6F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_O)

(defmacro iSMALL_P
  "Primitive int constant of value 0x70."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_P)

(defmacro iSMALL_Q
  "Primitive int constant of value 0x71."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_Q)

(defmacro iSMALL_R
  "Primitive int constant of value 0x72."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_R)

(defmacro iSMALL_S
  "Primitive int constant of value 0x73."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_S)

(defmacro iSMALL_T
  "Primitive int constant of value 0x74."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_T)

(defmacro iSMALL_U
  "Primitive int constant of value 0x75."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_U)

(defmacro iSMALL_V
  "Primitive int constant of value 0x76."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_V)

(defmacro iSMALL_W
  "Primitive int constant of value 0x77."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_W)

(defmacro iSMALL_X
  "Primitive int constant of value 0x78."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_X)

(defmacro iSMALL_Y
  "Primitive int constant of value 0x79."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_Y)

(defmacro iSMALL_Z
  "Primitive int constant of value 0x7A."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/SMALL_Z)

(defmacro iLBRACE
  "Primitive int constant of value 0x7B."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/LBRACE)

(defmacro iVBAR
  "Primitive int constant of value 0x7C."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/VBAR)

(defmacro iRBRACE
  "Primitive int constant of value 0x7D."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/RBRACE)

(defmacro iTILDE
  "Primitive int constant of value 0x7E."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/TILDE)

(defmacro iDEL
  "Primitive int constant of value 0x7F."
  {:added v1
   :category "ASCII"}
  []
  `dunaj.lang.Int/DEL)

;;; operations

(defmacro iloop
  "Similar to `dunaj.flow/loop`, but does not implicitly convert
  primitive numbers."
  {:added v1
   :see '[dunaj.flow/loop]
   :category "Primary"
   :highlight :flow
   :indent 1
   :let-bindings true}
  [bindings & exprs]
  `(clojure.bootstrap/iloop ~bindings ~@exprs))

(defn iint :- Int
  "Returns int coercion of `_x_`. Subject to rounding or truncation."
  {:added v1
   :category "Primary"
   :inline (fn [x] `(clojure.lang.RT/uncheckedIntCast ~x))}
  [x :- Any]
  (clojure.lang.RT/uncheckedIntCast x))

(defn iadd :- Int
  "Returns the sum of `_x_` and `_y_`, both primitive ints.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/add]
   :category "Operations"
   :inline (fn [x y] `(clojure.lang.Numbers/unchecked_int_add ~x ~y))}
  [x :- Int, y :- Int]
  (clojure.lang.Numbers/unchecked_int_add x y))

(defn ineg :- Int
  "Returns the negation of `_x_`, an int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/neg]
   :category "Operations"
   :inline (fn [x] `(clojure.lang.Numbers/unchecked_int_negate ~x))}
  [x :- Int]
  (clojure.lang.Numbers/unchecked_int_negate x))

(defn isub :- Int
  "Returns the difference of `_x_` and `_y_`, both int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/subtract]
   :category "Operations"
   :inline (fn [x y]
             `(clojure.lang.Numbers/unchecked_int_subtract ~x ~y))}
  [x :- Int, y :- Int]
  (clojure.lang.Numbers/unchecked_int_subtract x y))

(defn imul :- Int
  "Returns the product of `_x_` and `_y_`, both int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/multiply]
   :category "Operations"
   :inline (fn [x y]
             `(clojure.lang.Numbers/unchecked_int_multiply ~x ~y))}
  [x :- Int, y :- Int]
  (clojure.lang.Numbers/unchecked_int_multiply x y))

(defn idiv :- Int
  "Returns the division of `_x_` by `_y_`, both int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/divide]
   :category "Operations"
   :inline (fn [x y]
             `(clojure.lang.Numbers/unchecked_int_divide ~x ~y))}
  [x :- Int, y :- Int]
  (clojure.lang.Numbers/unchecked_int_divide x y))

(defn irem :- Int
  "Returns the remainder of division of `_x_` by `_y_`, both int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/rem]
   :category "Operations"
   :inline (fn [x y]
             `(clojure.lang.Numbers/unchecked_int_remainder ~x ~y))}
  [x :- Int, y :- Int]
  (clojure.lang.Numbers/unchecked_int_remainder x y))

(defn imin :- Int
  "Returns the least of integers `_x_` and `_y_`."
  {:added v1
   :see '[dunaj.math/min]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/min ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/min x y))

(defn imax :- Int
  "Returns the greatest of integers `_x_` and `_y_`."
  {:added v1
   :see '[imax0 dunaj.math/max]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/max ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/max x y))

(defn imax0 :- Int
  "Returns the greatest of integers 0 and `_x_`."
  {:added v1
   :see '[imax]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/max (i0) ~x))}
  [x :- Int]
  (dunaj.lang.Int/max (i0) x))

(defn icompare :- Int
  "Returns a -1, 0, or +1 when `_x_` is 'less than', 'equal to',
  or 'greater than' `_y_`."
  {:added v1
   :see '[dunaj.compare/compare]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/compare ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/compare x y))

(defn i<< :- Int
  "Bitwise shift left (<<) for ints."
  {:added v1
   :see '[dunaj.bit/<<]
   :category "Bitwise"
   :inline (fn [x y] `(dunaj.lang.Int/shiftLeft ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/shiftLeft x y))

(defn i>> :- Int
  "Bitwise shift right (>>) for ints."
  {:added v1
   :see '[dunaj.bit/>>]
   :category "Bitwise"
   :inline (fn [x y] `(dunaj.lang.Int/shiftRight ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/shiftRight x y))

(defn i>>> :- Int
  "Bitwise unsigned shift right (>>>) for ints."
  {:added v1
   :see '[dunaj.bit/>>>]
   :category "Bitwise"
   :inline (fn [x y] `(dunaj.lang.Int/unsignedShiftRight ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/unsignedShiftRight x y))

(defn iabs :- Int
  "Returns absolute value of integer `_x_`."
  {:added v1
   :see '[dunaj.math/abs]
   :category "Operations"
   :inline (fn [x] `(dunaj.lang.Int/abs ~x))}
  [x :- Int]
  (dunaj.lang.Int/abs x))

(defn iinc :- Int
  "Returns a number one greater than `_x_`, an int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/inc]
   :category "Operations"
   :inline (fn [x] `(clojure.lang.Numbers/unchecked_int_inc ~x))}
  [x :- Int]
  (clojure.lang.Numbers/unchecked_int_inc x))

(defn idec :- Int
  "Returns a number one greater than `_x_`, an int.
  Uses a primitive operator subject to overflow."
  {:added v1
   :see '[dunaj.math/dec]
   :category "Operations"
   :inline (fn [x] `(clojure.lang.Numbers/unchecked_int_dec ~x))}
  [x :- Int]
  (clojure.lang.Numbers/unchecked_int_dec x))

(defn idigit->int :- Int
  "Returns a number represented by given unicode digit `_codepoint_`."
  {:added v1
   :see '[ihexa->int]
   :category "Primary"
   :inline (fn [x] `(dunaj.lang.Int/digitToInt ~x))}
  [codepoint :- Int]
  (dunaj.lang.Int/digitToInt codepoint))

(defn ihexa->int :- Int
  "Returns a number represented by given hexadecimal
  unicode `_codepoint_`."
  {:added v1
   :see '[idigit->int]
   :category "Primary"
   :inline (fn [x] `(dunaj.lang.Int/hexaToInt ~x))}
  [codepoint :- Int]
  (dunaj.lang.Int/hexaToInt codepoint))

(defn inot :- Int
  "Returns a bitwise NOT on a integer `_x_`."
  {:added v1
   :see '[dunaj.bit/not]
   :category "Bitwise"
   :inline (fn [x] `(dunaj.lang.Int/not ~x))}
  [x :- Int]
  (dunaj.lang.Int/not x))

(defn iand :- Int
  "Returns a bitwise AND on integers `_x_` and `_y_`."
  {:added v1
   :see '[dunaj.bit/and]
   :category "Bitwise"
   :inline (fn [x y] `(dunaj.lang.Int/and ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/and x y))

(defn ior :- Int
  "Returns a bitwise OR on integers `_x_` and `_y_`."
  {:added v1
   :see '[dunaj.bit/or]
   :category "Bitwise"
   :inline (fn [x y] `(dunaj.lang.Int/or ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/or x y))

(defn ixor :- Int
  "Returns a bitwise XOR on integers `_x_` and `_y_`."
  {:added v1
   :see '[dunaj.bit/xor]
   :category "Bitwise"
   :inline (fn [x y] `(dunaj.lang.Int/xor ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/xor x y))

(defn i< :- Boolean
  "Returns `true` if `_x_` is less than `_y_`, otherwise returns
  `false`."
  {:added v1
   :see '[dunaj.math/<]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/lt ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/lt ~x ~y))

(defn i<= :- Boolean
  "Returns `true` if `_x_` is less than or equal to `_y_`,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/<=]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/lte ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/lte ~x ~y))

(defn i> :- Boolean
  "Returns `true` if `_x_` is greater than `_y_`, otherwise returns
  `false`."
  {:added v1
   :see '[dunaj.math/>]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/gt ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/gt ~x ~y))

(defn i>= :- Boolean
  "Returns `true` if `_x_` is greater than or equal to `_y_`,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/>=]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/gte ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/gte ~x ~y))

(defn i== :- Boolean
  "Returns `true` if `_x_` is equal to `_y_`,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/==]
   :category "Comparison"
   :inline (fn [x y] `(dunaj.lang.Int/eq ~x ~y))}
  [x :- Int, y :- Int]
  (dunaj.lang.Int/eq ~x ~y))

(defn izero? :- Boolean
  "Returns `true` if `_x_` is 0, otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/zero?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/zerop ~x))}
  [x :- Int]
  (dunaj.lang.Int/zerop ~x))

(defn ione? :- Boolean
  "Returns `true` if `_x_` is 1, otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/one?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/onep ~x))}
  [x :- Int]
  (dunaj.lang.Int/onep ~x))

(defn ipos? :- Boolean
  "Returns `true` if `_x_` is greater than 0,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/pos?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/pos ~x))}
  [x :- Int]
  (dunaj.lang.Int/pos ~x))

(defn inpos? :- Boolean
  "Returns `true` if `_x_` is less than 1,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/npos?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/npos ~x))}
  [x :- Int]
  (dunaj.lang.Int/npos ~x))

(defn ineg? :- Boolean
  "Returns `true` if `_x_` is less than 0,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/neg?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/neg ~x))}
  [x :- Int]
  (dunaj.lang.Int/neg ~x))

(defn inneg? :- Boolean
  "Returns `true` if `_x_` is greater than -1,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/nneg?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/nneg ~x))}
  [x :- Int]
  (dunaj.lang.Int/nneg ~x))

(defn ieven? :- Boolean
  "Returns `true` if `_x_` is an even integer,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/even?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/even ~x))}
  [x :- Int]
  (dunaj.lang.Int/even ~x))

(defn iodd? :- Boolean
  "Returns `true` if `_x_` is an odd integer,
  otherwise returns `false`."
  {:added v1
   :see '[dunaj.math/odd?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/odd ~x))}
  [x :- Int]
  (dunaj.lang.Int/odd ~x))

(defn ismall-letter? :- Boolean
  "Returns `true` of `_x_` is an unicode codepoint of a small latin
  letter [a-z], otherwise returns `false`."
  {:added v1
   :see '[icapital-letter?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/isSmallLetter ~x))}
  [x :- Int]
  (dunaj.lang.Int/isSmallLetter ~x))

(defn icapital-letter? :- Boolean
  "Returns `true` of `_x_` is an unicode codepoint of a capital latin
  letter [A-Z], otherwise returns `false`."
  {:added v1
   :see '[ismall-letter?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/isCapitalLetter ~x))}
  [x :- Int]
  (dunaj.lang.Int/isCapitalLetter ~x))

(defn idigit? :- Boolean
  "Returns `true` of `_x_` is an unicode codepoint of a decimal digit,
  otherwise returns `false`."
  {:added v1
   :see '[ioctal? ihexa?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/isDigit ~x))}
  [x :- Int]
  (dunaj.lang.Int/isDigit ~x))

(defn ioctal? :- Boolean
  "Returns `true` of `_x_` is an unicode codepoint of an octal digit,
  otherwise returns `false`."
  {:added v1
   :see '[idigit? ihexa?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/isOctal ~x))}
  [x :- Int]
  (dunaj.lang.Int/isOctal ~x))

(defn ihexa? :- Boolean
  "Returns `true` of `_x_` is an unicode codepoint of
  a hexadecimal digit, otherwise returns `false`."
  {:added v1
   :see '[ioctal? idigit?]
   :category "Comparison"
   :inline (fn [x] `(dunaj.lang.Int/isHexa ~x))}
  [x :- Int]
  (dunaj.lang.Int/isHexa ~x))


;;;; Testing

(clojure.core/require
 '[clojure.bootstrap :refer [assert-int assert-boolean]])
(clojure.core/require '[clojure.core :refer [if recur]])

(iloop [x (i10)]
  (assert-int x)
  (if (izero? x) nil (recur (idec x))))

(assert-int
 (i0)
 (imin (i0) (i1))
 (imin 1 2)
 (idigit->int (iint \3))
 (iSMALL_A))

(assert-boolean
 (ipos? 4)
 (ihexa? (iint \A)))
