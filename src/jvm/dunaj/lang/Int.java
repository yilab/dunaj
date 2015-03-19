/**
 *   Copyright (C) 2013, 2015, Jozef Wagner. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *   the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package dunaj.lang;

public class Int {

    // numbers

    public static final int IM1 = -1;
    public static final int I00 = 0;
    public static final int I01 = 1;
    public static final int I02 = 2;
    public static final int I03 = 3;
    public static final int I04 = 4;
    public static final int I05 = 5;
    public static final int I06 = 6;
    public static final int I07 = 7;
    public static final int I08 = 8;
    public static final int I09 = 9;
    public static final int I10 = 10;
    public static final int I11 = 11;
    public static final int I12 = 12;
    public static final int I13 = 13;
    public static final int I14 = 14;
    public static final int I15 = 15;
    public static final int I16 = 16;
    public static final int I17 = 17;
    public static final int I18 = 18;
    public static final int I19 = 19;
    public static final int I20 = 20;
    public static final int I21 = 21;
    public static final int I22 = 22;
    public static final int I23 = 23;
    public static final int I24 = 24;
    public static final int I25 = 25;
    public static final int I26 = 26;
    public static final int I27 = 27;
    public static final int I28 = 28;
    public static final int I29 = 29;
    public static final int I30 = 30;
    public static final int I31 = 31;
    public static final int I32 = 32;
    public static final int IFF = 0xFF;

    // control characters

    public static final int NUL = 0x00;
    public static final int SOH = 0x01;
    public static final int STX = 0x02;
    public static final int ETX = 0x03;
    public static final int EOT = 0x04;
    public static final int ENQ = 0x05;
    public static final int ACK = 0x06;
    public static final int BEL = 0x07;
    public static final int BS  = 0x08;
    public static final int HT  = 0x09;
    public static final int LF  = 0x0A;
    public static final int VT  = 0x0B;
    public static final int FF  = 0x0C;
    public static final int CR  = 0x0D;
    public static final int SO  = 0x0E;
    public static final int SI  = 0x0F;

    public static final int DLE = 0x10;
    public static final int DC1 = 0x11;
    public static final int DC2 = 0x12;
    public static final int DC3 = 0x13;
    public static final int DC4 = 0x14;
    public static final int NAK = 0x15;
    public static final int SYN = 0x16;
    public static final int ETB = 0x17;
    public static final int CAN = 0x18;
    public static final int EM  = 0x19;
    public static final int SUB = 0x1A;
    public static final int ESC = 0x1B;
    public static final int FS  = 0x1C;
    public static final int GS  = 0x1D;
    public static final int RS  = 0x1E;
    public static final int US  = 0x1F;

    // punctiation

    public static final int SPACE = 0x20;
    public static final int BANG = 0x21;
    public static final int QUOTE = 0x22;
    public static final int HASH = 0x23;
    public static final int DOLLAR = 0x24;
    public static final int PERCENT = 0x25;
    public static final int AMP = 0x26;
    public static final int APOS = 0x27;
    public static final int LPAR = 0x28;
    public static final int RPAR = 0x29;
    public static final int STAR = 0x2A;
    public static final int PLUS = 0x2B;
    public static final int COMMA = 0x2C;
    public static final int MINUS = 0x2D;
    public static final int DOT = 0x2E;
    public static final int SLASH = 0x2F;


    // digits

    public static final int ZERO = 0x30;
    public static final int ONE = 0x31;
    public static final int TWO = 0x32;
    public static final int THREE = 0x33;
    public static final int FOUR = 0x34;
    public static final int FIVE = 0x35;
    public static final int SIX = 0x36;
    public static final int SEVEN = 0x37;
    public static final int EIGHT = 0x38;
    public static final int NINE = 0x39;

    // punctuation

    public static final int COLON = 0x3A;
    public static final int SEMICOLON = 0x3B;
    public static final int LT = 0x3C;
    public static final int EQ = 0x3D;
    public static final int GT = 0x3E;
    public static final int QMARK = 0x3F;
    public static final int AT = 0x40;

    // big letters

    public static final int CAPITAL_A = 0x41;
    public static final int CAPITAL_B = 0x42;
    public static final int CAPITAL_C = 0x43;
    public static final int CAPITAL_D = 0x44;
    public static final int CAPITAL_E = 0x45;
    public static final int CAPITAL_F = 0x46;
    public static final int CAPITAL_G = 0x47;
    public static final int CAPITAL_H = 0x48;
    public static final int CAPITAL_I = 0x49;
    public static final int CAPITAL_J = 0x4A;
    public static final int CAPITAL_K = 0x4B;
    public static final int CAPITAL_L = 0x4C;
    public static final int CAPITAL_M = 0x4D;
    public static final int CAPITAL_N = 0x4E;
    public static final int CAPITAL_O = 0x4F;
    public static final int CAPITAL_P = 0x50;
    public static final int CAPITAL_Q = 0x51;
    public static final int CAPITAL_R = 0x52;
    public static final int CAPITAL_S = 0x53;
    public static final int CAPITAL_T = 0x54;
    public static final int CAPITAL_U = 0x55;
    public static final int CAPITAL_V = 0x56;
    public static final int CAPITAL_W = 0x57;
    public static final int CAPITAL_X = 0x58;
    public static final int CAPITAL_Y = 0x59;
    public static final int CAPITAL_Z = 0x5A;

    // punctuation

    public static final int LBRACKET = 0x5B;
    public static final int BACKSLASH = 0x5C;
    public static final int RBRACKET = 0x5D;
    public static final int ARROWHEAD = 0x5E;
    public static final int UNDERSCORE = 0x5F;
    public static final int BACKQUOTE = 0x60;

    // small letters

    public static final int SMALL_A = 0x61;
    public static final int SMALL_B = 0x62;
    public static final int SMALL_C = 0x63;
    public static final int SMALL_D = 0x64;
    public static final int SMALL_E = 0x65;
    public static final int SMALL_F = 0x66;
    public static final int SMALL_G = 0x67;
    public static final int SMALL_H = 0x68;
    public static final int SMALL_I = 0x69;
    public static final int SMALL_J = 0x6A;
    public static final int SMALL_K = 0x6B;
    public static final int SMALL_L = 0x6C;
    public static final int SMALL_M = 0x6D;
    public static final int SMALL_N = 0x6E;
    public static final int SMALL_O = 0x6F;
    public static final int SMALL_P = 0x70;
    public static final int SMALL_Q = 0x71;
    public static final int SMALL_R = 0x72;
    public static final int SMALL_S = 0x73;
    public static final int SMALL_T = 0x74;
    public static final int SMALL_U = 0x75;
    public static final int SMALL_V = 0x76;
    public static final int SMALL_W = 0x77;
    public static final int SMALL_X = 0x78;
    public static final int SMALL_Y = 0x79;
    public static final int SMALL_Z = 0x7A;

    // punctuation

    public static final int LBRACE = 0x7B;
    public static final int VBAR = 0x7C;
    public static final int RBRACE = 0x7D;
    public static final int TILDE = 0x7E;

    // control characters

    public static final int DEL = 0x7F;

    public static boolean isSmallLetter(int x) {
        return (x > 0x60) && (x < 0x7B);
    }

    public static boolean isCapitalLetter(int x) {
        return (x > 0x40) && (x < 0x5B);
    }

    public static boolean isDigit(int x) {
        return (x > 0x2F) && (x < 0x3A);
    }

    public static boolean isOctal(int x) {
        return (x > 0x2F) && (x < 0x38);
    }

    public static boolean isHexa(int x) {
        return isDigit(x)
               || ((x > 0x40) && (x < 0x47))
               || ((x > 0x60) && (x < 0x67));
    }

    public static int digitToInt(int x) {
        return x - 0x30;
    }

    public static int hexaToInt(int x) {
        if (x > 0x60) {
            return x - 0x61;
        } else if (x > 0x40) {
            return x - 0x41;
        } else {
            return x - 0x30;
        }
    }

    public static int not(int x) {
        return ~x;
    }

    public static int and(int x, int y) {
        return x & y;
    }

    public static int or(int x, int y) {
        return x | y;
    }

    public static int xor(int x, int y) {
        return x ^ y;
    }

    public static boolean lt(int x, int y) {
        return x < y;
    }

    public static boolean lte(int x, int y) {
        return x <= y;
    }

    public static boolean gt(int x, int y) {
        return x > y;
    }

    public static boolean gte(int x, int y) {
        return x >= y;
    }

    public static boolean eq(int x, int y) {
        return x == y;
    }

    public static boolean zerop(int x) {
        return x == 0;
    }

    public static boolean onep(int x) {
        return x == 1;
    }

    public static boolean pos(int x) {
        return x > 0;
    }

    public static boolean npos(int x) {
        return x < 1;
    }

    public static boolean neg(int x) {
        return x < 0;
    }

    public static boolean nneg(int x) {
        return x > -1;
    }

    public static boolean even(int x) {
        return (x & 1) == 0;
    }

    public static boolean odd(int x) {
        return (x & 1) == 1;
    }

    public static int min(int x, int y) {
        return Math.min(x, y);
    }

    public static int max(int x, int y) {
        return Math.max(x, y);
    }

    public static int abs(int x) {
        return Math.abs(x);
    }

    public static int compare(int x, int y) {
        if(x < y)
            return -1;
        else if(y < x)
            return 1;
        return 0;
    }

    public static int shiftLeft(int x, int y) {
        return x << y;
    }

    public static int shiftRight(int x, int y) {
        return x >> y;
    }

    public static int unsignedShiftRight(int x, int y) {
        return x >>> y;
    }

}
