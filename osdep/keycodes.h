/*
 * KEY code definitions for GyS-TermIO v2.0
 *
 * copyright (C) 1999 A'rpi/ESP-team
 *
 * This file is part of MPlayer.
 *
 * MPlayer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * MPlayer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with MPlayer; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef MPLAYER_KEYCODES_H
#define MPLAYER_KEYCODES_H

#define KEY_ENTER 13
#define KEY_TAB 9

#define KEY_BASE 0x100

/*  Function keys  */
#define KEY_F (KEY_BASE+64)

/* Control keys */
#define KEY_CTRL (KEY_BASE)
#define KEY_BACKSPACE (KEY_CTRL+0)
#define KEY_DELETE (KEY_CTRL+1)
#define KEY_INSERT (KEY_CTRL+2)
#define KEY_HOME (KEY_CTRL+3)
#define KEY_END (KEY_CTRL+4)
#define KEY_PAGE_UP (KEY_CTRL+5)
#define KEY_PAGE_DOWN (KEY_CTRL+6)
#define KEY_ESC (KEY_CTRL+7)

/* Control keys short name */
#define KEY_BS KEY_BACKSPACE
#define KEY_DEL KEY_DELETE
#define KEY_INS KEY_INSERT
#define KEY_PGUP KEY_PAGE_UP
#define KEY_PGDOWN KEY_PAGE_DOWN
#define KEY_PGDWN KEY_PAGE_DOWN

/* Cursor movement */
#define KEY_CRSR (KEY_BASE+16)
#define KEY_RIGHT (KEY_CRSR+0)
#define KEY_LEFT (KEY_CRSR+1)
#define KEY_DOWN (KEY_CRSR+2)
#define KEY_UP (KEY_CRSR+3)

/* Multimedia keyboard/remote keys */
#define KEY_MM_BASE (0x100+384)
#define KEY_POWER (KEY_MM_BASE+0)
#define KEY_MENU (KEY_MM_BASE+1)
#define KEY_PLAY (KEY_MM_BASE+2)
#define KEY_PAUSE (KEY_MM_BASE+3)
#define KEY_PLAYPAUSE (KEY_MM_BASE+4)
#define KEY_STOP (KEY_MM_BASE+5)
#define KEY_FORWARD (KEY_MM_BASE+6)
#define KEY_REWIND (KEY_MM_BASE+7)
#define KEY_NEXT (KEY_MM_BASE+8)
#define KEY_PREV (KEY_MM_BASE+9)
#define KEY_VOLUME_UP (KEY_MM_BASE+10)
#define KEY_VOLUME_DOWN (KEY_MM_BASE+11)
#define KEY_MUTE (KEY_MM_BASE+12)

/* Keypad keys */
#define KEY_KEYPAD (KEY_BASE+32)
#define KEY_KP0 (KEY_KEYPAD+0)
#define KEY_KP1 (KEY_KEYPAD+1)
#define KEY_KP2 (KEY_KEYPAD+2)
#define KEY_KP3 (KEY_KEYPAD+3)
#define KEY_KP4 (KEY_KEYPAD+4)
#define KEY_KP5 (KEY_KEYPAD+5)
#define KEY_KP6 (KEY_KEYPAD+6)
#define KEY_KP7 (KEY_KEYPAD+7)
#define KEY_KP8 (KEY_KEYPAD+8)
#define KEY_KP9 (KEY_KEYPAD+9)
#define KEY_KPDEC (KEY_KEYPAD+10)
#define KEY_KPINS (KEY_KEYPAD+11)
#define KEY_KPDEL (KEY_KEYPAD+12)
#define KEY_KPENTER (KEY_KEYPAD+13)

/* Special keys */
#define KEY_INTERN (0x1000)
#define KEY_CLOSE_WIN (KEY_INTERN+0)

/* Ctrl + Key */
#define CTRL_A      (0x01)
#define CTRL_B      (0x02)
#define CTRL_C      (0x03)
#define CTRL_D      (0x04)
#define CTRL_E      (0x05)
#define CTRL_F      (0x06)
#define CTRL_G      (0x07)
/* #define CTRL_H      (0x08) */ /* KEY_BACKSPACE */
/* #define CTRL_I      (0x09) */ /* KEY_TAB */
#define CTRL_J      (0x0a)
#define CTRL_K      (0x0b)
/* #define CTRL_M      (0x0c) */ /* KEY_ENTER */
#define CTRL_N      (0x0e)
#define CTRL_O      (0x0f)
#define CTRL_P      (0x10)
#define CTRL_Q      (0x11)
#define CTRL_L      (0x12)
#define CTRL_V      (0x16)
#define CTRL_W      (0x17)
#define CTRL_R      (0x18)
#define CTRL_S      (0x19)
#define CTRL_Z      (0x1a)
#define CTRL_SO     (0x1b)
#define CTRL_BSL    (0x1c)
#define CTRL_SC     (0x1d)
#define CTRL_T      (0x20)
#define CTRL_U      (0x21)
#define CTRL_Y      (0x25)

/* Modifiers added to individual keys */
#define KEY_MODIFIER_SHIFT  0x2000
#define KEY_MODIFIER_CTRL   0x4000
#define KEY_MODIFIER_ALT    0x8000
#define KEY_MODIFIER_META  0x10000

#endif /* MPLAYER_KEYCODES_H */
