/*
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

#ifndef MPLAYER_CFG_MPLAYER_DEF_H
#define MPLAYER_CFG_MPLAYER_DEF_H

static char* default_config=
"# Default options for Kovensky's MPlayer (http://kovensky.project357.com)\n"
"# Manual available at http://www.mplayerhq.hu/DOCS/man/en/mplayer.1.html\n"
"font=Arial\n\n"

"# vo=xv is the default on X11-based systems\n"
"# vo=directx is the default for pre-Vista OSes\n"
"# vo=gl:yuv=2 is the default for Vista+\n\n"

"# Allows taking screenshots with 's'\n"
"vf=screenshot\n\n"
;

#endif /* MPLAYER_CFG_MPLAYER_DEF_H */
