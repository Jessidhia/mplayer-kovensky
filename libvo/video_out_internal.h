/*
 * Copyright (C) Aaron Holtzman - Aug 1999
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

#ifndef MPLAYER_VIDEO_OUT_INTERNAL_H
#define MPLAYER_VIDEO_OUT_INTERNAL_H

#include <stdint.h>

/* All video drivers will want this */
#include "libmpcodecs/vfcap.h"
#include "libmpcodecs/mp_image.h"
#include "geometry.h"
#include "old_vo_wrapper.h"
#include "old_vo_defines.h"

static int control(uint32_t request, void *data);
static int config(uint32_t width, uint32_t height, uint32_t d_width,
		     uint32_t d_height, uint32_t fullscreen, char *title,
		     uint32_t format);
static int draw_frame(uint8_t *src[]);
static int draw_slice(uint8_t *image[], int stride[], int w,int h,int x,int y);
static void draw_osd(void);
static void flip_page(void);
static void check_events(void);
static void uninit(void);
static int preinit(const char *);

#define LIBVO_EXTERN(x) struct vo_driver video_out_##x =\
{\
    .is_new = 0,\
    .info = &info,\
    .preinit = old_vo_preinit,\
    .config = old_vo_config,\
    .control = old_vo_control,\
    .draw_slice = old_vo_draw_slice,\
    .draw_osd = old_vo_draw_osd,\
    .flip_page = old_vo_flip_page,\
    .check_events = old_vo_check_events,\
    .uninit = old_vo_uninit,\
    .old_functions = &(struct vo_old_functions){\
	preinit,\
	config,\
	control,\
	draw_frame,\
	draw_slice,\
     	draw_osd,\
	flip_page,\
	check_events,\
        uninit,\
    }\
};

#include "osd.h"

#endif /* MPLAYER_VIDEO_OUT_INTERNAL_H */
