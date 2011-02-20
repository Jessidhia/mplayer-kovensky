/*
 * video output through mga_vid kernel driver
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "mp_msg.h"
#include "video_out.h"
#include "video_out_internal.h"

#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <linux/fb.h>

#include "drivers/mga_vid.h"
#include "sub/sub.h"
#include "aspect.h"

static const vo_info_t info =
{
	"Matrox G200/G4x0/G550 overlay (/dev/mga_vid)",
	"mga",
	"A'rpi",
	"Based on some code by Aaron Holtzman <aholtzma@ess.engr.uvic.ca>"
};

const LIBVO_EXTERN(mga)

#include "mga_template.c"

#define FBDEV	"/dev/fb0"

static int config(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{

//	if (f >= 0) mga_uninit();
	if(!vo_screenwidth || !vo_screenheight) {
		int fd;
		struct fb_var_screeninfo fbinfo;

		if(-1 != (fd = open(FBDEV, O_RDONLY))) {
			if(0 == ioctl(fd, FBIOGET_VSCREENINFO, &fbinfo)) {
				if(!vo_screenwidth)   vo_screenwidth = fbinfo.xres;
				if(!vo_screenheight) vo_screenheight = fbinfo.yres;
			} else {
				perror("FBIOGET_VSCREENINFO");
			}
			close(fd);
		} else {
			perror(FBDEV);
		}
	}

	if(vo_screenwidth && vo_screenheight){
		aspect_save_orig(width,height);
		aspect_save_prescale(d_width,d_height);
		aspect_save_screenres(vo_screenwidth,vo_screenheight);

		if(flags&VOFLAG_FULLSCREEN) { /* -fs */
			aspect(&d_width,&d_height,A_ZOOM);
			vo_fs = VO_TRUE;
		} else {
			aspect(&d_width,&d_height,A_NOZOOM);
			vo_fs = VO_FALSE;
		}
		mp_tmsg(MSGT_VO,MSGL_INFO, "[VO_MGA] aspect(): resized to %dx%d.\n",d_width,d_height);
	}

	vo_dwidth=d_width; vo_dheight=d_height;
	mga_vid_config.dest_width = d_width;
	mga_vid_config.dest_height= d_height;
	mga_vid_config.x_org= 0; // (720-mga_vid_config.dest_width)/2;
	mga_vid_config.y_org= 0; // (576-mga_vid_config.dest_height)/2;
	if(vo_screenwidth && vo_screenheight){
		mga_vid_config.x_org=(vo_screenwidth-d_width)/2;
		mga_vid_config.y_org=(vo_screenheight-d_height)/2;
	}

    return mga_init(width,height,format);
}

static void uninit(void)
{
    mp_tmsg(MSGT_VO,MSGL_INFO, "[VO] uninit!\n");
    mga_uninit();
}

static void flip_page(void)
{
    vo_mga_flip_page();
}


static void check_events(void)
{
}
