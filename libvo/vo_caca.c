/*
 * video output driver for libcaca
 *
 * by Pigeon <pigeon@pigeond.net>
 *
 * Some functions/codes/ideas are from x11 and aalib vo
 *
 * TODO: support draw_alpha?
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
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include "config.h"
#include "video_out.h"
#include "video_out_internal.h"
#include "sub/sub.h"

#include "osdep/keycodes.h"
#include "mp_msg.h"
#include "mp_fifo.h"

#include <caca.h>
#ifdef CACA_API_VERSION_1
  /* Include the pre-1.x compatibility header.
   * Once libcaca 1.x is widespread, vo_caca should be fully
   * converted to the new API. A patch exists:
   * http://lists.mplayerhq.hu/pipermail/mplayer-dev-eng/2006-July/044669.html
   */
  #include <caca0.h>
#endif

static const vo_info_t info = {
  "libcaca",
  "caca",
  "Pigeon <pigeon@pigeond.net>",
  ""
};

const LIBVO_EXTERN (caca)

/* caca stuff */
static caca_canvas_t *canvas;
static caca_display_t *display;
static caca_dither_t *dither = NULL;
static const char *dither_antialias = "default";
static const char *dither_charset = "default";
static const char *dither_color = "default";
static const char *dither_algo = "none";

/* image infos */
static int image_format;
static int image_width;
static int image_height;

static int screen_w, screen_h;

/* We want 24bpp always for now */
static unsigned int bpp = 24;
static unsigned int depth = 3;
static unsigned int rmask = 0xff0000;
static unsigned int gmask = 0x00ff00;
static unsigned int bmask = 0x0000ff;
static unsigned int amask = 0;

#define MESSAGE_SIZE 512
#define MESSAGE_DURATION 5

static time_t stoposd = 0;
static int showosdmessage = 0;
static char osdmessagetext[MESSAGE_SIZE];
static char posbar[MESSAGE_SIZE];

static int osdx = 0, osdy = 0;
static int posbary = 2;

static void osdmessage(int duration, const char *fmt, ...)
{
    /*
     * for outputting a centered string at the bottom
     * of our window for a while
     */
    va_list ar;
    char m[MESSAGE_SIZE];

    va_start(ar, fmt);
    vsprintf(m, fmt, ar);
    va_end(ar);
    strcpy(osdmessagetext, m);

    showosdmessage = 1;
    stoposd = time(NULL) + duration;
    osdx = (screen_w - strlen (osdmessagetext)) / 2;
    posbar[0] = '\0';
}

static void osdpercent(int duration, int min, int max, int val, const char *desc, const char *unit)
{
    /*
     * prints a bar for setting values
     */
    float step;
    int where, i;

    step = (float)screen_w / (float)(max - min);
    where = (val - min) * step;
    osdmessage (duration, "%s: %i%s", desc, val, unit);
    posbar[0] = '|';
    posbar[screen_w - 1] = '|';

    for (i = 0; i < screen_w; i++)
    {
	if (i == where)
	    posbar[i] = '#';
	else
	    posbar[i] = '-';
    }

    if (where != 0)
	posbar[0] = '|';

    if (where != (screen_w - 1))
	posbar[screen_w - 1] = '|';

    posbar[screen_w] = '\0';
}

static int resize(void)
{
    screen_w = caca_get_canvas_width(canvas);
    screen_h = caca_get_canvas_height(canvas);

    if (dither)
	caca_free_dither(dither);

    dither = caca_create_dither(bpp, image_width, image_height,
				depth * image_width, rmask, gmask, bmask,
				amask);

    /* Default libcaca features */
    caca_set_dither_antialias(dither, dither_antialias);
    caca_set_dither_charset(dither, dither_charset);
    caca_set_dither_color(dither, dither_color);
    caca_set_dither_algorithm(dither, dither_algo);

    if (!dither)
	mp_msg(MSGT_VO, MSGL_FATAL, "vo_caca: caca_create_dither failed!\n");
    return 0;
}

static int config(uint32_t width, uint32_t height, uint32_t d_width,
	uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
    image_height = height;
    image_width = width;
    image_format = format;

    showosdmessage = 0;
    posbar[0] = '\0';

    return resize ();
}

static int draw_frame(uint8_t *src[])
{
    caca_dither_bitmap(canvas, 0, 0, screen_w, screen_h, dither, src[0]);
    return 0;
}

static int draw_slice(uint8_t *src[], int stride[], int w, int h, int x, int y)
{
    return 0;
}

static void flip_page(void)
{

    if (showosdmessage)
    {
	if (time(NULL) >= stoposd)
	{
	    showosdmessage = 0;
	    if (*posbar)
		posbar[0] = '\0';
	} else {
	    caca_put_str(canvas, osdx, osdy, osdmessagetext);
	    if (*posbar)
		caca_put_str(canvas, 0, posbary, posbar);
	}
    }
    caca_refresh_display(display);
}

static void set_next_str(const char * const *list, const char ** str,
                         const char ** msg) {
    int ind;
    for(ind = 0; list[ind]; ind+=2) {
	if(strcmp(list[ind], *str) == 0)
	{
	    if(list[ind+2] == NULL)
		ind = -2;
	    *str = list[ind+2];
	    *msg = list[ind+3];
	    return;
	}
    }

    *str = list[0];
    *msg = list[1];
}

static void check_events (void)
{
    caca_event_t cev;
    if (!caca_get_event(display, CACA_EVENT_ANY, &cev, 1))
        return;

    switch (cev.type)
    {
        case CACA_EVENT_RESIZE:
            caca_refresh_display(display);
            resize();
            break;

        case CACA_EVENT_KEY_RELEASE:
        {
            int key = cev.data.key.ch;
            const char *msg_name;

            switch (key)
            {
                case 'e':
                case 'E':
                    /* Toggle dithering algorithm */
                    set_next_str(caca_get_dither_algorithm_list(dither), &dither_algo, &msg_name);
                    caca_set_dither_algorithm(dither, dither_algo);
                    osdmessage(MESSAGE_DURATION, "Using %s", msg_name);
                    break;

                case 'a':
                case 'A':
                    /* Toggle antialiasing method */
                    set_next_str(caca_get_dither_antialias_list(dither), &dither_antialias, &msg_name);
                    caca_set_dither_antialias(dither, dither_antialias);
                    osdmessage(MESSAGE_DURATION, "Using %s", msg_name);
                    break;

                case 'h':
                case 'H':
                    /* Toggle charset method */
                    set_next_str(caca_get_dither_charset_list(dither), &dither_charset, &msg_name);
                    caca_set_dither_charset(dither, dither_charset);
                    osdmessage(MESSAGE_DURATION, "Using %s", msg_name);
                    break;

                case 'c':
                case 'C':
                    /* Toggle color method */
                    set_next_str(caca_get_dither_color_list(dither), &dither_color, &msg_name);
                    caca_set_dither_color(dither, dither_color);
                    osdmessage(MESSAGE_DURATION, "Using %s", msg_name);
                    break;

                case CACA_KEY_UP:
                    mplayer_put_key(KEY_UP);
                    break;
                case CACA_KEY_DOWN:
                    mplayer_put_key(KEY_DOWN);
                    break;
                case CACA_KEY_LEFT:
                    mplayer_put_key(KEY_LEFT);
                    break;
                case CACA_KEY_RIGHT:
                    mplayer_put_key(KEY_RIGHT);
                    break;
                case CACA_KEY_ESCAPE:
                    mplayer_put_key(KEY_ESC);
                    break;
                case CACA_KEY_PAGEUP:
                    mplayer_put_key(KEY_PAGE_UP);
                    break;
                case CACA_KEY_PAGEDOWN:
                    mplayer_put_key(KEY_PAGE_DOWN);
                    break;
                case CACA_KEY_RETURN:
                    mplayer_put_key(KEY_ENTER);
                    break;
                case CACA_KEY_HOME:
                    mplayer_put_key(KEY_HOME);
                    break;
                case CACA_KEY_END:
                    mplayer_put_key(KEY_END);
                    break;
                default:
                    if (key <= 255)
                        mplayer_put_key (key);
                    break;
	        }
	    }
    }
}

static void uninit(void)
{
    caca_free_dither(dither);
    dither = NULL;
    caca_free_display(display);
    caca_free_canvas(canvas);
}


static void draw_osd(void)
{
    if (vo_osd_progbar_type != -1)
	osdpercent(MESSAGE_DURATION, 0, 255,
		  vo_osd_progbar_value, sub_osd_names[vo_osd_progbar_type],
		  "");
}

static int preinit(const char *arg)
{
    const char *caca_driver = NULL;
    if (arg)
    {
        char const * const *list = caca_get_display_driver_list();
        int i;
        for (i=0; list[i]; i+=2)
        {
            if (strcmp(arg, "list"))
            {
                if (strcmp(arg, list[i])) continue;
                caca_driver = arg;
            }
            else
            mp_msg(MSGT_VO, MSGL_INFO, "vo_caca: %s\t%s\n", list[i], list[i+1]);
        }
        if (!strcmp(arg, "list")) return ENOSYS;
        if (!caca_driver)
        {
            mp_msg(MSGT_VO, MSGL_ERR, "vo_caca: invalid subdevice, use -vo caca:list to display available drivers\n");
            return ENOSYS;
        }
    }

    if ((canvas = caca_create_canvas(0, 0)) == NULL)
    {
	mp_msg(MSGT_VO, MSGL_ERR, "vo_caca: failed to create canvas\n");
	return ENOSYS;
    }

    if (caca_driver)
    {
        if ((display = caca_create_display_with_driver(canvas, caca_driver)) == NULL)
        {
            mp_msg(MSGT_VO, MSGL_ERR, "vo_caca: failed to create display\n");
            return ENOSYS;
        }
    }
    else
    if ((display = caca_create_display(canvas)) == NULL)
    {
	mp_msg(MSGT_VO, MSGL_ERR, "vo_caca: failed to create display\n");
	return ENOSYS;
    }

    caca_set_display_title(display, "MPlayer");

    return 0;
}

static int query_format(uint32_t format)
{
    if (format == IMGFMT_BGR24)
      return VFCAP_OSD | VFCAP_CSP_SUPPORTED;

    return 0;
}

static int control(uint32_t request, void *data)
{
    switch(request)
    {
    case VOCTRL_QUERY_FORMAT:
      return query_format(*((uint32_t *)data));
    default:
      return VO_NOTIMPL;
    }
}
