/*
 * MatrixView video output driver for MPlayer
 * 
 * by Pigeon <pigeon at pigeond.net>
 *
 * Based on MatrixView the screensaver from http://rss-glx.sf.net/
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "config.h"
#include "mp_msg.h"
#include "subopt-helper.h"
#include "video_out.h"
#include "video_out_internal.h"
#include "gl_common.h"
#include "aspect.h"
#include "libswscale/swscale.h"
#include "libmpcodecs/vf_scale.h"
#include "osdep/timer.h"

#include "matrixview.h"

static vo_info_t info = 
{
    "MatrixView (OpenGL)",
    "matrixview",
    "Pigeon <pigeon@pigeond.net>",
    "Based on MatrixView from rss-glx.sf.net"
};

LIBVO_EXTERN(matrixview)

#ifdef GL_WIN32
static int gl_vinfo = 0;
static HGLRC gl_context = 0;
#define update_xinerama_info w32_update_xinerama_info
#define vo_init vo_w32_init
#define vo_window vo_w32_window
#else
static XVisualInfo *gl_vinfo = NULL;
static GLXContext gl_context = 0;
static int wsGLXAttrib[] = {
    GLX_RGBA,
    GLX_RED_SIZE,1,
    GLX_GREEN_SIZE,1,
    GLX_BLUE_SIZE,1,
    GLX_DEPTH_SIZE,1,
    GLX_DOUBLEBUFFER,
    None
};
#endif

static int int_pause = 0;
static int eq_contrast = 0.0;
static int eq_brightness = 0.0;
static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_format;
static struct SwsContext *sws = NULL;

static uint8_t *map_image[3] = { NULL, NULL, NULL };
static int map_stride[3] = { 0, 0, 0 };

#define MATRIX_ROWS 90
#define MATRIX_COLS 120
static int matrix_rows = MATRIX_ROWS;
static int matrix_cols = MATRIX_COLS;

#define DEFAULT_CONTRAST        0.90f
#define CONTRAST_MULTIPLIER     0.02f

#define DEFAULT_BRIGHTNESS      1.0f
#define BRIGHTNESS_MULTIPLIER   0.02f


static void
contrast_set(int value)
{
    float contrast;
    eq_contrast = value;
    contrast = value * CONTRAST_MULTIPLIER + DEFAULT_CONTRAST;
    if (contrast >= 0) {
        matrixview_contrast_set(contrast);
    }
}


static void
brightness_set(int value)
{
    float brightness;
    eq_brightness = value;
    brightness = value * BRIGHTNESS_MULTIPLIER + DEFAULT_BRIGHTNESS;
    if (brightness >= 0) {
        matrixview_brightness_set(brightness);
    }
}


static int 
config(uint32_t width, uint32_t height,
        uint32_t d_width, uint32_t d_height,
        uint32_t flags, char *title, uint32_t format)
{
    image_height = height;
    image_width = width;
    image_format = format;

    int_pause = 0;

    panscan_init();
    aspect_save_orig(width,height);
    aspect_save_prescale(d_width,d_height);
    update_xinerama_info();

    aspect((int *) &d_width, (int *) &d_height,A_NOZOOM);
    vo_dx = (int)(vo_screenwidth - d_width) / 2;
    vo_dy = (int)(vo_screenheight - d_height) / 2;
    geometry((int *) &vo_dx, (int *) &vo_dy,
            (int *) &d_width, (int *) &d_height,
            vo_screenwidth, vo_screenheight);
    vo_dx += xinerama_x;
    vo_dy += xinerama_y;

    vo_dwidth = d_width;
    vo_dheight = d_height;
#ifdef GL_WIN32
  if (!vo_w32_config(d_width, d_height, flags))
    return -1;
#else
    if (WinID >= 0) {
        vo_window = WinID ? (Window) WinID : mRootWin;
        vo_x11_selectinput_witherr(mDisplay, vo_window,
                StructureNotifyMask | KeyPressMask | PointerMotionMask |
                ButtonPressMask | ButtonReleaseMask | ExposureMask);
        goto glconfig;
    }

    if (vo_window == None) {
        unsigned int fg, bg;
        XSizeHints hint;
        XVisualInfo *vinfo;
        XEvent xev;
        
        vo_fs = VO_FALSE;
        
        hint.x = vo_dx;
        hint.y = vo_dy;
        hint.width = d_width;
        hint.height = d_height;
        hint.flags = PPosition | PSize;
        
        /* Get some colors */
        bg = WhitePixel(mDisplay, mScreen);
        fg = BlackPixel(mDisplay, mScreen);

        /* Make the window */

        vinfo=glXChooseVisual( mDisplay,mScreen,wsGLXAttrib );
        if (vinfo == NULL)
        {
            mp_msg(MSGT_VO, MSGL_ERR, "[matrixview] no GLX support present\n");
            return -1;
        }


        vo_window = vo_x11_create_smooth_window(mDisplay, mRootWin, vinfo->visual, hint.x, hint.y, hint.width, hint.height, vinfo->depth, XCreateColormap(mDisplay, mRootWin, vinfo->visual, AllocNone));

        vo_x11_classhint(mDisplay,vo_window,"matrixview");
        vo_hidecursor(mDisplay,vo_window);

        XSelectInput(mDisplay, vo_window, StructureNotifyMask);
        /* Tell other applications about this window */
        XSetStandardProperties(mDisplay, vo_window, title, title, None, NULL, 0, &hint);
        /* Map window. */
        XMapWindow(mDisplay, vo_window);
        
        /* Wait for map. */
        do 
        {
              XNextEvent(mDisplay, &xev);
        }
        while (xev.type != MapNotify || xev.xmap.event != vo_window);
        
        XSelectInput(mDisplay, vo_window, NoEventMask);

        XSync(mDisplay, False);

        vo_x11_selectinput_witherr(mDisplay, vo_window, StructureNotifyMask | KeyPressMask | PointerMotionMask | ButtonPressMask | ButtonReleaseMask | ExposureMask);
    }

    if (vo_ontop) vo_x11_setlayer(mDisplay, vo_window, vo_ontop);

    if (vo_config_count == 0) {
        vo_x11_nofs_sizepos(vo_dx, vo_dy, d_width, d_height);
        if (vo_fs ^ (flags & VOFLAG_FULLSCREEN))
            vo_x11_fullscreen();
    }
#endif /* GL_WIN32 */
glconfig:
    setGlWindow(&gl_vinfo, &gl_context, vo_window);

    if(sws) {
        sws_freeContext(sws);
    }

    sws = sws_getContextFromCmdLine(image_width, image_height, image_format, matrix_cols, matrix_rows, IMGFMT_Y8);
    if (!sws) {
        mp_msg(MSGT_VO, MSGL_ERR, "[matrixview] Cannot create SwsContext context\n");
        return -1;
    }

    if(!map_image[0]) {
          map_image[0] = malloc(matrix_cols * matrix_rows);
    }

    map_stride[0] = matrix_cols;

    matrixview_init(vo_dwidth, vo_dheight);
    matrixview_matrix_resize(matrix_cols, matrix_rows);

    contrast_set(eq_contrast);
    brightness_set(eq_brightness);
#ifdef GL_WIN32
    /* On win32 starts with black screen */
    matrixview_reshape(vo_dwidth, vo_dheight);
#endif
    return 0;
}


static void check_events(void)
{
    int e=vo_check_events();
    if(e & VO_EVENT_RESIZE) {
        matrixview_reshape(vo_dwidth, vo_dheight);
    }
    if(e & VO_EVENT_EXPOSE && int_pause) flip_page();
}


static void draw_osd(void)
{
    return;
}


static void do_matrixview()
{
    matrixview_draw(vo_dwidth, vo_dheight, GetTimer(), 0.0, map_image[0]);
}


static void
flip_page(void)
{
    do_matrixview();
    swapGlBuffers();
}



static int draw_slice(uint8_t *src[], int stride[],
        int w, int h, int x, int y)
{
    sws_scale_ordered(sws, src, stride, y, h, map_image, map_stride);
    return 0;
}


static int
draw_frame(uint8_t *src[])
{
    return 0;

#if 0
    int stride[3] = { 0, 0, 0 };

    switch(image_format) {
        case IMGFMT_YV12:
            stride[0] = image_width * 12 / 8;
            break;
        case IMGFMT_BGR32:
        case IMGFMT_RGB32:
            stride[0] = image_width * 4;
            break;
        case IMGFMT_BGR24:
        case IMGFMT_RGB24:
            stride[0] = image_width * 3;
            break;
        case IMGFMT_BGR16:
        case IMGFMT_BGR15:
            stride[0] = image_width * 2;
            break;
    }

    sws_scale_ordered(sws, src, stride, 0, image_height,
        map_image, map_stride);

    return 0;
#endif
}


static int
query_format(uint32_t format)
{
    int caps = VFCAP_CSP_SUPPORTED | VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN |
        VFCAP_ACCEPT_STRIDE;

    switch(format) {
        case IMGFMT_YV12:
        case IMGFMT_BGR32:
        case IMGFMT_BGR24:
        case IMGFMT_BGR16:
        case IMGFMT_BGR15:
        case IMGFMT_RGB32:
        case IMGFMT_RGB24:
        case IMGFMT_ARGB:
            return caps;
        default:
            break;
    }

    return 0;
}


static void
uninit(void)
{
    if (!vo_config_count) return;
    releaseGlContext(&gl_vinfo, &gl_context);
    vo_uninit();
    free(map_image[0]);
    map_image[0] = NULL;
    sws_freeContext(sws);
    sws = NULL;
}


static opt_t subopts[] =
{
    { "rows",       OPT_ARG_INT, &matrix_rows, (opt_test_f) int_non_neg, 0 },
    { "cols",       OPT_ARG_INT, &matrix_cols, (opt_test_f) int_non_neg, 0 },
    { NULL, 0, NULL, NULL, 0 },
};


static int preinit(const char *arg)
{
    if(!vo_init()) return -1;

    matrix_rows = MATRIX_ROWS;
    matrix_cols = MATRIX_COLS;

    if(subopt_parse(arg, subopts) != 0) {
        mp_msg(MSGT_VO, MSGL_FATAL,
                "\n-vo matrixview command line help:\n"
                "Example: mplayer -vo matrixview:cols=320:rows=240\n"
                "\n"
                "Options:\n"
                "\n"
                "  cols=<12-320>\n"
                "    Specify the number of columns of the matrix view, default %d\n"
                "\n"
                "  rows=<12-240>\n"
                "    Specify the number of rows of the matrix view, default %d\n"
                "\n"
                ,
                MATRIX_COLS, MATRIX_ROWS
              );
        return -1;
    }

    if(matrix_cols > 320)
    {
        matrix_cols = 320;
    }
    else if(matrix_cols < 12)
    {
        matrix_cols = 12;
    }

    if(matrix_rows > 240)
    {
        matrix_rows = 240;
    }
    else if(matrix_rows < 12)
    {
        matrix_rows = 12;
    }

    return 0;
}


static int control(uint32_t request, void *data, ...)
{
    switch (request) {
    case VOCTRL_PAUSE: return (int_pause=1);
    case VOCTRL_RESUME: return (int_pause=0);
    case VOCTRL_QUERY_FORMAT:
        return query_format(*((uint32_t*)data));
    case VOCTRL_ONTOP:
        vo_ontop();
        return VO_TRUE;
    case VOCTRL_FULLSCREEN:
        vo_fullscreen();
        matrixview_reshape(vo_dwidth, vo_dheight);
        return VO_TRUE;
#ifdef GL_WIN32
    case VOCTRL_BORDER:
        vo_w32_border();
        return VO_TRUE;
#endif
    case VOCTRL_GET_EQUALIZER:
        {
            va_list va;
            int *value;
            va_start(va, data);
            value = va_arg(va, int *);
            va_end(va);
            if (strcasecmp(data, "contrast") == 0)
            {
                *value = eq_contrast;
            }
            else if (strcasecmp(data, "brightness") == 0)
            {
                *value = eq_brightness;
            }
        }
        return VO_TRUE;
    case VOCTRL_SET_EQUALIZER:
        {
            va_list va;
            int value;
            va_start(va, data);
            value = va_arg(va, int);
            va_end(va);
            if (strcasecmp(data, "contrast") == 0)
            {
                contrast_set(value);
            }
            else if (strcasecmp(data, "brightness") == 0)
            {
                brightness_set(value);
            }
        }
        return VO_TRUE;
    }
    return VO_NOTIMPL;
}

