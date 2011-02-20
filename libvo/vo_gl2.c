/*
 * X11/OpenGL interface
 * based on video_out_x11 by Aaron Holtzman,
 * and WS opengl window manager by Pontscho/Fresh!
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
#include "subopt-helper.h"
#include "video_out.h"
#include "video_out_internal.h"
#include "sub/sub.h"

#include "gl_common.h"
#include "aspect.h"

#undef TEXTUREFORMAT_ALWAYS
#ifdef __APPLE__
#define TEXTUREFORMAT_ALWAYS GL_RGBA8
#endif

//! force texture height, useful for debugging
#define TEXTURE_HEIGHT 128
#undef TEXTURE_HEIGHT
//! force texture width, useful for debugging
#define TEXTURE_WIDTH 128
#undef TEXTURE_WIDTH

static const vo_info_t info =
{
  "X11 (OpenGL) - multiple textures version",
  "gl2",
  "Arpad Gereoffy & Sven Goethel",
  ""
};

const LIBVO_EXTERN(gl2)

/* local data */
static unsigned char *ImageData=NULL;

static MPGLContext glctx;

static uint32_t image_width;
static uint32_t image_height;
static uint32_t image_format;
static uint32_t image_bpp;
static uint32_t image_bytes;

static int int_pause;

static uint32_t texture_width;
static uint32_t texture_height;
static int texnumx, texnumy, raw_line_len;
static int texdirty;
static struct TexSquare * texgrid = NULL;
static GLuint   fragprog;
static GLuint   lookupTex;
static GLint    gl_internal_format;
static int      rgb_sz, r_sz, g_sz, b_sz, a_sz;
static GLenum   gl_bitmap_format;
static GLenum   gl_bitmap_type;
static int      isGL12 = GL_FALSE;

static int      gl_bilinear=1;
static int      gl_antialias=0;
static int      use_yuv;
static int      is_yuv;
static int      use_glFinish;

static void (*draw_alpha_fnc)
                 (int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride);


/* The squares that are tiled to make up the game screen polygon */

struct TexSquare
{
  GLubyte *texture;
  GLuint texobj;
  GLuint uvtexobjs[2];
  GLfloat fx, fy, fw, fh;
};

static GLint getInternalFormat(void)
{
  switch (glctx.type) {
#ifdef CONFIG_GL_WIN32
  case GLTYPE_W32:
  {
  PIXELFORMATDESCRIPTOR pfd;
  HDC vo_hdc = vo_w32_get_dc(vo_w32_window);
  int pf = GetPixelFormat(vo_hdc);
  if (!DescribePixelFormat(vo_hdc, pf, sizeof pfd, &pfd)) {
    r_sz = g_sz = b_sz = a_sz = 0;
  } else {
    r_sz = pfd.cRedBits;
    g_sz = pfd.cGreenBits;
    b_sz = pfd.cBlueBits;
    a_sz = pfd.cAlphaBits;
  }
  vo_w32_release_dc(vo_w32_window, vo_hdc);
  }
  break;
#endif
#ifdef CONFIG_GL_X11
  case GLTYPE_X11:
  if (glXGetConfig(mDisplay, glctx.vinfo.x11, GLX_RED_SIZE, &r_sz) != 0) r_sz = 0;
  if (glXGetConfig(mDisplay, glctx.vinfo.x11, GLX_GREEN_SIZE, &g_sz) != 0) g_sz = 0;
  if (glXGetConfig(mDisplay, glctx.vinfo.x11, GLX_BLUE_SIZE, &b_sz) != 0) b_sz = 0;
  if (glXGetConfig(mDisplay, glctx.vinfo.x11, GLX_ALPHA_SIZE, &a_sz) != 0) a_sz = 0;
  break;
#endif
  }

  rgb_sz=r_sz+g_sz+b_sz;
  if(rgb_sz<=0) rgb_sz=24;

#ifdef TEXTUREFORMAT_ALWAYS
  return TEXTUREFORMAT_ALWAYS;
#else
  if(r_sz==3 && g_sz==3 && b_sz==2 && a_sz==0)
    return GL_R3_G3_B2;
  if(r_sz==4 && g_sz==4 && b_sz==4 && a_sz==0)
    return GL_RGB4;
  if(r_sz==5 && g_sz==5 && b_sz==5 && a_sz==0)
    return GL_RGB5;
  if(r_sz==8 && g_sz==8 && b_sz==8 && a_sz==0)
    return GL_RGB8;
  if(r_sz==10 && g_sz==10 && b_sz==10 && a_sz==0)
    return GL_RGB10;
  if(r_sz==2 && g_sz==2 && b_sz==2 && a_sz==2)
    return GL_RGBA2;
  if(r_sz==4 && g_sz==4 && b_sz==4 && a_sz==4)
    return GL_RGBA4;
  if(r_sz==5 && g_sz==5 && b_sz==5 && a_sz==1)
    return GL_RGB5_A1;
  if(r_sz==8 && g_sz==8 && b_sz==8 && a_sz==8)
    return GL_RGBA8;
  if(r_sz==10 && g_sz==10 && b_sz==10 && a_sz==2)
    return GL_RGB10_A2;
#endif
  return GL_RGB;
}

static int initTextures(void)
{
  struct TexSquare *tsq=0;
  GLfloat texpercx, texpercy;
  int s;
  int x=0, y=0;

  // textures smaller than 64x64 might not be supported
  s=64;
  while (s<image_width)
    s*=2;
  texture_width=s;

  s=64;
  while (s<image_height)
    s*=2;
  texture_height=s;

  if (!is_yuv)
    gl_internal_format = getInternalFormat();

  /* Test the max texture size */
  do {
    GLint w;
    glTexImage2D (GL_PROXY_TEXTURE_2D, 0,
                  gl_internal_format,
                  texture_width, texture_height,
                  0, gl_bitmap_format, gl_bitmap_type, NULL);

    glGetTexLevelParameteriv
      (GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &w);

    if (w >= texture_width)
      break;

    mp_msg (MSGT_VO, MSGL_V, "[gl2] Needed texture [%dx%d] too big, trying ",
            texture_width, texture_height);

    if (texture_width > texture_height)
      texture_width /= 2;
    else
      texture_height /= 2;

    mp_msg (MSGT_VO, MSGL_V, "[%dx%d] !\n", texture_width, texture_height);

    if(texture_width < 64 || texture_height < 64) {
      mp_msg (MSGT_VO, MSGL_FATAL, "[gl2] Give up .. usable texture size not available, or texture config error !\n");
      return -1;
    }
  } while (texture_width > 1 && texture_height > 1);
#ifdef TEXTURE_WIDTH
  texture_width = TEXTURE_WIDTH;
#endif
#ifdef TEXTURE_HEIGHT
  texture_height = TEXTURE_HEIGHT;
#endif

  texnumx = image_width / texture_width;
  if ((image_width % texture_width) > 0)
    texnumx++;

  texnumy = image_height / texture_height;
  if ((image_height % texture_height) > 0)
    texnumy++;

  mp_msg(MSGT_VO, MSGL_V, "[gl2] Creating %dx%d textures of size %dx%d ...\n",
         texnumx, texnumy, texture_width,texture_height);

  /* Allocate the texture memory */

  texpercx = (GLfloat) texture_width / (GLfloat) image_width;
  texpercy = (GLfloat) texture_height / (GLfloat) image_height;

  free(texgrid);
  texgrid = calloc (texnumx * texnumy, sizeof (struct TexSquare));

  raw_line_len = image_width * image_bytes;

  mp_msg (MSGT_VO, MSGL_DBG2, "[gl2] texture-usage %d*width=%d, %d*height=%d\n",
          (int) texnumx, (int) texture_width, (int) texnumy,
          (int) texture_height);

  tsq = texgrid;
  for (y = 0; y < texnumy; y++) {
    for (x = 0; x < texnumx; x++) {
      tsq->fx = x * texpercx;
      tsq->fy = y * texpercy;
      tsq->fw = texpercx;
      tsq->fh = texpercy;

      tsq->texobj=0;
      tsq->uvtexobjs[0] = tsq->uvtexobjs[1] = 0;

      glGenTextures (1, &(tsq->texobj));

      glBindTexture (GL_TEXTURE_2D, tsq->texobj);
      if (is_yuv) {
        glGenTextures(2, tsq->uvtexobjs);
        mpglActiveTexture(GL_TEXTURE1);
        glBindTexture (GL_TEXTURE_2D, tsq->uvtexobjs[0]);
        mpglActiveTexture(GL_TEXTURE2);
        glBindTexture (GL_TEXTURE_2D, tsq->uvtexobjs[1]);
        mpglActiveTexture(GL_TEXTURE0);
      }

      glCreateClearTex(GL_TEXTURE_2D, gl_internal_format, gl_bitmap_format,  gl_bitmap_type, GL_LINEAR,
                       texture_width, texture_height, 0);

      glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
      if (is_yuv) {
        int xs, ys;
        mp_get_chroma_shift(image_format, &xs, &ys);
        mpglActiveTexture(GL_TEXTURE1);
        glCreateClearTex(GL_TEXTURE_2D, gl_internal_format, gl_bitmap_format,  gl_bitmap_type, GL_LINEAR,
                         texture_width >> xs, texture_height >> ys, 128);
        mpglActiveTexture(GL_TEXTURE2);
        glCreateClearTex(GL_TEXTURE_2D, gl_internal_format, gl_bitmap_format,  gl_bitmap_type, GL_LINEAR,
                         texture_width >> xs, texture_height >> ys, 128);
        mpglActiveTexture(GL_TEXTURE0);
      }

      tsq++;
    }  /* for all texnumx */
  }  /* for all texnumy */

  return 0;
}

static void resetTexturePointers(unsigned char *imageSource)
{
  unsigned char *texdata_start, *line_start;
  struct TexSquare *tsq = texgrid;
  int x=0, y=0;

  line_start = (unsigned char *) imageSource;

  for (y = 0; y < texnumy; y++) {
    texdata_start = line_start;
    for (x = 0; x < texnumx; x++) {
      tsq->texture = texdata_start;
      texdata_start += texture_width * image_bytes;
      tsq++;
    }  /* for all texnumx */
    line_start += texture_height * raw_line_len;
  }  /* for all texnumy */
}

static void gl_set_bilinear (int val)
{
  int x, y;

  if(val>=0)
    gl_bilinear = val;
  else
    gl_bilinear++;

  gl_bilinear=gl_bilinear%2;
  /* no mipmap yet .. */

  for (y = 0; y < texnumy; y++) {
    for (x = 0; x < texnumx; x++) {
      glBindTexture (GL_TEXTURE_2D, texgrid[y * texnumx + x].texobj);

      switch (gl_bilinear) {
        case 0:
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
          mp_msg(MSGT_VO, MSGL_INFO, "[gl2] bilinear off\n");
          break;
        case 1:
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
          mp_msg(MSGT_VO, MSGL_INFO, "[gl2] bilinear linear\n");
          break;
        case 2:
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_NEAREST);
          mp_msg(MSGT_VO, MSGL_INFO, "[gl2] bilinear mipmap nearest\n");
          break;
        case 3:
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
          glTexParameteri (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_LINEAR);
          mp_msg(MSGT_VO, MSGL_INFO, "[gl2] bilinear mipmap linear\n");
          break;
      }
    }
  }
}

static void gl_set_antialias (int val)
{
  gl_antialias=val;

  if (gl_antialias) {
    glShadeModel (GL_SMOOTH);
    glEnable (GL_POLYGON_SMOOTH);
    glEnable (GL_LINE_SMOOTH);
    glEnable (GL_POINT_SMOOTH);
    mp_msg(MSGT_VO, MSGL_INFO, "[gl2] antialiasing on\n");
  } else {
    glShadeModel (GL_FLAT);
    glDisable (GL_POLYGON_SMOOTH);
    glDisable (GL_LINE_SMOOTH);
    glDisable (GL_POINT_SMOOTH);
    mp_msg(MSGT_VO, MSGL_INFO, "[gl2] antialiasing off\n");
  }
}


static void drawTextureDisplay (void)
{
  struct TexSquare *square = texgrid;
  int x, y;

  glColor3f(1.0,1.0,1.0);

  if (is_yuv)
    glEnableYUVConversion(GL_TEXTURE_2D, use_yuv);
  for (y = 0; y < texnumy; y++) {
    int thish = texture_height;
    if (y == texnumy - 1 && image_height % texture_height)
      thish = image_height % texture_height;
    for (x = 0; x < texnumx; x++) {
      int thisw = texture_width;
      if (x == texnumx - 1 && image_width % texture_width)
        thisw = image_width % texture_width;
      glBindTexture (GL_TEXTURE_2D, square->texobj);
      if (is_yuv) {
        mpglActiveTexture(GL_TEXTURE1);
        glBindTexture (GL_TEXTURE_2D, square->uvtexobjs[0]);
        mpglActiveTexture(GL_TEXTURE2);
        glBindTexture (GL_TEXTURE_2D, square->uvtexobjs[1]);
        mpglActiveTexture(GL_TEXTURE0);
      }

      if (texdirty) {
        glUploadTex(GL_TEXTURE_2D, gl_bitmap_format,  gl_bitmap_type,
                    square->texture, image_width * image_bytes,
                    0, 0, thisw, thish, 0);
      }

      glDrawTex(square->fx, square->fy, square->fw, square->fh,
                0, 0, texture_width, texture_height,
                texture_width, texture_height,
                0, is_yuv, 0);
      square++;
    } /* for all texnumx */
  } /* for all texnumy */
  if (is_yuv)
    glDisableYUVConversion(GL_TEXTURE_2D, use_yuv);
  texdirty = 0;
}


static void resize(int x,int y){
  mp_msg(MSGT_VO,MSGL_V,"[gl2] Resize: %dx%d\n",x,y);
  if(aspect_scaling()) {
    glClear(GL_COLOR_BUFFER_BIT);
    aspect(&x, &y, A_WINZOOM);
    panscan_calc_windowed();
    x += vo_panscan_x;
    y += vo_panscan_y;
    glViewport( (vo_dwidth-x)/2, (vo_dheight-y)/2, x, y);
  } else {
    //aspect(x, y, A_NOZOOM);
    if (WinID >= 0) {
      int left = 0, top = 0, w = x, h = y;
      geometry(&left, &top, &w, &h, vo_dwidth, vo_dheight);
      top = y - h - top;
      glViewport(left, top, w, h);
    } else
      glViewport( 0, 0, x, y );
  }

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, 1, 1, 0, -1.0, 1.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

static void draw_alpha_32(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb32(w,h,src,srca,stride,ImageData+4*(y0*image_width+x0),4*image_width);
}

static void draw_alpha_24(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb24(w,h,src,srca,stride,ImageData+3*(y0*image_width+x0),3*image_width);
}

static void draw_alpha_16(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb16(w,h,src,srca,stride,ImageData+2*(y0*image_width+x0),2*image_width);
}

static void draw_alpha_15(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
   vo_draw_alpha_rgb15(w,h,src,srca,stride,ImageData+2*(y0*image_width+x0),2*image_width);
}

static void draw_alpha_null(int x0,int y0, int w,int h, unsigned char* src, unsigned char *srca, int stride){
}

#ifdef CONFIG_GL_WIN32

static int config_w32(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format) {
  if (!vo_w32_config(d_width, d_height, flags))
    return -1;

  return 0;
}

#endif

#ifdef CONFIG_GL_X11
static int choose_glx_visual(Display *dpy, int scr, XVisualInfo *res_vi)
{
  XVisualInfo template, *vi_list;
  int vi_num, i, best_i, best_weight;

  template.screen = scr;
  vi_list = XGetVisualInfo(dpy, VisualScreenMask, &template, &vi_num);
  if (!vi_list) return -1;
  best_weight = 1000000; best_i=0;
  for (i = 0; i < vi_num; i++) {
    int val, res, w = 0;
    /* of course, the visual must support OpenGL rendering... */
    res = glXGetConfig(dpy, vi_list + i, GLX_USE_GL, &val);
    if (res || val == False) continue;
    /* also it must be doublebuffered ... */
    res = glXGetConfig(dpy, vi_list + i, GLX_DOUBLEBUFFER, &val);
    if (res || val == False) continue;
    /* furthermore it must be RGBA (not color indexed) ... */
    res = glXGetConfig(dpy, vi_list + i, GLX_RGBA, &val);
    if (res || val == False) continue;
    /* prefer less depth buffer size, */
    res = glXGetConfig(dpy, vi_list + i, GLX_DEPTH_SIZE, &val);
    if (res) continue;
    w += val*2;
    /* stencil buffer size */
    res = glXGetConfig(dpy, vi_list + i, GLX_STENCIL_SIZE, &val);
    if (res) continue;
    w += val*2;
    /* and colorbuffer alpha size */
    res = glXGetConfig(dpy, vi_list + i, GLX_ALPHA_SIZE, &val);
    if (res) continue;
    w += val;
    /* and finally, prefer DirectColor-ed visuals to allow color corrections */
    if (vi_list[i].class != DirectColor) w += 100;

    // avoid bad-looking visual with less that 8bit per color
    res = glXGetConfig(dpy, vi_list + i, GLX_RED_SIZE, &val);
    if (res) continue;
    if (val < 8) w += 50;
    res = glXGetConfig(dpy, vi_list + i, GLX_GREEN_SIZE, &val);
    if (res) continue;
    if (val < 8) w += 70;
    res = glXGetConfig(dpy, vi_list + i, GLX_BLUE_SIZE, &val);
    if (res) continue;
    if (val < 8) w += 50;

    if (w < best_weight) {
      best_weight = w;
      best_i = i;
    }
  }
  if (best_weight < 1000000) *res_vi = vi_list[best_i];
  XFree(vi_list);
  return (best_weight < 1000000) ? 0 : -1;
}

static int config_glx(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format) {
  XVisualInfo *vinfo, vinfo_buf;
    vinfo = choose_glx_visual(mDisplay,mScreen,&vinfo_buf) < 0 ? NULL : &vinfo_buf;
    if (vinfo == NULL) {
      mp_msg(MSGT_VO, MSGL_FATAL, "[gl2] no GLX support present\n");
      return -1;
    }

  vo_x11_create_vo_window(vinfo, vo_dx, vo_dy, d_width, d_height,
          flags, vo_x11_create_colormap(vinfo), "gl2", title);

  return 0;
}
#endif

static int initGl(uint32_t d_width, uint32_t d_height)
{
  fragprog = lookupTex = 0;
  if (initTextures() < 0)
    return -1;

  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glDisable(GL_CULL_FACE);
  glEnable (GL_TEXTURE_2D);
  if (is_yuv) {
    int xs, ys;
    gl_conversion_params_t params = {GL_TEXTURE_2D, use_yuv,
          {-1, -1, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0},
          texture_width, texture_height, 0, 0, 0};
    switch (use_yuv) {
      case YUV_CONVERSION_FRAGMENT_LOOKUP:
        glGenTextures(1, &lookupTex);
        mpglActiveTexture(GL_TEXTURE3);
        glBindTexture(GL_TEXTURE_2D, lookupTex);
        mpglActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, 0);
      case YUV_CONVERSION_FRAGMENT_POW:
      case YUV_CONVERSION_FRAGMENT:
        if (!mpglGenPrograms || !mpglBindProgram) {
          mp_msg(MSGT_VO, MSGL_ERR, "[gl] fragment program functions missing!\n");
          break;
        }
        mpglGenPrograms(1, &fragprog);
        mpglBindProgram(GL_FRAGMENT_PROGRAM, fragprog);
        break;
    }
    mp_get_chroma_shift(image_format, &xs, &ys);
    params.chrom_texw = params.texw >> xs;
    params.chrom_texh = params.texh >> ys;
    glSetupYUVConversion(&params);
  }

  gl_set_antialias(0);
  gl_set_bilinear(1);

  mp_msg(MSGT_VO, MSGL_V, "[gl2] Using image_bpp=%d, image_bytes=%d, \n\tgl_bitmap_format=%s, gl_bitmap_type=%s, \n\trgb_size=%d (%d,%d,%d), a_sz=%d, \n\tgl_internal_format=%s\n",
        image_bpp, image_bytes,
        glValName(gl_bitmap_format), glValName(gl_bitmap_type),
        rgb_sz, r_sz, g_sz, b_sz, a_sz, glValName(gl_internal_format));

  resize(d_width, d_height);

  glClearColor( 0.0f,0.0f,0.0f,0.0f );
  glClear( GL_COLOR_BUFFER_BIT );

  drawTextureDisplay ();

  return 0;
}

/* connect to server, create and map window,
 * allocate colors and (shared) memory
 */
static int
config(uint32_t width, uint32_t height, uint32_t d_width, uint32_t d_height, uint32_t flags, char *title, uint32_t format)
{
  int xs, ys;
  const unsigned char * glVersion;

  image_height = height;
  image_width = width;
  image_format = format;
  is_yuv = mp_get_chroma_shift(image_format, &xs, &ys) > 0;
  is_yuv |= (xs << 8) | (ys << 16);

#ifdef CONFIG_GL_WIN32
  if (config_w32(width, height, d_width, d_height, flags, title, format) == -1)
#endif
#ifdef CONFIG_GL_X11
  if (config_glx(width, height, d_width, d_height, flags, title, format) == -1)
#endif
    return -1;

  if (glctx.setGlWindow(&glctx) == SET_WINDOW_FAILED)
    return -1;

  glVersion = glGetString(GL_VERSION);

  mp_msg(MSGT_VO, MSGL_V, "[gl2] OpenGL Driver Information:\n");
  mp_msg(MSGT_VO, MSGL_V, "\tvendor: %s,\n\trenderer %s,\n\tversion %s\n",
         glGetString(GL_VENDOR), glGetString(GL_RENDERER), glVersion);

  if(glVersion[0]>'1' || (glVersion[0]=='1' && glVersion[2]>='2') )
    isGL12 = GL_TRUE;
  else
    isGL12 = GL_FALSE;

  if(isGL12) {
    mp_msg(MSGT_VO, MSGL_INFO, "[gl2] You have OpenGL >= 1.2 capable drivers, GOOD (16bpp and BGR is ok!)\n");
  } else {
    mp_msg(MSGT_VO, MSGL_INFO, "[gl2] You have OpenGL < 1.2 drivers, BAD (16bpp and BGR may be damaged!)\n");
  }

  glFindFormat(format, &image_bpp, &gl_internal_format, &gl_bitmap_format, &gl_bitmap_type);

  image_bytes=(image_bpp+7)/8;

  draw_alpha_fnc=draw_alpha_null;

  switch(image_bpp) {
    case 15:
      draw_alpha_fnc=draw_alpha_15; break;
    case 16:
      draw_alpha_fnc=draw_alpha_16; break;
    case 24:
      draw_alpha_fnc=draw_alpha_24; break;
    case 32:
      draw_alpha_fnc=draw_alpha_32; break;
  }

  if (initGl(vo_dwidth, vo_dheight) == -1)
    return -1;

  return 0;
}

#ifdef CONFIG_GL_X11
static int gl_handlekey(int key)
{
  if(key=='a'||key=='A') {
    gl_set_antialias(!gl_antialias);
    return 0;
  } else if(key=='b'||key=='B') {
    gl_set_bilinear(-1);
    return 0;
  }
  return 1;
}
#endif

static void check_events(void)
{
  int e;
#ifdef CONFIG_GL_X11
  XEvent         Event;
  char           buf[100];
  KeySym         keySym;
  int            key;
  static XComposeStatus stat;

  while ( XPending( mDisplay ) ) {
    XNextEvent( mDisplay,&Event );
    if( Event.type == KeyPress ) {
      XLookupString( &Event.xkey,buf,sizeof(buf),&keySym,&stat );
      key = (keySym&0xff00) != 0 ? (keySym&0x00ff) + 256 : keySym;
      if(gl_handlekey(key))
        XPutBackEvent(mDisplay, &Event);
      break;
    } else {
      XPutBackEvent(mDisplay, &Event);
      break;
    }
  }
#endif
  e=glctx.check_events();
  if(e&VO_EVENT_RESIZE) resize(vo_dwidth, vo_dheight);
  if(e&VO_EVENT_EXPOSE && int_pause) flip_page();
}

static void draw_osd(void)
{
  if (ImageData)
    vo_draw_text(image_width,image_height,draw_alpha_fnc);
}

static void
flip_page(void)
{
  drawTextureDisplay();

//  glFlush();
  if (use_glFinish)
  glFinish();
  glctx.swapGlBuffers(&glctx);

  if (aspect_scaling()) // Avoid flickering borders in fullscreen mode
    glClear (GL_COLOR_BUFFER_BIT);
}

static int draw_slice(uint8_t *src[], int stride[], int w,int h,int x,int y)
{
  uint8_t *yptr = src[0], *uptr = src[1], *vptr = src[2];
  int ystride = stride[0], ustride = stride[1], vstride = stride[2];
  int rem_h = h;
  struct TexSquare *texline = &texgrid[y / texture_height * texnumx];
  int subtex_y = y % texture_width;
  int xs, ys;
  mp_get_chroma_shift(image_format, &xs, &ys);
  while (rem_h > 0) {
    int rem_w = w;
    struct TexSquare *tsq = &texline[x / texture_width];
    int subtex_x = x % texture_height;
    int subtex_h = rem_h;
    if (subtex_y + subtex_h > texture_height)
      subtex_h = texture_height - subtex_y;
    while (rem_w > 0) {
      int subtex_w = rem_w;
      if (subtex_x + subtex_w > texture_width)
        subtex_w = texture_width - subtex_x;
      mpglActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, tsq->texobj);
      glUploadTex(GL_TEXTURE_2D, gl_bitmap_format,  gl_bitmap_type,
                  yptr, ystride, subtex_x, subtex_y,
                  subtex_w, subtex_h, 0);
      mpglActiveTexture(GL_TEXTURE1);
      glBindTexture(GL_TEXTURE_2D, tsq->uvtexobjs[0]);
      glUploadTex(GL_TEXTURE_2D, gl_bitmap_format,  gl_bitmap_type,
                  uptr, ustride, subtex_x >> xs, subtex_y >> ys,
                  subtex_w >> xs, subtex_h >> ys, 0);
      mpglActiveTexture(GL_TEXTURE2);
      glBindTexture(GL_TEXTURE_2D, tsq->uvtexobjs[1]);
      glUploadTex(GL_TEXTURE_2D, gl_bitmap_format,  gl_bitmap_type,
                  vptr, vstride, subtex_x >> xs, subtex_y >> ys,
                  subtex_w >> xs, subtex_h >> ys, 0);
      subtex_x = 0;
      yptr += subtex_w;
      uptr += subtex_w >> xs;
      vptr += subtex_w >> xs;
      tsq++;
      rem_w -= subtex_w;
    }
    subtex_y = 0;
    yptr += subtex_h * ystride - w;
    uptr += (subtex_h >> ys) * ustride - (w >> xs);
    vptr += (subtex_h >> ys) * vstride - (w >> xs);
    texline += texnumx;
    rem_h -= subtex_h;
  }
  mpglActiveTexture(GL_TEXTURE0);
    return 0;
}

static int
draw_frame(uint8_t *src[])
{
  if (is_yuv) {
    mp_msg(MSGT_VO, MSGL_ERR, "[gl2] error: draw_frame called for YV12!\n");
    return 0;
  }
  ImageData=(unsigned char *)src[0];
  resetTexturePointers(ImageData);
  texdirty = 1;
  return 0;
}

static int
query_format(uint32_t format)
{
  if (use_yuv && mp_get_chroma_shift(format, NULL, NULL) &&
      (IMGFMT_IS_YUVP16_NE(format) || !IMGFMT_IS_YUVP16(format)))
    return VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW | VFCAP_OSD |
           VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN | VFCAP_ACCEPT_STRIDE;
  switch(format) {
#ifdef __APPLE__
    case IMGFMT_RGB32:
#else
    case IMGFMT_RGB24:
    case IMGFMT_BGR24:
//    case IMGFMT_RGB32:
//    case IMGFMT_BGR32:
#endif
        return VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW | VFCAP_OSD;
  }
  return 0;
}


static void
uninit(void)
{
  if ( !vo_config_count ) return;
  free(texgrid);
  texgrid = NULL;
  uninit_mpglcontext(&glctx);
}

static const opt_t subopts[] = {
  {"yuv",          OPT_ARG_INT,  &use_yuv,      int_non_neg},
  {"glfinish",     OPT_ARG_BOOL, &use_glFinish, NULL},
  {NULL}
};

static int preinit(const char *arg)
{
  enum MPGLType gltype = GLTYPE_X11;
  // set defaults
#ifdef CONFIG_GL_WIN32
  gltype = GLTYPE_W32;
#endif
  use_yuv = -1;
  use_glFinish = 1;
  if (subopt_parse(arg, subopts) != 0) {
    mp_msg(MSGT_VO, MSGL_FATAL,
            "\n-vo gl2 command line help:\n"
            "Example: mplayer -vo gl2:noglfinish\n"
            "\nOptions:\n"
            "  noglfinish\n"
            "    Do not call glFinish() before swapping buffers\n"
            "  yuv=<n>\n"
            "    0: use software YUV to RGB conversion.\n"
            "    1: use register combiners (nVidia only, for older cards).\n"
            "    2: use fragment program.\n"
            "    3: use fragment program with gamma correction.\n"
            "    4: use fragment program with gamma correction via lookup.\n"
            "    5: use ATI-specific method (for older cards).\n"
            "\n" );
    return -1;
  }
    if(!init_mpglcontext(&glctx, gltype)) goto err_out;
    if (use_yuv == -1) {
#ifdef CONFIG_GL_WIN32
      if (config_w32(320, 200, 320, 200, VOFLAG_HIDDEN, "", 0) == -1)
#endif
#ifdef CONFIG_GL_X11
      if (config_glx(320, 200, 320, 200, VOFLAG_HIDDEN, "", 0) == -1)
#endif
        goto err_out;
      if (glctx.setGlWindow(&glctx) == SET_WINDOW_FAILED)
        goto err_out;
      use_yuv = glAutodetectYUVConversion();
    }
    return 0;

err_out:
    uninit();
    return -1;
}

static int control(uint32_t request, void *data)
{
  switch (request) {
    case VOCTRL_PAUSE:
    case VOCTRL_RESUME:
      int_pause = (request == VOCTRL_PAUSE);
      return VO_TRUE;
    case VOCTRL_QUERY_FORMAT:
      return query_format(*((uint32_t*)data));
    case VOCTRL_ONTOP:
      glctx.ontop();
      return VO_TRUE;
    case VOCTRL_FULLSCREEN:
      glctx.fullscreen();
      if (glctx.setGlWindow(&glctx) == SET_WINDOW_REINIT)
        initGl(vo_dwidth, vo_dheight);
      resize(vo_dwidth, vo_dheight);
      return VO_TRUE;
    case VOCTRL_BORDER:
      glctx.border();
      return VO_TRUE;
    case VOCTRL_GET_PANSCAN:
      return VO_TRUE;
    case VOCTRL_SET_PANSCAN:
      resize(vo_dwidth, vo_dheight);
      return VO_TRUE;
#ifdef CONFIG_GL_X11
    case VOCTRL_SET_EQUALIZER:
    {
      struct voctrl_set_equalizer_args *args = data;
      return vo_x11_set_equalizer(args->name, args->value);
    }
    case VOCTRL_GET_EQUALIZER:
    {
      struct voctrl_get_equalizer_args *args = data;
      return vo_x11_get_equalizer(args->name, args->valueptr);
    }
#endif
    case VOCTRL_UPDATE_SCREENINFO:
      glctx.update_xinerama_info();
      return VO_TRUE;
  }
  return VO_NOTIMPL;
}
