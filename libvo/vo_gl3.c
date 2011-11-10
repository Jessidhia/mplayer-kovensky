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
 *
 * You can alternatively redistribute this file and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <assert.h>

#include "config.h"
#include "talloc.h"
#include "mp_msg.h"
#include "subopt-helper.h"
#include "video_out.h"
#include "libmpcodecs/vfcap.h"
#include "libmpcodecs/mp_image.h"
#include "geometry.h"
#include "osd.h"
#include "sub/font_load.h"
#include "sub/sub.h"
#include "eosd_packer.h"

#include "gl_common.h"
#include "aspect.h"
#include "fastmemcpy.h"
#include "sub/ass_mp.h"

//! How many parts the OSD may consist of at most
#define MAX_OSD_PARTS 20

//for gl_priv.use_yuv
#define MASK_ALL_YUV (~(1 << YUV_CONVERSION_NONE))
#define MASK_NOT_COMBINERS (~((1 << YUV_CONVERSION_NONE) | (1 << YUV_CONVERSION_COMBINERS)))
#define MASK_GAMMA_SUPPORT (MASK_NOT_COMBINERS & ~(1 << YUV_CONVERSION_FRAGMENT))

struct vertex_eosd {
    float x, y;
    uint8_t color[4];
    float u, v;
};

struct texplane {
    // (this can be false even for plane 1+2 if the format is planar RGB)
    bool is_chroma;
    // chroma shifts
    // e.g. get the plane's width in pixels with (priv->src_width >> shift_x)
    int shift_x, shift_y;
    // GL state
    GLuint gl_texture;
    // temporary locking during uploading the frame (e.g. for draw_slice)
    int gl_buffer;
    int buffer_size;
    void *buffer_ptr;
    // value used to clear the image with memset (YUV chroma planes do not use
    // the value 0 for this)
    uint8_t clear_val;
};

struct gl_priv {
    MPGLContext *glctx;
    GL *gl;

    //! Textures for OSD
    GLuint osdtex[MAX_OSD_PARTS];
#ifndef FAST_OSD
    //! Alpha textures for OSD
    GLuint osdatex[MAX_OSD_PARTS];
#endif
    GLuint eosd_texture;
    int eosd_texture_width, eosd_texture_height;
    struct eosd_packer *eosd;
    struct vertex_eosd *eosd_va;
    //! Display lists that draw the OSD parts
    GLuint osdDispList[MAX_OSD_PARTS];
#ifndef FAST_OSD
    GLuint osdaDispList[MAX_OSD_PARTS];
#endif
    //! How many parts the OSD currently consists of
    int osdtexCnt;
    int osd_color;

    int use_ycbcr;
    int use_yuv;
    struct mp_csp_details colorspace;
    int is_yuv;
    int lscale;
    int cscale;
    float filter_strength;
    int yuvconvtype;
    int use_rectangle;
    int err_shown;
    uint32_t image_width;
    uint32_t image_height;
    uint32_t image_format;
    uint32_t image_d_width;
    uint32_t image_d_height;
    int many_fmts;
    int have_texture_rg;
    int ati_hack;
    int force_pbo;
    int use_glFinish;
    int swap_interval;
    GLenum target;

    // per pixel (full pixel when packed, each component when planar)
    int plane_bytes;
    int plane_bits;

    GLint gl_internal_format;
    GLenum gl_format;
    GLenum gl_type;

    int plane_count;
    struct texplane planes[3];

    GLuint fragprog;
    char *custom_prog;
    char *custom_tex;
    int custom_tlin;
    int custom_trect;
    int mipmap_gen;
    int stereo_mode;

    struct mp_csp_equalizer video_eq;

    int texture_width;
    int texture_height;
    int mpi_flipped;
    int vo_flipped;
    int ass_border_x, ass_border_y;

    unsigned int slice_height;
};

static void resize(struct vo *vo, int x, int y)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    mp_msg(MSGT_VO, MSGL_V, "[gl] Resize: %dx%d\n", x, y);
    if (WinID >= 0) {
        int left = 0, top = 0, w = x, h = y;
        geometry(&left, &top, &w, &h, vo->dwidth, vo->dheight);
        top = y - h - top;
        gl->Viewport(left, top, w, h);
    } else
        gl->Viewport(0, 0, x, y);

    gl->MatrixMode(GL_PROJECTION);
    gl->LoadIdentity();
    p->ass_border_x = p->ass_border_y = 0;
    if (aspect_scaling()) {
        int new_w, new_h;
        GLdouble scale_x, scale_y;
        aspect(vo, &new_w, &new_h, A_WINZOOM);
        panscan_calc_windowed(vo);
        new_w += vo->panscan_x;
        new_h += vo->panscan_y;
        scale_x = (GLdouble)new_w / (GLdouble)x;
        scale_y = (GLdouble)new_h / (GLdouble)y;
        gl->Scaled(scale_x, scale_y, 1);
        p->ass_border_x = (vo->dwidth - new_w) / 2;
        p->ass_border_y = (vo->dheight - new_h) / 2;
    }
    gl->Ortho(0, p->image_width, p->image_height, 0, -1, 1);

    gl->MatrixMode(GL_MODELVIEW);
    gl->LoadIdentity();

#ifdef CONFIG_FREETYPE
    // adjust font size to display size
    force_load_font = 1;
#endif
    vo_osd_changed(OSDTYPE_OSD);

    gl->Clear(GL_COLOR_BUFFER_BIT);
    vo->want_redraw = true;
}

static void texSize(struct vo *vo, int w, int h, int *texw, int *texh)
{
    struct gl_priv *p = vo->priv;

    if (p->use_rectangle) {
        *texw = w;
        *texh = h;
    } else {
        *texw = 32;
        while (*texw < w)
            *texw *= 2;
        *texh = 32;
        while (*texh < h)
            *texh *= 2;
    }
    if (p->ati_hack)
        *texw = (*texw + 511) & ~511;
}

//! maximum size of custom fragment program
#define MAX_CUSTOM_PROG_SIZE (1024 * 1024)
static void update_yuvconv(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    int xs, ys, depth;
    struct mp_csp_params cparams = { .colorspace = p->colorspace };
    mp_csp_copy_equalizer_values(&cparams, &p->video_eq);
    gl_conversion_params_t params = {
        p->target, p->yuvconvtype, cparams,
        p->texture_width, p->texture_height, 0, 0, p->filter_strength
    };
    mp_get_chroma_shift(p->image_format, &xs, &ys, &depth);
    params.chrom_texw = params.texw >> xs;
    params.chrom_texh = params.texh >> ys;
    params.csp_params.input_shift = -depth & 7;
    glSetupYUVConversion(gl, &params);
    if (p->custom_prog) {
        FILE *f = fopen(p->custom_prog, "rb");
        if (!f) {
            mp_msg(MSGT_VO, MSGL_WARN,
                   "[gl] Could not read customprog %s\n", p->custom_prog);
        } else {
            char *prog = calloc(1, MAX_CUSTOM_PROG_SIZE + 1);
            fread(prog, 1, MAX_CUSTOM_PROG_SIZE, f);
            fclose(f);
            loadGPUProgram(gl, GL_FRAGMENT_PROGRAM, prog);
            free(prog);
        }
        gl->ProgramEnvParameter4f(GL_FRAGMENT_PROGRAM, 0,
                                  1.0 / p->texture_width,
                                  1.0 / p->texture_height,
                                  p->texture_width, p->texture_height);
    }
    if (p->custom_tex) {
        FILE *f = fopen(p->custom_tex, "rb");
        if (!f) {
            mp_msg(MSGT_VO, MSGL_WARN,
                   "[gl] Could not read customtex %s\n", p->custom_tex);
        } else {
            int width, height, maxval;
            gl->ActiveTexture(GL_TEXTURE3);
            if (glCreatePPMTex(gl, p->custom_trect ? GL_TEXTURE_RECTANGLE : GL_TEXTURE_2D,
                               0, p->custom_tlin ? GL_LINEAR : GL_NEAREST,
                               f, &width, &height, &maxval)) {
                gl->ProgramEnvParameter4f(GL_FRAGMENT_PROGRAM, 1,
                                          1.0 / width, 1.0 / height,
                                          width, height);
            } else
                mp_msg(MSGT_VO, MSGL_WARN,
                       "[gl] Error parsing customtex %s\n", p->custom_tex);
            fclose(f);
            gl->ActiveTexture(GL_TEXTURE0);
        }
    }
}

/**
 * \brief remove all OSD textures and display-lists, thus clearing it.
 */
static void clearOSD(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    int i;
    if (!p->osdtexCnt)
        return;
    gl->DeleteTextures(p->osdtexCnt, p->osdtex);
#ifndef FAST_OSD
    gl->DeleteTextures(p->osdtexCnt, p->osdatex);
    for (i = 0; i < p->osdtexCnt; i++)
        gl->DeleteLists(p->osdaDispList[i], 1);
#endif
    for (i = 0; i < p->osdtexCnt; i++)
        gl->DeleteLists(p->osdDispList[i], 1);
    p->osdtexCnt = 0;
}

/**
 * \brief construct display list from ass image list
 * \param img image list to create OSD from.
 */
static void genEOSD(struct vo *vo, mp_eosd_images_t *imgs)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    bool need_upload, need_allocate;
    eosd_packer_generate(p->eosd, imgs, &need_upload, &need_allocate);

    if (!need_upload)
        return;

    if (!p->eosd_texture) {
        gl->GenTextures(1, &p->eosd_texture);
        need_allocate = true;
    }

    gl->BindTexture(p->target, p->eosd_texture);

    if (need_allocate) {
        texSize(vo, p->eosd->surface.w, p->eosd->surface.h,
                &p->eosd_texture_width, &p->eosd_texture_height);
        // xxx it doesn't need to be cleared, that's a waste of time
        glCreateClearTex(gl, p->target, GL_ALPHA, GL_ALPHA, GL_UNSIGNED_BYTE,
                         GL_NEAREST, p->eosd_texture_width,
                         p->eosd_texture_height, 0);
    }

    // 2 triangles primitives per quad = 6 vertices per quad
    // not using GL_QUADS, as it is deprecated in OpenGL 3.x and later
    p->eosd_va = talloc_realloc_size(p->eosd, p->eosd_va,
                                     p->eosd->targets_count
                                     * sizeof(struct vertex_eosd) * 6);

    float eosd_w = p->eosd_texture_width;
    float eosd_h = p->eosd_texture_height;

    if (p->use_rectangle == 1)
        eosd_w = eosd_h = 1.0f;

    for (int n = 0; n < p->eosd->targets_count; n++) {
        struct eosd_target *target = &p->eosd->targets[n];
        ASS_Image *i = target->ass_img;

        glUploadTex(gl, p->target, GL_ALPHA, GL_UNSIGNED_BYTE, i->bitmap,
                    i->stride, target->source.x0, target->source.y0,
                    i->w, i->h, 0);

        uint8_t color[4] = { i->color >> 24, (i->color >> 16) & 0xff,
                            (i->color >> 8) & 0xff, 255 - (i->color & 0xff) };

        float x0 = target->dest.x0;
        float y0 = target->dest.y0;
        float x1 = target->dest.x1;
        float y1 = target->dest.y1;
        float tx0 = target->source.x0 / eosd_w;
        float ty0 = target->source.y0 / eosd_h;
        float tx1 = target->source.x1 / eosd_w;
        float ty1 = target->source.y1 / eosd_h;

#define COLOR_INIT {color[0], color[1], color[2], color[3]}
        struct vertex_eosd *va = &p->eosd_va[n * 6];
        va[0] = (struct vertex_eosd) { x0, y0, COLOR_INIT, tx0, ty0 };
        va[1] = (struct vertex_eosd) { x0, y1, COLOR_INIT, tx0, ty1 };
        va[2] = (struct vertex_eosd) { x1, y0, COLOR_INIT, tx1, ty0 };
        va[3] = (struct vertex_eosd) { x1, y1, COLOR_INIT, tx1, ty1 };
        va[4] = va[2];
        va[5] = va[1];
#undef COLOR_INIT
    }

    gl->BindTexture(p->target, 0);
}

// Note: relies on state being setup, like projection matrix and blending
static void drawEOSD(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (p->eosd->targets_count == 0)
        return;

    gl->BindTexture(p->target, p->eosd_texture);

    struct vertex_eosd *va = p->eosd_va;
    size_t stride = sizeof(struct vertex_eosd);

    gl->VertexPointer(2, GL_FLOAT, stride, &va[0].x);
    gl->ColorPointer(4, GL_UNSIGNED_BYTE, stride, &va[0].color[0]);
    gl->TexCoordPointer(2, GL_FLOAT, stride, &va[0].u);

    gl->EnableClientState(GL_VERTEX_ARRAY);
    gl->EnableClientState(GL_TEXTURE_COORD_ARRAY);
    gl->EnableClientState(GL_COLOR_ARRAY);

    gl->DrawArrays(GL_TRIANGLES, 0, p->eosd->targets_count * 6);

    gl->DisableClientState(GL_VERTEX_ARRAY);
    gl->DisableClientState(GL_TEXTURE_COORD_ARRAY);
    gl->DisableClientState(GL_COLOR_ARRAY);

    gl->BindTexture(p->target, 0);
}

/**
 * \brief uninitialize OpenGL context, freeing textures, buffers etc.
 */
static void uninitGl(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (gl->DeletePrograms && p->fragprog)
        gl->DeletePrograms(1, &p->fragprog);
    p->fragprog = 0;
    for (int n = 0; n < 3; n++) {
        struct texplane *plane = &p->planes[n];
        if (plane->gl_texture)
            gl->DeleteTextures(1, &plane->gl_texture);
        plane->gl_texture = 0;
        if (gl->DeleteBuffers && plane->gl_buffer)
            gl->DeleteBuffers(1, &plane->gl_buffer);
        plane->gl_buffer = 0;
        plane->buffer_ptr = NULL;
        plane->buffer_size = 0;
    }
    clearOSD(vo);
    if (p->eosd_texture)
        gl->DeleteTextures(1, &p->eosd_texture);
    eosd_packer_reinit(p->eosd, 0, 0);
    p->eosd_texture = 0;
    p->err_shown = 0;
}

static int isSoftwareGl(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    const char *renderer = p->gl->GetString(GL_RENDERER);
    return !renderer || strcmp(renderer, "Software Rasterizer") == 0 ||
           strstr(renderer, "llvmpipe");
}

static void autodetectGlExtensions(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    const char *extensions = gl->GetString(GL_EXTENSIONS);
    const char *vendor     = gl->GetString(GL_VENDOR);
    const char *version    = gl->GetString(GL_VERSION);
    const char *renderer   = gl->GetString(GL_RENDERER);
    int is_ati = vendor && strstr(vendor, "ATI") != NULL;
    int ati_broken_pbo = 0;
    mp_msg(MSGT_VO, MSGL_V, "[gl] Running on OpenGL '%s' by '%s', version '%s'\n",
           renderer, vendor, version);
    if (is_ati && strncmp(version, "2.1.", 4) == 0) {
        int ver = atoi(version + 4);
        mp_msg(MSGT_VO, MSGL_V, "[gl] Detected ATI driver version: %i\n", ver);
        ati_broken_pbo = ver && ver < 8395;
    }
    if (p->ati_hack == -1)
        p->ati_hack = ati_broken_pbo;
    if (p->force_pbo == -1) {
        p->force_pbo = 0;
        if (extensions && strstr(extensions, "_pixel_buffer_object"))
            p->force_pbo = is_ati;
    }
    p->have_texture_rg = extensions && strstr(extensions, "GL_ARB_texture_rg");
    if (p->use_rectangle == -1) {
        p->use_rectangle = 0;
        if (extensions) {
//      if (strstr(extensions, "_texture_non_power_of_two"))
            if (strstr(extensions, "_texture_rectangle"))
                p->use_rectangle = renderer
                    && strstr(renderer, "Mesa DRI R200") ? 1 : 0;
        }
    }
    if (p->use_yuv == -1)
        p->use_yuv = glAutodetectYUVConversion(gl);

    int eq_caps = 0;
    int yuv_mask = (1 << p->use_yuv);
    if (!(yuv_mask & MASK_NOT_COMBINERS)) {
        // combiners
        eq_caps = (1 << MP_CSP_EQ_HUE) | (1 << MP_CSP_EQ_SATURATION);
    } else if (yuv_mask & MASK_ALL_YUV) {
        eq_caps = MP_CSP_EQ_CAPS_COLORMATRIX;
        if (yuv_mask & MASK_GAMMA_SUPPORT)
            eq_caps |= MP_CSP_EQ_CAPS_GAMMA;
    }
    p->video_eq.capabilities = eq_caps;

    if (is_ati && (p->lscale == 1 || p->lscale == 2 || p->cscale == 1 || p->cscale == 2))
        mp_msg(MSGT_VO, MSGL_WARN, "[gl] Selected scaling mode may be broken on"
               " ATI cards.\n"
               "Tell _them_ to fix GL_REPEAT if you have issues.\n");
    mp_msg(MSGT_VO, MSGL_V, "[gl] Settings after autodetection: ati-hack = %i, "
           "force-pbo = %i, rectangle = %i, yuv = %i\n",
           p->ati_hack, p->force_pbo, p->use_rectangle, p->use_yuv);
}

static GLint get_scale_type(struct vo *vo, int chroma)
{
    struct gl_priv *p = vo->priv;

    int nearest = (chroma ? p->cscale : p->lscale) & 64;
    if (nearest)
        return p->mipmap_gen ? GL_NEAREST_MIPMAP_NEAREST : GL_NEAREST;
    return p->mipmap_gen ? GL_LINEAR_MIPMAP_NEAREST : GL_LINEAR;
}

// Return the high byte of the value that represents white in chroma (U/V)
static int get_chroma_clear_val(int bit_depth)
{
    return 1 << (bit_depth - 1 & 7);
}

/**
 * \brief Initialize a (new or reused) OpenGL context.
 * set global gl-related variables to their default values
 */
static int initGl(struct vo *vo, uint32_t d_width, uint32_t d_height)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    autodetectGlExtensions(vo);
    p->target = p->use_rectangle == 1 ? GL_TEXTURE_RECTANGLE : GL_TEXTURE_2D;
    p->yuvconvtype = SET_YUV_CONVERSION(p->use_yuv) |
                     SET_YUV_LUM_SCALER(p->lscale) |
                     SET_YUV_CHROM_SCALER(p->cscale);

    texSize(vo, p->image_width, p->image_height,
            &p->texture_width, &p->texture_height);

    gl->Disable(GL_BLEND);
    gl->Disable(GL_DEPTH_TEST);
    gl->DepthMask(GL_FALSE);
    gl->Disable(GL_CULL_FACE);
    gl->Enable(p->target);
    gl->DrawBuffer(vo_doublebuffering ? GL_BACK : GL_FRONT);
    gl->TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    mp_msg(MSGT_VO, MSGL_V, "[gl] Creating %dx%d texture...\n",
           p->texture_width, p->texture_height);

    for (int n = 0; n < p->plane_count; n++) {
        struct texplane *plane = &p->planes[n];

        gl->ActiveTexture(GL_TEXTURE0 + n);
        gl->GenTextures(1, &plane->gl_texture);
        gl->BindTexture(p->target, plane->gl_texture);

        GLint scale_type = get_scale_type(vo, plane->is_chroma);

        glCreateClearTex(gl, p->target, p->gl_internal_format, p->gl_format,
                         p->gl_type, scale_type,
                         p->texture_width >> plane->shift_x,
                         p->texture_height >> plane->shift_y,
                         plane->clear_val);

        if (p->mipmap_gen)
            gl->TexParameteri(p->target, GL_GENERATE_MIPMAP, GL_TRUE);
    }
    gl->ActiveTexture(GL_TEXTURE0);

    if (p->is_yuv || p->custom_prog) {
        if ((MASK_NOT_COMBINERS & (1 << p->use_yuv)) || p->custom_prog) {
            if (!gl->GenPrograms || !gl->BindProgram)
                mp_msg(MSGT_VO, MSGL_ERR,
                       "[gl] fragment program functions missing!\n");
            else {
                gl->GenPrograms(1, &p->fragprog);
                gl->BindProgram(GL_FRAGMENT_PROGRAM, p->fragprog);
            }
        }
        update_yuvconv(vo);
    }

    GLint max_texture_size;
    gl->GetIntegerv(GL_MAX_TEXTURE_SIZE, &max_texture_size);
    eosd_packer_reinit(p->eosd, max_texture_size, max_texture_size);

    resize(vo, d_width, d_height);

    gl->ClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    gl->Clear(GL_COLOR_BUFFER_BIT);
    if (gl->SwapInterval && p->swap_interval >= 0)
        gl->SwapInterval(p->swap_interval);
    return 1;
}

static int create_window(struct vo *vo, uint32_t d_width, uint32_t d_height,
                         uint32_t flags)
{
    struct gl_priv *p = vo->priv;

    if (p->stereo_mode == GL_3D_QUADBUFFER)
        flags |= VOFLAG_STEREO;

    return p->glctx->create_window(p->glctx, d_width, d_height, flags);
}

static int config(struct vo *vo, uint32_t width, uint32_t height,
                  uint32_t d_width, uint32_t d_height, uint32_t flags,
                  uint32_t format)
{
    struct gl_priv *p = vo->priv;

    int xs, ys, depth;
    p->image_height = height;
    p->image_width = width;
    p->image_format = format;
    p->image_d_width = d_width;
    p->image_d_height = d_height;
    p->is_yuv = mp_get_chroma_shift(p->image_format, &xs, &ys, &depth) > 0;
    p->is_yuv |= (xs << 8) | (ys << 16);
    glFindFormat(format, p->have_texture_rg, NULL, &p->gl_internal_format,
                 &p->gl_format, &p->gl_type);

    if (!p->is_yuv) {
        // xxx mp_image_setfmt calculates this as well
        depth = glFmt2bpp(p->gl_format, p->gl_type) * 8;
    }

    p->plane_bits = depth;
    p->plane_bytes = (depth + 7) / 8;

    p->plane_count = p->is_yuv ? 3 : 1;

    for (int n = 0; n < p->plane_count; n++) {
        struct texplane *plane = &p->planes[n];

        plane->is_chroma = n > 0;
        plane->shift_x = n > 0 ? xs : 0;
        plane->shift_y = n > 0 ? ys : 0;
        plane->clear_val = n > 0 ? get_chroma_clear_val(p->plane_bits) : 0;
    }

    p->vo_flipped = !!(flags & VOFLAG_FLIPPING);

    if (create_window(vo, d_width, d_height, flags) < 0)
        return -1;

    if (vo->config_count)
        uninitGl(vo);
    if (p->glctx->setGlWindow(p->glctx) == SET_WINDOW_FAILED)
        return -1;
    initGl(vo, vo->dwidth, vo->dheight);

    return 0;
}

static void check_events(struct vo *vo)
{
    struct gl_priv *p = vo->priv;

    int e = p->glctx->check_events(vo);
    if (e & VO_EVENT_REINIT) {
        uninitGl(vo);
        initGl(vo, vo->dwidth, vo->dheight);
    }
    if (e & VO_EVENT_RESIZE)
        resize(vo, vo->dwidth, vo->dheight);
    if (e & VO_EVENT_EXPOSE)
        vo->want_redraw = true;
}

/**
 * Creates the textures and the display list needed for displaying
 * an OSD part.
 * Callback function for osd_draw_text_ext().
 */
static void create_osd_texture(void *ctx, int x0, int y0, int w, int h,
                               unsigned char *src, unsigned char *srca,
                               int stride)
{
    struct vo *vo = ctx;
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    // initialize to 8 to avoid special-casing on alignment
    int sx = 8, sy = 8;
    GLint scale_type = GL_NEAREST;

    if (w <= 0 || h <= 0 || stride < w) {
        mp_msg(MSGT_VO, MSGL_V, "Invalid dimensions OSD for part!\n");
        return;
    }
    texSize(vo, w, h, &sx, &sy);

    if (p->osdtexCnt >= MAX_OSD_PARTS) {
        mp_msg(MSGT_VO, MSGL_ERR, "Too many OSD parts, contact the developers!\n");
        return;
    }

    // create Textures for OSD part
    gl->GenTextures(1, &p->osdtex[p->osdtexCnt]);
    gl->BindTexture(p->target, p->osdtex[p->osdtexCnt]);
    glCreateClearTex(gl, p->target, GL_LUMINANCE, GL_LUMINANCE,
                     GL_UNSIGNED_BYTE, scale_type, sx, sy, 0);
    glUploadTex(gl, p->target, GL_LUMINANCE, GL_UNSIGNED_BYTE, src, stride,
                0, 0, w, h, 0);

#ifndef FAST_OSD
    gl->GenTextures(1, &p->osdatex[p->osdtexCnt]);
    gl->BindTexture(p->target, p->osdatex[p->osdtexCnt]);
    glCreateClearTex(gl, p->target, GL_ALPHA, GL_ALPHA, GL_UNSIGNED_BYTE,
                     scale_type, sx, sy, 0);
    {
        int i;
        char *tmp = malloc(stride * h);
        // convert alpha from weird MPlayer scale.
        // in-place is not possible since it is reused for future OSDs
        for (i = h * stride - 1; i >= 0; i--)
            tmp[i] = -srca[i];
        glUploadTex(gl, p->target, GL_ALPHA, GL_UNSIGNED_BYTE, tmp, stride,
                    0, 0, w, h, 0);
        free(tmp);
    }
#endif

    gl->BindTexture(p->target, 0);

    // Create a list for rendering this OSD part
#ifndef FAST_OSD
    p->osdaDispList[p->osdtexCnt] = gl->GenLists(1);
    gl->NewList(p->osdaDispList[p->osdtexCnt], GL_COMPILE);
    // render alpha
    gl->BindTexture(p->target, p->osdatex[p->osdtexCnt]);
    glDrawTex(gl, x0, y0, w, h, 0, 0, w, h, sx, sy, p->use_rectangle == 1, 0, 0);
    gl->EndList();
#endif
    p->osdDispList[p->osdtexCnt] = gl->GenLists(1);
    gl->NewList(p->osdDispList[p->osdtexCnt], GL_COMPILE);
    // render OSD
    gl->BindTexture(p->target, p->osdtex[p->osdtexCnt]);
    glDrawTex(gl, x0, y0, w, h, 0, 0, w, h, sx, sy, p->use_rectangle == 1, 0, 0);
    gl->EndList();

    p->osdtexCnt++;
}

#define RENDER_OSD  1
#define RENDER_EOSD 2

/**
 * \param type bit 0: render OSD, bit 1: render EOSD
 */
static void do_render_osd(struct vo *vo, int type)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    int draw_osd  = (type & RENDER_OSD) && p->osdtexCnt > 0;
    int draw_eosd = (type & RENDER_EOSD);
    if (!draw_osd && !draw_eosd)
        return;

    gl->MatrixMode(GL_PROJECTION);
    gl->PushMatrix();
    gl->LoadIdentity();
    gl->Ortho(0, vo->dwidth, vo->dheight, 0, -1, 1);

    gl->Enable(GL_BLEND);
    if (draw_eosd) {
        gl->BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        drawEOSD(vo);
    }
    if (draw_osd) {
        gl->Color4ub((p->osd_color >> 16) & 0xff, (p->osd_color >> 8) & 0xff,
                     p->osd_color & 0xff, 0xff - (p->osd_color >> 24));
        // draw OSD
#ifndef FAST_OSD
        gl->BlendFunc(GL_ZERO, GL_ONE_MINUS_SRC_ALPHA);
        gl->CallLists(p->osdtexCnt, GL_UNSIGNED_INT, p->osdaDispList);
#endif
        gl->BlendFunc(GL_SRC_ALPHA, GL_ONE);
        gl->CallLists(p->osdtexCnt, GL_UNSIGNED_INT, p->osdDispList);
    }
    // set rendering parameters back to defaults
    gl->Disable(GL_BLEND);
    gl->PopMatrix();
    gl->BindTexture(p->target, 0);
}

static void draw_osd(struct vo *vo, struct osd_state *osd)
{
    struct gl_priv *p = vo->priv;

    if (vo_osd_changed(0)) {
        clearOSD(vo);
        osd_draw_text_ext(osd, vo->dwidth, vo->dheight, p->ass_border_x,
                          p->ass_border_y, p->ass_border_x,
                          p->ass_border_y, p->image_width,
                          p->image_height, create_osd_texture, vo);
    }
    if (vo_doublebuffering)
        do_render_osd(vo, RENDER_OSD);
}

static void do_render(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

//  Enable(GL_TEXTURE_2D);
//  BindTexture(GL_TEXTURE_2D, texture_id);

    gl->BindTexture(p->target, p->planes[0].gl_texture);

    gl->Color3f(1, 1, 1);
    if (p->is_yuv || p->custom_prog)
        glEnableYUVConversion(gl, p->target, p->yuvconvtype);
    if (p->stereo_mode) {
        glEnable3DLeft(gl, p->stereo_mode);
        glDrawTex(gl, 0, 0, p->image_width, p->image_height,
                  0, 0, p->image_width >> 1, p->image_height,
                  p->texture_width, p->texture_height,
                  p->use_rectangle == 1, p->is_yuv,
                  p->mpi_flipped ^ p->vo_flipped);
        glEnable3DRight(gl, p->stereo_mode);
        glDrawTex(gl, 0, 0, p->image_width, p->image_height,
                  p->image_width >> 1, 0, p->image_width >> 1,
                  p->image_height, p->texture_width, p->texture_height,
                  p->use_rectangle == 1, p->is_yuv,
                  p->mpi_flipped ^ p->vo_flipped);
        glDisable3D(gl, p->stereo_mode);
    } else {
        glDrawTex(gl, 0, 0, p->image_width, p->image_height,
                  0, 0, p->image_width, p->image_height,
                  p->texture_width, p->texture_height,
                  p->use_rectangle == 1, p->is_yuv,
                  p->mpi_flipped ^ p->vo_flipped);
    }
    if (p->is_yuv || p->custom_prog)
        glDisableYUVConversion(gl, p->target, p->yuvconvtype);
}

static void flip_page(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (vo_doublebuffering) {
        if (p->use_glFinish)
            gl->Finish();
        p->glctx->swapGlBuffers(p->glctx);
        if (aspect_scaling())
            gl->Clear(GL_COLOR_BUFFER_BIT);
    } else {
        do_render(vo);
        do_render_osd(vo, RENDER_OSD | RENDER_EOSD);
        if (p->use_glFinish)
            gl->Finish();
        else
            gl->Flush();
    }
}

static int draw_slice(struct vo *vo, uint8_t *src[], int stride[], int w, int h,
                      int x, int y)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    p->mpi_flipped = stride[0] < 0;

    for (int n = 0; n < p->plane_count; n++) {
        gl->ActiveTexture(GL_TEXTURE0 + n);
        gl->BindTexture(p->target, p->planes[n].gl_texture);
        int xs = p->planes[n].shift_x, ys = p->planes[n].shift_y;
        glUploadTex(gl, p->target, p->gl_format, p->gl_type, src[n], stride[n],
                    x >> xs, y >> ys, w >> xs, h >> ys, p->slice_height);
    }
    gl->ActiveTexture(GL_TEXTURE0);

    return 0;
}

static uint32_t get_image(struct vo *vo, mp_image_t *mpi)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    assert(mpi->num_planes == p->plane_count);

    if (!gl->GenBuffers || !gl->BindBuffer || !gl->BufferData || !gl->MapBuffer) {
        if (!p->err_shown)
            mp_msg(MSGT_VO, MSGL_ERR, "[gl] extensions missing for dr\n"
                   "Expect a _major_ speed penalty\n");
        p->err_shown = 1;
        return VO_FALSE;
    }
    if (mpi->flags & MP_IMGFLAG_READABLE)
        return VO_FALSE;
    if (mpi->type != MP_IMGTYPE_STATIC && mpi->type != MP_IMGTYPE_TEMP &&
        (mpi->type != MP_IMGTYPE_NUMBERED || mpi->number))
        return VO_FALSE;
    if (p->ati_hack) {
        mpi->width = p->texture_width;
        mpi->height = p->texture_height;
    }
    mpi->flags &= ~MP_IMGFLAG_COMMON_PLANE;
    for (int n = 0; n < p->plane_count; n++) {
        struct texplane *plane = &p->planes[n];
        int w = mpi->width >> plane->shift_x, h = mpi->height >> plane->shift_y;
        mpi->stride[n] = w * p->plane_bytes;
        int needed_size = mpi->stride[n] * h;
        if (!plane->gl_buffer)
            gl->GenBuffers(1, &plane->gl_buffer);
        gl->BindBuffer(GL_PIXEL_UNPACK_BUFFER, plane->gl_buffer);
        if (needed_size > plane->buffer_size) {
            plane->buffer_size = needed_size;
            gl->BufferData(GL_PIXEL_UNPACK_BUFFER, plane->buffer_size,
                           NULL, GL_DYNAMIC_DRAW);
        }
        if (!plane->buffer_ptr)
            plane->buffer_ptr = gl->MapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
        mpi->planes[n] = plane->buffer_ptr;
        gl->BindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    }
    mpi->flags |= MP_IMGFLAG_DIRECT;
    return VO_TRUE;
}

static void clear_border(struct vo *vo, uint8_t *dst, int start, int stride,
                         int height, int full_height, int value)
{
    int right_border = stride - start;
    int bottom_border = full_height - height;
    while (height > 0) {
        if (right_border > 0)
            memset(dst + start, value, right_border);
        dst += stride;
        height--;
    }
    if (bottom_border > 0)
        memset(dst, value, stride * bottom_border);
}

static uint32_t draw_image(struct vo *vo, mp_image_t *mpi)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;
    int n;

    int slice = p->slice_height;
    mp_image_t mpi2 = *mpi;
    int w = mpi->w, h = mpi->h;
    if (mpi->flags & MP_IMGFLAG_DRAW_CALLBACK)
        goto skip_upload;
    mpi2.flags = 0;
    mpi2.type = MP_IMGTYPE_TEMP;
    mpi2.width = mpi2.w;
    mpi2.height = mpi2.h;
    if (p->force_pbo && !(mpi->flags & MP_IMGFLAG_DIRECT) && !p->planes[0].buffer_ptr
        && get_image(vo, &mpi2) == VO_TRUE)
    {
        for (n = 0; n < p->plane_count; n++) {
            struct texplane *plane = &p->planes[n];
            int xs = plane->shift_x, ys = plane->shift_y;
            int line_bytes = (mpi->w >> xs) * p->plane_bytes;
            memcpy_pic(mpi2.planes[n], mpi->planes[n], line_bytes, mpi->h >> ys,
                       mpi2.stride[n], mpi->stride[n]);
            if (p->ati_hack) {
                // since we have to do a full upload we need to clear the borders
                clear_border(vo, mpi2.planes[n], line_bytes, mpi2.stride[n],
                             mpi->h >> ys, mpi2.height >> ys, plane->clear_val);
            }
        }
        mpi = &mpi2;
    }
    p->mpi_flipped = mpi->stride[0] < 0;
    if (mpi->flags & MP_IMGFLAG_DIRECT) {
        if (p->ati_hack) {
            w = p->texture_width;
            h = p->texture_height;
        }
        slice = 0; // always "upload" full texture
    }
    for (n = 0; n < p->plane_count; n++) {
        struct texplane *plane = &p->planes[n];
        int xs = plane->shift_x, ys = plane->shift_y;
        void *plane_ptr = mpi->planes[n];
        if (mpi->flags & MP_IMGFLAG_DIRECT) {
            gl->BindBuffer(GL_PIXEL_UNPACK_BUFFER, plane->gl_buffer);
            gl->UnmapBuffer(GL_PIXEL_UNPACK_BUFFER);
            plane->buffer_ptr = NULL;
            plane_ptr = NULL; // PBO offset 0
        }
        gl->ActiveTexture(GL_TEXTURE0 + n);
        gl->BindTexture(p->target, plane->gl_texture);
        glUploadTex(gl, p->target, p->gl_format, p->gl_type, plane_ptr,
                    mpi->stride[n], mpi->x >> xs, mpi->y >> ys, w >> xs,
                    h >> ys, slice);
    }
    gl->ActiveTexture(GL_TEXTURE0);
    gl->BindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
skip_upload:
    if (vo_doublebuffering)
        do_render(vo);
    return VO_TRUE;
}

static int query_format(struct vo *vo, uint32_t format)
{
    struct gl_priv *p = vo->priv;

    int depth;
    int caps = VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW | VFCAP_FLIP |
               VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN | VFCAP_ACCEPT_STRIDE |
               VFCAP_OSD | VFCAP_EOSD | VFCAP_EOSD_UNSCALED;
    if (format == IMGFMT_RGB24 || format == IMGFMT_RGBA)
        return caps;
    if (p->use_yuv && mp_get_chroma_shift(format, NULL, NULL, &depth) &&
        (depth == 8 || depth == 16 || glYUVLargeRange(p->use_yuv)) &&
        (IMGFMT_IS_YUVP16_NE(format) || !IMGFMT_IS_YUVP16(format)))
        return caps;
    // HACK, otherwise we get only b&w with some filters (e.g. -vf eq)
    // ideally MPlayer should be fixed instead not to use Y800 when it has the choice
    if (!p->use_yuv && (format == IMGFMT_Y8 || format == IMGFMT_Y800))
        return 0;
    if (!p->use_ycbcr && (format == IMGFMT_UYVY || format == IMGFMT_YVYU))
        return 0;
    if (p->many_fmts &&
        glFindFormat(format, p->have_texture_rg, NULL, NULL, NULL, NULL))
        return caps;
    return 0;
}

static void uninit(struct vo *vo)
{
    struct gl_priv *p = vo->priv;

    if (p->glctx)
        uninitGl(vo);
    free(p->custom_prog);
    p->custom_prog = NULL;
    free(p->custom_tex);
    p->custom_tex = NULL;
    uninit_mpglcontext(p->glctx);
    p->glctx = NULL;
    p->gl = NULL;
}

static int preinit_internal(struct vo *vo, const char *arg, int allow_sw,
                            enum MPGLType gltype)
{
    struct gl_priv *p = talloc_zero(vo, struct gl_priv);
    vo->priv = p;

    *p = (struct gl_priv) {
        .many_fmts = 1,
        .use_yuv = -1,
        .colorspace = MP_CSP_DETAILS_DEFAULTS,
        .filter_strength = 0.5,
        .use_rectangle = -1,
        .ati_hack = -1,
        .force_pbo = -1,
        .swap_interval = 1,
        .custom_prog = NULL,
        .custom_tex = NULL,
        .custom_tlin = 1,
        .osd_color = 0xffffff,
    };

    p->eosd = eosd_packer_create(vo);

    //essentially unused; for legacy warnings only
    int user_colorspace = 0;
    int levelconv = -1;
    int aspect = -1;

    const opt_t subopts[] = {
        {"manyfmts",     OPT_ARG_BOOL, &p->many_fmts,    NULL},
        {"ycbcr",        OPT_ARG_BOOL, &p->use_ycbcr,    NULL},
        {"slice-height", OPT_ARG_INT,  &p->slice_height, int_non_neg},
        {"rectangle",    OPT_ARG_INT,  &p->use_rectangle,int_non_neg},
        {"yuv",          OPT_ARG_INT,  &p->use_yuv,      int_non_neg},
        {"lscale",       OPT_ARG_INT,  &p->lscale,       int_non_neg},
        {"cscale",       OPT_ARG_INT,  &p->cscale,       int_non_neg},
        {"filter-strength", OPT_ARG_FLOAT, &p->filter_strength, NULL},
        {"ati-hack",     OPT_ARG_BOOL, &p->ati_hack,     NULL},
        {"force-pbo",    OPT_ARG_BOOL, &p->force_pbo,    NULL},
        {"glfinish",     OPT_ARG_BOOL, &p->use_glFinish, NULL},
        {"swapinterval", OPT_ARG_INT,  &p->swap_interval,NULL},
        {"customprog",   OPT_ARG_MSTRZ,&p->custom_prog,  NULL},
        {"customtex",    OPT_ARG_MSTRZ,&p->custom_tex,   NULL},
        {"customtlin",   OPT_ARG_BOOL, &p->custom_tlin,  NULL},
        {"customtrect",  OPT_ARG_BOOL, &p->custom_trect, NULL},
        {"mipmapgen",    OPT_ARG_BOOL, &p->mipmap_gen,   NULL},
        {"osdcolor",     OPT_ARG_INT,  &p->osd_color,    NULL},
        {"stereo",       OPT_ARG_INT,  &p->stereo_mode,  NULL},
        // Removed options.
        // They are only parsed to notify the user about the replacements.
        {"aspect",       OPT_ARG_BOOL, &aspect,          NULL},
        {"colorspace",   OPT_ARG_INT,  &user_colorspace, NULL},
        {"levelconv",    OPT_ARG_INT,  &levelconv,       NULL},
        {NULL}
    };

    if (subopt_parse(arg, subopts) != 0) {
        mp_msg(MSGT_VO, MSGL_FATAL,
               "\n-vo gl command line help:\n"
               "Example: mplayer -vo gl:slice-height=4\n"
               "\nOptions:\n"
               "  nomanyfmts\n"
               "    Disable extended color formats for OpenGL 1.2 and later\n"
               "  slice-height=<0-...>\n"
               "    Slice size for texture transfer, 0 for whole image\n"
               "  rectangle=<0,1,2>\n"
               "    0: use power-of-two textures\n"
               "    1: use texture_rectangle\n"
               "    2: use texture_non_power_of_two\n"
               "  ati-hack\n"
               "    Workaround ATI bug with PBOs\n"
               "  force-pbo\n"
               "    Force use of PBO even if this involves an extra memcpy\n"
               "  glfinish\n"
               "    Call glFinish() before swapping buffers\n"
               "  swapinterval=<n>\n"
               "    Interval in displayed frames between to buffer swaps.\n"
               "    1 is equivalent to enable VSYNC, 0 to disable VSYNC.\n"
               "    Requires GLX_SGI_swap_control support to work.\n"
               "  ycbcr\n"
               "    also try to use the GL_MESA_ycbcr_texture extension\n"
               "  yuv=<n>\n"
               "    0: use software YUV to RGB conversion.\n"
               "    1: deprecated, will use yuv=2 (used to be nVidia register combiners).\n"
               "    2: use fragment program.\n"
               "    3: use fragment program with gamma correction.\n"
               "    4: use fragment program with gamma correction via lookup.\n"
               "    5: use ATI-specific method (for older cards).\n"
               "    6: use lookup via 3D texture.\n"
               "  lscale=<n>\n"
               "    0: use standard bilinear scaling for luma.\n"
               "    1: use improved bicubic scaling for luma.\n"
               "    2: use cubic in X, linear in Y direction scaling for luma.\n"
               "    3: as 1 but without using a lookup texture.\n"
               "    4: experimental unsharp masking (sharpening).\n"
               "    5: experimental unsharp masking (sharpening) with larger radius.\n"
               "  cscale=<n>\n"
               "    as lscale but for chroma (2x slower with little visible effect).\n"
               "  filter-strength=<value>\n"
               "    set the effect strength for some lscale/cscale filters\n"
               "  customprog=<filename>\n"
               "    use a custom YUV conversion program\n"
               "  customtex=<filename>\n"
               "    use a custom YUV conversion lookup texture\n"
               "  nocustomtlin\n"
               "    use GL_NEAREST scaling for customtex texture\n"
               "  customtrect\n"
               "    use texture_rectangle for customtex texture\n"
               "  mipmapgen\n"
               "    generate mipmaps for the video image (use with TXB in customprog)\n"
               "  osdcolor=<0xAARRGGBB>\n"
               "    use the given color for the OSD\n"
               "  stereo=<n>\n"
               "    0: normal display\n"
               "    1: side-by-side to red-cyan stereo\n"
               "    2: side-by-side to green-magenta stereo\n"
               "    3: side-by-side to quadbuffer stereo\n"
               "\n");
        return -1;
    }
    if (user_colorspace != 0 || levelconv != -1) {
        mp_msg(MSGT_VO, MSGL_ERR, "[gl] \"colorspace\" and \"levelconv\" "
               "suboptions have been removed. Use options --colormatrix and"
               " --colormatrix-input-range/--colormatrix-output-range instead.\n");
        return -1;
    }
    if (aspect != -1) {
        mp_msg(MSGT_VO, MSGL_ERR, "[gl] \"noaspect\" suboption has been "
               "removed. Use --noaspect instead.\n");
        return -1;
    }
    if (p->use_yuv == 1) {
        mp_msg(MSGT_VO, MSGL_WARN, "[gl] yuv=1 (nVidia register combiners) have"
               " been removed, using yuv=2 instead.\n");
        p->use_yuv = 2;
    }
    p->glctx = init_mpglcontext(gltype, vo);
    if (!p->glctx)
        goto err_out;
    p->gl = p->glctx->gl;

    if (p->glctx->type == GLTYPE_SDL && p->use_yuv == -1) {
        // Apparently it's not possible to implement VOFLAG_HIDDEN on SDL 1.2,
        // so don't do autodetection. Use a sufficiently useful and safe YUV
        // conversion mode.
        p->use_yuv = YUV_CONVERSION_FRAGMENT;
    }

    if (p->use_yuv == -1 || !allow_sw) {
        if (create_window(vo, 320, 200, VOFLAG_HIDDEN) < 0)
            goto err_out;
        if (p->glctx->setGlWindow(p->glctx) == SET_WINDOW_FAILED)
            goto err_out;
        if (!allow_sw && isSoftwareGl(vo))
            goto err_out;
        autodetectGlExtensions(vo);
        // We created a window to test whether the GL context supports hardware
        // acceleration and so on. Destroy that window to make sure all state
        // associated with it is lost.
        uninit(vo);
        p->glctx = init_mpglcontext(gltype, vo);
        if (!p->glctx)
            goto err_out;
        p->gl = p->glctx->gl;
    }
    if (p->many_fmts)
        mp_msg(MSGT_VO, MSGL_INFO, "[gl] using extended formats. "
               "Use -vo gl:nomanyfmts if playback fails.\n");
    mp_msg(MSGT_VO, MSGL_V, "[gl] Using %d as slice height "
           "(0 means image height).\n", p->slice_height);

    return 0;

err_out:
    uninit(vo);
    return -1;
}

static int preinit(struct vo *vo, const char *arg)
{
    return preinit_internal(vo, arg, 1, GLTYPE_AUTO);
}

static int control(struct vo *vo, uint32_t request, void *data)
{
    struct gl_priv *p = vo->priv;

    switch (request) {
    case VOCTRL_QUERY_FORMAT:
        return query_format(vo, *(uint32_t *)data);
    case VOCTRL_GET_IMAGE:
        return get_image(vo, data);
    case VOCTRL_DRAW_IMAGE:
        return draw_image(vo, data);
    case VOCTRL_DRAW_EOSD:
        if (!data)
            return VO_FALSE;
        genEOSD(vo, data);
        if (vo_doublebuffering)
            do_render_osd(vo, RENDER_EOSD);
        return VO_TRUE;
    case VOCTRL_GET_EOSD_RES: {
        mp_eosd_res_t *r = data;
        r->w = vo->dwidth;
        r->h = vo->dheight;
        r->mt = r->mb = r->ml = r->mr = 0;
        if (aspect_scaling()) {
            r->ml = r->mr = p->ass_border_x;
            r->mt = r->mb = p->ass_border_y;
        }
        return VO_TRUE;
    }
    case VOCTRL_ONTOP:
        if (!p->glctx->ontop)
            break;
        p->glctx->ontop(vo);
        return VO_TRUE;
    case VOCTRL_FULLSCREEN:
        p->glctx->fullscreen(vo);
        resize(vo, vo->dwidth, vo->dheight);
        return VO_TRUE;
    case VOCTRL_BORDER:
        if (!p->glctx->border)
            break;
        p->glctx->border(vo);
        resize(vo, vo->dwidth, vo->dheight);
        return VO_TRUE;
    case VOCTRL_GET_PANSCAN:
        return VO_TRUE;
    case VOCTRL_SET_PANSCAN:
        resize(vo, vo->dwidth, vo->dheight);
        return VO_TRUE;
    case VOCTRL_GET_EQUALIZER:
        if (p->is_yuv) {
            struct voctrl_get_equalizer_args *args = data;
            return mp_csp_equalizer_get(&p->video_eq, args->name, args->valueptr)
                   >= 0 ? VO_TRUE : VO_NOTIMPL;
        }
        break;
    case VOCTRL_SET_EQUALIZER:
        if (p->is_yuv) {
            struct voctrl_set_equalizer_args *args = data;
            if (mp_csp_equalizer_set(&p->video_eq, args->name, args->value) < 0)
                return VO_NOTIMPL;
            update_yuvconv(vo);
            vo->want_redraw = true;
            return VO_TRUE;
        }
        break;
    case VOCTRL_SET_YUV_COLORSPACE: {
        bool supports_csp = (1 << p->use_yuv) & MASK_NOT_COMBINERS;
        if (vo->config_count && supports_csp) {
            p->colorspace = *(struct mp_csp_details *)data;
            update_yuvconv(vo);
            vo->want_redraw = true;
        }
        return VO_TRUE;
    }
    case VOCTRL_GET_YUV_COLORSPACE:
        *(struct mp_csp_details *)data = p->colorspace;
        return VO_TRUE;
    case VOCTRL_UPDATE_SCREENINFO:
        if (!p->glctx->update_xinerama_info)
            break;
        p->glctx->update_xinerama_info(vo);
        return VO_TRUE;
    case VOCTRL_REDRAW_FRAME:
        if (vo_doublebuffering)
            do_render(vo);
        return true;
    }
    return VO_NOTIMPL;
}

const struct vo_driver video_out_gl3 = {
    .is_new = true,
    .info = &(const vo_info_t) {
        "OpenGL",
        "gl3",
        "Reimar Doeffinger <Reimar.Doeffinger@gmx.de> and others",
        ""
    },
    .preinit = preinit,
    .config = config,
    .control = control,
    .draw_slice = draw_slice,
    .draw_osd = draw_osd,
    .flip_page = flip_page,
    .check_events = check_events,
    .uninit = uninit,
};
