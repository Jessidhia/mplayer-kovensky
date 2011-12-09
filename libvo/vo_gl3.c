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

//xxx
//#define USE_GLEW
#ifdef USE_GLEW
#include <GL/glew.h>
#endif

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
#include "filter_kernels.h"
#include "aspect.h"
#include "fastmemcpy.h"
#include "sub/ass_mp.h"

// generated from libvo/vo_gl3_shaders.glsl
#include "libvo/vo_gl3_shaders.h"

//! How many parts the OSD may consist of at most
#define MAX_OSD_PARTS 20

// Pixel width of 1D lookup textures.
#define LOOKUP_TEXTURE_SIZE 256

typedef struct string2 {
    const char *a, *b;
} string2;

// Map command line lscale/cscale arguments to shader filter routines.
// Note that there are convolution filters additional to this list; the filters
// listed here are called "fixed" to distinguish them from filter-based ones.
static const string2 fixed_scale_filters[] = {
    {"bilinear", "sample_bilinear"},
    {"bicubic_fast", "sample_bicubic"},
    {"sharpen3", "sample_unsharp3"},
    {"sharpen5", "sample_unsharp5"},
    {"lanczos2_slow", "sample_lanczos2_nolookup"},
    {"0", "sample_bilinear"},
    {"1", "sample_bicubic"}, // was bicubic scaling with lookup texture
    {"2", "sample_bicubic"}, // was like 1, but linear in Y direction (faster)
    {"3", "sample_bicubic"},
    {"4", "sample_unsharp3"},
    {"5", "sample_unsharp5"},
    {0}
};

struct convolution_filters {
    const char *shader_fn;
    bool use_2d;
    GLint internal_format;
    GLenum format;
};

// indexed with filter_kernel->size
struct convolution_filters convolution_filters[] = {
    [2] = {"sample_convolution2", false, GL_RG16F,   GL_RG},
    [4] = {"sample_convolution4", false, GL_RGBA16F, GL_RGBA},
    [6] = {"sample_convolution6", true,  GL_RGB16F,  GL_RGB},
    [8] = {"sample_convolution8", true,  GL_RGBA16F, GL_RGBA},
};

struct vertex {
    float position[2];
    uint8_t color[4];
    float texcoord[2];
};

#define VERTICES_PER_QUAD 6

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

struct vertex_array {
    GLuint program;
    GLuint buffer;
    GLuint vao;
    int vertex_count;
};

struct scaler {
    int id;
    char *name;
    const char *shader_fn;
    struct filter_kernel *kernel;
    GLuint gl_lut;
    int texunit;
    const char *lut_name;
};

struct gl_priv {
    MPGLContext *glctx;
    GL *gl;

    int gl_debug;

    struct vertex_array va_osd, va_eosd, va_video;

    //! Textures for OSD
    GLuint osdtex[MAX_OSD_PARTS];
    GLuint eosd_texture;
    int eosd_texture_width, eosd_texture_height;
    struct eosd_packer *eosd;
    struct vertex *eosd_va;
    struct vertex osd_va[MAX_OSD_PARTS * VERTICES_PER_QUAD];
    //! How many parts the OSD currently consists of
    int osdtexCnt;
    int osd_color;

    int use_ycbcr;
    int use_gamma;
    struct mp_csp_details colorspace;
    int is_yuv;
    float filter_strength;
    int use_rectangle;
    uint32_t image_width;
    uint32_t image_height;
    uint32_t image_format;
    uint32_t image_d_width;
    uint32_t image_d_height;
    int force_pbo;
    int use_glFinish;
    int swap_interval;

    // per pixel (full pixel when packed, each component when planar)
    int plane_bytes;
    int plane_bits;

    GLint gl_internal_format;
    GLenum gl_format;
    GLenum gl_type;

    int plane_count;
    struct texplane planes[3];

    // state for luma and chroma scalers
    struct scaler scalers[2];

    int mipmap_gen;
    int stereo_mode;

    struct mp_csp_equalizer video_eq;

    int texture_width;
    int texture_height;
    int mpi_flipped;
    int vo_flipped;

    struct vo_rect src_rect;    // displayed part of the source video
    struct vo_rect dst_rect;    // video rectangle on output window
    int border_x, border_y;     // OSD borders
};


static void matrix_ortho2d(float m[3][3], float x0, float x1,
                           float y0, float y1)
{
    memset(m, 0, 9 * sizeof(float));
    m[0][0] = 2.0f / (x1 - x0);
    m[1][1] = 2.0f / (y1 - y0);
    m[2][0] = -(x1 + x0) / (x1 - x0);
    m[2][1] = -(y1 + y0) / (y1 - y0);
    m[2][2] = 1.0f;
}

// Return the shader routine for the given scaler, or NULL if not found.
static char *find_fixed_scaler(const char *name)
{
    for (const string2 *entry = &fixed_scale_filters[0]; entry->a; entry++) {
        if (strcmp(entry->a, name) == 0)
            return (char *)entry->b;
    }
    return NULL;
}

static bool can_use_filter_kernel(struct filter_kernel *kernel)
{
    if (!kernel)
        return false;
    if (kernel->size >= sizeof(convolution_filters) / sizeof(convolution_filters[0]))
        return false;
    return !!convolution_filters[kernel->size].shader_fn;
}

static void vertex_array_init(GL *gl, struct vertex_array * va, GLuint program)
{
    size_t stride = sizeof(struct vertex);
    GLint loc;

    *va = (struct vertex_array) { .program = program };

    gl->GenBuffers(1, &va->buffer);
    gl->GenVertexArrays(1, &va->vao);

    gl->BindBuffer(GL_ARRAY_BUFFER, va->buffer);
    gl->BindVertexArray(va->vao);

    loc = gl->GetAttribLocation(program, "vertex_position");
    if (loc >= 0) {
        gl->EnableVertexAttribArray(loc);
        gl->VertexAttribPointer(loc, 2, GL_FLOAT, GL_FALSE, stride,
                                (void*)offsetof(struct vertex, position));
    }

    loc = gl->GetAttribLocation(program, "vertex_color");
    if (loc >= 0) {
        gl->EnableVertexAttribArray(loc);
        gl->VertexAttribPointer(loc, 4, GL_UNSIGNED_BYTE, GL_TRUE, stride,
                                (void*)offsetof(struct vertex, color));
    }

    loc = gl->GetAttribLocation(program, "vertex_texcoord");
    if (loc >= 0) {
        gl->EnableVertexAttribArray(loc);
        gl->VertexAttribPointer(loc, 2, GL_FLOAT, GL_FALSE, stride,
                                (void*)offsetof(struct vertex, texcoord));
    }

    gl->BindBuffer(GL_ARRAY_BUFFER, 0);
    gl->BindVertexArray(0);
}

static void vertex_array_uninit(GL *gl, struct vertex_array *va)
{
    gl->DeleteVertexArrays(1, &va->vao);
    gl->DeleteBuffers(1, &va->buffer);
    *va = (struct vertex_array) {0};
}

static void vertex_array_upload(GL *gl, struct vertex_array *va,
                                struct vertex *vb, int vertex_count)
{
    gl->BindBuffer(GL_ARRAY_BUFFER, va->buffer);
    gl->BufferData(GL_ARRAY_BUFFER, vertex_count * sizeof(struct vertex), vb,
                   GL_DYNAMIC_DRAW);
    gl->BindBuffer(GL_ARRAY_BUFFER, 0);
    va->vertex_count = vertex_count;
}

static void vertex_array_draw(GL *gl, struct vertex_array *va)
{
    gl->UseProgram(va->program);
    gl->BindVertexArray(va->vao);
    gl->DrawArrays(GL_TRIANGLES, 0, va->vertex_count);
    gl->BindVertexArray(0);
    gl->UseProgram(0);

    glCheckError(gl, "after rendering");
}

// Write a textured quad to a vertex array.
// va = destination vertex array, VERTICES_PER_QUAD entries will be overwritten
// x0, y0, x1, y1 = destination coordinates of the quad
// tx0, ty0, tx1, ty1 = source texture coordinates (in pixels)
// texture_w, texture_h = size of the texture
// color = optional color for all vertices, NULL for opaque white
// flip = flip vertically
static void write_quad(struct vertex *va,
                       float x0, float y0, float x1, float y1,
                       float tx0, float ty0, float tx1, float ty1,
                       float texture_w, float texture_h,
                       const uint8_t color[4], bool flip)
{
    static const uint8_t white[4] = { 255, 255, 255, 255 };

    if (!color)
        color = white;

    tx0 /= texture_w;
    ty0 /= texture_h;
    tx1 /= texture_w;
    ty1 /= texture_h;

    if (flip) {
        float tmp = ty0;
        ty0 = ty1;
        ty1 = tmp;
    }

#define COLOR_INIT {color[0], color[1], color[2], color[3]}
    va[0] = (struct vertex) { {x0, y0}, COLOR_INIT, {tx0, ty0} };
    va[1] = (struct vertex) { {x0, y1}, COLOR_INIT, {tx0, ty1} };
    va[2] = (struct vertex) { {x1, y0}, COLOR_INIT, {tx1, ty0} };
    va[3] = (struct vertex) { {x1, y1}, COLOR_INIT, {tx1, ty1} };
    va[4] = va[2];
    va[5] = va[1];
#undef COLOR_INIT
}

static void scaler_texture(struct vo *vo, GLuint program, struct scaler *scaler)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (!scaler->kernel)
        return;

    int size = scaler->kernel->size;
    struct convolution_filters *entry = &convolution_filters[size];

    GLint loc = gl->GetUniformLocation(program, scaler->lut_name);
    if (loc < 0)
        return;

    gl->Uniform1i(loc, scaler->texunit);

    gl->ActiveTexture(GL_TEXTURE0 + scaler->texunit);
    GLenum target = entry->use_2d ? GL_TEXTURE_2D : GL_TEXTURE_1D;

    if (scaler->gl_lut) {
        gl->BindTexture(target, scaler->gl_lut);
    } else {
        gl->GenTextures(1, &scaler->gl_lut);
        gl->BindTexture(target, scaler->gl_lut);
        gl->PixelStorei(GL_UNPACK_ALIGNMENT, 4);
        gl->PixelStorei(GL_UNPACK_ROW_LENGTH, 0);

        float *weights = talloc_array(NULL, float, LOOKUP_TEXTURE_SIZE * size);
        mp_compute_lut(scaler->kernel, LOOKUP_TEXTURE_SIZE, weights);
        if (entry->use_2d) {
            gl->TexImage2D(GL_TEXTURE_2D, 0, entry->internal_format, 2,
                           LOOKUP_TEXTURE_SIZE, 0, entry->format, GL_FLOAT,
                           weights);
        } else {
            gl->TexImage1D(GL_TEXTURE_1D, 0, entry->internal_format,
                           LOOKUP_TEXTURE_SIZE, 0, entry->format, GL_FLOAT,
                           weights);
        }
        talloc_free(weights);

        gl->TexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        gl->TexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        gl->TexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        gl->TexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    }

    gl->ActiveTexture(GL_TEXTURE0);
}

static void update_uniforms(struct vo *vo, GLuint program)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;
    GLint loc;

    if (program == 0)
        return;

    gl->UseProgram(program);

    struct mp_csp_params cparams = {
        .colorspace = p->colorspace,
        .input_shift = -p->plane_bits & 7,
    };
    mp_csp_copy_equalizer_values(&cparams, &p->video_eq);

    loc = gl->GetUniformLocation(program, "transform");
    if (loc >= 0) {
        float matrix[3][3];
        matrix_ortho2d(matrix, 0, vo->dwidth, vo->dheight, 0);
        gl->UniformMatrix3fv(loc, 1, GL_FALSE, &matrix[0][0]);
    }

    loc = gl->GetUniformLocation(program, "colormatrix");
    if (loc >= 0) {
        float yuv2rgb[3][4];
        mp_get_yuv2rgb_coeffs(&cparams, yuv2rgb);
        gl->UniformMatrix4x3fv(loc, 1, GL_TRUE, &yuv2rgb[0][0]);
    }

    loc = gl->GetUniformLocation(program, "inv_gamma");
    if (loc >= 0) {
        gl->Uniform3f(loc, 1.0 / cparams.rgamma, 1.0 / cparams.ggamma,
                      1.0 / cparams.bgamma);
    }

    loc = gl->GetUniformLocation(program, "texture1");
    if (loc >= 0)
        gl->Uniform1i(loc, 0);
    loc = gl->GetUniformLocation(program, "texture2");
    if (loc >= 0)
        gl->Uniform1i(loc, 1);
    loc = gl->GetUniformLocation(program, "texture3");
    if (loc >= 0)
        gl->Uniform1i(loc, 2);

    scaler_texture(vo, program, &p->scalers[0]);
    scaler_texture(vo, program, &p->scalers[1]);

    loc = gl->GetUniformLocation(program, "filter_strength");
    if (loc >= 0)
        gl->Uniform1f(loc, p->filter_strength);

    gl->UseProgram(0);

    glCheckError(gl, "update_uniforms()");
}

static void update_all_uniforms(struct vo *vo)
{
    struct gl_priv *p = vo->priv;

    update_uniforms(vo, p->va_osd.program);
    update_uniforms(vo, p->va_eosd.program);
    update_uniforms(vo, p->va_video.program);
}

static void resize(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    mp_msg(MSGT_VO, MSGL_V, "[gl] Resize: %dx%d\n", vo->dwidth, vo->dheight);
    int left = 0, top = 0;
    if (WinID >= 0) {
        int w = vo->dwidth, h = vo->dheight;
        int old_y = vo->dheight;
        geometry(&left, &top, &w, &h, vo->dwidth, vo->dheight);
        top = old_y - h - top;
    }
    gl->Viewport(left, top, vo->dwidth, vo->dheight);

    struct vo_rect borders;
    calc_src_dst_rects(vo, p->image_width, p->image_height, &p->src_rect,
                       &p->dst_rect, &borders, NULL);
    p->border_x = borders.left;
    p->border_y = borders.top;

    update_all_uniforms(vo);

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
}

/**
 * \brief remove all OSD textures and display-lists, thus clearing it.
 */
static void clearOSD(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (!p->osdtexCnt)
        return;
    gl->DeleteTextures(p->osdtexCnt, p->osdtex);
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

    bool need_repos, need_upload, need_allocate;
    eosd_packer_generate(p->eosd, imgs, &need_repos, &need_upload,
                         &need_allocate);

    if (!need_repos)
        return;

    if (!p->eosd_texture) {
        gl->GenTextures(1, &p->eosd_texture);
        need_allocate = true;
    }

    gl->BindTexture(GL_TEXTURE_2D, p->eosd_texture);

    if (need_allocate) {
        texSize(vo, p->eosd->surface.w, p->eosd->surface.h,
                &p->eosd_texture_width, &p->eosd_texture_height);
        // xxx it doesn't need to be cleared, that's a waste of time
        glCreateClearTex(gl, GL_TEXTURE_2D, GL_RED, GL_RED,
                         GL_UNSIGNED_BYTE, GL_NEAREST,
                         p->eosd_texture_width, p->eosd_texture_height, 0);
    }

    // 2 triangles primitives per quad = 6 vertices per quad
    // not using GL_QUADS, as it is deprecated in OpenGL 3.x and later
    p->eosd_va = talloc_realloc_size(p->eosd, p->eosd_va,
                                     p->eosd->targets_count
                                     * sizeof(struct vertex)
                                     * VERTICES_PER_QUAD);

    for (int n = 0; n < p->eosd->targets_count; n++) {
        struct eosd_target *target = &p->eosd->targets[n];
        ASS_Image *i = target->ass_img;

        if (need_upload) {
            glUploadTex(gl, GL_TEXTURE_2D, GL_RED, GL_UNSIGNED_BYTE, i->bitmap,
                        i->stride, target->source.x0, target->source.y0,
                        i->w, i->h, 0);
        }

        uint8_t color[4] = { i->color >> 24, (i->color >> 16) & 0xff,
                            (i->color >> 8) & 0xff, 255 - (i->color & 0xff) };

        write_quad(&p->eosd_va[n * VERTICES_PER_QUAD],
                   target->dest.x0, target->dest.y0,
                   target->dest.x1, target->dest.y1,
                   target->source.x0, target->source.y0,
                   target->source.x1, target->source.y1,
                   p->eosd_texture_width, p->eosd_texture_height,
                   color, false);
    }

    gl->BindTexture(GL_TEXTURE_2D, 0);

    vertex_array_upload(gl, &p->va_eosd, p->eosd_va,
                        p->eosd->targets_count * VERTICES_PER_QUAD);
}

// Note: relies on state being setup, like projection matrix and blending
static void drawEOSD(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (p->eosd->targets_count == 0)
        return;

    gl->BindTexture(GL_TEXTURE_2D, p->eosd_texture);
    vertex_array_draw(gl, &p->va_eosd);
    gl->BindTexture(GL_TEXTURE_2D, 0);
}

// Free video resources etc.
static void uninitVideo(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    gl->DeleteProgram(p->va_osd.program);
    gl->DeleteProgram(p->va_eosd.program);
    gl->DeleteProgram(p->va_video.program);
    vertex_array_uninit(gl, &p->va_osd);
    vertex_array_uninit(gl, &p->va_eosd);
    vertex_array_uninit(gl, &p->va_video);

    for (int n = 0; n < 2; n++) {
        gl->DeleteTextures(1, &p->scalers->gl_lut);
        p->scalers->gl_lut = 0;
    }

    for (int n = 0; n < 3; n++) {
        struct texplane *plane = &p->planes[n];

        gl->DeleteTextures(1, &plane->gl_texture);
        plane->gl_texture = 0;
        gl->DeleteBuffers(1, &plane->gl_buffer);
        plane->gl_buffer = 0;
        plane->buffer_ptr = NULL;
        plane->buffer_size = 0;
    }
}

/**
 * \brief uninitialize OpenGL context, freeing textures, buffers etc.
 */
static void uninitGL(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    uninitVideo(vo);

    clearOSD(vo);
    gl->DeleteTextures(1, &p->eosd_texture);
    eosd_packer_reinit(p->eosd, 0, 0);
    p->eosd_texture = 0;
}

static GLint get_scale_type(struct vo *vo, int chroma)
{
    struct gl_priv *p = vo->priv;

    int nearest = 0;
    if (nearest)
        return p->mipmap_gen ? GL_NEAREST_MIPMAP_NEAREST : GL_NEAREST;
    return p->mipmap_gen ? GL_LINEAR_MIPMAP_NEAREST : GL_LINEAR;
}

// Return the high byte of the value that represents white in chroma (U/V)
static int get_chroma_clear_val(int bit_depth)
{
    return 1 << (bit_depth - 1 & 7);
}

static GLuint create_shader(GL *gl, GLenum type, const char *header,
                            const char *source)
{
    GLuint shader = gl->CreateShader(type);
    gl->ShaderSource(shader, 2, (const char*[]) { header, source }, NULL);
    gl->CompileShader(shader);
    GLint status;
    gl->GetShaderiv(shader, GL_COMPILE_STATUS, &status);
    GLint log_length;
    gl->GetShaderiv(shader, GL_INFO_LOG_LENGTH, &log_length);

    if (!status || log_length > 1) {
        GLchar *log = talloc_zero_size(NULL, log_length + 1);
        gl->GetShaderInfoLog(shader, log_length, NULL, log);
        mp_msg(MSGT_VO, status ? MSGL_V : MSGL_ERR,
               "[gl] shader compile log (status=%d): %s\n", status, log);
        talloc_free(log);
    }

    return shader;
}

static void prog_create_shader(GL *gl, GLuint program, GLenum type,
                               const char *header, const char *source)
{
    GLuint shader = create_shader(gl, type, header, source);
    gl->AttachShader(program, shader);
    gl->DeleteShader(shader);
}

static void link_shader(GL *gl, GLuint program)
{
    gl->LinkProgram(program);
    GLint status;
    gl->GetProgramiv(program, GL_LINK_STATUS, &status);
    GLint log_length;
    gl->GetProgramiv(program, GL_INFO_LOG_LENGTH, &log_length);

    if (!status || log_length > 1) {
        GLchar *log = talloc_zero_size(NULL, log_length + 1);
        gl->GetProgramInfoLog(program, log_length, NULL, log);
        mp_msg(MSGT_VO, status ? MSGL_V : MSGL_ERR,
               "[gl] shader link log (status=%d): %s\n", status, log);
        talloc_free(log);
    }
}

static GLuint create_program(GL *gl, const char *header, const char *vertex,
                             const char *frag)
{
    GLuint prog = gl->CreateProgram();
    prog_create_shader(gl, prog, GL_VERTEX_SHADER, header, vertex);
    prog_create_shader(gl, prog, GL_FRAGMENT_SHADER, header, frag);
    link_shader(gl, prog);
    return prog;
}

static void shader_def(char **shader, const char *name,
                       const char *value)
{
    *shader = talloc_asprintf_append(*shader, "#define %s %s\n", name, value);
}

static void shader_def_b(char **shader, const char *name, bool b)
{
    shader_def(shader, name, b ? "1" : "0");
}

static void shader_setup_scaler(char **shader, struct scaler *scaler)
{
    const char *target = scaler->id == 0 ? "SAMPLE_L" : "SAMPLE_C";
    if (!scaler->kernel) {
        shader_def(shader, target, scaler->shader_fn);
    } else {
        *shader = talloc_asprintf_append(*shader,
            "#define %s(p0, p1) %s(%s, p0, p1)\n", target, scaler->shader_fn,
            scaler->lut_name);
    }
}

static void compile_shaders(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;
    void *tmp = talloc_new(NULL);

    char *header = talloc_strdup(tmp, "");

    shader_def_b(&header, "USE_PLANAR", p->is_yuv);
    shader_def_b(&header, "USE_COLORMATRIX", p->is_yuv);
    shader_def_b(&header, "USE_GAMMA_POW", p->use_gamma);

    shader_setup_scaler(&header, &p->scalers[0]);
    shader_setup_scaler(&header, &p->scalers[1]);

    mp_msg(MSGT_VO, MSGL_V, "[gl] shader config:\n%s", header);

    header = talloc_asprintf(tmp, "%s%s", shader_prelude, header);

    vertex_array_init(gl, &p->va_eosd,
        create_program(gl, header, vertex_shader, frag_shader_eosd));

    vertex_array_init(gl, &p->va_osd,
        create_program(gl, header, vertex_shader, frag_shader_osd));

    vertex_array_init(gl, &p->va_video,
        create_program(gl, header, vertex_shader, frag_shader_video));

    glCheckError(gl, "shader compilation");

    talloc_free(tmp);
}


// First-time initialization of the GL state.
static int initGL(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

#ifdef USE_GLEW
    // NOTE: needs a GL context
    GLenum err = glewInit();
    if (err != GLEW_OK) {
        printf("glew error: %s\n", glewGetErrorString(err));
        abort();
        return 0;
    }

    if (!glewIsSupported("GL_VERSION_3_3")) {
        printf("OpenGL 3.3 not available\n");
        abort();
        return 0;
    }
#endif

    glCheckError(p->gl, "before initGL");

    const char *vendor     = gl->GetString(GL_VENDOR);
    const char *version    = gl->GetString(GL_VERSION);
    const char *renderer   = gl->GetString(GL_RENDERER);
    mp_msg(MSGT_VO, MSGL_V, "[gl] GL_RENDERER='%s', GL_VENDOR='%s', "
                            "GL_VERSION='%s'\n", renderer, vendor, version);

    gl->Disable(GL_BLEND);
    gl->Disable(GL_DEPTH_TEST);
    gl->DepthMask(GL_FALSE);
    gl->Disable(GL_CULL_FACE);
    gl->DrawBuffer(vo_doublebuffering ? GL_BACK : GL_FRONT);

    GLint max_texture_size;
    gl->GetIntegerv(GL_MAX_TEXTURE_SIZE, &max_texture_size);
    eosd_packer_reinit(p->eosd, max_texture_size, max_texture_size);

    gl->ClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    gl->Clear(GL_COLOR_BUFFER_BIT);
    if (gl->SwapInterval && p->swap_interval >= 0)
        gl->SwapInterval(p->swap_interval);

    glCheckError(gl, "after initGL");

    return 1;
}

static void initVideo(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    int xs, ys, depth;
    p->is_yuv = mp_get_chroma_shift(p->image_format, &xs, &ys, &depth) > 0;
    glFindFormat(p->image_format, true, NULL, &p->gl_internal_format,
                 &p->gl_format, &p->gl_type);

    // fix for legacy crap from gl_common.c
    if (p->gl_internal_format == 1) p->gl_internal_format = GL_RED;
    if (p->gl_internal_format == 2) p->gl_internal_format = GL_RG;
    if (p->gl_internal_format == 3) p->gl_internal_format = GL_RGB;
    if (p->gl_internal_format == 4) p->gl_internal_format = GL_RGBA;
    if (p->gl_format == GL_LUMINANCE)
        p->gl_format = GL_RED;

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

    int eq_caps = 0;
    if (p->is_yuv)
        eq_caps |= MP_CSP_EQ_CAPS_COLORMATRIX;
    if (p->use_gamma)
        eq_caps |= MP_CSP_EQ_CAPS_GAMMA;
    p->video_eq.capabilities = eq_caps;

    compile_shaders(vo);

    glCheckError(gl, "before video texture creation");

    texSize(vo, p->image_width, p->image_height,
            &p->texture_width, &p->texture_height);

    for (int n = 0; n < p->plane_count; n++) {
        struct texplane *plane = &p->planes[n];

        int w = p->texture_width >> plane->shift_x;
        int h = p->texture_height >> plane->shift_y;

        mp_msg(MSGT_VO, MSGL_V, "[gl] Texture for plane %d: %dx%d\n", n, w, h);

        gl->ActiveTexture(GL_TEXTURE0 + n);
        gl->GenTextures(1, &plane->gl_texture);
        gl->BindTexture(GL_TEXTURE_2D, plane->gl_texture);

        GLint scale_type = get_scale_type(vo, plane->is_chroma);
        glCreateClearTex(gl, GL_TEXTURE_2D, p->gl_internal_format, p->gl_format,
                         p->gl_type, scale_type, w, h, plane->clear_val);

        if (p->mipmap_gen)
            gl->TexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
    }
    gl->ActiveTexture(GL_TEXTURE0);

    glCheckError(gl, "after video texture creation");
}


static int create_window(struct vo *vo, uint32_t d_width, uint32_t d_height,
                         uint32_t flags)
{
    struct gl_priv *p = vo->priv;

    if (p->stereo_mode == GL_3D_QUADBUFFER)
        flags |= VOFLAG_STEREO;

    int mpgl_version = MPGL_VER(3, 1);
    int mpgl_flags = 0;
    if (p->gl_debug)
        mpgl_flags |= MPGLFLAG_DEBUG;

    return create_mpglcontext(p->glctx, mpgl_flags, mpgl_version, d_width,
                              d_height, flags);
}

static int config(struct vo *vo, uint32_t width, uint32_t height,
                  uint32_t d_width, uint32_t d_height, uint32_t flags,
                  uint32_t format)
{
    struct gl_priv *p = vo->priv;

    if (create_window(vo, d_width, d_height, flags) == SET_WINDOW_FAILED)
        return -1;

    if (!vo->config_count)
        initGL(vo);

    p->image_d_width = d_width;
    p->image_d_height = d_height;
    p->vo_flipped = !!(flags & VOFLAG_FLIPPING);

    if (p->image_format != format || p->image_width != width
        || p->image_height != height)
    {
        uninitVideo(vo);
        p->image_height = height;
        p->image_width = width;
        p->image_format = format;
        initVideo(vo);
    }

    resize(vo);

    return 0;
}

static void check_events(struct vo *vo)
{
    struct gl_priv *p = vo->priv;

    int e = p->glctx->check_events(vo);
    if (e & VO_EVENT_REINIT) {
        uninitGL(vo);
        initGL(vo);
        initVideo(vo);
        resize(vo);
    }
    if (e & VO_EVENT_RESIZE)
        resize(vo);
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

    gl->GenTextures(1, &p->osdtex[p->osdtexCnt]);
    gl->BindTexture(GL_TEXTURE_2D, p->osdtex[p->osdtexCnt]);
    glCreateClearTex(gl, GL_TEXTURE_2D, GL_RG, GL_RG, GL_UNSIGNED_BYTE,
                     scale_type, sx, sy, 0);
    {
        int i;
        unsigned char *tmp = malloc(stride * h * 2);
        // convert alpha from weird MPlayer scale.
        for (i = 0; i < h * stride; i++) {
            tmp[i*2+0] = src[i];
            tmp[i*2+1] = -srca[i];
        }
        glUploadTex(gl, GL_TEXTURE_2D, GL_RG, GL_UNSIGNED_BYTE, tmp, stride * 2,
                    0, 0, w, h, 0);
        free(tmp);
    }

    gl->BindTexture(GL_TEXTURE_2D, 0);

    uint8_t color[4] = {(p->osd_color >> 16) & 0xff, (p->osd_color >> 8) & 0xff,
                        p->osd_color & 0xff, 0xff - (p->osd_color >> 24)};

    write_quad(&p->osd_va[p->osdtexCnt * VERTICES_PER_QUAD],
               x0, y0, x0 + w, y0 + h, 0, 0, w, h,
               sx, sy, color, false);

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

    gl->Enable(GL_BLEND);
    gl->BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (draw_eosd) {
        drawEOSD(vo);
    }
    if (draw_osd) {
        gl->UseProgram(p->va_osd.program);
        gl->BindVertexArray(p->va_osd.vao);

        for (int n = 0; n < p->osdtexCnt; n++) {
            gl->BindTexture(GL_TEXTURE_2D, p->osdtex[n]);
            gl->DrawArrays(GL_TRIANGLES, n * VERTICES_PER_QUAD,
                           VERTICES_PER_QUAD);
        }

        gl->BindVertexArray(0);
        gl->UseProgram(0);
    }
    // set rendering parameters back to defaults
    gl->Disable(GL_BLEND);
    gl->BindTexture(GL_TEXTURE_2D, 0);
}

static void draw_osd(struct vo *vo, struct osd_state *osd)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    if (vo_osd_changed(0)) {
        clearOSD(vo);
        osd_draw_text_ext(osd, vo->dwidth, vo->dheight, p->border_x,
                          p->border_y, p->border_x,
                          p->border_y, p->image_width,
                          p->image_height, create_osd_texture, vo);
        vertex_array_upload(gl, &p->va_osd, &p->osd_va[0],
                            p->osdtexCnt * VERTICES_PER_QUAD);
    }
    if (vo_doublebuffering)
        do_render_osd(vo, RENDER_OSD);
}

static void do_render(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;
    struct vertex vb[VERTICES_PER_QUAD * 2];
    bool is_flipped = p->mpi_flipped ^ p->vo_flipped;

    gl->BindTexture(GL_TEXTURE_2D, p->planes[0].gl_texture);

    if (p->stereo_mode) {
        int w = p->src_rect.width;
        int imgw = p->image_width;
        write_quad(vb,
                   p->dst_rect.left, p->dst_rect.top,
                   p->dst_rect.right, p->dst_rect.bottom,
                   p->src_rect.left / 2, p->src_rect.top,
                   p->src_rect.left / 2 + w / 2, p->src_rect.bottom,
                   p->texture_width, p->texture_height,
                   NULL, is_flipped);
        write_quad(vb + VERTICES_PER_QUAD,
                   p->dst_rect.left, p->dst_rect.top,
                   p->dst_rect.right, p->dst_rect.bottom,
                   p->src_rect.left / 2 + imgw / 2, p->src_rect.top,
                   p->src_rect.left / 2 + imgw / 2 + w / 2, p->src_rect.bottom,
                   p->texture_width, p->texture_height,
                   NULL, is_flipped);

        vertex_array_upload(gl, &p->va_video, vb, VERTICES_PER_QUAD * 2);

        gl->UseProgram(p->va_video.program);
        gl->BindVertexArray(p->va_video.vao);

        glEnable3DLeft(gl, p->stereo_mode);
        gl->DrawArrays(GL_TRIANGLES, 0, VERTICES_PER_QUAD);
        glEnable3DRight(gl, p->stereo_mode);
        gl->DrawArrays(GL_TRIANGLES, VERTICES_PER_QUAD, VERTICES_PER_QUAD);
        glDisable3D(gl, p->stereo_mode);

        gl->BindVertexArray(0);
        gl->UseProgram(0);
    } else {
        write_quad(vb,
                   p->dst_rect.left, p->dst_rect.top,
                   p->dst_rect.right, p->dst_rect.bottom,
                   p->src_rect.left, p->src_rect.top,
                   p->src_rect.right, p->src_rect.bottom,
                   p->texture_width, p->texture_height,
                   NULL, is_flipped);

        vertex_array_upload(gl, &p->va_video, vb, VERTICES_PER_QUAD);
        vertex_array_draw(gl, &p->va_video);
    }
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
        gl->BindTexture(GL_TEXTURE_2D, p->planes[n].gl_texture);
        int xs = p->planes[n].shift_x, ys = p->planes[n].shift_y;
        glUploadTex(gl, GL_TEXTURE_2D, p->gl_format, p->gl_type, src[n],
                    stride[n], x >> xs, y >> ys, w >> xs, h >> ys, 0);
    }
    gl->ActiveTexture(GL_TEXTURE0);

    return 0;
}

static uint32_t get_image(struct vo *vo, mp_image_t *mpi)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    assert(mpi->num_planes == p->plane_count);

    if (mpi->flags & MP_IMGFLAG_READABLE)
        return VO_FALSE;
    if (mpi->type != MP_IMGTYPE_STATIC && mpi->type != MP_IMGTYPE_TEMP &&
        (mpi->type != MP_IMGTYPE_NUMBERED || mpi->number))
        return VO_FALSE;
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

static uint32_t draw_image(struct vo *vo, mp_image_t *mpi)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;
    int n;

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
        }
        mpi = &mpi2;
    }
    p->mpi_flipped = mpi->stride[0] < 0;
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
        gl->BindTexture(GL_TEXTURE_2D, plane->gl_texture);
        glUploadTex(gl, GL_TEXTURE_2D, p->gl_format, p->gl_type, plane_ptr,
                    mpi->stride[n], mpi->x >> xs, mpi->y >> ys, w >> xs,
                    h >> ys, 0);
    }
    gl->ActiveTexture(GL_TEXTURE0);
    gl->BindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
skip_upload:
    if (vo_doublebuffering)
        do_render(vo);
    return VO_TRUE;
}

static mp_image_t *get_screenshot(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    mp_image_t *image = alloc_mpi(p->texture_width, p->texture_height,
                                  p->image_format);
    assert(image->num_planes == p->plane_count);

    for (int n = 0; n < p->plane_count; n++) {
        gl->ActiveTexture(GL_TEXTURE0 + n);
        gl->BindTexture(GL_TEXTURE_2D, p->planes[n].gl_texture);
        glDownloadTex(gl, GL_TEXTURE_2D, p->gl_format, p->gl_type,
                      image->planes[n], image->stride[n]);
    }
    gl->ActiveTexture(GL_TEXTURE0);

    image->width = p->image_width;
    image->height = p->image_height;

    image->w = p->image_d_width;
    image->h = p->image_d_height;

    return image;
}

static mp_image_t *get_window_screenshot(struct vo *vo)
{
    struct gl_priv *p = vo->priv;
    GL *gl = p->gl;

    GLint vp[4]; //x, y, w, h
    gl->GetIntegerv(GL_VIEWPORT, vp);
    mp_image_t *image = alloc_mpi(vp[2], vp[3], IMGFMT_RGB24);
    gl->PixelStorei(GL_PACK_ALIGNMENT, 4);
    gl->PixelStorei(GL_PACK_ROW_LENGTH, 0);
    gl->ReadBuffer(GL_FRONT);
    //flip image while reading
    for (int y = 0; y < vp[3]; y++) {
        gl->ReadPixels(vp[0], vp[1] + vp[3] - y - 1, vp[2], 1,
                       GL_RGB, GL_UNSIGNED_BYTE,
                       image->planes[0] + y * image->stride[0]);
    }
    return image;
}

static int query_format(struct vo *vo, uint32_t format)
{
    struct gl_priv *p = vo->priv;

    int depth;
    int caps = VFCAP_CSP_SUPPORTED | VFCAP_CSP_SUPPORTED_BY_HW | VFCAP_FLIP |
               VFCAP_HWSCALE_UP | VFCAP_HWSCALE_DOWN | VFCAP_ACCEPT_STRIDE |
               VFCAP_OSD | VFCAP_EOSD | VFCAP_EOSD_UNSCALED;
    if (mp_get_chroma_shift(format, NULL, NULL, &depth) &&
        (IMGFMT_IS_YUVP16_NE(format) || !IMGFMT_IS_YUVP16(format)))
        return caps;
    if (!p->use_ycbcr && (format == IMGFMT_UYVY || format == IMGFMT_YVYU))
        return 0;
    if (glFindFormat(format, true, NULL, NULL, NULL, NULL))
        return caps;
    return 0;
}

static void uninit(struct vo *vo)
{
    struct gl_priv *p = vo->priv;

    // NOTE: GL functions might not be loaded yet
    if (p->glctx && p->gl->DeleteTextures)
        uninitGL(vo);
    uninit_mpglcontext(p->glctx);
    p->glctx = NULL;
    p->gl = NULL;
}

static bool handle_scaler_opt(struct vo *vo, struct scaler *scaler)
{
    if (!scaler->name || scaler->name[0] == '\0')
        scaler->name = talloc_strdup(vo, "bilinear");

    if (strcmp(scaler->name, "help") == 0) {
        mp_msg(MSGT_VO, MSGL_INFO, "Available scalers:\n");
        for (const string2 *e = &fixed_scale_filters[0]; e->a; e++) {
            mp_msg(MSGT_VO, MSGL_INFO, "    %s\n", e->a);
        }
        for (const struct filter_kernel *e = &mp_filter_kernels[0]; e->name; e++)
        {
            mp_msg(MSGT_VO, MSGL_INFO, "    %s\n", e->name);
        }
        return false;
    }

    scaler->kernel = NULL;
    scaler->shader_fn = find_fixed_scaler(scaler->name);
    if (scaler->shader_fn)
        return true;

    scaler->kernel = mp_find_filter_kernel(scaler->name);
    if (can_use_filter_kernel(scaler->kernel)) {
        const struct convolution_filters *entry =
            &convolution_filters[scaler->kernel->size];
        scaler->shader_fn = entry->shader_fn;
        bool is_luma = scaler->id == 0;
        scaler->lut_name = entry->use_2d
                           ? (is_luma ? "lut_l_2d" : "lut_c_2d")
                           : (is_luma ? "lut_l_1d" : "lut_c_1d");
        return true;
    }

    mp_msg(MSGT_VO, MSGL_FATAL, "[gl] Error: scaler '%s' not found.\n",
           scaler->name);
    return false;
}

static int preinit_internal(struct vo *vo, const char *arg, int allow_sw,
                            enum MPGLType gltype)
{
    struct gl_priv *p = talloc_zero(vo, struct gl_priv);
    vo->priv = p;

    *p = (struct gl_priv) {
        .colorspace = MP_CSP_DETAILS_DEFAULTS,
        .filter_strength = 0.5,
        .use_rectangle = 1,
        .force_pbo = 0,
        .swap_interval = 1,
        .osd_color = 0xffffff,
        .scalers = {
            { .id = 0, .texunit = 5 },
            { .id = 1, .texunit = 6 },
        },
    };

    p->eosd = eosd_packer_create(vo);

    char *lscale = NULL;
    char *cscale = NULL;

    const opt_t subopts[] = {
        {"gamma",        OPT_ARG_BOOL, &p->use_gamma,    NULL},
        {"ycbcr",        OPT_ARG_BOOL, &p->use_ycbcr,    NULL},
        {"rectangle",    OPT_ARG_INT,  &p->use_rectangle,int_non_neg},
        {"filter-strength", OPT_ARG_FLOAT, &p->filter_strength, NULL},
        {"force-pbo",    OPT_ARG_BOOL, &p->force_pbo,    NULL},
        {"glfinish",     OPT_ARG_BOOL, &p->use_glFinish, NULL},
        {"swapinterval", OPT_ARG_INT,  &p->swap_interval,NULL},
        {"mipmapgen",    OPT_ARG_BOOL, &p->mipmap_gen,   NULL},
        {"osdcolor",     OPT_ARG_INT,  &p->osd_color,    NULL},
        {"stereo",       OPT_ARG_INT,  &p->stereo_mode,  NULL},
        {"lscale",       OPT_ARG_MSTRZ,&lscale,          NULL},
        {"cscale",       OPT_ARG_MSTRZ,&cscale,          NULL},
        {"debug",        OPT_ARG_BOOL, &p->gl_debug,     NULL},
        {NULL}
    };

    if (subopt_parse(arg, subopts) != 0) {
        mp_msg(MSGT_VO, MSGL_FATAL,
               "\n-vo gl command line help:\n"
               "Example: mplayer -vo gl:yuv=2\n"
               "\nOptions:\n"
               "  gamma\n"
               "    Enable gamma control.\n"
               "  rectangle=<0,1,2>\n"
               "    0: use power-of-two textures\n"
               "    1 and 2: use texture_non_power_of_two\n"
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
               "  lscale=<filter>\n"
               "    bilinear: use standard bilinear scaling for luma.\n"
               "    bicubic_fast: bicubic filter (without lookup texture).\n"
               "    sharpen3: unsharp masking (sharpening) with radius=3.\n"
               "    sharpen5: unsharp masking (sharpening) with radius=5.\n"
               "    lanczos2: Lanczos with radius=2.\n"
               "    lanczos3: Lanczos with radius=3.\n"
               "  There are more filters - print a list with lscale=help.\n"
               "  Old filter values for lscale:\n"
               "    0: bilinear\n"
               "    1,2,3: bicubic_fast\n"
               "    4: sharpen3\n"
               "    5: sharpen5\n"
               "  cscale=<n>\n"
               "    as lscale but for chroma (2x slower with little visible effect).\n"
               "  filter-strength=<value>\n"
               "    set the effect strength for some sharpen4/sharpen5 filters\n"
               "  mipmapgen\n"
               "    generate mipmaps for the video image (helps with downscaling)\n"
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

    p->scalers[0].name = talloc_strdup(vo, lscale);
    p->scalers[1].name = talloc_strdup(vo, cscale);
    free(lscale);
    free(cscale);

    if (!handle_scaler_opt(vo, &p->scalers[0]))
        goto err_out;
    if (!handle_scaler_opt(vo, &p->scalers[1]))
        goto err_out;

    p->glctx = init_mpglcontext(gltype, vo);
    if (!p->glctx)
        goto err_out;
    p->gl = p->glctx->gl;

    if (true) {
        if (create_window(vo, 320, 200, VOFLAG_HIDDEN) == SET_WINDOW_FAILED)
            goto err_out;
        if (!initGL(vo))
            goto err_out;
        // We created a window to test whether the GL context supports hardware
        // acceleration and so on. Destroy that window to make sure all state
        // associated with it is lost.
        uninit(vo);
        p->glctx = init_mpglcontext(gltype, vo);
        if (!p->glctx)
            goto err_out;
        p->gl = p->glctx->gl;
    }

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
            r->ml = r->mr = p->border_x;
            r->mt = r->mb = p->border_y;
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
        resize(vo);
        return VO_TRUE;
    case VOCTRL_BORDER:
        if (!p->glctx->border)
            break;
        p->glctx->border(vo);
        resize(vo);
        return VO_TRUE;
    case VOCTRL_GET_PANSCAN:
        return VO_TRUE;
    case VOCTRL_SET_PANSCAN:
        resize(vo);
        return VO_TRUE;
    case VOCTRL_GET_EQUALIZER: {
        struct voctrl_get_equalizer_args *args = data;
        return mp_csp_equalizer_get(&p->video_eq, args->name, args->valueptr)
                >= 0 ? VO_TRUE : VO_NOTIMPL;
    }
    case VOCTRL_SET_EQUALIZER: {
        struct voctrl_set_equalizer_args *args = data;
        if (mp_csp_equalizer_set(&p->video_eq, args->name, args->value) < 0)
            return VO_NOTIMPL;
        update_all_uniforms(vo);
        vo->want_redraw = true;
        return VO_TRUE;
    }
    case VOCTRL_SET_YUV_COLORSPACE: {
        if (p->is_yuv) {
            p->colorspace = *(struct mp_csp_details *)data;
            update_all_uniforms(vo);
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
    case VOCTRL_SCREENSHOT: {
        struct voctrl_screenshot_args *args = data;
        if (args->full_window)
            args->out_image = get_window_screenshot(vo);
        else
            args->out_image = get_screenshot(vo);
        return true;
    }
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
