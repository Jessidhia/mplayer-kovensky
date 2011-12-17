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

#ifndef MPLAYER_VF_H
#define MPLAYER_VF_H

#include "mp_image.h"
#include "mpcommon.h"
#include "stdbool.h"

#include "mpc_info.h"
#include "vfcap.h"

struct MPOpts;
struct vf_instance;
struct vf_priv_s;

typedef struct vf_info {
    const char *info;
    const char *name;
    const char *author;
    const char *comment;
    int (*vf_open)(struct vf_instance *vf, char *args);
    // Ptr to a struct dscribing the options
    const void *opts;
} vf_info_t;

#define NUM_NUMBERED_MPI 50

struct vf_image_context {
    mp_image_t *static_images[2];
    mp_image_t *temp_images[1];
    mp_image_t *export_images[1];
    mp_image_t *numbered_images[NUM_NUMBERED_MPI];
    int static_idx;
};

struct vf_format_context {
    int have_configured;
    int orig_width, orig_height, orig_fmt;
};

typedef struct vf_instance {
    const vf_info_t *info;
    // funcs:
    int (*config)(struct vf_instance *vf,
                  int width, int height, int d_width, int d_height,
                  unsigned int flags, unsigned int outfmt);
    int (*control)(struct vf_instance *vf, int request, void *data);
    int (*query_format)(struct vf_instance *vf, unsigned int fmt);
    void (*get_image)(struct vf_instance *vf, mp_image_t *mpi);
    int (*put_image)(struct vf_instance *vf, mp_image_t *mpi, double pts);
    void (*start_slice)(struct vf_instance *vf, mp_image_t *mpi);
    void (*draw_slice)(struct vf_instance *vf, unsigned char **src,
                       int *stride, int w, int h, int x, int y);
    void (*uninit)(struct vf_instance *vf);

    int (*continue_buffered_image)(struct vf_instance *vf);
    // caps:
    unsigned int default_caps; // used by default query_format()
    unsigned int default_reqs; // used by default config()
    // data:
    int w, h;
    struct vf_image_context imgctx;
    struct vf_format_context fmt;
    struct vf_instance *next;
    mp_image_t *dmpi;
    struct vf_priv_s *priv;
    struct MPOpts *opts;
} vf_instance_t;

typedef struct vf_seteq {
    const char *item;
    int value;
} vf_equalizer_t;

struct vf_ctrl_screenshot {
    // When the screenshot is complete, pass it to this callback.
    void (*image_callback)(void *, mp_image_t *);
    void *image_callback_ctx;
};

#define VFCTRL_QUERY_MAX_PP_LEVEL 4 // query max postprocessing level (if any)
#define VFCTRL_SET_PP_LEVEL 5       // set postprocessing level
#define VFCTRL_SET_EQUALIZER 6 // set color options (brightness,contrast etc)
#define VFCTRL_GET_EQUALIZER 8 // get color options (brightness,contrast etc)
#define VFCTRL_DRAW_OSD 7
#define VFCTRL_CHANGE_RECTANGLE 9  // vf_rectangle control
#define VFCTRL_DUPLICATE_FRAME 11  // For encoding - encode zero-change frame
#define VFCTRL_SKIP_NEXT_FRAME 12  // For encoding - drop the next frame that passes thru
#define VFCTRL_FLUSH_FRAMES    13  // For encoding - flush delayed frames
#define VFCTRL_SCREENSHOT      14  // Take screenshot, arg is vf_ctrl_screenshot
#define VFCTRL_INIT_EOSD       15  // Select EOSD renderer
#define VFCTRL_DRAW_EOSD       16  // Render EOSD */
#define VFCTRL_SET_DEINTERLACE 18  // Set deinterlacing status
#define VFCTRL_GET_DEINTERLACE 19  // Get deinterlacing status
/* Hack to make the OSD state object available to vf_expand and vf_ass which
 * access OSD/subtitle state outside of normal OSD draw time. */
#define VFCTRL_SET_OSD_OBJ 20
#define VFCTRL_SET_YUV_COLORSPACE 22 // arg is struct mp_csp_details*
#define VFCTRL_GET_YUV_COLORSPACE 23 // arg is struct mp_csp_details*

// functions:
void vf_mpi_clear(mp_image_t *mpi, int x0, int y0, int w, int h);
mp_image_t *vf_get_image(vf_instance_t *vf, unsigned int outfmt,
                         int mp_imgtype, int mp_imgflag, int w, int h);

vf_instance_t *vf_open_plugin(struct MPOpts *opts,
        const vf_info_t * const *filter_list, vf_instance_t *next,
        const char *name, char **args);
struct vf_instance *vf_open_plugin_noerr(struct MPOpts *opts,
        const vf_info_t *const *filter_list, vf_instance_t *next,
        const char *name, char **args, int *retcode);
vf_instance_t *vf_open_filter(struct MPOpts *opts, vf_instance_t *next,
                              const char *name, char **args);
vf_instance_t *vf_add_before_vo(vf_instance_t **vf, char *name, char **args);
vf_instance_t *vf_open_encoder(struct MPOpts *opts, vf_instance_t *next,
                               const char *name, char *args);

unsigned int vf_match_csp(vf_instance_t **vfp, const unsigned int *list,
                          unsigned int preferred);
void vf_clone_mpi_attributes(mp_image_t *dst, mp_image_t *src);
void vf_queue_frame(vf_instance_t *vf, int (*)(vf_instance_t *));
int vf_output_queued_frame(vf_instance_t *vf);

// default wrappers:
int vf_next_config(struct vf_instance *vf,
                   int width, int height, int d_width, int d_height,
                   unsigned int flags, unsigned int outfmt);
int vf_next_control(struct vf_instance *vf, int request, void *data);
int vf_next_query_format(struct vf_instance *vf, unsigned int fmt);
int vf_next_put_image(struct vf_instance *vf, mp_image_t *mpi, double pts);
void vf_next_draw_slice(struct vf_instance *vf, unsigned char **src,
                        int *stride, int w, int h, int x, int y);

struct m_obj_settings;
vf_instance_t *append_filters(vf_instance_t *last,
                              struct m_obj_settings *vf_settings);

void vf_uninit_filter(vf_instance_t *vf);
void vf_uninit_filter_chain(vf_instance_t *vf);

int vf_config_wrapper(struct vf_instance *vf,
                      int width, int height, int d_width, int d_height,
                      unsigned int flags, unsigned int outfmt);

static inline int norm_qscale(int qscale, int type)
{
    switch (type) {
    case 0: // MPEG-1
        return qscale;
    case 1: // MPEG-2
        return qscale >> 1;
    case 2: // H264
        return qscale >> 2;
    case 3: // VP56
        return (63 - qscale + 2) >> 2;
    }
    return qscale;
}

struct vf_detc_pts_buf {
    double inpts_prev, outpts_prev;
    double lastdelta;
};
void vf_detc_init_pts_buf(struct vf_detc_pts_buf *p);
/* Adjust pts when detelecining.
 * skip_frame: do not render this frame
 * reset_pattern: set to 1 if the telecine pattern has reset due to scene cut
 */
double vf_detc_adjust_pts(struct vf_detc_pts_buf *p, double pts,
                          bool reset_pattern, bool skip_frame);
double vf_softpulldown_adjust_pts(struct vf_detc_pts_buf *p, double pts,
                                  bool reset_pattern, bool skip_frame,
                                  int last_frame_duration);

#endif /* MPLAYER_VF_H */
