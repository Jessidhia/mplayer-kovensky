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

#ifndef MPLAYER_STHEADER_H
#define MPLAYER_STHEADER_H

#include <stdbool.h>

#include "aviheader.h"
#include "ms_hdr.h"
struct MPOpts;
struct demuxer;

// Stream headers:

#define SH_COMMON                                                       \
    struct MPOpts *opts;                                                \
    struct demux_stream *ds;                                            \
    struct codecs *codec;                                               \
    unsigned int format;                                                \
    int initialized;                                                    \
    /* number of seconds stream should be delayed                       \
     * (according to dwStart or similar) */                             \
    float stream_delay;                                                 \
    /* things needed for parsing */                                     \
    bool needs_parsing;                                                 \
    struct AVCodecContext *avctx;                                       \
    struct AVCodecParserContext *parser;                                \
    /* audio: last known pts value in output from decoder               \
     * video: predicted/interpolated PTS of the current frame */        \
    double pts;                                                         \
    /* decoder context */                                               \
    void *context;                                                      \
    char *lang;   /* track language */                                  \
    char *title;  /* track title */                                     \
    bool default_track;                                                 \

typedef struct sh_common {
    SH_COMMON
} sh_common_t;

typedef struct sh_audio {
    SH_COMMON
    int aid;
    // output format:
    int sample_format;
    int samplerate;
    int container_out_samplerate;
    int samplesize;
    int channels;
    int o_bps; // == samplerate*samplesize*channels   (uncompr. bytes/sec)
    int i_bps; // == bitrate  (compressed bytes/sec)
    // in buffers:
    int audio_in_minsize;   // initial size to allocate for a_in_buffer if any
    char *a_in_buffer;      // input buffer used by some decoders
    int a_in_buffer_len;
    int a_in_buffer_size;
    // decoder buffers:
    int audio_out_minsize;  // minimal output from decoder may be this much
    char *a_buffer;         // buffer for decoder output
    int a_buffer_len;
    int a_buffer_size;
    struct af_stream *afilter;          // the audio filter stream
    const struct ad_functions *ad_driver;
#ifdef CONFIG_DYNAMIC_PLUGINS
    void *dec_handle;
#endif
    // win32-compatible codec parameters:
    AVIStreamHeader audio;
    WAVEFORMATEX *wf;
    // note codec extradata may be either under "wf" or "codecdata"
    unsigned char *codecdata;
    int codecdata_len;
    int pts_bytes;   // bytes output by decoder after last known pts
} sh_audio_t;

typedef struct sh_video {
    SH_COMMON
    int vid;
    float timer;     // absolute time in video stream, since last start/seek
    // frame counters:
    float num_frames;       // number of frames played
    int num_frames_decoded; // number of frames decoded
    double i_pts;   // PTS for the _next_ I/P frame (internal mpeg demuxing)
    float next_frame_time;
    double last_pts;
    double buffered_pts[32];
    int num_buffered_pts;
    double codec_reordered_pts;
    double prev_codec_reordered_pts;
    int num_reordered_pts_problems;
    double sorted_pts;
    double prev_sorted_pts;
    int num_sorted_pts_problems;
    int pts_assoc_mode;
    // output format: (set by demuxer)
    float fps;            // frames per second (set only if constant fps)
    float frametime;      // 1/fps
    float aspect;         // aspect ratio stored in the file (for prescaling)
    float stream_aspect;  // aspect ratio in media headers (DVD IFO files)
    int i_bps;            // == bitrate  (compressed bytes/sec)
    int disp_w, disp_h;   // display size (filled by demuxer)
    // output driver/filters: (set by libmpcodecs core)
    unsigned int outfmt;
    unsigned int outfmtidx;
    struct vf_instance *vfilter;  // video filter chain
    int output_flags;       // query_format() results for output filters+vo
    const struct vd_functions *vd_driver;
    int vf_initialized;   // -1 failed, 0 not done, 1 done
#ifdef CONFIG_DYNAMIC_PLUGINS
    void *dec_handle;
#endif
    // win32-compatible codec parameters:
    AVIStreamHeader video;
    BITMAPINFOHEADER *bih;
    void *ImageDesc; // for quicktime codecs
} sh_video_t;

typedef struct sh_sub {
    SH_COMMON
    int sid;
    char type;  // t = text, v = VobSub, a = SSA/ASS, m, x, b, d, p
    bool active; // after track switch decoder may stay initialized, not active
    unsigned char *extradata;   // extra header data passed from demuxer
    int extradata_len;
    const struct sd_functions *sd_driver;
} sh_sub_t;

// demuxer.c:
#define new_sh_audio(d, i) new_sh_audio_aid(d, i, i)
struct sh_audio *new_sh_audio_aid(struct demuxer *demuxer, int id, int aid);
#define new_sh_video(d, i) new_sh_video_vid(d, i, i)
struct sh_video *new_sh_video_vid(struct demuxer *demuxer, int id, int vid);
#define new_sh_sub(d, i) new_sh_sub_sid(d, i, i)
struct sh_sub *new_sh_sub_sid(struct demuxer *demuxer, int id, int sid);
struct sh_sub *new_sh_sub_sid_lang(struct demuxer *demuxer, int id, int sid,
                                   const char *lang);
void free_sh_audio(struct demuxer *demuxer, int id);
void free_sh_video(struct sh_video *sh);

const char *sh_sub_type2str(int type);

// video.c:
int video_read_properties(struct sh_video *sh_video);
int video_read_frame(struct sh_video *sh_video, float *frame_time_ptr,
                     unsigned char **start, int force_fps);

#endif /* MPLAYER_STHEADER_H */
