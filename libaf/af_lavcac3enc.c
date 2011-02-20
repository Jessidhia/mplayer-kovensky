/*
 * audio filter for runtime AC-3 encoding with libavcodec.
 *
 * Copyright (C) 2007 Ulion <ulion A gmail P com>
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
#include <inttypes.h>
#include <assert.h>

#include "libmpcodecs/vd_ffmpeg.h"
#include "config.h"
#include "af.h"
#include "reorder_ch.h"

#include "libavcodec/avcodec.h"
#include "ffmpeg_files/intreadwrite.h"

#define AC3_MAX_CHANNELS 6
#define AC3_MAX_CODED_FRAME_SIZE 3840
#define AC3_FRAME_SIZE (6  * 256)
const uint16_t ac3_bitrate_tab[19] = {
    32, 40, 48, 56, 64, 80, 96, 112, 128,
    160, 192, 224, 256, 320, 384, 448, 512, 576, 640
};

// Data for specific instances of this filter
typedef struct af_ac3enc_s {
    struct AVCodec        *lavc_acodec;
    struct AVCodecContext *lavc_actx;
    int add_iec61937_header;
    int bit_rate;
    int pending_data_size;
    char *pending_data;
    int pending_len;
    int expect_len;
    int min_channel_num;
    int in_sampleformat;
} af_ac3enc_t;

// Initialization and runtime control
static int control(struct af_instance_s *af, int cmd, void *arg)
{
    af_ac3enc_t *s  = (af_ac3enc_t *)af->setup;
    af_data_t *data = (af_data_t *)arg;
    int i, bit_rate, test_output_res;
    static const int default_bit_rate[AC3_MAX_CHANNELS+1] = \
        {0, 96000, 192000, 256000, 384000, 448000, 448000};

    switch (cmd){
    case AF_CONTROL_REINIT:
        if (AF_FORMAT_IS_AC3(data->format) || data->nch < s->min_channel_num)
            return AF_DETACH;

        af->data->format = s->in_sampleformat;
        af->data->bps = af_fmt2bits(s->in_sampleformat) / 8;
        if (data->rate == 48000 || data->rate == 44100 || data->rate == 32000)
            af->data->rate = data->rate;
        else
            af->data->rate = 48000;
        if (data->nch > AC3_MAX_CHANNELS)
            af->data->nch = AC3_MAX_CHANNELS;
        else
            af->data->nch = data->nch;
        test_output_res = af_test_output(af, data);

        s->pending_len = 0;
        s->expect_len = AC3_FRAME_SIZE * data->nch * af->data->bps;
        assert(s->expect_len <= s->pending_data_size);
        if (s->add_iec61937_header)
            af->mul = (double)AC3_FRAME_SIZE * 2 * 2 / s->expect_len;
        else
            af->mul = (double)AC3_MAX_CODED_FRAME_SIZE / s->expect_len;

        mp_msg(MSGT_AFILTER, MSGL_DBG2, "af_lavcac3enc reinit: %d, %d, %f, %d.\n",
               data->nch, data->rate, af->mul, s->expect_len);

        bit_rate = s->bit_rate ? s->bit_rate : default_bit_rate[af->data->nch];

        if (s->lavc_actx->channels != af->data->nch ||
                s->lavc_actx->sample_rate != af->data->rate ||
                s->lavc_actx->bit_rate != bit_rate) {

            if (s->lavc_actx->codec)
                avcodec_close(s->lavc_actx);

            // Put sample parameters
            s->lavc_actx->channels = af->data->nch;
            s->lavc_actx->sample_rate = af->data->rate;
            s->lavc_actx->bit_rate = bit_rate;

            if(avcodec_open(s->lavc_actx, s->lavc_acodec) < 0) {
                mp_tmsg(MSGT_AFILTER, MSGL_ERR, "Couldn't open codec %s, br=%d.\n", "ac3", bit_rate);
                return AF_ERROR;
            }
        }
        if (s->lavc_actx->frame_size != AC3_FRAME_SIZE) {
            mp_msg(MSGT_AFILTER, MSGL_ERR, "lavcac3enc: unexpected ac3 "
                   "encoder frame size %d\n", s->lavc_actx->frame_size);
            return AF_ERROR;
        }
        af->data->format = AF_FORMAT_AC3_BE;
        af->data->bps = 2;
        af->data->nch = 2;
        return test_output_res;
    case AF_CONTROL_COMMAND_LINE:
        mp_msg(MSGT_AFILTER, MSGL_DBG2, "af_lavcac3enc cmdline: %s.\n", (char*)arg);
        s->bit_rate = 0;
        s->min_channel_num = 0;
        s->add_iec61937_header = 0;
        sscanf((char*)arg,"%d:%d:%d", &s->add_iec61937_header, &s->bit_rate,
               &s->min_channel_num);
        if (s->bit_rate < 1000)
            s->bit_rate *= 1000;
        if (s->bit_rate) {
            for (i = 0; i < 19; ++i)
                if (ac3_bitrate_tab[i] * 1000 == s->bit_rate)
                    break;
            if (i >= 19) {
                mp_msg(MSGT_AFILTER, MSGL_WARN, "af_lavcac3enc unable set unsupported "
                       "bitrate %d, use default bitrate (check manpage to see "
                       "supported bitrates).\n", s->bit_rate);
                s->bit_rate = 0;
            }
        }
        if (s->min_channel_num == 0)
            s->min_channel_num = 5;
        mp_msg(MSGT_AFILTER, MSGL_V, "af_lavcac3enc config spdif:%d, bitrate:%d, "
               "minchnum:%d.\n", s->add_iec61937_header, s->bit_rate,
               s->min_channel_num);
        return AF_OK;
    }
    return AF_UNKNOWN;
}

// Deallocate memory
static void uninit(struct af_instance_s* af)
{
    if (af->data)
        free(af->data->audio);
    free(af->data);
    if (af->setup) {
        af_ac3enc_t *s = af->setup;
        af->setup = NULL;
        if(s->lavc_actx) {
            if (s->lavc_actx->codec)
                avcodec_close(s->lavc_actx);
            free(s->lavc_actx);
        }
        free(s->pending_data);
        free(s);
    }
}

// Filter data through filter
static af_data_t* play(struct af_instance_s* af, af_data_t* data)
{
    af_ac3enc_t *s = af->setup;
    af_data_t *c = data;    // Current working data
    af_data_t *l;
    int len, left, outsize = 0, destsize;
    char *buf, *src, *dest;
    int max_output_len;
    int frame_num = (data->len + s->pending_len) / s->expect_len;

    if (s->add_iec61937_header)
        max_output_len = AC3_FRAME_SIZE * 2 * 2 * frame_num;
    else
        max_output_len = AC3_MAX_CODED_FRAME_SIZE * frame_num;

    if (af->data->len < max_output_len) {
        mp_msg(MSGT_AFILTER, MSGL_V, "[libaf] Reallocating memory in module %s, "
               "old len = %i, new len = %i\n", af->info->name, af->data->len,
                max_output_len);
        free(af->data->audio);
        af->data->audio = malloc(max_output_len);
        if (!af->data->audio) {
            mp_msg(MSGT_AFILTER, MSGL_FATAL, "[libaf] Could not allocate memory \n");
            return NULL;
        }
        af->data->len = max_output_len;
    }

    l = af->data;           // Local data
    buf = (char *)l->audio;
    src = (char *)c->audio;
    left = c->len;


    while (left > 0) {
        if (left + s->pending_len < s->expect_len) {
            memcpy(s->pending_data + s->pending_len, src, left);
            src += left;
            s->pending_len += left;
            left = 0;
            break;
        }

        dest = s->add_iec61937_header ? buf + 8 : buf;
        destsize = (char *)l->audio + l->len - buf;

        if (s->pending_len) {
            int needs = s->expect_len - s->pending_len;
            if (needs > 0) {
                memcpy(s->pending_data + s->pending_len, src, needs);
                src += needs;
                left -= needs;
            }

            if (c->nch >= 5)
                reorder_channel_nch(s->pending_data,
                                    AF_CHANNEL_LAYOUT_MPLAYER_DEFAULT,
                                    AF_CHANNEL_LAYOUT_LAVC_DEFAULT,
                                    c->nch,
                                    s->expect_len / 2, 2);

            len = avcodec_encode_audio(s->lavc_actx, dest, destsize,
                                       (void *)s->pending_data);
            s->pending_len = 0;
        }
        else {
            if (c->nch >= 5)
                reorder_channel_nch(src,
                                    AF_CHANNEL_LAYOUT_MPLAYER_DEFAULT,
                                    AF_CHANNEL_LAYOUT_LAVC_DEFAULT,
                                    c->nch,
                                    s->expect_len / 2, 2);
            len = avcodec_encode_audio(s->lavc_actx,dest,destsize,(void *)src);
            src += s->expect_len;
            left -= s->expect_len;
        }
        mp_msg(MSGT_AFILTER, MSGL_DBG2, "avcodec_encode_audio got %d, pending %d.\n",
               len, s->pending_len);

        if (s->add_iec61937_header) {
            int bsmod = dest[5] & 0x7;

            AV_WB16(buf,     0xF872);   // iec 61937 syncword 1
            AV_WB16(buf + 2, 0x4E1F);   // iec 61937 syncword 2
            buf[4] = bsmod;             // bsmod
            buf[5] = 0x01;              // data-type ac3
            AV_WB16(buf + 6, len << 3); // number of bits in payload

            memset(buf + 8 + len, 0, AC3_FRAME_SIZE * 2 * 2 - 8 - len);
            len = AC3_FRAME_SIZE * 2 * 2;
        }

        outsize += len;
        buf += len;
    }
    c->audio = l->audio;
    c->nch   = 2;
    c->bps   = 2;
    c->len   = outsize;
    mp_msg(MSGT_AFILTER, MSGL_DBG2, "play return size %d, pending %d\n",
           outsize, s->pending_len);
    return c;
}

static int af_open(af_instance_t* af){

    af_ac3enc_t *s = calloc(1,sizeof(af_ac3enc_t));
    af->control=control;
    af->uninit=uninit;
    af->play=play;
    af->mul=1;
    af->data=calloc(1,sizeof(af_data_t));
    af->setup=s;

    init_avcodec();

    s->lavc_acodec = avcodec_find_encoder_by_name("ac3");
    if (!s->lavc_acodec) {
        mp_tmsg(MSGT_AFILTER, MSGL_ERR, "Audio LAVC, couldn't find encoder for codec %s.\n", "ac3");
        return AF_ERROR;
    }

    s->lavc_actx = avcodec_alloc_context();
    if (!s->lavc_actx) {
        mp_tmsg(MSGT_AFILTER, MSGL_ERR, "Audio LAVC, couldn't allocate context!\n");
        return AF_ERROR;
    }
    // using deprecated SampleFormat/FMT, AV* versions only added in 2010-11
    const enum SampleFormat *fmts = s->lavc_acodec->sample_fmts;
    for (int i = 0; ; i++) {
        if (fmts[i] == SAMPLE_FMT_NONE) {
            mp_msg(MSGT_AFILTER, MSGL_ERR, "Audio LAVC, encoder doesn't "
                   "support expected sample formats!\n");
            return AF_ERROR;
        } else if (fmts[i] == SAMPLE_FMT_S16) {
            s->in_sampleformat = AF_FORMAT_S16_NE;
            s->lavc_actx->sample_fmt = fmts[i];
            break;
        } else if (fmts[i] == SAMPLE_FMT_FLT) {
            s->in_sampleformat = AF_FORMAT_FLOAT_NE;
            s->lavc_actx->sample_fmt = fmts[i];
            break;
        }
    }
    char buf[100];
    mp_msg(MSGT_AFILTER, MSGL_V, "[af_lavcac3enc]: in sample format: %s\n",
           af_fmt2str(s->in_sampleformat, buf, 100));
    s->pending_data_size = AF_NCH * AC3_FRAME_SIZE *
        af_fmt2bits(s->in_sampleformat) / 8;
    s->pending_data = malloc(s->pending_data_size);

    return AF_OK;
}

af_info_t af_info_lavcac3enc = {
    "runtime encode to ac3 using libavcodec",
    "lavcac3enc",
    "Ulion",
    "",
    AF_FLAGS_REENTRANT,
    af_open
};
