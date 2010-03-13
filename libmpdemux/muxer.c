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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>

#include "config.h"
#include "aviheader.h"
#include "ms_hdr.h"

#include "stream/stream.h"
#include "muxer.h"
#include "demuxer.h"
#include "mp_msg.h"
#include "stheader.h"

muxer_t *muxer_new_muxer(int type,stream_t *stream){
    muxer_t* muxer=calloc(1,sizeof(muxer_t));
    if(!muxer)
        return NULL;
    muxer->stream = stream;
    switch (type) {
      case MUXER_TYPE_MPEG:
	if(! muxer_init_muxer_mpeg(muxer))
	  goto fail;
	break;
      case MUXER_TYPE_RAWVIDEO:
        if(! muxer_init_muxer_rawvideo(muxer))
	  goto fail;
	break;
      case MUXER_TYPE_RAWAUDIO:
        if(! muxer_init_muxer_rawaudio(muxer))
	  goto fail;
        break;
#ifdef CONFIG_LIBAVFORMAT
      case MUXER_TYPE_LAVF:
        if(! muxer_init_muxer_lavf(muxer))
	  goto fail;
        break;
#endif
      case MUXER_TYPE_AVI:
      default:
	if(! muxer_init_muxer_avi(muxer))
	  goto fail;
    }
    return muxer;

fail:
    free(muxer);
    return NULL;
}

/* buffer frames until we either:
 * (a) have at least one frame from each stream
 * (b) run out of memory */
void muxer_write_chunk(muxer_stream_t *s, size_t len, unsigned int flags, double dts, double pts) {
    if(dts == MP_NOPTS_VALUE) dts= s->timer;
    if(pts == MP_NOPTS_VALUE) pts= s->timer; // this is wrong

    if (s->muxer->muxbuf_skip_buffer) {
      s->muxer->cont_write_chunk(s, len, flags, dts, pts);
    }
    else {
      int num = s->muxer->muxbuf_num++;
      muxbuf_t *buf, *tmp;

      tmp = realloc_struct(s->muxer->muxbuf, (num+1), sizeof(muxbuf_t));
      if(!tmp) {
        mp_tmsg(MSGT_MUXER, MSGL_FATAL, "Muxer frame buffer cannot reallocate memory!\n");
        return;
      }
      s->muxer->muxbuf = tmp;
      buf = s->muxer->muxbuf + num;

      /* buffer this frame */
      buf->stream = s;
      buf->dts= dts;
      buf->pts= pts;
      buf->len = len;
      buf->flags = flags;
      buf->buffer = malloc(len);
      if (!buf->buffer) {
        mp_tmsg(MSGT_MUXER, MSGL_FATAL, "Muxer frame buffer cannot allocate memory!\n");
        return;
      }
      memcpy(buf->buffer, s->buffer, buf->len);
      s->muxbuf_seen = 1;

      /* see if we need to keep buffering */
      s->muxer->muxbuf_skip_buffer = 1;
      for (num = 0; s->muxer->streams[num]; ++num)
        if (!s->muxer->streams[num]->muxbuf_seen)
          s->muxer->muxbuf_skip_buffer = 0;

      /* see if we can flush buffer now */
      if (s->muxer->muxbuf_skip_buffer) {
        mp_tmsg(MSGT_MUXER, MSGL_V, "Muxer frame buffer sending %d frame(s) to the muxer.\n", s->muxer->muxbuf_num);

        /* fix parameters for all streams */
        for (num = 0; s->muxer->streams[num]; ++num) {
          muxer_stream_t *str = s->muxer->streams[num];
          if(str->muxer->fix_stream_parameters)
            muxer_stream_fix_parameters(str->muxer, str);
        }

        /* write header */
        if (s->muxer->cont_write_header)
          muxer_write_header(s->muxer);

        /* send all buffered frames to muxer */
        for (num = 0; num < s->muxer->muxbuf_num; ++num) {
          muxbuf_t tmp_buf;
          buf = s->muxer->muxbuf + num;
          s = buf->stream;

          /* 1. save timer and buffer (might have changed by now) */
          tmp_buf.dts = s->timer;
          tmp_buf.buffer = s->buffer;

          /* 2. move stored timer and buffer into stream and mux it */
          s->timer = buf->dts;
          s->buffer = buf->buffer;
          s->muxer->cont_write_chunk(s, buf->len, buf->flags, buf->dts, buf->pts);

          /* 3. restore saved timer and buffer */
          s->timer = tmp_buf.dts;
          s->buffer = tmp_buf.buffer;
        }

        free(s->muxer->muxbuf);
        s->muxer->muxbuf_num = 0;
      }
    }

    /* this code moved directly from muxer_avi.c */
    // alter counters:
    if(s->h.dwSampleSize){
      // CBR
      s->h.dwLength+=len/s->h.dwSampleSize;
      if(len%s->h.dwSampleSize) mp_tmsg(MSGT_MUXER, MSGL_WARN, "Warning, len isn't divisible by samplesize!\n");
    } else {
      // VBR
      s->h.dwLength++;
    }
    s->timer=(double)s->h.dwLength*s->h.dwScale/s->h.dwRate;
    s->size+=len;

    return;
}
