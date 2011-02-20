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
#include <unistd.h>

#include "config.h"
#include "mp_msg.h"
#include "options.h"

#include "ad_internal.h"
#include "vd_ffmpeg.h"
#include "libaf/reorder_ch.h"

#include "mpbswap.h"

static const ad_info_t info =
{
	"FFmpeg/libavcodec audio decoders",
	"ffmpeg",
	"Nick Kurshev",
	"ffmpeg.sf.net",
	""
};

LIBAD_EXTERN(ffmpeg)

#define assert(x)

#include "libavcodec/avcodec.h"


static int preinit(sh_audio_t *sh)
{
  sh->audio_out_minsize=AVCODEC_MAX_AUDIO_FRAME_SIZE;
  return 1;
}

/* Prefer playing audio with the samplerate given in container data
 * if available, but take number the number of channels and sample format
 * from the codec, since if the codec isn't using the correct values for
 * those everything breaks anyway.
 */
static int setup_format(sh_audio_t *sh_audio, const AVCodecContext *lavc_context)
{
    int sample_format = sh_audio->sample_format;
    switch (lavc_context->sample_fmt) {
        case SAMPLE_FMT_U8:  sample_format = AF_FORMAT_U8;       break;
        case SAMPLE_FMT_S16: sample_format = AF_FORMAT_S16_NE;   break;
        case SAMPLE_FMT_S32: sample_format = AF_FORMAT_S32_NE;   break;
        case SAMPLE_FMT_FLT: sample_format = AF_FORMAT_FLOAT_NE; break;
        default:
            mp_msg(MSGT_DECAUDIO, MSGL_FATAL, "Unsupported sample format\n");
    }

    bool broken_srate        = false;
    int samplerate           = lavc_context->sample_rate;
    int container_samplerate = sh_audio->container_out_samplerate;
    if (!container_samplerate && sh_audio->wf)
        container_samplerate = sh_audio->wf->nSamplesPerSec;
    if (lavc_context->codec_id == CODEC_ID_AAC
        && samplerate == 2 * container_samplerate)
        broken_srate = true;
    else if (container_samplerate)
        samplerate = container_samplerate;

    if (lavc_context->channels != sh_audio->channels ||
        samplerate != sh_audio->samplerate ||
        sample_format != sh_audio->sample_format) {
        sh_audio->channels=lavc_context->channels;
        sh_audio->samplerate=samplerate;
        sh_audio->sample_format = sample_format;
        sh_audio->samplesize=af_fmt2bits(sh_audio->sample_format)/ 8;
        if (broken_srate)
            mp_msg(MSGT_DECAUDIO, MSGL_WARN,
                   "Ignoring broken container sample rate for AAC with SBR\n");
        return 1;
    }
    return 0;
}

static int init(sh_audio_t *sh_audio)
{
    struct MPOpts *opts = sh_audio->opts;
    int tries = 0;
    int x;
    AVCodecContext *lavc_context;
    AVCodec *lavc_codec;

    mp_msg(MSGT_DECAUDIO,MSGL_V,"FFmpeg's libavcodec audio codec\n");
    init_avcodec();

    lavc_codec = avcodec_find_decoder_by_name(sh_audio->codec->dll);
    if(!lavc_codec){
	mp_tmsg(MSGT_DECAUDIO,MSGL_ERR,"Cannot find codec '%s' in libavcodec...\n",sh_audio->codec->dll);
	return 0;
    }

    lavc_context = avcodec_alloc_context();
    sh_audio->context=lavc_context;

    lavc_context->drc_scale = opts->drc_level;
    lavc_context->sample_rate = sh_audio->samplerate;
    lavc_context->bit_rate = sh_audio->i_bps * 8;
    if(sh_audio->wf){
	lavc_context->channels = sh_audio->wf->nChannels;
	lavc_context->sample_rate = sh_audio->wf->nSamplesPerSec;
	lavc_context->bit_rate = sh_audio->wf->nAvgBytesPerSec * 8;
	lavc_context->block_align = sh_audio->wf->nBlockAlign;
	lavc_context->bits_per_coded_sample = sh_audio->wf->wBitsPerSample;
    }
    lavc_context->request_channels = opts->audio_output_channels;
    lavc_context->codec_tag = sh_audio->format; //FOURCC
    lavc_context->codec_type = CODEC_TYPE_AUDIO;
    lavc_context->codec_id = lavc_codec->id; // not sure if required, imho not --A'rpi

    /* alloc extra data */
    if (sh_audio->wf && sh_audio->wf->cbSize > 0) {
        lavc_context->extradata = av_mallocz(sh_audio->wf->cbSize + FF_INPUT_BUFFER_PADDING_SIZE);
        lavc_context->extradata_size = sh_audio->wf->cbSize;
        memcpy(lavc_context->extradata, sh_audio->wf + 1,
               lavc_context->extradata_size);
    }

    // for QDM2
    if (sh_audio->codecdata_len && sh_audio->codecdata && !lavc_context->extradata)
    {
        lavc_context->extradata = av_malloc(sh_audio->codecdata_len +
                                            FF_INPUT_BUFFER_PADDING_SIZE);
        lavc_context->extradata_size = sh_audio->codecdata_len;
        memcpy(lavc_context->extradata, (char *)sh_audio->codecdata,
               lavc_context->extradata_size);
    }

    /* open it */
    if (avcodec_open(lavc_context, lavc_codec) < 0) {
        mp_tmsg(MSGT_DECAUDIO,MSGL_ERR, "Could not open codec.\n");
        return 0;
    }
   mp_msg(MSGT_DECAUDIO,MSGL_V,"INFO: libavcodec \"%s\" init OK!\n", lavc_codec->name);

//   printf("\nFOURCC: 0x%X\n",sh_audio->format);
   if(sh_audio->format==0x3343414D){
       // MACE 3:1
       sh_audio->ds->ss_div = 2*3; // 1 samples/packet
       sh_audio->ds->ss_mul = 2*sh_audio->wf->nChannels; // 1 byte*ch/packet
   } else
   if(sh_audio->format==0x3643414D){
       // MACE 6:1
       sh_audio->ds->ss_div = 2*6; // 1 samples/packet
       sh_audio->ds->ss_mul = 2*sh_audio->wf->nChannels; // 1 byte*ch/packet
   }

   // Decode at least 1 byte:  (to get header filled)
   do {
       x=decode_audio(sh_audio,sh_audio->a_buffer,1,sh_audio->a_buffer_size);
   } while (x <= 0 && tries++ < 5);
   if(x>0) sh_audio->a_buffer_len=x;

  sh_audio->i_bps=lavc_context->bit_rate/8;
  if (sh_audio->wf && sh_audio->wf->nAvgBytesPerSec)
      sh_audio->i_bps=sh_audio->wf->nAvgBytesPerSec;

  switch (lavc_context->sample_fmt) {
      case SAMPLE_FMT_U8:
      case SAMPLE_FMT_S16:
      case SAMPLE_FMT_S32:
      case SAMPLE_FMT_FLT:
          break;
      default:
          return 0;
  }
  return 1;
}

static void uninit(sh_audio_t *sh)
{
    AVCodecContext *lavc_context = sh->context;

    if (avcodec_close(lavc_context) < 0)
	mp_tmsg(MSGT_DECVIDEO, MSGL_ERR, "Could not close codec.\n");
    av_freep(&lavc_context->extradata);
    av_freep(&lavc_context);
}

static int control(sh_audio_t *sh,int cmd,void* arg, ...)
{
    AVCodecContext *lavc_context = sh->context;
    switch(cmd){
    case ADCTRL_RESYNC_STREAM:
        avcodec_flush_buffers(lavc_context);
        ds_clear_parser(sh->ds);
    return CONTROL_TRUE;
    }
    return CONTROL_UNKNOWN;
}

static int decode_audio(sh_audio_t *sh_audio,unsigned char *buf,int minlen,int maxlen)
{
    unsigned char *start=NULL;
    int y,len=-1;
    while(len<minlen){
	AVPacket pkt;
	int len2=maxlen;
	double pts;
	int x=ds_get_packet_pts(sh_audio->ds,&start, &pts);
	if(x<=0) {
	    start = NULL;
	    x = 0;
	    ds_parse(sh_audio->ds, &start, &x, MP_NOPTS_VALUE, 0);
	    if (x <= 0)
	        break; // error
	} else {
	    int in_size = x;
	    int consumed = ds_parse(sh_audio->ds, &start, &x, pts, 0);
	    sh_audio->ds->buffer_pos -= in_size - consumed;
	}
	av_init_packet(&pkt);
	pkt.data = start;
	pkt.size = x;
	if (pts != MP_NOPTS_VALUE) {
	    sh_audio->pts = pts;
	    sh_audio->pts_bytes = 0;
	}
	y=avcodec_decode_audio3(sh_audio->context,(int16_t*)buf,&len2,&pkt);
//printf("return:%d samples_out:%d bitstream_in:%d sample_sum:%d\n", y, len2, x, len); fflush(stdout);
	// LATM may need many packets to find mux info
	if (y == AVERROR(EAGAIN))
	    continue;
	if(y<0){ mp_msg(MSGT_DECAUDIO,MSGL_V,"lavc_audio: error\n");break; }
	if(!sh_audio->parser && y<x)
	    sh_audio->ds->buffer_pos+=y-x;  // put back data (HACK!)
	if(len2>0){
	  if (((AVCodecContext *)sh_audio->context)->channels >= 5) {
            int samplesize = av_get_bits_per_sample_format(((AVCodecContext *)
                                    sh_audio->context)->sample_fmt) / 8;
            reorder_channel_nch(buf, AF_CHANNEL_LAYOUT_LAVC_DEFAULT,
                                AF_CHANNEL_LAYOUT_MPLAYER_DEFAULT,
                                ((AVCodecContext *)sh_audio->context)->channels,
                                len2 / samplesize, samplesize);
	  }
	  //len=len2;break;
	  if(len<0) len=len2; else len+=len2;
	  buf+=len2;
	  maxlen -= len2;
	  sh_audio->pts_bytes += len2;
	}
        mp_dbg(MSGT_DECAUDIO,MSGL_DBG2,"Decoded %d -> %d  \n",y,len2);

        if (setup_format(sh_audio, sh_audio->context))
            break;
    }
  return len;
}
