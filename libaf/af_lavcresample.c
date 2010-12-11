/*
 * Copyright (c) 2004 Michael Niedermayer <michaelni@gmx.at>
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

#include "config.h"
#include "af.h"
#include "libavcodec/avcodec.h"
#include "libavutil/rational.h"

// Data for specific instances of this filter
typedef struct af_resample_s{
    struct AVResampleContext *avrctx;
    int16_t *in[AF_NCH];
    int in_alloc;
    int index;

    int filter_length;
    int linear;
    int phase_shift;
    double cutoff;

    int ctx_out_rate;
    int ctx_in_rate;
    int ctx_filter_size;
    int ctx_phase_shift;
    int ctx_linear;
    double ctx_cutoff;
}af_resample_t;


// Initialization and runtime control
static int control(struct af_instance_s* af, int cmd, void* arg)
{
  af_resample_t* s   = (af_resample_t*)af->setup;
  af_data_t *data= (af_data_t*)arg;
  int out_rate, test_output_res; // helpers for checking input format

  switch(cmd){
  case AF_CONTROL_REINIT:
    if((af->data->rate == data->rate) || (af->data->rate == 0))
        return AF_DETACH;

    af->data->nch    = data->nch;
    if (af->data->nch > AF_NCH) af->data->nch = AF_NCH;
    af->data->format = AF_FORMAT_S16_NE;
    af->data->bps    = 2;
    af->mul = (double)af->data->rate / data->rate;
    af->delay = af->data->nch * s->filter_length / min(af->mul, 1); // *bps*.5

    if (s->ctx_out_rate != af->data->rate || s->ctx_in_rate != data->rate || s->ctx_filter_size != s->filter_length ||
        s->ctx_phase_shift != s->phase_shift || s->ctx_linear != s->linear || s->ctx_cutoff != s->cutoff) {
        if(s->avrctx) av_resample_close(s->avrctx);
        s->avrctx= av_resample_init(af->data->rate, /*in_rate*/data->rate, s->filter_length, s->phase_shift, s->linear, s->cutoff);
        s->ctx_out_rate    = af->data->rate;
        s->ctx_in_rate     = data->rate;
        s->ctx_filter_size = s->filter_length;
        s->ctx_phase_shift = s->phase_shift;
        s->ctx_linear      = s->linear;
        s->ctx_cutoff      = s->cutoff;
    }

    // hack to make af_test_output ignore the samplerate change
    out_rate = af->data->rate;
    af->data->rate = data->rate;
    test_output_res = af_test_output(af, (af_data_t*)arg);
    af->data->rate = out_rate;
    return test_output_res;
  case AF_CONTROL_COMMAND_LINE:{
    s->cutoff= 0.0;
    sscanf((char*)arg,"%d:%d:%d:%d:%lf", &af->data->rate, &s->filter_length, &s->linear, &s->phase_shift, &s->cutoff);
    if(s->cutoff <= 0.0) s->cutoff= max(1.0 - 6.5/(s->filter_length+8), 0.80);
    return AF_OK;
  }
  case AF_CONTROL_RESAMPLE_RATE | AF_CONTROL_SET:
    af->data->rate = *(int*)arg;
    return AF_OK;
  }
  return AF_UNKNOWN;
}

// Deallocate memory
static void uninit(struct af_instance_s* af)
{
    if(af->data)
        free(af->data->audio);
    free(af->data);
    if(af->setup){
        int i;
        af_resample_t *s = af->setup;
        if(s->avrctx) av_resample_close(s->avrctx);
        for (i=0; i < AF_NCH; i++)
            free(s->in[i]);
        free(s);
    }
}

// Filter data through filter
static af_data_t* play(struct af_instance_s* af, af_data_t* data)
{
  af_resample_t *s = af->setup;
  int i, j, consumed, ret;
  int16_t *in = (int16_t*)data->audio;
  int16_t *out;
  int chans   = data->nch;
  int in_len  = data->len/(2*chans);
  int out_len = in_len * af->mul + 10;
  int16_t tmp[AF_NCH][out_len];

  if(AF_OK != RESIZE_LOCAL_BUFFER(af,data))
      return NULL;

  out= (int16_t*)af->data->audio;

  out_len= min(out_len, af->data->len/(2*chans));

  if(s->in_alloc < in_len + s->index){
      s->in_alloc= in_len + s->index;
      for(i=0; i<chans; i++){
          s->in[i]= realloc(s->in[i], s->in_alloc*sizeof(int16_t));
      }
  }

  if(chans==1){
      memcpy(&s->in[0][s->index], in, in_len * sizeof(int16_t));
  }else if(chans==2){
      for(j=0; j<in_len; j++){
          s->in[0][j + s->index]= *(in++);
          s->in[1][j + s->index]= *(in++);
      }
  }else{
      for(j=0; j<in_len; j++){
          for(i=0; i<chans; i++){
              s->in[i][j + s->index]= *(in++);
          }
      }
  }
  in_len += s->index;

  for(i=0; i<chans; i++){
      ret= av_resample(s->avrctx, tmp[i], s->in[i], &consumed, in_len, out_len, i+1 == chans);
  }
  out_len= ret;

  s->index= in_len - consumed;
  for(i=0; i<chans; i++){
      memmove(s->in[i], s->in[i] + consumed, s->index*sizeof(int16_t));
  }

  if(chans==1){
      memcpy(out, tmp[0], out_len*sizeof(int16_t));
  }else if(chans==2){
      for(j=0; j<out_len; j++){
          *(out++)= tmp[0][j];
          *(out++)= tmp[1][j];
      }
  }else{
      for(j=0; j<out_len; j++){
          for(i=0; i<chans; i++){
              *(out++)= tmp[i][j];
          }
      }
  }

  data->audio = af->data->audio;
  data->len   = out_len*chans*2;
  data->rate  = af->data->rate;
  return data;
}

static int af_open(af_instance_t* af){
  af_resample_t *s = calloc(1,sizeof(af_resample_t));
  af->control=control;
  af->uninit=uninit;
  af->play=play;
  af->mul=1;
  af->data=calloc(1,sizeof(af_data_t));
  s->filter_length= 16;
  s->cutoff= max(1.0 - 6.5/(s->filter_length+8), 0.80);
  s->phase_shift= 10;
//  s->setup = RSMP_INT | FREQ_SLOPPY;
  af->setup=s;
  return AF_OK;
}

af_info_t af_info_lavcresample = {
  "Sample frequency conversion using libavcodec",
  "lavcresample",
  "Michael Niedermayer",
  "",
  AF_FLAGS_REENTRANT,
  af_open
};
