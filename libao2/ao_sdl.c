/*
 * SDLlib audio output driver for MPlayer
 *
 * Copyleft 2001 by Felix Bünemann (atmosfear@users.sf.net)
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
 * along with MPlayer; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "mp_msg.h"
#include "help_mp.h"

#include "audio_out.h"
#include "audio_out_internal.h"
#include "libaf/af_format.h"
#ifdef CONFIG_SDL_SDL_H
#include <SDL/SDL.h>
#else
#include <SDL.h>
#endif
#include "osdep/timer.h"

#include "libavutil/fifo.h"

static const ao_info_t info =
{
	"SDLlib audio output",
	"sdl",
	"Felix Buenemann <atmosfear@users.sourceforge.net>",
	""
};

LIBAO_EXTERN(sdl)

// turn this on if you want to use the slower SDL_MixAudio
#undef USE_SDL_INTERNAL_MIXER

// Samplesize used by the SDLlib AudioSpec struct
#if defined(__MINGW32__) || defined(__CYGWIN__) || defined(__AMIGAOS4__)
#define SAMPLESIZE 2048
#else
#define SAMPLESIZE 1024
#endif

#define CHUNK_SIZE 4096
#define NUM_CHUNKS 8
#define BUFFSIZE (NUM_CHUNKS * CHUNK_SIZE)

static AVFifoBuffer *buffer;

#ifdef USE_SDL_INTERNAL_MIXER
static unsigned char volume=SDL_MIX_MAXVOLUME;
#endif

static int write_buffer(unsigned char* data,int len){
  int free = av_fifo_space(buffer);
  if (len > free) len = free;
  return av_fifo_generic_write(buffer, data, len, NULL);
}

#ifdef USE_SDL_INTERNAL_MIXER
static void mix_audio(void *dst, void *src, int len) {
  SDL_MixAudio(dst, src, len, volume);
}
#endif

static int read_buffer(unsigned char* data,int len){
  int buffered = av_fifo_size(buffer);
  if (len > buffered) len = buffered;
#ifdef USE_SDL_INTERNAL_MIXER
  av_fifo_generic_read(buffer, data, len, mix_audio);
#else
  av_fifo_generic_read(buffer, data, len, NULL);
#endif
  return len;
}

// end ring buffer stuff


// to set/get/query special features/parameters
static int control(int cmd,void *arg){
#ifdef USE_SDL_INTERNAL_MIXER
	switch (cmd) {
		case AOCONTROL_GET_VOLUME:
		{
			ao_control_vol_t* vol = (ao_control_vol_t*)arg;
			vol->left = vol->right = volume * 100 / SDL_MIX_MAXVOLUME;
			return CONTROL_OK;
		}
		case AOCONTROL_SET_VOLUME:
		{
			int diff;
			ao_control_vol_t* vol = (ao_control_vol_t*)arg;
			diff = (vol->left+vol->right) / 2;
			volume = diff * SDL_MIX_MAXVOLUME / 100;
			return CONTROL_OK;
		}
	}
#endif
	return CONTROL_UNKNOWN;
}

// SDL Callback function
static void outputaudio(void *unused, Uint8 *stream, int len) {
	//SDL_MixAudio(stream, read_buffer(buffers, len), len, SDL_MIX_MAXVOLUME);
	//if(!full_buffers) printf("SDL: Buffer underrun!\n");

	read_buffer(stream, len);
	//printf("SDL: Full Buffers: %i\n", full_buffers);
}

// open & setup audio device
// return: 1=success 0=fail
static int init(int rate,int channels,int format,int flags){

	/* SDL Audio Specifications */
	SDL_AudioSpec aspec, obtained;

	/* Allocate ring-buffer memory */
	buffer = av_fifo_alloc(BUFFSIZE);

	mp_tmsg(MSGT_AO,MSGL_INFO,"[AO SDL] Samplerate: %iHz Channels: %s Format %s\n", rate, (channels > 1) ? "Stereo" : "Mono", af_fmt2str_short(format));

	if(ao_subdevice) {
		setenv("SDL_AUDIODRIVER", ao_subdevice, 1);
		mp_tmsg(MSGT_AO,MSGL_INFO,"[AO SDL] using %s audio driver.\n", ao_subdevice);
	}

	ao_data.channels=channels;
	ao_data.samplerate=rate;
	ao_data.format=format;

	ao_data.bps=channels*rate;
	if(format != AF_FORMAT_U8 && format != AF_FORMAT_S8)
	  ao_data.bps*=2;

	/* The desired audio format (see SDL_AudioSpec) */
	switch(format) {
	    case AF_FORMAT_U8:
		aspec.format = AUDIO_U8;
	    break;
	    case AF_FORMAT_S16_LE:
		aspec.format = AUDIO_S16LSB;
	    break;
	    case AF_FORMAT_S16_BE:
		aspec.format = AUDIO_S16MSB;
	    break;
	    case AF_FORMAT_S8:
		aspec.format = AUDIO_S8;
	    break;
	    case AF_FORMAT_U16_LE:
		aspec.format = AUDIO_U16LSB;
	    break;
	    case AF_FORMAT_U16_BE:
		aspec.format = AUDIO_U16MSB;
	    break;
	    default:
                aspec.format = AUDIO_S16LSB;
                ao_data.format = AF_FORMAT_S16_LE;
                mp_tmsg(MSGT_AO,MSGL_WARN,"[AO SDL] Unsupported audio format: 0x%x.\n", format);
	}

	/* The desired audio frequency in samples-per-second. */
	aspec.freq     = rate;

	/* Number of channels (mono/stereo) */
	aspec.channels = channels;

	/* The desired size of the audio buffer in samples. This number should be a power of two, and may be adjusted by the audio driver to a value more suitable for the hardware. Good values seem to range between 512 and 8192 inclusive, depending on the application and CPU speed. Smaller values yield faster response time, but can lead to underflow if the application is doing heavy processing and cannot fill the audio buffer in time. A stereo sample consists of both right and left channels in LR ordering. Note that the number of samples is directly related to time by the following formula: ms = (samples*1000)/freq */
	aspec.samples  = SAMPLESIZE;

	/* This should be set to a function that will be called when the audio device is ready for more data. It is passed a pointer to the audio buffer, and the length in bytes of the audio buffer. This function usually runs in a separate thread, and so you should protect data structures that it accesses by calling SDL_LockAudio and SDL_UnlockAudio in your code. The callback prototype is:
void callback(void *userdata, Uint8 *stream, int len); userdata is the pointer stored in userdata field of the SDL_AudioSpec. stream is a pointer to the audio buffer you want to fill with information and len is the length of the audio buffer in bytes. */
	aspec.callback = outputaudio;

	/* This pointer is passed as the first parameter to the callback function. */
	aspec.userdata = NULL;

	/* initialize the SDL Audio system */
        if (SDL_Init (SDL_INIT_AUDIO/*|SDL_INIT_NOPARACHUTE*/)) {
                mp_tmsg(MSGT_AO,MSGL_ERR,"[AO SDL] SDL Audio initialization failed: %s\n", SDL_GetError());
                return 0;
        }

	/* Open the audio device and start playing sound! */
	if(SDL_OpenAudio(&aspec, &obtained) < 0) {
        	mp_tmsg(MSGT_AO,MSGL_ERR,"[AO SDL] Unable to open audio: %s\n", SDL_GetError());
        	return 0;
	}

	/* did we got what we wanted ? */
	ao_data.channels=obtained.channels;
	ao_data.samplerate=obtained.freq;

	switch(obtained.format) {
	    case AUDIO_U8 :
		ao_data.format = AF_FORMAT_U8;
	    break;
	    case AUDIO_S16LSB :
		ao_data.format = AF_FORMAT_S16_LE;
	    break;
	    case AUDIO_S16MSB :
		ao_data.format = AF_FORMAT_S16_BE;
	    break;
	    case AUDIO_S8 :
		ao_data.format = AF_FORMAT_S8;
	    break;
	    case AUDIO_U16LSB :
		ao_data.format = AF_FORMAT_U16_LE;
	    break;
	    case AUDIO_U16MSB :
		ao_data.format = AF_FORMAT_U16_BE;
	    break;
	    default:
                mp_tmsg(MSGT_AO,MSGL_WARN,"[AO SDL] Unsupported audio format: 0x%x.\n", obtained.format);
                return 0;
	}

	mp_msg(MSGT_AO,MSGL_V,"SDL: buf size = %d\n",obtained.size);
	ao_data.buffersize=obtained.size;
	ao_data.outburst = CHUNK_SIZE;

	/* unsilence audio, if callback is ready */
	SDL_PauseAudio(0);

	return 1;
}

// close audio device
static void uninit(int immed){
	mp_msg(MSGT_AO,MSGL_V,"SDL: Audio Subsystem shutting down!\n");
	if (!immed)
	  usec_sleep(get_delay() * 1000 * 1000);
	SDL_CloseAudio();
	SDL_QuitSubSystem(SDL_INIT_AUDIO);
	av_fifo_free(buffer);
}

// stop playing and empty buffers (for seeking/pause)
static void reset(void){

	//printf("SDL: reset called!\n");

	SDL_PauseAudio(1);
	/* Reset ring-buffer state */
	av_fifo_reset(buffer);
	SDL_PauseAudio(0);
}

// stop playing, keep buffers (for pause)
static void audio_pause(void)
{

	//printf("SDL: audio_pause called!\n");
	SDL_PauseAudio(1);

}

// resume playing, after audio_pause()
static void audio_resume(void)
{
	//printf("SDL: audio_resume called!\n");
	SDL_PauseAudio(0);
}


// return: how many bytes can be played without blocking
static int get_space(void){
    return av_fifo_space(buffer);
}

// plays 'len' bytes of 'data'
// it should round it down to outburst*n
// return: number of bytes played
static int play(void* data,int len,int flags){

	if (!(flags & AOPLAY_FINAL_CHUNK))
	len = (len/ao_data.outburst)*ao_data.outburst;
#if 0
	int ret;

	/* Audio locking prohibits call of outputaudio */
	SDL_LockAudio();
	// copy audio stream into ring-buffer
	ret = write_buffer(data, len);
	SDL_UnlockAudio();

    	return ret;
#else
	return write_buffer(data, len);
#endif
}

// return: delay in seconds between first and last sample in buffer
static float get_delay(void){
    int buffered = av_fifo_size(buffer); // could be less
    return (float)(buffered + ao_data.buffersize)/(float)ao_data.bps;
}
