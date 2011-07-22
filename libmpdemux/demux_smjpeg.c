/*
 * SMJPEG file parser
 * copyright (c) 2002 Alex Beregszaszi
 * based on text by Arpi (SMJPEG-format.txt) and later on
 * http://www.lokigames.com/development/download/smjpeg/SMJPEG.txt
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
#include <unistd.h>
#include <string.h> /* strtok */

#include "config.h"
#include "mp_msg.h"
#include "libavutil/attributes.h"
#include "stream/stream.h"
#include "demuxer.h"
#include "stheader.h"

static int smjpeg_check_file(demuxer_t* demuxer){
    int orig_pos = stream_tell(demuxer->stream);
    char buf[8];
    int version;

    mp_msg(MSGT_DEMUX, MSGL_V, "Checking for SMJPEG\n");

    if (stream_read_word(demuxer->stream) == 0xA)
    {
	stream_read(demuxer->stream, buf, 6);
	buf[7] = 0;

	if (strncmp("SMJPEG", buf, 6)) {
	    mp_msg(MSGT_DEMUX, MSGL_DBG2, "Failed: SMJPEG\n");
	    return 0;
	}
    }
    else
	return 0;

    version = stream_read_dword(demuxer->stream);
    if (version != 0)
    {
	mp_msg(MSGT_DEMUX, MSGL_ERR, "Unknown version (%d) of SMJPEG. Please report!\n",
	    version);
	return 0;
    }

    stream_seek(demuxer->stream, orig_pos);

    return DEMUXER_TYPE_SMJPEG;
}


// return value:
//     0 = EOF or no stream found
//     1 = successfully read a packet
static int demux_smjpeg_fill_buffer(demuxer_t *demux, demux_stream_t *ds)
{
    int dtype, dsize, dpts;

    demux->filepos = stream_tell(demux->stream);

    dtype = stream_read_dword_le(demux->stream);
    dpts = stream_read_dword(demux->stream);
    dsize = stream_read_dword(demux->stream);

    switch(dtype)
    {
	case mmioFOURCC('s','n','d','D'):
	    /* fixme, but no decoder implemented yet */
	    ds_read_packet(demux->audio, demux->stream, dsize,
		(float)dpts/1000.0, demux->filepos, 0);
	    break;
	case mmioFOURCC('v','i','d','D'):
	    ds_read_packet(demux->video, demux->stream, dsize,
		(float)dpts/1000.0, demux->filepos, 0);
	    break;
	case mmioFOURCC('D','O','N','E'):
	    return 1;
	default:
	    return 0;
    }

    return 1;
}

static demuxer_t* demux_open_smjpeg(demuxer_t* demuxer){
    sh_video_t* sh_video;
    sh_audio_t* sh_audio;
    unsigned int htype = 0, hleng av_unused;
    int i = 0;

    /* file header */
    stream_skip(demuxer->stream, 8); /* \x00\x0aSMJPEG */
    stream_skip(demuxer->stream, 4);

    mp_msg(MSGT_DEMUX, MSGL_INFO, "This clip is %d seconds\n",
	stream_read_dword(demuxer->stream));

    /* stream header */
    while (i < 3)
    {
	i++;
	htype = stream_read_dword_le(demuxer->stream);
	if (htype == mmioFOURCC('H','E','N','D'))
	    break;
	hleng = (stream_read_word(demuxer->stream)<<16)|stream_read_word(demuxer->stream);
	switch(htype)
	{
	case mmioFOURCC('_','V','I','D'):
	    sh_video = new_sh_video(demuxer, 0);
	    demuxer->video->sh = sh_video;
	    sh_video->ds = demuxer->video;

	    sh_video->bih = calloc(1, sizeof(*sh_video->bih));

	    stream_skip(demuxer->stream, 4); /* number of frames */
//	    sh_video->fps = 24;
//	    sh_video->frametime = 1.0f/sh_video->fps;
	    sh_video->disp_w = stream_read_word(demuxer->stream);
	    sh_video->disp_h = stream_read_word(demuxer->stream);
	    sh_video->format = stream_read_dword_le(demuxer->stream);

	    /* these are false values */
	    sh_video->bih->biSize = 40;
	    sh_video->bih->biWidth = sh_video->disp_w;
	    sh_video->bih->biHeight = sh_video->disp_h;
	    sh_video->bih->biPlanes = 3;
	    sh_video->bih->biBitCount = 12;
	    sh_video->bih->biCompression = sh_video->format;
	    sh_video->bih->biSizeImage = sh_video->disp_w*sh_video->disp_h;
	    break;
	case mmioFOURCC('_','S','N','D'):
	    sh_audio = new_sh_audio(demuxer, 0);
	    demuxer->audio->id = 0;
	    demuxer->audio->sh = sh_audio;
	    sh_audio->ds = demuxer->audio;

	    sh_audio->wf = calloc(1, sizeof(*sh_audio->wf));

	    sh_audio->samplerate = stream_read_word(demuxer->stream);
	    sh_audio->wf->wBitsPerSample = stream_read_char(demuxer->stream);
	    sh_audio->channels = stream_read_char(demuxer->stream);
	    sh_audio->format = stream_read_dword_le(demuxer->stream);
	    sh_audio->wf->wFormatTag = sh_audio->format;
	    sh_audio->wf->nChannels = sh_audio->channels;
	    sh_audio->wf->nSamplesPerSec = sh_audio->samplerate;
	    sh_audio->wf->nAvgBytesPerSec = sh_audio->wf->nChannels*
	    sh_audio->wf->wBitsPerSample*sh_audio->wf->nSamplesPerSec/8;
	    sh_audio->wf->nBlockAlign = sh_audio->channels *2;
	    sh_audio->wf->cbSize = 0;
	    break;
	case mmioFOURCC('_','T','X','T'):
	    stream_skip(demuxer->stream, stream_read_dword(demuxer->stream));
	    break;
	}
    }

    demuxer->seekable = 0;

    return demuxer;
}

static void demux_close_smjpeg(demuxer_t *demuxer)
{
    return;
}


const demuxer_desc_t demuxer_desc_smjpeg = {
  "smjpeg demuxer",
  "smjpeg",
  "SMJPEG",
  "Alex Beregszasi",
  "",
  DEMUXER_TYPE_SMJPEG,
  1, // safe autodetect
  smjpeg_check_file,
  demux_smjpeg_fill_buffer,
  demux_open_smjpeg,
  demux_close_smjpeg,
  NULL,
  NULL
};
