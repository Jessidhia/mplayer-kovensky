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

#ifndef MPLAYER_CODEC_CFG_H
#define MPLAYER_CODEC_CFG_H

#define CODEC_CFG_MIN   20070407

#define CODECS_MAX_FOURCC       92
#define CODECS_MAX_OUTFMT       16
#define CODECS_MAX_INFMT        16

// Global flags:
#define CODECS_FLAG_SEEKABLE    (1<<0)
#define CODECS_FLAG_ALIGN16     (1<<1)

// Outfmt flags:
#define CODECS_FLAG_FLIP        (1<<0)
#define CODECS_FLAG_NOFLIP      (1<<1)
#define CODECS_FLAG_YUVHACK     (1<<2)
#define CODECS_FLAG_QUERY       (1<<3)
#define CODECS_FLAG_STATIC      (1<<4)

#define CODECS_STATUS__MIN              0
#define CODECS_STATUS_NOT_WORKING      -1
#define CODECS_STATUS_PROBLEMS          0
#define CODECS_STATUS_WORKING           1
#define CODECS_STATUS_UNTESTED          2
#define CODECS_STATUS__MAX              2


#if !defined(GUID_TYPE) && !defined(GUID_DEFINED)
#define GUID_TYPE    1
#define GUID_DEFINED 1
typedef struct {
    unsigned long  f1;
    unsigned short f2;
    unsigned short f3;
    unsigned char  f4[8];
} GUID;
#endif


typedef struct codecs {
    unsigned int fourcc[CODECS_MAX_FOURCC];
    unsigned int fourccmap[CODECS_MAX_FOURCC];
    unsigned int outfmt[CODECS_MAX_OUTFMT];
    unsigned char outflags[CODECS_MAX_OUTFMT];
    unsigned int infmt[CODECS_MAX_INFMT];
    unsigned char inflags[CODECS_MAX_INFMT];
    char *name;
    char *info;
    char *comment;
    char *dll;
    char* drv;
    GUID guid;
//    short driver;
    short flags;
    short status;
    short cpuflags;
} codecs_t;

int parse_codec_cfg(const char *cfgfile);
codecs_t* find_video_codec(unsigned int fourcc, unsigned int *fourccmap,
                           codecs_t *start, int force);
codecs_t* find_audio_codec(unsigned int fourcc, unsigned int *fourccmap,
                           codecs_t *start, int force);
codecs_t* find_codec(unsigned int fourcc, unsigned int *fourccmap,
                     codecs_t *start, int audioflag, int force);
void list_codecs(int audioflag);
void codecs_uninit_free(void);

typedef char ** stringset_t;
void stringset_init(stringset_t *set);
void stringset_free(stringset_t *set);
void stringset_add(stringset_t *set, const char *str);
int stringset_test(stringset_t *set, const char *str);

#endif /* MPLAYER_CODEC_CFG_H */
