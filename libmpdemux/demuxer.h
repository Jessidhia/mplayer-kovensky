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

#ifndef MPLAYER_DEMUXER_H
#define MPLAYER_DEMUXER_H

#include <sys/types.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "stream/stream.h"
#include "bstr.h"
#include "mpcommon.h"

struct MPOpts;

#ifdef HAVE_BUILTIN_EXPECT
#define likely(x) __builtin_expect ((x) != 0, 1)
#define unlikely(x) __builtin_expect ((x) != 0, 0)
#else
#define likely(x) (x)
#define unlikely(x) (x)
#endif

#define MAX_PACKS 4096
#define MAX_PACK_BYTES 0x8000000  // 128 MiB

#define DEMUXER_TYPE_UNKNOWN 0
#define DEMUXER_TYPE_MPEG_ES 1
#define DEMUXER_TYPE_MPEG_PS 2
#define DEMUXER_TYPE_AVI 3
#define DEMUXER_TYPE_AVI_NI 4
#define DEMUXER_TYPE_AVI_NINI 5
#define DEMUXER_TYPE_ASF 6
#define DEMUXER_TYPE_MOV 7
#define DEMUXER_TYPE_VIVO 8
#define DEMUXER_TYPE_TV 9
#define DEMUXER_TYPE_FLI 10
#define DEMUXER_TYPE_REAL 11
#define DEMUXER_TYPE_Y4M 12
#define DEMUXER_TYPE_FILM 14
#define DEMUXER_TYPE_ROQ 15
#define DEMUXER_TYPE_MF 16
#define DEMUXER_TYPE_AUDIO 17
#define DEMUXER_TYPE_OGG 18
#define DEMUXER_TYPE_RAWAUDIO 20
#define DEMUXER_TYPE_RTP 21
#define DEMUXER_TYPE_RAWDV 22
#define DEMUXER_TYPE_PVA 23
#define DEMUXER_TYPE_SMJPEG 24
#define DEMUXER_TYPE_XMMS 25
#define DEMUXER_TYPE_RAWVIDEO 26
#define DEMUXER_TYPE_MPEG4_ES 27
#define DEMUXER_TYPE_GIF 28
#define DEMUXER_TYPE_MPEG_TS 29
#define DEMUXER_TYPE_H264_ES 30
#define DEMUXER_TYPE_MATROSKA 31
#define DEMUXER_TYPE_REALAUDIO 32
#define DEMUXER_TYPE_MPEG_TY 33
#define DEMUXER_TYPE_LMLM4 34
#define DEMUXER_TYPE_LAVF 35
#define DEMUXER_TYPE_NSV 36
#define DEMUXER_TYPE_VQF 37
#define DEMUXER_TYPE_AVS 38
#define DEMUXER_TYPE_AAC 39
#define DEMUXER_TYPE_MPC 40
#define DEMUXER_TYPE_MPEG_PES 41
#define DEMUXER_TYPE_MPEG_GXF 42
#define DEMUXER_TYPE_NUT 43
#define DEMUXER_TYPE_LAVF_PREFERRED 44
#define DEMUXER_TYPE_RTP_NEMESI 45
#define DEMUXER_TYPE_MNG 46

// This should always match the higest demuxer type number.
// Unless you want to disallow users to force the demuxer to some types
#define DEMUXER_TYPE_MIN 0
#define DEMUXER_TYPE_MAX 46

#define DEMUXER_TYPE_DEMUXERS (1<<16)
// A virtual demuxer type for the network code
#define DEMUXER_TYPE_PLAYLIST (2<<16)

enum timestamp_type {
    TIMESTAMP_TYPE_PTS,
    TIMESTAMP_TYPE_SORT,
};


// DEMUXER control commands/answers
#define DEMUXER_CTRL_NOTIMPL -1
#define DEMUXER_CTRL_DONTKNOW 0
#define DEMUXER_CTRL_OK 1
#define DEMUXER_CTRL_GUESS 2
#define DEMUXER_CTRL_GET_TIME_LENGTH 10
#define DEMUXER_CTRL_GET_PERCENT_POS 11
#define DEMUXER_CTRL_SWITCH_AUDIO 12
#define DEMUXER_CTRL_RESYNC 13
#define DEMUXER_CTRL_SWITCH_VIDEO 14
#define DEMUXER_CTRL_IDENTIFY_PROGRAM 15
#define DEMUXER_CTRL_CORRECT_PTS 16

#define SEEK_ABSOLUTE (1 << 0)
#define SEEK_FACTOR   (1 << 1)
#define SEEK_FORWARD  (1 << 2)
#define SEEK_BACKWARD (1 << 3)

#define MP_INPUT_BUFFER_PADDING_SIZE 64

// Holds one packet/frame/whatever
typedef struct demux_packet {
  int len;
  double pts;
  double duration;
  double stream_pts;
  off_t pos;  // position in index (AVI) or file (MPG)
  unsigned char* buffer;
  int flags; // keyframe, etc
  int refcount;   //refcounter for the master packet, if 0, buffer can be free()d
  struct demux_packet *master; //pointer to the master packet if this one is a cloned one
  struct demux_packet *next;
} demux_packet_t;

typedef struct demux_stream {
  int buffer_pos;          // current buffer position
  int buffer_size;         // current buffer size
  unsigned char* buffer;   // current buffer, never free() it, always use free_demux_packet(buffer_ref);
  double pts;              // current buffer's pts
  int pts_bytes;           // number of bytes read after last pts stamp
  int eof;                 // end of demuxed stream? (true if all buffer empty)
  off_t pos;                 // position in the input stream (file)
  off_t dpos;                // position in the demuxed stream
  int pack_no;		   // serial number of packet
  int flags;               // flags of current packet (keyframe etc)
  int non_interleaved;     // 1 if this stream is not properly interleaved,
                           // so e.g. subtitle handling must do explicit reads.
//---------------
  int packs;              // number of packets in buffer
  int bytes;              // total bytes of packets in buffer
  demux_packet_t *first;  // read to current buffer from here
  demux_packet_t *last;   // append new packets from input stream to here
  demux_packet_t *current;// needed for refcounting of the buffer
  int id;                 // stream ID  (for multiple audio/video streams)
  struct demuxer *demuxer; // parent demuxer structure (stream handler)
// ---- asf -----
  demux_packet_t *asf_packet;  // read asf fragments here
  int asf_seq;
// ---- mov -----
  unsigned int ss_mul,ss_div;
// ---- stream header ----
  void* sh;
} demux_stream_t;

typedef struct demuxer_info {
  char *name;
  char *author;
  char *encoder;
  char *comments;
  char *copyright;
} demuxer_info_t;

#define MAX_A_STREAMS 256
#define MAX_V_STREAMS 256
#define MAX_S_STREAMS 256

struct demuxer;

/**
 * Demuxer description structure
 */
typedef struct demuxer_desc {
  const char *info; ///< What is it (long name and/or description)
  const char *name; ///< Demuxer name, used with -demuxer switch
  const char *shortdesc; ///< Description printed at demuxer detection
  const char *author; ///< Demuxer author(s)
  const char *comment; ///< Comment, printed with -demuxer help

  int type; ///< DEMUXER_TYPE_xxx
  int safe_check; ///< If 1 detection is safe and fast, do it before file extension check

  /// Check if can demux the file, return DEMUXER_TYPE_xxx on success
  int (*check_file)(struct demuxer *demuxer); ///< Mandatory if safe_check == 1, else optional
  /// Get packets from file, return 0 on eof
  int (*fill_buffer)(struct demuxer *demuxer, demux_stream_t *ds); ///< Mandatory
  /// Open the demuxer, return demuxer on success, NULL on failure
  struct demuxer* (*open)(struct demuxer *demuxer); ///< Optional
  /// Close the demuxer
  void (*close)(struct demuxer *demuxer); ///< Optional
  // Seek
  void (*seek)(struct demuxer *demuxer, float rel_seek_secs, float audio_delay, int flags); ///< Optional
  // Control
  int (*control)(struct demuxer *demuxer, int cmd, void *arg); ///< Optional
} demuxer_desc_t;

typedef struct demux_chapter
{
  uint64_t start, end;
  char* name;
} demux_chapter_t;

struct matroska_data {
    unsigned char segment_uid[16];
    // Ordered chapter information if any
    struct matroska_chapter {
        uint64_t start;
        uint64_t end;
        bool has_segment_uid;
        unsigned char segment_uid[16];
        char *name;
    } *ordered_chapters;
    int num_ordered_chapters;
};

typedef struct demux_attachment
{
  char* name;
  char* type;
  void* data;
  unsigned int data_size;
} demux_attachment_t;

typedef struct demuxer {
  const demuxer_desc_t *desc;  ///< Demuxer description structure
  char *filetype; // format name when not identified by demuxer (libavformat)
  off_t filepos; // input stream current pos.
  off_t movi_start;
  off_t movi_end;
  stream_t *stream;
  double stream_pts;       // current stream pts, if applicable (e.g. dvd)
  double reference_clock;
  char *filename; ///< Needed by avs_check_file
  int synced;  // stream synced (used by mpeg)
  int type;    // demuxer type: mpeg PS, mpeg ES, avi, avi-ni, avi-nini, asf
  int file_format;  // file format: mpeg/avi/asf
  int seekable;  // flag
    /* Set if using absolute seeks for small movements is OK (no pts resets
     * that would make pts ambigious, preferably supports back/forward flags */
    bool accurate_seek;
    enum timestamp_type timestamp_type;
  //
  demux_stream_t *audio; // audio buffer/demuxer
  demux_stream_t *video; // video buffer/demuxer
  demux_stream_t *sub;   // dvd subtitle buffer/demuxer

  // stream headers:
  struct sh_audio *a_streams[MAX_A_STREAMS];
  struct sh_video *v_streams[MAX_V_STREAMS];
  struct sh_sub   *s_streams[MAX_S_STREAMS];

  // pointer to teletext decoder private data, if demuxer stream contains teletext
  void *teletext;

  demux_chapter_t* chapters;
  int num_chapters;

  demux_attachment_t* attachments;
  int num_attachments;

    struct matroska_data matroska_data;

  void* priv;  // fileformat-dependent data
  char** info;
  struct MPOpts *opts;
} demuxer_t;

typedef struct {
  int progid;        //program id
  int aid, vid, sid; //audio, video and subtitle id
} demux_program_t;

struct demux_packet *new_demux_packet(size_t len);
void resize_demux_packet(struct demux_packet *dp, size_t len);
struct demux_packet *clone_demux_packet(struct demux_packet *pack);
void free_demux_packet(struct demux_packet *dp);

#ifndef SIZE_MAX
#define SIZE_MAX ((size_t)-1)
#endif

static inline void *realloc_struct(void *ptr, size_t nmemb, size_t size) {
  if (nmemb > SIZE_MAX / size) {
    free(ptr);
    return NULL;
  }
  return realloc(ptr, nmemb * size);
}

demux_stream_t* new_demuxer_stream(struct demuxer *demuxer,int id);
demuxer_t* new_demuxer(struct MPOpts *opts, stream_t *stream,int type,int a_id,int v_id,int s_id,char *filename);
void free_demuxer_stream(demux_stream_t *ds);
void free_demuxer(demuxer_t *demuxer);

void ds_add_packet(demux_stream_t *ds,demux_packet_t* dp);
void ds_read_packet(demux_stream_t *ds, stream_t *stream, int len, double pts, off_t pos, int flags);

int demux_fill_buffer(demuxer_t *demux,demux_stream_t *ds);
int ds_fill_buffer(demux_stream_t *ds);

static inline off_t ds_tell(demux_stream_t *ds){
  return (ds->dpos-ds->buffer_size)+ds->buffer_pos;
}

static inline int ds_tell_pts(demux_stream_t *ds){
  return (ds->pts_bytes-ds->buffer_size)+ds->buffer_pos;
}

int demux_read_data(demux_stream_t *ds,unsigned char* mem,int len);
int demux_pattern_3(demux_stream_t *ds, unsigned char *mem, int maxlen,
                    int *read, uint32_t pattern);

#define demux_peekc(ds) (\
     (likely(ds->buffer_pos<ds->buffer_size)) ? ds->buffer[ds->buffer_pos] \
     :((unlikely(!ds_fill_buffer(ds)))? (-1) : ds->buffer[ds->buffer_pos] ) )
#if 1
#define demux_getc(ds) (\
     (likely(ds->buffer_pos<ds->buffer_size)) ? ds->buffer[ds->buffer_pos++] \
     :((unlikely(!ds_fill_buffer(ds)))? (-1) : ds->buffer[ds->buffer_pos++] ) )
#else
static inline int demux_getc(demux_stream_t *ds){
  if(ds->buffer_pos>=ds->buffer_size){
    if(!ds_fill_buffer(ds)){
//      printf("DEMUX_GETC: EOF reached!\n");
      return -1; // EOF
    }
  }
//  printf("[%02X]",ds->buffer[ds->buffer_pos]);
  return ds->buffer[ds->buffer_pos++];
}
#endif

void ds_free_packs(demux_stream_t *ds);
int ds_get_packet(demux_stream_t *ds,unsigned char **start);
int ds_get_packet_pts(demux_stream_t *ds, unsigned char **start, double *pts);
int ds_get_packet_sub(demux_stream_t *ds,unsigned char **start);
double ds_get_next_pts(demux_stream_t *ds);
int ds_parse(demux_stream_t *sh, uint8_t **buffer, int *len, double pts, off_t pos);
void ds_clear_parser(demux_stream_t *sh);

// This is defined here because demux_stream_t ins't defined in stream.h
stream_t* new_ds_stream(demux_stream_t *ds);

static inline int avi_stream_id(unsigned int id){
  unsigned char a,b;
  a = id - '0';
  b = (id >> 8) - '0';
  if(a>9 || b>9) return 100; // invalid ID
  return a*10+b;
}

demuxer_t* demux_open(struct MPOpts *opts, stream_t *stream,int file_format,int aid,int vid,int sid,char* filename);
void demux_flush(demuxer_t *demuxer);
int demux_seek(demuxer_t *demuxer,float rel_seek_secs,float audio_delay,int flags);
demuxer_t*  new_demuxers_demuxer(demuxer_t* vd, demuxer_t* ad, demuxer_t* sd);

// AVI demuxer params:
extern int index_mode;  // -1=untouched  0=don't use index  1=use (geneate) index
extern char *index_file_save, *index_file_load;
extern int force_ni;
extern int pts_from_bps;

extern int extension_parsing;

int demux_info_add(demuxer_t *demuxer, const char *opt, const char *param);
int demux_info_add_bstr(demuxer_t *demuxer, struct bstr opt, struct bstr param);
char* demux_info_get(demuxer_t *demuxer, const char *opt);
int demux_info_print(demuxer_t *demuxer);
int demux_control(demuxer_t *demuxer, int cmd, void *arg);

int demuxer_switch_audio(demuxer_t *demuxer, int index);
int demuxer_switch_video(demuxer_t *demuxer, int index);

int demuxer_type_by_filename(char* filename);

void demuxer_help(void);
int get_demuxer_type_from_name(char *demuxer_name, int *force);

int demuxer_add_attachment(demuxer_t *demuxer, struct bstr name,
                           struct bstr type, struct bstr data);
int demuxer_add_chapter(demuxer_t *demuxer, struct bstr name,
                        uint64_t start, uint64_t end);
int demuxer_seek_chapter(demuxer_t *demuxer, int chapter, double *seek_pts,
                         char **chapter_name);

/// Get current chapter index if available.
int demuxer_get_current_chapter(demuxer_t *demuxer, double time_now);
/// Get chapter name by index if available.
char *demuxer_chapter_name(demuxer_t *demuxer, int chapter);
/// Get chapter display name by index.
char *demuxer_chapter_display_name(demuxer_t *demuxer, int chapter);
/// Get chapter start time and end time by index if available.
float demuxer_chapter_time(demuxer_t *demuxer, int chapter, float *end);
/// Get total chapter number.
int demuxer_chapter_count(demuxer_t *demuxer);
/// Get current angle index.
int demuxer_get_current_angle(demuxer_t *demuxer);
/// Set angle.
int demuxer_set_angle(demuxer_t *demuxer, int angle);
/// Get number of angles.
int demuxer_angles_count(demuxer_t *demuxer);

/* Get the index of a track.
 * lang is a comma-separated list, NULL is same as empty list
 * Sort tracks based on the following criteria:
 * 1) earlier match in lang list, or last no match
 * 2) track is marked default (default wins)
 * 3) track number (lower wins)
 * For audio, select best track according to these criteria; only return -1
 * if there are no tracks at all.
 * For subs, select best track according to the same criteria, but return -1
 * if all tracks are no-lang-match, not-default.
 */
int demuxer_audio_track_by_lang_and_default(struct demuxer *d, char *lang);
int demuxer_sub_track_by_lang_and_default(struct demuxer *d, char *lang);

#endif /* MPLAYER_DEMUXER_H */
