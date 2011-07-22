/*
 * VideoCD BinCue
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

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "config.h"
#include "mp_msg.h"

#include "stream.h"

#include "m_option.h"
#include "m_struct.h"
#include "libavutil/avstring.h"

#define SIZERAW 2352
#define SIZEISO_MODE1 2048
#define SIZEISO_MODE2_RAW 2352
#define SIZEISO_MODE2_FORM1 2048
#define SIZEISO_MODE2_FORM2 2336
#define AUDIO 0
#define MODE1 1
#define MODE2 2
#define MODE1_2352 10
#define MODE2_2352 20
#define MODE1_2048 30
#define MODE2_2336 40
#define UNKNOWN -1

static struct stream_priv_s {
  char* filename;
} stream_priv_dflts = {
  NULL
};

#define ST_OFF(f) M_ST_OFF(struct stream_priv_s,f)
/// URL definition
static const m_option_t stream_opts_fields[] = {
  { "string", ST_OFF(filename), CONF_TYPE_STRING, 0, 0 ,0, NULL},
  { NULL, NULL, 0, 0, 0, 0,  NULL }
};
static const struct m_struct_st stream_opts = {
  "cue",
  sizeof(struct stream_priv_s),
  &stream_priv_dflts,
  stream_opts_fields
};

static char cue_filename[256];
static char bincue_path[256];


typedef struct track
{
   unsigned short mode;
   unsigned short minute;
   unsigned short second;
   unsigned short frame;

   /* (min*60 + sec) * 75 + fps   */

   unsigned long start_sector;

   /* = the sizes in bytes off all tracks bevor this one */
   /* its needed if there are mode1 tracks befor the mpeg tracks */
   unsigned long start_offset;

   /*   unsigned char num[3]; */
} tTrack;

/* max 99 tracks on a cd */
static tTrack tracks[100];

static struct cue_track_pos {
  int track;
  unsigned short mode;
  unsigned short minute;
  unsigned short second;
  unsigned short frame;
} cue_current_pos;

/* number of tracks on the cd */
static int nTracks = 0;

static int digits2int(const char s[2], int errval) {
  uint8_t a = s[0] - '0';
  uint8_t b = s[1] - '0';
  if (a > 9 || b > 9)
    return errval;
  return a * 10 + b;
}

/* presumes Line is preloaded with the "current" line of the file */
static int cue_getTrackinfo(FILE *fd_cue, char *Line, tTrack *track)
{
  int already_set = 0;

  /* Get the 'mode' */
  if (strncmp(&Line[2], "TRACK ", 6)==0)
  {
/*    strncpy(track->num, &Line[8], 2); track->num[2] = '\0'; */

    track->mode = UNKNOWN;
    if(strncmp(&Line[11], "AUDIO", 5)==0) track->mode = AUDIO;
    if(strncmp(&Line[11], "MODE1/2352", 10)==0) track->mode = MODE1_2352;
    if(strncmp(&Line[11], "MODE1/2048", 10)==0) track->mode = MODE1_2048;
    if(strncmp(&Line[11], "MODE2/2352", 10)==0) track->mode = MODE2_2352;
    if(strncmp(&Line[11], "MODE2/2336", 10)==0) track->mode = MODE2_2336;
  }
  else return 1;

  /* Get the track indexes */
  while(1) {
    if(! fgets( Line, 256, fd_cue ) ) { break;}

    if (strncmp(&Line[2], "TRACK ", 6)==0)
    {
      /* next track starting */
      break;
    }

    /* Track 0 or 1, take the first an get fill the values*/
    if (strncmp(&Line[4], "INDEX ", 6)==0)
    {
      /* check stuff here so if the answer is false the else stuff below won't be executed */
      if ((already_set == 0) && digits2int(Line + 10, 100) <= 1)
      {
        already_set = 1;

        track->minute = digits2int(Line + 13, 0);
        track->second = digits2int(Line + 16, 0);
        track->frame  = digits2int(Line + 19, 0);
      }
    }
    else if (strncmp(&Line[4], "PREGAP ", 7)==0) { ; /* ignore */ }
    else if (strncmp(&Line[4], "FLAGS ", 6)==0)  { ; /* ignore */ }
    else mp_tmsg (MSGT_OPEN,MSGL_INFO,
                 "[bincue] Unexpected cuefile line: %s\n", Line);
  }
  return 0;
}



/* FIXME: the string operations ( strcpy,strcat ) below depend
 * on the arrays to have the same size, thus we need to make
 * sure the sizes are in sync.
 */
static int cue_find_bin (const char *firstline) {
  struct stat filestat;
  const char *cur_name;
  char bin_filename[256];
  char s[256];
  char t[256];
  int fd_bin;
  int i = 0;

  /* get the filename out of that */
  /*                      12345 6  */
  mp_msg (MSGT_OPEN,MSGL_INFO, "[bincue] cue_find_bin(%s)\n", firstline);
  if (strncmp(firstline, "FILE \"",6)==0)
  {
    firstline += 6;
    while ( *firstline && *firstline != '"')
    {
      bin_filename[i] = *firstline++;

      /* if I found a path info, then delete all before it */
      switch (bin_filename[i])
      {
        case '\\':
          i = 0;
          break;

        case '/':
          i = 0;
          break;

        default:
          i++;
      }
    }
  }
  bin_filename[i] = '\0';

  fd_bin = -1;
  for (i = 0; fd_bin == -1 && i < 6; i++) {
    if (i <=1 && bin_filename[0] == '\0')
      continue;
    if (i > 1 && strlen(cue_filename) < 3)
      break;

    switch (i) {
    case 0:
      /* now try to open that file, without path */
      cur_name = bin_filename;
      break;
    case 1:
      /* now try to find it with the path of the cue file */
      snprintf(s,sizeof( s ),"%s/%s",bincue_path,bin_filename);
      cur_name = s;
      break;
    case 2:
      /* now I would say the whole filename is shit, build our own */
      av_strlcpy(s, cue_filename, strlen(cue_filename) - 3 );
      strcat(s, ".bin");
      cur_name = s;
      break;
    case 3:
      /* ok try it with path */
      snprintf(t, sizeof( t ), "%s/%s", bincue_path, s);
      cur_name = t;
      break;
    case 4:
      /* now I would say the whole filename is shit, build our own */
      av_strlcpy(s, cue_filename, strlen(cue_filename) - 3 );
      strcat(s, ".img");
      cur_name = s;
      break;
    case 5:
      /* ok try it with path */
      snprintf(t, sizeof( t ), "%s/%s", bincue_path, s);
      cur_name = t;
      break;
    }
    fd_bin = open(cur_name, O_RDONLY);
    if (fstat(fd_bin, &filestat) == -1 || !S_ISREG(filestat.st_mode)) {
        close(fd_bin);
        fd_bin = -1;
    }
    if (fd_bin == -1) {
      mp_tmsg(MSGT_OPEN,MSGL_STATUS, "[bincue] bin filename tested: %s\n",
              cur_name);
    }
  }

  if (fd_bin == -1)
  {
    /* I'll give up */
    mp_tmsg(MSGT_OPEN,MSGL_ERR,
            "[bincue] Couldn't find the bin file - giving up.\n");
    return -1;
  }

  mp_tmsg(MSGT_OPEN,MSGL_INFO,
          "[bincue] Using bin file %s.\n", cur_name);
  return fd_bin;
}

static inline int cue_msf_2_sector(int minute, int second, int frame) {
 return frame + (second + minute * 60 ) * 75;
}

static inline int cue_get_msf(void) {
  return cue_msf_2_sector (cue_current_pos.minute,
                           cue_current_pos.second,
                           cue_current_pos.frame);
}

static inline void cue_set_msf(unsigned int sect){
  cue_current_pos.frame=sect%75;
  sect=sect/75;
  cue_current_pos.second=sect%60;
  sect=sect/60;
  cue_current_pos.minute=sect;
}

static inline int cue_mode_2_sector_size(int mode)
{
  switch (mode)
  {
    case AUDIO:      return AUDIO;
    case MODE1_2352: return SIZERAW;
    case MODE1_2048: return SIZEISO_MODE1;
    case MODE2_2352: return SIZEISO_MODE2_RAW;
    case MODE2_2336: return SIZEISO_MODE2_FORM2;

    default:
      mp_tmsg(MSGT_OPEN,MSGL_FATAL,
             "[bincue] unknown mode for binfile. Should not happen. Aborting.\n");
      abort();
  }

}


static int cue_read_cue (const char *in_cue_filename)
{
  struct stat filestat;
  char sLine[256];
  unsigned int sect;
  char *s,*t;
  int i;
  int fd_bin;
  FILE *fd_cue;

  /* we have no tracks at the beginning */
  nTracks = 0;

  /* split the filename into a path and filename part */
  s = strdup(in_cue_filename);
  t = strrchr(s, '/');
  if (!t)
     t = ".";
  else {
     *t = '\0';
     t = s;
     if (*t == '\0')
       strcpy(t, "/");
  }

  av_strlcpy(bincue_path,t,sizeof( bincue_path ));
  mp_msg(MSGT_OPEN,MSGL_V,"dirname: %s, cuepath: %s\n", t, bincue_path);
  free(s);
  s = t = NULL;

  /* no path at all? */
  if (strcmp(bincue_path, ".") == 0) {
    mp_msg(MSGT_OPEN,MSGL_V,"bincue_path: %s\n", bincue_path);
    av_strlcpy(cue_filename,in_cue_filename,sizeof( cue_filename ));
  } else {
    av_strlcpy(cue_filename,in_cue_filename + strlen(bincue_path) + 1,
            sizeof( cue_filename ));
  }



  /* open the cue file */
  fd_cue = fopen (in_cue_filename, "r");
  if (fd_cue == NULL)
  {
    mp_tmsg(MSGT_OPEN,MSGL_ERR,
           "[bincue] Cannot open %s.\n", in_cue_filename);
    return -1;
  }

  /* read the first line and hand it to find_bin, which will
     test more than one possible name of the file */

  if(! fgets( sLine, sizeof(sLine), fd_cue ) )
  {
    mp_tmsg(MSGT_OPEN,MSGL_ERR,
           "[bincue] Error reading from %s\n", in_cue_filename);
    fclose (fd_cue);
    return -1;
  }

  fd_bin = cue_find_bin(sLine);
  if (fd_bin == -1) {
    fclose (fd_cue);
    return -1;
  }


  /* now build the track list */
  /* red the next line and call our track finder */
  if(! fgets( sLine, sizeof(sLine), fd_cue ) )
  {
    mp_tmsg(MSGT_OPEN,MSGL_ERR,
           "[bincue] Error reading from %s\n", in_cue_filename);
    fclose (fd_cue);
    return -1;
  }

  while(!feof(fd_cue))
  {
    if (cue_getTrackinfo(fd_cue, sLine, &tracks[nTracks++]) != 0)
    {
      mp_tmsg(MSGT_OPEN,MSGL_ERR,
             "[bincue] Error reading from %s\n", in_cue_filename);
      fclose (fd_cue);
      return -1;
    }
  }

  /* make a fake track with stands for the Lead out */
  if (fstat (fd_bin, &filestat) == -1) {
    mp_tmsg(MSGT_OPEN,MSGL_ERR,
           "[bincue] Error getting size of bin file.\n");
    fclose (fd_cue);
    return -1;
  }

  sect = filestat.st_size / 2352;

  tracks[nTracks].frame = sect%75;
  sect=sect/75;
  tracks[nTracks].second = sect%60;
  sect=sect/60;
  tracks[nTracks].minute = sect;


  /* let's calculate the start sectors and offsets */
  for(i = 0; i <= nTracks; i++)
  {
    tracks[i].start_sector = cue_msf_2_sector(tracks[i].minute,
                                              tracks[nTracks].second,
                                              tracks[nTracks].frame);

    /* if we're the first track we don't need to offset of the one befor */
    if (i == 0)
    {
      /* was always 0 on my svcds, but who knows */
      tracks[0].start_offset = tracks[0].start_sector *
        cue_mode_2_sector_size(tracks[0].mode);
    } else
    {
      tracks[i].start_offset = tracks[i-1].start_offset +
        (tracks[i].start_sector - tracks[i-1].start_sector) *
        cue_mode_2_sector_size(tracks[i-1].mode);
    }
  }

  fclose (fd_cue);

  return fd_bin;
}




static int cue_read_toc_entry(int track) {
  /* check if its a valid track, if not return -1 */
  if (track <= 0 || track > nTracks)
    return -1;


  cue_current_pos.track = track;
  track--;
  switch (tracks[track].mode)
  {
    case AUDIO:
      cue_current_pos.mode = AUDIO;
      break;
    case MODE1_2352:
      cue_current_pos.mode = MODE1;
      break;
    case MODE1_2048:
      cue_current_pos.mode = MODE1;
      break;
    default: /* MODE2_2352 and MODE2_2336 */
      cue_current_pos.mode = MODE2;
  }
  cue_current_pos.minute = tracks[track].minute;
  cue_current_pos.second = tracks[track].second;
  cue_current_pos.frame = tracks[track].frame;

  return 0;
}

static int cue_vcd_get_track_end (int track){
  int sector = cue_msf_2_sector(tracks[track].minute, tracks[track].second,
                                tracks[track].frame);

  return VCD_SECTOR_DATA * sector;
}

static int seek(stream_t *s,off_t newpos) {
  s->pos=newpos;
  cue_set_msf(s->pos/VCD_SECTOR_DATA);
  return 1;
}

static int cue_vcd_seek_to_track (stream_t *stream, int track){
  int pos;
  if (cue_read_toc_entry (track))
    return -1;

  pos = VCD_SECTOR_DATA * cue_get_msf();
  stream->start_pos = pos;
  stream->end_pos = cue_vcd_get_track_end(track);
  seek(stream, pos);
  return pos;
}

static void cue_vcd_read_toc(void){
  int i;
  for (i = 0; i < nTracks; ++i) {

    mp_tmsg(MSGT_OPEN,MSGL_INFO,
           "track %02d:  format=%d  %02d:%02d:%02d\n",
           i+1,
           tracks[i].mode,
           tracks[i].minute,
           tracks[i].second,
           tracks[i].frame
           );
  }
}

static int cue_vcd_read(stream_t *stream, char *mem, int size) {
  unsigned long position;
  int fd_bin = stream->fd;
  int track = cue_current_pos.track - 1;

  position = tracks[track].start_offset +
             (cue_msf_2_sector(cue_current_pos.minute,
                               cue_current_pos.second,
                               cue_current_pos.frame) -
              tracks[track].start_sector)
             * cue_mode_2_sector_size(tracks[track].mode);


  if(position >= tracks[track+1].start_offset)
    return 0;

  if(lseek(fd_bin, position+VCD_SECTOR_OFFS, SEEK_SET) == -1) {
    mp_tmsg(MSGT_OPEN,MSGL_ERR, "[bincue] unexpected end of bin file\n");
    return 0;
  }

  if(read(fd_bin, mem, VCD_SECTOR_DATA) != VCD_SECTOR_DATA) {
    mp_tmsg(MSGT_OPEN,MSGL_ERR, "[bincue] Couldn't read %d bytes of payload.\n", VCD_SECTOR_DATA);
    return 0;
  }

  cue_current_pos.frame++;
  if (cue_current_pos.frame==75){
    cue_current_pos.frame=0;
    cue_current_pos.second++;
    if (cue_current_pos.second==60){
      cue_current_pos.second=0;
      cue_current_pos.minute++;
    }
  }

  return VCD_SECTOR_DATA;
}

static int control(stream_t *stream, int cmd, void *arg) {
  switch(cmd) {
    case STREAM_CTRL_GET_NUM_CHAPTERS:
    {
      *(unsigned int *)arg = nTracks;
      return STREAM_OK;
    }
    case STREAM_CTRL_SEEK_TO_CHAPTER:
    {
      int r;
      unsigned int track = *(unsigned int *)arg + 1;
      r = cue_vcd_seek_to_track(stream, track);
      if (r >= 0) {
        return STREAM_OK;
      }
      break;
    }
    case STREAM_CTRL_GET_CURRENT_CHAPTER:
    {
      *(unsigned int *)arg = cue_current_pos.track - 1;
      return STREAM_OK;
    }
  }
  return STREAM_UNSUPPORTED;
}

static int open_s(stream_t *stream,int mode, void* opts, int* file_format) {
  struct stream_priv_s* p = (struct stream_priv_s*)opts;
  int ret,f,track = 0;
  char *filename = NULL, *colon = NULL;

  if(mode != STREAM_READ || !p->filename) {
    m_struct_free(&stream_opts,opts);
    return STREAM_UNSUPPORTED;
  }
  filename = strdup(p->filename);
  if(!filename) {
    m_struct_free(&stream_opts,opts);
    return STREAM_UNSUPPORTED;
  }
  colon = strstr(filename, ":");
  if(colon) {
    if(strlen(colon)>1)
      track = atoi(colon+1);
    *colon = 0;
  }
  if(!track)
    track = 1;

  f = cue_read_cue(filename);
  if(f < 0) {
    m_struct_free(&stream_opts,opts);
    return STREAM_UNSUPPORTED;
  }
  cue_vcd_read_toc();
  ret=cue_vcd_seek_to_track(stream, track);
  if(ret<0){
      mp_msg(MSGT_OPEN, MSGL_ERR, "%s (seek)\n",
             mp_gtext("Error selecting VCD track."));
    return STREAM_UNSUPPORTED;
  }
  mp_tmsg(MSGT_OPEN, MSGL_INFO,
          "CUE stream_open, filename=%s, track=%d, "
          "available tracks: %d -> %d\n",
          filename, track, ret, (int)stream->end_pos);

  stream->fd = f;
  stream->type = STREAMTYPE_VCDBINCUE;
  stream->sector_size = VCD_SECTOR_DATA;
  stream->flags = STREAM_READ | MP_STREAM_SEEK_FW;
  stream->fill_buffer = cue_vcd_read;
  stream->seek = seek;
  stream->control = control;

  free(filename);
  m_struct_free(&stream_opts,opts);
  return STREAM_OK;
}

const stream_info_t stream_info_cue = {
  "CUE track",
  "cue",
  "Albeu",
  "based on the code from ???",
  open_s,
  { "cue", NULL },
  &stream_opts,
  1 // Urls are an option string
};
