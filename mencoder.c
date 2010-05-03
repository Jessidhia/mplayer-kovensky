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

#define VCODEC_COPY 0
#define VCODEC_FRAMENO 1
// real codecs:
#define VCODEC_LIBAVCODEC 4
#define VCODEC_VFW 7
#define VCODEC_LIBDV 8
#define VCODEC_XVID 9
#define VCODEC_QTVIDEO 10
#define VCODEC_NUV 11
#define VCODEC_RAW 12
#define VCODEC_X264 13

#define ACODEC_COPY 0
#define ACODEC_PCM 1
#define ACODEC_VBRMP3 2
#define ACODEC_NULL 3
#define ACODEC_LAVC 4
#define ACODEC_TOOLAME 5
#define ACODEC_FAAC 6
#define ACODEC_TWOLAME 7

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include "config.h"

#ifdef __MINGW32__
#define        SIGHUP 1
#define        SIGQUIT 3
#define        SIGPIPE 13
#endif
#if defined(__MINGW32__) || defined(__CYGWIN__)
#include <windows.h>
#endif

#include <sys/time.h>

#include "mp_msg.h"
#include "av_log.h"

#include "codec-cfg.h"
#include "m_option.h"
#include "m_config.h"
#include "parser-mecmd.h"
#include "parser-cfg.h"
#include "mp_fifo.h"
#include "path.h"

#include "stream/stream.h"
#include "libmpdemux/aviprint.h"
#include "libmpdemux/demuxer.h"
#include "libmpdemux/stheader.h"
#include "libmpdemux/mp3_hdr.h"
#include "libmpdemux/muxer.h"

#include "input/input.h"
#include "libvo/video_out.h"

#include "libaf/af_format.h"

#include "libmpcodecs/mp_image.h"
#include "libmpcodecs/dec_audio.h"
#include "libmpcodecs/dec_video.h"
#include "libmpcodecs/vf.h"
#include "libmpcodecs/vd.h"

// for MPEGLAYER3WAVEFORMAT:
#include "libmpdemux/ms_hdr.h"

#include <inttypes.h>

#include "libvo/fastmemcpy.h"

#include "osdep/timer.h"

#ifdef CONFIG_DVDREAD
#include "stream/stream_dvd.h"
#endif

#include "stream/stream_dvdnav.h"
#include "libavcodec/avcodec.h"

#include "libmpcodecs/ae.h"
#include "options.h"
#include "defaultopts.h"

MPOpts opts;
struct osd_state *osd;

const int under_mencoder = 1;
int vo_doublebuffering=0;
int vo_directrendering=0;
int forced_subs_only=0;

//--------------------------

// cache2:
int stream_cache_size=-1;
#ifdef CONFIG_STREAM_CACHE
extern int cache_fill_status;

float stream_cache_min_percent=20.0;
float stream_cache_seek_min_percent=50.0;
#else
#define cache_fill_status 0
#endif

int vobsub_id=-1;
char* audio_lang=NULL;
char* dvdsub_lang=NULL;
static char* spudec_ifo=NULL;

static char** audio_codec_list=NULL;  // override audio codec
static char** video_codec_list=NULL;  // override video codec
static char** audio_fm_list=NULL;     // override audio codec family
static char** video_fm_list=NULL;     // override video codec family
extern char *demuxer_name; // override demuxer
extern char *audio_demuxer_name; // override audio demuxer
extern char *sub_demuxer_name; // override sub demuxer

static int out_audio_codec=-1;
static int out_video_codec=-1;

int out_file_format=MUXER_TYPE_AVI;	// default to AVI
int quiet=0;
double video_time_usage=0;
double vout_time_usage=0;
double max_video_time_usage=0;
double max_vout_time_usage=0;
double cur_video_time_usage=0;
double cur_vout_time_usage=0;
int benchmark=0;

#include "osdep/priority.h"

// A-V sync:
int delay_corrected=1;
static float default_max_pts_correction=-1;//0.01f;
static float max_pts_correction=0;//default_max_pts_correction;
static float c_total=0;

static float audio_preload=0.5;
static float audio_delay_fix=0.0;
static float audio_delay=0.0;
static int ignore_start=0;
static int audio_density=2;

double force_fps=0;
static double force_ofps=0; // set to 24 for inverse telecine
static int skip_limit=-1;

static int force_srate=0;
static int audio_output_format=0;

char *vobsub_out=NULL;
unsigned int vobsub_out_index=0;
char *vobsub_out_id=NULL;

char* out_filename=NULL;

char *force_fourcc=NULL;
int force_audiofmttag=-1;

char* passtmpfile="divx2pass.log";

static int play_n_frames=-1;
static int play_n_frames_mf=-1;

#include "libvo/font_load.h"
#include "libvo/sub.h"

// sub:
char *font_name=NULL;
char *sub_font_name=NULL;
extern int font_fontconfig;
float font_factor=0.75;
char **sub_name=NULL;
float sub_delay=0;
float sub_fps=0;
int   sub_auto = 0;
int   subcc_enabled=0;
int   suboverlap_enabled = 1;

int auto_expand=1;
int encode_duplicates=1;

// infos are empty by default
char *info_name=NULL;
char *info_artist=NULL;
char *info_genre=NULL;
char *info_subject=NULL;
char *info_copyright=NULL;
char *info_sourceform=NULL;
char *info_comment=NULL;

// Needed by libmpcodecs vf_vo.c
int vo_config(struct vo *vo, uint32_t width, uint32_t height,
	      uint32_t d_width, uint32_t d_height, uint32_t flags,
	      char *title, uint32_t format) { abort(); }
int vo_control(struct vo *vo, uint32_t request, void *data) { abort(); }
void vo_seek_reset(struct vo *vo) { abort(); }
int vo_draw_image(struct vo *vo, struct mp_image *mpi, double pts) { abort(); }
int vo_draw_frame(struct vo *vo, uint8_t *src[]) { abort(); }
int vo_draw_slice(struct vo *vo, uint8_t *src[], int stride[], int w, int h, int x, int y) { abort(); }
void vo_draw_osd(struct vo *vo, struct osd_state *osd) { abort(); }
void vo_flip_page(struct vo *vo, uint32_t pts_us, int duration) { abort(); }
void vo_check_events(struct vo *vo) { abort(); }

// Needed by getch2
struct mp_fifo;
void mplayer_put_key(struct mp_fifo *fifo, int code)
{
}

#ifdef CONFIG_ASS
#include "ass/ass.h"
#include "ass_mp.h"
#endif
char *current_module;
#include "mpcommon.h"

// Needed by mpcommon.c
void set_osd_subtitle(struct MPContext *mpctx, subtitle *subs) {
    vo_sub = subs;
    vo_osd_changed(OSDTYPE_SUBTITLE);
}

//-------------------------- config stuff:

m_config_t* mconfig;

static int cfg_inc_verbose(m_option_t *conf){ ++verbose; return 0;}

static int cfg_include(m_option_t *conf, char *filename){
	return m_config_parse_config_file(mconfig, filename);
}

static double seek_to_sec;
static off_t seek_to_byte=0;

static m_time_size_t end_at = { .type = END_AT_NONE, .pos = 0 };

static char * frameno_filename=NULL;

typedef struct {
    unsigned char* start;
    int in_size;
    float frame_time;
    int already_read;
} s_frame_data;

#include "edl.h"
static edl_record_ptr edl_records = NULL; ///< EDL entries memory area
static edl_record_ptr next_edl_record = NULL; ///< only for traversing edl_records
static short edl_muted; ///< Stores whether EDL is currently in muted mode.
static short edl_seeking; ///< When non-zero, stream is seekable.
static short edl_seek_type; ///< When non-zero, frames are discarded instead of seeking.

#include "cfg-mencoder.h"

#include "spudec.h"
#include "vobsub.h"

#include "libao2/audio_out.h"
/* FIXME */
static void mencoder_exit(int level, const char *how)
{
    if (how)
	mp_tmsg(MSGT_MENCODER, MSGL_INFO, "\nExiting... (%s)\n", how);
    else
	mp_tmsg(MSGT_MENCODER, MSGL_INFO, "\nExiting...\n");

    exit(level);
}

static void parse_cfgfiles( m_config_t* conf )
{
  char *conffile;
  if (!disable_system_conf &&
      m_config_parse_config_file(conf, MPLAYER_CONFDIR "/mencoder.conf") < 0)
    mencoder_exit(1,_("config file error"));

  if (!disable_user_conf) {
    if ((conffile = get_path("mencoder.conf")) == NULL) {
      mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"get_path(\"config\") problem\n");
    } else {
      if (m_config_parse_config_file(conf, conffile) < 0)
        mencoder_exit(1,_("config file error"));
      free(conffile);
    }
  }
}


//---------------------------------------------------------------------------

static int dec_audio(sh_audio_t *sh_audio,unsigned char* buffer,int total){
    int size=0;
    int at_eof=0;
    while(size<total && !at_eof){
	int len=total-size;
		if (decode_audio(sh_audio, len) < 0) at_eof=1;
		if(len>sh_audio->a_out_buffer_len) len=sh_audio->a_out_buffer_len;
		fast_memcpy(buffer+size,sh_audio->a_out_buffer,len);
		sh_audio->a_out_buffer_len-=len; size+=len;
		if(sh_audio->a_out_buffer_len>0)
		    fast_memcpy(sh_audio->a_out_buffer,&sh_audio->a_out_buffer[len],sh_audio->a_out_buffer_len);
    }
    return size;
}

//---------------------------------------------------------------------------

static volatile int at_eof=0;
static volatile int interrupted=0;

static void exit_sighandler(int x){
    at_eof=1;
    interrupted=2; /* 1 means error */
}

static muxer_t* muxer=NULL;

static void add_subtitles(char *filename, float fps, int silent)
{
    sub_data *subd;
#ifdef CONFIG_ASS
    ASS_Track *asst = 0;
#endif

    if (!filename) return;

    subd = sub_read_file(filename, fps);
#ifdef CONFIG_ASS
    if (opts.ass_enabled)
#ifdef CONFIG_ICONV
        asst = ass_read_file(ass_library, filename, sub_cp);
#else
        asst = ass_read_file(ass_library, filename, 0);
#endif
    if (opts.ass_enabled && subd && !asst)
        asst = ass_read_subdata(ass_library, subd, fps);

    if (!asst && !subd && !silent)
#else
    if (!subd && !silent)
#endif
        mp_tmsg(MSGT_CPLAYER, MSGL_ERR, "Cannot load subtitles: %s\n",
                filename_recode(filename));

#ifdef CONFIG_ASS
    if (!asst && !subd) return;
    ass_track = asst;
#else
    if (!subd) return;
#endif
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_FILE_SUB_FILENAME=%s\n",
	   filename_recode(filename));
    subdata = subd;
}

/* This function returns the absolute time for which MEncoder will switch files
 * or move in the file so audio can be cut correctly. -1 if there is no limit. */
static float stop_time(demuxer_t* demuxer, muxer_stream_t* mux_v)
{
	float timeleft = -1;
	if (play_n_frames >= 0) timeleft = mux_v->timer + play_n_frames * (double)(mux_v->h.dwScale) / mux_v->h.dwRate;
	if (end_at.type == END_AT_TIME && (timeleft > end_at.pos || timeleft == -1)) timeleft = end_at.pos;
	if (next_edl_record && demuxer && demuxer->video) { // everything is OK to be checked
		float tmp = mux_v->timer + next_edl_record->start_sec - demuxer->video->pts;
		if (timeleft == -1 || timeleft > tmp) {
			// There's less time in EDL than what we already know
			if (next_edl_record->action == EDL_SKIP && edl_seeking) {
				timeleft = tmp;
			} else if (next_edl_record->action == EDL_MUTE) {
				//timeleft = next_edl_record->start_sec - demuxer->video->pts;
				// For the moment (and probably forever) EDL mute doesn't work in MEncoder
			}
		}
	}
	return timeleft;
}

/// Returns a_pts
static float calc_a_pts(demux_stream_t *d_audio)
{
    sh_audio_t * sh_audio = d_audio ? d_audio->sh : NULL;
    float a_pts = 0.;
    if (sh_audio)
        a_pts = d_audio->pts + (ds_tell_pts(d_audio) - sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
    return a_pts;
}

/** \brief Seeks audio forward to pts by dumping audio packets
 *  \return The current audio pts. */
static float forward_audio(float pts, demux_stream_t *d_audio, muxer_stream_t* mux_a)
{
    sh_audio_t * sh_audio = d_audio ? d_audio->sh : NULL;
    int samplesize, avg;
    float a_pts = calc_a_pts(d_audio);

    if (!sh_audio) return a_pts;

    if (sh_audio->audio.dwScale) samplesize = sh_audio->audio.dwSampleSize;
    else samplesize = (sh_audio->wf ? sh_audio->wf->nBlockAlign : 1);
    avg = (sh_audio->wf ? sh_audio->wf->nAvgBytesPerSec : sh_audio->i_bps);

    // after a demux_seek, a_pts will be zero until you read some audio.
    // carefully checking if a_pts is truely correct by reading tiniest amount of data possible.
    if (pts > a_pts && a_pts == 0.0 && samplesize) {
        if (demux_read_data(sh_audio->ds,mux_a->buffer,samplesize) <= 0) return a_pts; // EOF
        a_pts = calc_a_pts(d_audio);
    }

    while (pts > a_pts) {
        int len;
        if (samplesize) {
            len = avg * (pts - a_pts > 0.5 ? 0.5 : pts - a_pts);
            len/= samplesize; if(len<1) len=1;
            len*= samplesize;
            len = demux_read_data(sh_audio->ds,mux_a->buffer,len);
        } else {
            unsigned char * crap;
            len = ds_get_packet(sh_audio->ds, &crap);
        }
        if (len <= 0) break; // EOF of audio.
        a_pts = calc_a_pts(d_audio);
    }
    return a_pts;
}

/** \brief Seeks slowly by dumping frames.
 *  \return 1 for success, 2 for EOF. */
static int slowseek(float end_pts, demux_stream_t *d_video,
                    demux_stream_t *d_audio, muxer_stream_t *mux_a,
                    s_frame_data *frame_data, int framecopy, int print_info)
{
    sh_video_t * sh_video = d_video->sh;
    vf_instance_t * vfilter = sh_video ? sh_video->vfilter : NULL;
    int done = 0;

    while (!interrupted) {
        float a_pts = 0.;

        if (!frame_data->already_read) { // when called after fixdelay, a frame is already read
            frame_data->in_size = video_read_frame(sh_video, &frame_data->frame_time, &frame_data->start, force_fps);
            if(frame_data->in_size<0) return 2;
            sh_video->timer += frame_data->frame_time;
        }
        frame_data->already_read = 0;

        a_pts = forward_audio(sh_video->pts - frame_data->frame_time + audio_delay, d_audio, mux_a);

        if (done) {
            // wait for keyframe in case of -ovc copy
            if (!framecopy || (sh_video->ds->flags & 1)) {
                frame_data->already_read = 1;
                return 1;
            }
        }
        if (sh_video->pts >= end_pts) done = 1;

        if (vfilter) {
            sh_video->vfilter->control(sh_video->vfilter, VFCTRL_SET_OSD_OBJ,
                                       osd);
            int softskip = (vfilter->control(vfilter, VFCTRL_SKIP_NEXT_FRAME, 0) == CONTROL_TRUE);
            void *decoded_frame = decode_video(sh_video, frame_data->start, frame_data->in_size, !softskip, MP_NOPTS_VALUE);
	    if (decoded_frame)
                if (filter_video(sh_video, decoded_frame, MP_NOPTS_VALUE)) {
                    struct vf_instance *vf = sh_video->vfilter;
                    vf->control(vf, VFCTRL_DRAW_EOSD, NULL);
                    vf->control(vf, VFCTRL_DRAW_OSD, osd);
                }
        }

        if (print_info) mp_tmsg(MSGT_MENCODER, MSGL_STATUS,
               "EDL SKIP: Start: %.2f  End: %.2f   Current: V: %.2f  A: %.2f     \r",
               next_edl_record->start_sec, next_edl_record->stop_sec,
               sh_video->pts, a_pts);
    }
    if (interrupted) return 2;
    return 1;
}

/// Deletes audio or video as told by -delay to sync
static void fixdelay(demux_stream_t *d_video, demux_stream_t *d_audio,
                     muxer_stream_t *mux_a, s_frame_data *frame_data,
                     int framecopy)
{
    // TODO: Find a way to encode silence instead of deleting video
    sh_video_t * sh_video = d_video->sh;
    float a_pts;

    // demux_seek has a weirdness that sh_video->pts is meaningless,
    // until a single frame is read... Same for audio actually too.
    // Reading one frame, and keeping it.
    frame_data->in_size = video_read_frame(sh_video, &frame_data->frame_time, &frame_data->start, force_fps);
    if(frame_data->in_size<0) return;
    sh_video->timer += frame_data->frame_time;
    frame_data->already_read = 1;

    a_pts = forward_audio(sh_video->pts - frame_data->frame_time + audio_delay, d_audio, mux_a);

    if (audio_delay > 0) return;
    else if (sh_video->pts - frame_data->frame_time + audio_delay >= a_pts) return;

    slowseek(a_pts - audio_delay, d_video, d_audio, mux_a, frame_data, framecopy, 0);
}

/** \brief Seeks for EDL
 *  \return 1 for success, 0 for failure, 2 for EOF. */
static int edl_seek(edl_record_ptr next_edl_record, demuxer_t *demuxer,
                    demux_stream_t *d_audio, muxer_stream_t *mux_a,
                    s_frame_data *frame_data, int framecopy)
{
    sh_video_t * sh_video = demuxer->video ? demuxer->video->sh : NULL;

    if (!sh_video) return 0;
    if (sh_video->pts >= next_edl_record->stop_sec) return 1; // nothing to do...

    if (!edl_seek_type) {
        if(demux_seek(demuxer, next_edl_record->stop_sec - sh_video->pts, audio_delay, 0)){
            sh_video->pts = demuxer->video->pts;
            //if (vo_vobsub) vobsub_seek(vo_vobsub,sh_video->pts);
            resync_video_stream(sh_video);
            //if(vo_spudec) spudec_reset(vo_spudec);
            if (audio_delay != 0.0) fixdelay(demuxer->video, d_audio, mux_a, frame_data, framecopy);
            return 1;
        }
        // non-seekable stream.
        return 0;
    }

    // slow seek, read every frame.

    return slowseek(next_edl_record->stop_sec, demuxer->video, d_audio, mux_a, frame_data, framecopy, 1);
}


#ifdef PTW32_STATIC_LIB
static void detach_ptw32(void)
{
  pthread_win32_thread_detach_np();
  pthread_win32_process_detach_np();
}
#endif

int main(int argc,char* argv[]){

stream_t* stream=NULL;
stream_t* ostream=NULL;
demuxer_t* demuxer=NULL;
stream_t* stream2=NULL;
demuxer_t* demuxer2=NULL;
demux_stream_t *d_audio=NULL;
demux_stream_t *d_video=NULL;
demux_stream_t *d_dvdsub=NULL;
sh_audio_t *sh_audio=NULL;
sh_video_t *sh_video=NULL;
int file_format=DEMUXER_TYPE_UNKNOWN;
int i=DEMUXER_TYPE_UNKNOWN;
void *vobsub_writer=NULL;
s_frame_data frame_data = { .start = NULL, .in_size = 0, .frame_time = 0., .already_read = 0 };

uint32_t ptimer_start;
uint32_t audiorate=0;
uint32_t videorate=0;
uint32_t audiosamples=1;
uint32_t videosamples=1;
uint32_t skippedframes=0;
uint32_t duplicatedframes=0;
uint32_t badframes=0;

muxer_stream_t* mux_a=NULL;
muxer_stream_t* mux_v=NULL;
off_t muxer_f_size=0;

double v_pts_corr=0;
double v_timer_corr=0;

m_entry_t* filelist = NULL;
char* filename=NULL;

int decoded_frameno=0;
int next_frameno=-1;
int curfile=0;
int new_srate=0;

unsigned int timer_start=0;
ao_data_t ao_data = {0,0,0,0,OUTBURST,-1,0};

audio_encoding_params_t aparams;
audio_encoder_t *aencoder = NULL;

#ifdef PTW32_STATIC_LIB
  pthread_win32_process_attach_np();
  pthread_win32_thread_attach_np();
  atexit(detach_ptw32);
#endif

  mp_msg_init();
  set_av_log_callback();

  // Create the config context and register the options
  set_default_mencoder_options(&opts);
  mconfig = m_config_new(&opts, cfg_include);
  m_config_register_options(mconfig,mencoder_opts);

  // Preparse the command line
  m_config_preparse_command_line(mconfig,argc,argv);

  print_version("MEncoder");

#if (defined(__MINGW32__) || defined(__CYGWIN__)) && defined(CONFIG_WIN32DLL)
  set_path_env();
#endif

  InitTimer();

// check codec.conf
if(!codecs_file || !parse_codec_cfg(codecs_file)){
  if(!parse_codec_cfg(get_path("codecs.conf"))){
    if(!parse_codec_cfg(MPLAYER_CONFDIR "/codecs.conf")){
      if(!parse_codec_cfg(NULL)){
	mencoder_exit(1,NULL);
      }
      mp_tmsg(MSGT_MENCODER,MSGL_V,"Using built-in default codecs.conf.\n");
    }
  }
}

 parse_cfgfiles(mconfig);
 filelist = m_config_parse_me_command_line(mconfig, argc, argv);
 if(!filelist) mencoder_exit(1, _("error parsing command line"));

{
	char *extension;

	if (!out_filename) mencoder_exit(1,_("No output file specified, please see the -o option."));
	extension=strrchr(out_filename,'.');
	if (extension != NULL && strlen(extension) > 3 && strlen(extension) < 6)
	{
		extension++;

		switch (out_file_format)
		{
			case MUXER_TYPE_AVI:
			if (strcasecmp(extension,"avi"))
				mp_tmsg(MSGT_MENCODER, MSGL_WARN, "\nWARNING: OUTPUT FILE FORMAT IS _AVI_. See -of help.\n");
			break;

			case MUXER_TYPE_MPEG:
			if (strcasecmp(extension,"mpg") &&
				strcasecmp(extension,"mpeg") &&
				strcasecmp(extension,"vob"))
				mp_tmsg(MSGT_MENCODER, MSGL_WARN, "\nWARNING: OUTPUT FILE FORMAT IS _MPEG_. See -of help.\n");
			break;
		}
	}
}
 /* Display what configure line was used */
 mp_msg(MSGT_MENCODER, MSGL_V, "Configuration: " CONFIGURATION "\n");

#define FormatNotRecognized _("============ Sorry, this file format is not recognized/supported =============\n"\
"=== If this file is an AVI, ASF or MPEG stream, please contact the author! ===\n")


if (frameno_filename) {
  stream2=open_stream(frameno_filename,0,&i);
  if(stream2){
    demuxer2=demux_open(&opts, stream2,DEMUXER_TYPE_AVI,-1,-1,-2,NULL);
    if(demuxer2) mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Using pass3 control file: %s\n", frameno_filename);
    else mp_tmsg(MSGT_DEMUXER,MSGL_ERR, FormatNotRecognized);
  }
}

#ifdef CONFIG_PRIORITY
  set_priority();
#endif

  if (codec_path)
    set_codec_path(codec_path);

// check font
#ifdef CONFIG_FREETYPE
  init_freetype();
#endif
#ifdef CONFIG_FONTCONFIG
  if(font_fontconfig <= 0)
  {
#endif
#ifdef CONFIG_BITMAP_FONT
  if(font_name){
       vo_font=read_font_desc(font_name,font_factor,verbose>1);
       if(!vo_font) mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Cannot load bitmap font: %s\n",font_name);
  } else {
      // try default:
       vo_font=read_font_desc(get_path("font/font.desc"),font_factor,verbose>1);
       if(!vo_font)
         vo_font=read_font_desc(MPLAYER_DATADIR "/font/font.desc",font_factor,verbose>1);
  }
#endif
#ifdef CONFIG_FONTCONFIG
  }
#endif

  osd = osd_create();

  /* HACK, for some weird reason, push() has to be called twice,
     otherwise options are not saved correctly */
  m_config_push(mconfig);
play_next_file:
  m_config_push(mconfig);
  m_entry_set_options(mconfig,&filelist[curfile]);
  filename = filelist[curfile].name;

#ifdef CONFIG_ASS
  ass_library = ass_init();
#endif

  if(!filename){
	mp_tmsg(MSGT_CPLAYER, MSGL_FATAL, "\nFilename missing.\n\n");
	mencoder_exit(1,NULL);
  }
  stream=open_stream(filename,0,&file_format);

  if(!stream){
	mp_tmsg(MSGT_CPLAYER, MSGL_FATAL, "Cannot open file/device.\n");
	mencoder_exit(1,NULL);
  }

  mp_tmsg(MSGT_CPLAYER, MSGL_INFO, "success: format: %d  data: 0x%X - 0x%x\n", file_format, (int)(stream->start_pos), (int)(stream->end_pos));

#ifdef CONFIG_DVDREAD
if(stream->type==STREAMTYPE_DVD){
  if(audio_lang && opts.audio_id==-1) opts.audio_id=dvd_aid_from_lang(stream,audio_lang);
  if(dvdsub_lang && opts.sub_id==-1) opts.sub_id=dvd_sid_from_lang(stream,dvdsub_lang);
}
#endif

#ifdef CONFIG_DVDNAV
if(stream->type==STREAMTYPE_DVDNAV){
  if(audio_lang && opts.audio_id==-1) opts.audio_id=mp_dvdnav_aid_from_lang(stream,audio_lang);
  if(dvdsub_lang && opts.sub_id==-1) opts.sub_id=mp_dvdnav_sid_from_lang(stream,dvdsub_lang);
}
#endif

  stream->start_pos+=seek_to_byte;

  if(stream_cache_size>0) stream_enable_cache(stream,stream_cache_size*1024,0,0);

  if(demuxer2) opts.audio_id=-2; /* do NOT read audio packets... */

  demuxer=demux_open(&opts, stream,file_format,opts.audio_id,opts.video_id,opts.sub_id,filename);
  if(!demuxer){
    mp_tmsg(MSGT_DEMUXER, MSGL_FATAL, FormatNotRecognized);
    mp_tmsg(MSGT_DEMUXER, MSGL_FATAL, "Cannot open demuxer.\n");
	mencoder_exit(1,NULL);
  }

  if (ts_prog) {
    demux_program_t prog = { .progid = ts_prog };
    if (demux_control(demuxer, DEMUXER_CTRL_IDENTIFY_PROGRAM, &prog) != DEMUXER_CTRL_NOTIMPL) {
      opts.audio_id = prog.aid; // switching is handled by select_audio below
      opts.video_id = prog.vid;
      demuxer_switch_video(demuxer, opts.video_id);
    }
  }
  select_audio(demuxer, opts.audio_id, audio_lang);

  if (opts.sub_id < -1 && dvdsub_lang)
    opts.sub_id = demuxer_sub_track_by_lang(demuxer, dvdsub_lang);

  if (opts.sub_id < -1)
    opts.sub_id = demuxer_default_sub_track(demuxer);

  for (i = 0; i < MAX_S_STREAMS; i++) {
    sh_sub_t *sh = demuxer->s_streams[i];
    if (sh && sh->sid == opts.sub_id) {
      demuxer->sub->id = i;
      demuxer->sub->sh = sh;
      break;
    }
  }

  if(opts.chapterrange[0]>1) {
    double pts;
    if (demuxer_seek_chapter(demuxer, opts.chapterrange[0]-1, &pts, NULL) >= 0 && pts > -1.0)
      seek_to_sec = pts;
  }

d_audio=demuxer2 ? demuxer2->audio : demuxer->audio;
d_video=demuxer->video;
d_dvdsub=demuxer->sub;
sh_audio=d_audio->sh;
sh_video=d_video->sh;

  if(!sh_video)
  {
	mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"Video stream is mandatory!\n");
	mencoder_exit(1,NULL);
  }

  if(!video_read_properties(sh_video)){
      mp_tmsg(MSGT_CPLAYER, MSGL_FATAL, "Video: Cannot read properties.\n");
      mencoder_exit(1,NULL);
  }

  mp_tmsg(MSGT_MENCODER,MSGL_INFO, "[V] filefmt:%d  fourcc:0x%X  size:%dx%d  fps:%5.3f  ftime:=%6.4f\n",
   demuxer->file_format,sh_video->format, sh_video->disp_w,sh_video->disp_h,
   sh_video->fps,sh_video->frametime
  );

  if(force_fps){
    sh_video->fps=force_fps;
    sh_video->frametime=1.0f/sh_video->fps;
    mp_tmsg(MSGT_MENCODER,MSGL_INFO,"Input fps will be interpreted as %5.3f instead.\n", sh_video->fps);
  }

  if(sh_audio && out_audio_codec<0){
    if(opts.audio_id==-2)
	mp_tmsg(MSGT_MENCODER,MSGL_ERR,"This demuxer doesn't support -nosound yet.\n");
    mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nNo audio encoder (-oac) selected. Select one (see -oac help) or use -nosound.\n");
    mencoder_exit(1,NULL);
  }
  if(sh_video && out_video_codec<0){
    mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nNo video encoder (-ovc) selected. Select one (see -ovc help).\n");
    mencoder_exit(1,NULL);
  }

if(sh_audio && (out_audio_codec || seek_to_sec || !sh_audio->wf || opts.playback_speed != 1.0)){
  // Go through the codec.conf and find the best codec...
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
  if(!init_best_audio_codec(sh_audio,audio_codec_list,audio_fm_list)){
    sh_audio=d_audio->sh=NULL; // failed to init :(
  }
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
}

  if (sh_audio) {
    new_srate = sh_audio->samplerate;
    if (opts.playback_speed != 1.0) {
        new_srate *= opts.playback_speed;
        // limits are taken from libaf/af_resample.c
        if (new_srate < 8000) new_srate = 8000;
        if (new_srate > 192000) new_srate = 192000;
        opts.playback_speed = (float)new_srate / (float)sh_audio->samplerate;
    }
  }

// set up video encoder:

if (!curfile) { // curfile is non zero when a second file is opened
if (vobsub_out) {
    unsigned int palette[16], width, height;
    unsigned char tmp[3] = { 0, 0, 0 };
    if (spudec_ifo && vobsub_parse_ifo(NULL,spudec_ifo, palette, &width, &height, 1, opts.sub_id, tmp) >= 0)
	vobsub_writer = vobsub_out_open(vobsub_out, palette, sh_video->disp_w, sh_video->disp_h,
					vobsub_out_id?vobsub_out_id:(char *)tmp, vobsub_out_index);
#ifdef CONFIG_DVDREAD
    if (vobsub_writer == NULL) {
	char tmp[3];
	if (vobsub_out_id == NULL && stream->type == STREAMTYPE_DVD) {
	    int i;
	    dvd_priv_t *dvd = (dvd_priv_t*)stream->priv;
	    for (i = 0; i < dvd->nr_of_subtitles; ++i)
		if (dvd->subtitles[i].id == opts.sub_id) {
		    tmp[0] = (dvd->subtitles[i].language >> 8) & 0xff;
		    tmp[1] = dvd->subtitles[i].language & 0xff;
		    tmp[2] = 0;
		    vobsub_out_id = tmp;
		    break;
		}
	}
	vobsub_writer=vobsub_out_open(vobsub_out, stream->type==STREAMTYPE_DVD?((dvd_priv_t *)(stream->priv))->cur_pgc->palette:NULL,
				      sh_video->disp_w, sh_video->disp_h, vobsub_out_id, vobsub_out_index);
    }
#endif
}
else {
if (spudec_ifo) {
  unsigned int palette[16], width, height;
  if (vobsub_parse_ifo(NULL,spudec_ifo, palette, &width, &height, 1, -1, NULL) >= 0)
    vo_spudec=spudec_new_scaled(palette, sh_video->disp_w, sh_video->disp_h, NULL, 0);
}
#ifdef CONFIG_DVDREAD
if (vo_spudec==NULL) {
vo_spudec=spudec_new_scaled(stream->type==STREAMTYPE_DVD?((dvd_priv_t *)(stream->priv))->cur_pgc->palette:NULL,
                           sh_video->disp_w, sh_video->disp_h, NULL, 0);
}
#endif
if (vo_spudec)
  spudec_set_forced_subs_only(vo_spudec, forced_subs_only);
}

ostream = open_output_stream(out_filename, 0);
if(!ostream) {
  mp_tmsg(MSGT_MENCODER, MSGL_FATAL, "Cannot open output file '%s'.\n", out_filename);
  mencoder_exit(1,NULL);
}

muxer=muxer_new_muxer(out_file_format,ostream);
if(!muxer) {
  mp_tmsg(MSGT_MENCODER, MSGL_FATAL, "Cannot initialize muxer.");
  mencoder_exit(1,NULL);
}
#if 0
//disabled: it horrybly distorts filtered sound
if(out_file_format == MUXER_TYPE_MPEG) audio_preload = 0;
#endif

muxer->audio_delay_fix = audio_delay_fix;

// ============= VIDEO ===============

mux_v=muxer_new_stream(muxer,MUXER_TYPE_VIDEO);

mux_v->buffer_size=0x200000; // 2MB
mux_v->buffer=malloc(mux_v->buffer_size);

mux_v->source=sh_video;

mux_v->h.dwSampleSize=0; // VBR
#ifdef CONFIG_LIBAVCODEC
{
    double fps = force_ofps?force_ofps:sh_video->fps*opts.playback_speed;
    AVRational q= av_d2q(fps, fps*1001+2);
    mux_v->h.dwScale= q.den;
    mux_v->h.dwRate = q.num;
}
#else
mux_v->h.dwScale=10000;
mux_v->h.dwRate=mux_v->h.dwScale*(force_ofps?force_ofps:sh_video->fps*opts.playback_speed);
#endif

mux_v->codec=out_video_codec;

mux_v->bih=NULL;
}
sh_video->codec=NULL;
sh_video->vfilter=NULL; // fixme!

switch(mux_v->codec){
case VCODEC_COPY:
	if (!curfile) {
		if (sh_video->bih) {
			mux_v->bih=malloc(sh_video->bih->biSize);
			memcpy(mux_v->bih, sh_video->bih, sh_video->bih->biSize);
		}
    else
    {
	mux_v->bih=calloc(1,sizeof(BITMAPINFOHEADER));
	mux_v->bih->biSize=sizeof(BITMAPINFOHEADER);
	mux_v->bih->biWidth=sh_video->disp_w;
	mux_v->bih->biHeight=sh_video->disp_h;
	mux_v->bih->biCompression=sh_video->format;
	mux_v->bih->biPlanes=1;
	mux_v->bih->biBitCount=24; // FIXME!!!
	mux_v->bih->biSizeImage=mux_v->bih->biWidth*mux_v->bih->biHeight*(mux_v->bih->biBitCount/8);
    }
	}
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n",
	mux_v->bih->biWidth, mux_v->bih->biHeight,
	mux_v->bih->biBitCount, mux_v->bih->biCompression);

	if (curfile) {
		if (sh_video->bih) {
			if ((mux_v->bih->biSize != sh_video->bih->biSize) ||
			    memcmp(mux_v->bih, sh_video->bih, sh_video->bih->biSize))
			{
				mp_tmsg(MSGT_MENCODER, MSGL_INFO, "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n",
				       sh_video->bih->biWidth, sh_video->bih->biHeight,
				       sh_video->bih->biBitCount, sh_video->bih->biCompression);
				mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nAll video files must have identical fps, resolution, and codec for -ovc copy.\n");
				mencoder_exit(1,NULL);
			}
		}
		else {
			if ((mux_v->bih->biWidth != sh_video->disp_w) ||
			    (mux_v->bih->biHeight != sh_video->disp_h) ||
			    (mux_v->bih->biCompression != sh_video->format)) {
				mp_tmsg(MSGT_MENCODER, MSGL_INFO, "videocodec: framecopy (%dx%d %dbpp fourcc=%x)\n",
				       sh_video->disp_w, sh_video->disp_w, 24, sh_video->format);
				mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nAll video files must have identical fps, resolution, and codec for -ovc copy.\n");
				mencoder_exit(1,NULL);
			}
		}
	}
    break;
case VCODEC_FRAMENO:
	if (!curfile) {
    mux_v->bih=calloc(1,sizeof(BITMAPINFOHEADER));
    mux_v->bih->biSize=sizeof(BITMAPINFOHEADER);
    mux_v->bih->biWidth=sh_video->disp_w;
    mux_v->bih->biHeight=sh_video->disp_h;
    mux_v->bih->biPlanes=1;
    mux_v->bih->biBitCount=24;
    mux_v->bih->biCompression=mmioFOURCC('F','r','N','o');
    mux_v->bih->biSizeImage=mux_v->bih->biWidth*mux_v->bih->biHeight*(mux_v->bih->biBitCount/8);
	}
    break;
default: {
    static vf_instance_t * ve = NULL;
  if (!ve) {
    switch(mux_v->codec){
    case VCODEC_LIBAVCODEC:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"lavc",(char *)mux_v); break;
    case VCODEC_RAW:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"raw",(char *)mux_v); break;
    case VCODEC_VFW:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"vfw",(char *)mux_v); break;
    case VCODEC_LIBDV:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"libdv",(char *)mux_v); break;
    case VCODEC_XVID:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"xvid",(char *)mux_v); break;
    case VCODEC_QTVIDEO:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"qtvideo",(char *)mux_v); break;
    case VCODEC_NUV:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"nuv",(char *)mux_v); break;
    case VCODEC_X264:
        sh_video->vfilter=vf_open_encoder(&opts, NULL,"x264",(char *)mux_v); break;
    }
    if(!mux_v->bih || !sh_video->vfilter){
        mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"Failed to open the encoder.\n");
        mencoder_exit(1,NULL);
    }
    ve = sh_video->vfilter;
  } else sh_video->vfilter = ve;
    // append 'expand' filter, it fixes stride problems and renders osd:
#ifdef CONFIG_ASS
    if (auto_expand && !opts.ass_enabled) { /* we do not want both */
#else
    if (auto_expand) {
#endif
      char* vf_args[] = { "osd", "1", NULL };
      sh_video->vfilter=vf_open_filter(&opts, sh_video->vfilter,"expand",vf_args);
    }

#ifdef CONFIG_ASS
  if(opts.ass_enabled) {
    int i;
    int insert = 1;
    if (opts.vf_settings)
      for (i = 0; opts.vf_settings[i].name; ++i)
        if (strcmp(opts.vf_settings[i].name, "ass") == 0) {
          insert = 0;
          break;
        }
    if (insert) {
      extern vf_info_t vf_info_ass;
      vf_info_t* libass_vfs[] = {&vf_info_ass, NULL};
      char* vf_arg[] = {"auto", "1", NULL};
      vf_instance_t* vf_ass = vf_open_plugin(&opts,libass_vfs,sh_video->vfilter,"ass",vf_arg);
      if (vf_ass)
        sh_video->vfilter=(void*)vf_ass;
      else
        mp_msg(MSGT_CPLAYER,MSGL_ERR, "ASS: cannot add video filter\n");
    }

    if (ass_library) {
      for (i = 0; i < demuxer->num_attachments; ++i) {
        demux_attachment_t* att = demuxer->attachments + i;
        if (use_embedded_fonts &&
            att->name && att->type && att->data && att->data_size &&
            (strcmp(att->type, "application/x-truetype-font") == 0 ||
             strcmp(att->type, "application/x-font") == 0))
          ass_add_font(ass_library, att->name, att->data, att->data_size);
      }
    }
  }
#endif

    sh_video->vfilter=append_filters(sh_video->vfilter, opts.vf_settings);

#ifdef CONFIG_ASS
  if (opts.ass_enabled)
    ((vf_instance_t *)sh_video->vfilter)->control(sh_video->vfilter, VFCTRL_INIT_EOSD, ass_library);
#endif

// after reading video params we should load subtitles because
// we know fps so now we can adjust subtitles time to ~6 seconds AST
// check .sub
  if(sub_name && sub_name[0]){
    for (i = 0; sub_name[i] != NULL; ++i)
        add_subtitles (sub_name[i], sh_video->fps, 0);
  } else
  if(sub_auto && filename) { // auto load sub file ...
    char **tmp = NULL;
    int i = 0;
    char *psub = get_path( "sub/" );
    tmp = sub_filenames((psub ? psub : ""), filename);
    free(psub);
    while (tmp[i])
    {
      add_subtitles (tmp[i], sh_video->fps, 0);
      free(tmp[i++]);
    }
    free(tmp);
  }

    mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
    init_best_video_codec(sh_video,video_codec_list,video_fm_list);
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
    if(!sh_video->initialized) mencoder_exit(1,NULL);
 }
}

if (!curfile) {
/* force output fourcc to .. */
if ((force_fourcc != NULL) && (strlen(force_fourcc) >= 4))
{
    mux_v->bih->biCompression = mmioFOURCC(force_fourcc[0], force_fourcc[1],
					    force_fourcc[2], force_fourcc[3]);
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Forcing output FourCC to %x [%.4s].\n",
	mux_v->bih->biCompression, (char *)&mux_v->bih->biCompression);
}

if (! ignore_start)
    muxer->audio_delay_fix -= sh_video->stream_delay;

//if(demuxer->file_format!=DEMUXER_TYPE_AVI) pts_from_bps=0; // it must be 0 for mpeg/asf!

// ============= AUDIO ===============
if(sh_audio){

if (force_audiofmttag != -1) {
	sh_audio->format = force_audiofmttag;
	if (sh_audio->wf) {
		sh_audio->wf->wFormatTag = sh_audio->format;
	}
	mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Forcing output audio format tag to 0x%x.\n",
	       force_audiofmttag);
}

mux_a=muxer_new_stream(muxer,MUXER_TYPE_AUDIO);

mux_a->buffer_size=0x100000; //16384;
mux_a->buffer=malloc(mux_a->buffer_size);
if (!mux_a->buffer)
    mencoder_exit(1,_("Memory allocation failed.\n"));

mux_a->source=sh_audio;

mux_a->codec=out_audio_codec;

ao_data.samplerate = force_srate;
ao_data.channels = 0;
ao_data.format = audio_output_format;
if(!init_audio_filters(sh_audio,
   // input:
   new_srate,
   // output:
   &ao_data.samplerate, &ao_data.channels, &ao_data.format)) {
     mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Error at audio filter chain pre-init!\n");
     mencoder_exit(1, NULL);
   }

aparams.channels = ao_data.channels;
aparams.sample_rate = ao_data.samplerate;
aparams.audio_preload = 1000 * audio_preload;
if(mux_a->codec != ACODEC_COPY) {
    aencoder = new_audio_encoder(mux_a, &aparams);
    if(!aencoder)
        mencoder_exit(1, NULL);
    if(!init_audio_filters(sh_audio,
        new_srate,
        &aparams.sample_rate, &aparams.channels, &aencoder->input_format)) {
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"Couldn't find matching filter/ao format!\n");
      mencoder_exit(1,NULL);
    }
}

#define NoSpeedWithFrameCopy _("WARNING: -speed is not guaranteed to work correctly with -oac copy!\n"\
"Your encode might be broken!\n")

switch(mux_a->codec){
case ACODEC_COPY:
    if (opts.playback_speed != 1.0) mp_tmsg(MSGT_CPLAYER, MSGL_WARN, NoSpeedWithFrameCopy);
    if (sh_audio->format >= 0x10000) {
	mp_tmsg(MSGT_MENCODER,MSGL_ERR,"Audio format 0x%x is incompatible with '-oac copy', please try '-oac pcm' instead or use '-fafmttag' to override it.\n", sh_audio->format);
	mencoder_exit(1,NULL);
    }
    if (sh_audio->wf){
	mux_a->wf=malloc(sizeof(WAVEFORMATEX) + sh_audio->wf->cbSize);
	memcpy(mux_a->wf, sh_audio->wf, sizeof(WAVEFORMATEX) + sh_audio->wf->cbSize);
	if(!sh_audio->i_bps) sh_audio->i_bps=mux_a->wf->nAvgBytesPerSec;
    } else {
	mux_a->wf = malloc(sizeof(WAVEFORMATEX));
	mux_a->wf->nBlockAlign = 1; //mux_a->h.dwSampleSize;
	mux_a->wf->wFormatTag = sh_audio->format;
	mux_a->wf->nChannels = sh_audio->channels;
	mux_a->wf->nSamplesPerSec = sh_audio->samplerate;
	mux_a->wf->nAvgBytesPerSec=sh_audio->i_bps; //mux_a->h.dwSampleSize*mux_a->wf->nSamplesPerSec;
	mux_a->wf->wBitsPerSample = 16; // FIXME
	mux_a->wf->cbSize=0; // FIXME for l3codeca.acm
    }
    if(sh_audio->audio.dwScale){
	mux_a->h.dwSampleSize=sh_audio->audio.dwSampleSize;
	mux_a->h.dwScale=sh_audio->audio.dwScale;
	mux_a->h.dwRate=sh_audio->audio.dwRate;
    } else {
	mux_a->h.dwSampleSize=mux_a->wf->nBlockAlign;
	mux_a->h.dwScale=mux_a->h.dwSampleSize;
	mux_a->h.dwRate=mux_a->wf->nAvgBytesPerSec;
    }
    mux_a->h.dwRate *= opts.playback_speed;
    mux_a->wf->nSamplesPerSec *= opts.playback_speed;
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "audiocodec: framecopy (format=%x chans=%d rate=%d bits=%d B/s=%d sample-%d)\n",
	mux_a->wf->wFormatTag, mux_a->wf->nChannels, mux_a->wf->nSamplesPerSec,
	mux_a->wf->wBitsPerSample, mux_a->wf->nAvgBytesPerSec, mux_a->h.dwSampleSize);
    break;
}

if ( mp_msg_test(MSGT_MENCODER,MSGL_DBG2) ) print_wave_header(mux_a->wf, MSGL_DBG2);

if (! ignore_start)
    muxer->audio_delay_fix += sh_audio->stream_delay;

} // if(sh_audio)

decoded_frameno=0;

signal(SIGINT,exit_sighandler);  // Interrupt from keyboard
signal(SIGQUIT,exit_sighandler); // Quit from keyboard
signal(SIGTERM,exit_sighandler); // kill
signal(SIGHUP,exit_sighandler);  // broken terminal line
signal(SIGPIPE,exit_sighandler); // broken pipe

timer_start=GetTimerMS();
} // if (!curfile) // if this was the first file.
else {
	if (!mux_a != !sh_audio) {
		mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nCannot mix video-only files with audio and video files. Try -nosound.\n");
		mencoder_exit(1,NULL);
	}
	if (sh_audio && mux_a->codec == ACODEC_COPY) {
		if (opts.playback_speed != 1.0) mp_tmsg(MSGT_CPLAYER, MSGL_WARN, NoSpeedWithFrameCopy);
		mp_tmsg(MSGT_MENCODER, MSGL_INFO, "audiocodec: framecopy (format=%x chans=%d rate=%d bits=%d B/s=%d sample-%d)\n",
		       mux_a->wf->wFormatTag, mux_a->wf->nChannels, mux_a->wf->nSamplesPerSec,
		       mux_a->wf->wBitsPerSample, mux_a->wf->nAvgBytesPerSec, mux_a->h.dwSampleSize);
		if (sh_audio->wf) {
			if ((mux_a->wf->wFormatTag != sh_audio->wf->wFormatTag) ||
			    (mux_a->wf->nChannels != sh_audio->wf->nChannels) ||
			    (mux_a->wf->nSamplesPerSec != sh_audio->wf->nSamplesPerSec * opts.playback_speed))
			{
				mp_tmsg(MSGT_MENCODER, MSGL_INFO, "audiocodec: framecopy (format=%x chans=%d rate=%d bits=%d B/s=%d sample-%d)\n",
				       sh_audio->wf->wFormatTag, sh_audio->wf->nChannels, (int)(sh_audio->wf->nSamplesPerSec * opts.playback_speed),
				       sh_audio->wf->wBitsPerSample, sh_audio->wf->nAvgBytesPerSec, 0);
				mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nAll files must have identical audio codec and format for -oac copy.\n");
				mencoder_exit(1,NULL);
			}
		} else {
			if ((mux_a->wf->wFormatTag != sh_audio->format) ||
			    (mux_a->wf->nChannels != sh_audio->channels) ||
			    (mux_a->wf->nSamplesPerSec != sh_audio->samplerate * opts.playback_speed))
			{
				mp_tmsg(MSGT_MENCODER, MSGL_INFO, "audiocodec: framecopy (format=%x chans=%d rate=%d bits=%d B/s=%d sample-%d)\n",
				       sh_audio->wf->wFormatTag, sh_audio->wf->nChannels, (int)(sh_audio->wf->nSamplesPerSec * opts.playback_speed),
				       sh_audio->wf->wBitsPerSample, sh_audio->wf->nAvgBytesPerSec, 0);
				mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"\nAll files must have identical audio codec and format for -oac copy.\n");
				mencoder_exit(1,NULL);
			}
		}
	} else if (sh_audio) {
		int out_srate = mux_a->wf->nSamplesPerSec;
		int out_channels = mux_a->wf->nChannels;
		int out_format = aencoder->input_format;
		if (!init_audio_filters(sh_audio, new_srate,
					&out_srate, &out_channels,
					&out_format)) {
			mp_tmsg(MSGT_CPLAYER, MSGL_FATAL, "Couldn't find matching filter/ao format!\n");
			mencoder_exit(1, NULL);
		}
		mux_a->wf->nSamplesPerSec = out_srate;
		mux_a->wf->nChannels = out_channels;
	}
}

if (seek_to_sec) {
    demux_seek(demuxer, seek_to_sec, audio_delay, 1);
//  there is 2 way to handle the -ss option in 3-pass mode:
// > 1. do the first pass for the whole file, and use -ss for 2nd/3rd pases only
// > 2. do all the 3 passes with the same -ss value
//  this line enables behaviour 1. (and kills 2. at the same time):
//    if(demuxer2) demux_seek(demuxer2, d, audio_delay, 1);
}

if (out_file_format == MUXER_TYPE_MPEG)
	{
	if (audio_preload > 0.4) {
	  mp_tmsg(MSGT_MENCODER, MSGL_WARN, "Limiting audio preload to 0.4s.\n");
	  audio_preload = 0.4;
	}
	if (audio_density < 4) {
	  mp_tmsg(MSGT_MENCODER, MSGL_WARN, "Increasing audio density to 4.\n");
	  audio_density = 4;
	}
	}

if(file_format == DEMUXER_TYPE_TV)
	{
	mp_tmsg(MSGT_MENCODER, MSGL_WARN, "Forcing audio preload to 0, max pts correction to 0.\n");
	audio_preload = 0.0;
	default_max_pts_correction = 0;
	}

play_n_frames=play_n_frames_mf;
if (curfile && end_at.type == END_AT_TIME) end_at.pos += mux_v->timer;

if (edl_records) free_edl(edl_records);
next_edl_record = edl_records = NULL;
edl_muted = 0;
edl_seeking = 1;
if (edl_filename) {
    next_edl_record = edl_records = edl_parse_file();
}

if (sh_audio && audio_delay != 0.) fixdelay(d_video, d_audio, mux_a, &frame_data, mux_v->codec==VCODEC_COPY);

while(!at_eof){

    int blit_frame=0;
    float a_pts=0;
    float v_pts=0;
    int skip_flag=0; // 1=skip  -1=duplicate

    if((end_at.type == END_AT_SIZE && end_at.pos <= stream_tell(muxer->stream))  ||
       (end_at.type == END_AT_TIME && end_at.pos < mux_v->timer))
        break;

    if(play_n_frames>=0){
      --play_n_frames;
      if(play_n_frames<0) break;
    }
    if(opts.chapterrange[1]>0) {
      int cur_chapter = demuxer_get_current_chapter(demuxer);
      if(cur_chapter!=-1 && cur_chapter+1>opts.chapterrange[1])
        break;
    }

goto_redo_edl:
    if (next_edl_record && sh_video && sh_video->pts >= next_edl_record->start_sec) {
        if (next_edl_record->action == EDL_SKIP && edl_seeking) {
            float last_pos = d_video->pts;
            int result;
            mp_msg(MSGT_CPLAYER, MSGL_DBG4, "EDL_SKIP: start [%f], stop [%f], length [%f]\n",
                   next_edl_record->start_sec, next_edl_record->stop_sec, next_edl_record->length_sec);

            result = edl_seek(next_edl_record, demuxer, d_audio, mux_a, &frame_data, mux_v->codec==VCODEC_COPY);

            if (result == 2) { at_eof=1; break; } // EOF
            else if (result == 0) edl_seeking = 0; // no seeking
            else { // sucess
                edl_muted = 0;
                if (last_pos >= sh_video->pts) {
                    // backwards seek detected!! Forget about this EDL skip altogether.
                    next_edl_record = next_edl_record->next;
                }
                else for (next_edl_record = edl_records; next_edl_record; next_edl_record = next_edl_record->next) {
                    /* note the use of stop_sec,
                       meaning if by some magical way we landed in the MIDDLE of a censored area,
                       in the next loop it will jump out of it.
                    */
                    if (next_edl_record->stop_sec > sh_video->pts) break; // we got to the right place.
                    if (next_edl_record->action == EDL_MUTE) edl_muted = !edl_muted; // toggle mute each time.
                }

                /* for a pedantic EDL, that doesn't show even a single
                   frame from the "censored" area, uncomment next line. */
                goto goto_redo_edl;
            }
        } else if (next_edl_record->action == EDL_MUTE) {
            edl_muted = !edl_muted;  // This variable does nothing for now.
            mp_msg(MSGT_CPLAYER, MSGL_DBG4, "EDL_MUTE: [%f]\n", next_edl_record->start_sec );
            next_edl_record=next_edl_record->next;
        }
    }


if(sh_audio){
    // get audio:
    while(mux_a->timer-audio_preload<mux_v->timer){
        float tottime;
	int len=0;

	ptimer_start = GetTimerMS();
	// CBR - copy 0.5 sec of audio
	// or until the end of video:
	tottime = stop_time(demuxer, mux_v);
	if (tottime != -1) {
		tottime -= mux_a->timer;
		if (tottime > 1./audio_density) tottime = 1./audio_density;
	}
	else tottime = 1./audio_density;

	// let's not output more audio than necessary
	if (tottime <= 0) break;

	if(aencoder)
	{
		if(mux_a->h.dwSampleSize) /* CBR */
		{
			if(aencoder->set_decoded_len)
			{
				len = mux_a->h.dwSampleSize*(int)(mux_a->h.dwRate*tottime);
				aencoder->set_decoded_len(aencoder, len);
			}
			else
				len = aencoder->decode_buffer_size;

			len = dec_audio(sh_audio, aencoder->decode_buffer, len);
			mux_a->buffer_len += aencoder->encode(aencoder, mux_a->buffer + mux_a->buffer_len,
				aencoder->decode_buffer, len, mux_a->buffer_size-mux_a->buffer_len);
			if(mux_a->buffer_len < mux_a->wf->nBlockAlign)
				len = 0;
			else
				len = mux_a->wf->nBlockAlign*(mux_a->buffer_len/mux_a->wf->nBlockAlign);
		}
		else	/* VBR */
		{
			int sz = 0;
			while(1)
			{
				len = 0;
				if(! sz)
					sz = aencoder->get_frame_size(aencoder);
				if(sz > 0 && mux_a->buffer_len >= sz)
				{
					len = sz;
					break;
				}
				len = dec_audio(sh_audio,aencoder->decode_buffer, aencoder->decode_buffer_size);
				if(len <= 0)
				{
					len = 0;
					break;
				}
				len = aencoder->encode(aencoder, mux_a->buffer + mux_a->buffer_len, aencoder->decode_buffer, len, mux_a->buffer_size-mux_a->buffer_len);
				mux_a->buffer_len += len;
			}
	    }
	    if (mux_v->timer == 0) mux_a->h.dwInitialFrames++;
	}
	else {
	if(mux_a->h.dwSampleSize){
	    switch(mux_a->codec){
	    case ACODEC_COPY: // copy
		len=mux_a->wf->nAvgBytesPerSec*tottime;
		len/=mux_a->h.dwSampleSize;if(len<1) len=1;
		len*=mux_a->h.dwSampleSize;
		len=demux_read_data(sh_audio->ds,mux_a->buffer,len);
		break;
	    }
	} else {
	    // VBR - encode/copy an audio frame
	    switch(mux_a->codec){
	    case ACODEC_COPY: // copy
		len=ds_get_packet(sh_audio->ds,(unsigned char**) &mux_a->buffer);
		break;
		}
	    }
	}
	if(len<=0) break; // EOF?
	muxer_write_chunk(mux_a,len,AVIIF_KEYFRAME, MP_NOPTS_VALUE, MP_NOPTS_VALUE);
	if(!mux_a->h.dwSampleSize && mux_a->timer>0)
	    mux_a->wf->nAvgBytesPerSec=0.5f+(double)mux_a->size/mux_a->timer; // avg bps (VBR)
	if(mux_a->buffer_len>=len){
	    mux_a->buffer_len-=len;
	    fast_memcpy(mux_a->buffer,mux_a->buffer+len,mux_a->buffer_len);
	}


	audiosamples++;
	audiorate+= (GetTimerMS() - ptimer_start);

    }
}

    // get video frame!

    if (!frame_data.already_read) {
        frame_data.in_size=video_read_frame(sh_video,&frame_data.frame_time,&frame_data.start,force_fps);
        sh_video->timer+=frame_data.frame_time;
    }
    frame_data.frame_time /= opts.playback_speed;
    if(frame_data.in_size<0){ at_eof=1; break; }
    ++decoded_frameno;

    v_timer_corr-=frame_data.frame_time-(float)mux_v->h.dwScale/mux_v->h.dwRate;

if(demuxer2){	// 3-pass encoding, read control file (frameno.avi)
    // find our frame:
	while(next_frameno<decoded_frameno){
	    int* start;
	    int len=ds_get_packet(demuxer2->video,(unsigned char**) &start);
	    if(len<0){ at_eof=1;break;}
	    if(len==0) --skip_flag; else  // duplicate
	    if(len==4) next_frameno=start[0];
	}
    if(at_eof) break;
	skip_flag=next_frameno-decoded_frameno;
    // find next frame:
	while(next_frameno<=decoded_frameno){
	    int* start;
	    int len=ds_get_packet(demuxer2->video,(unsigned char**) &start);
	    if(len<0){ at_eof=1;break;}
	    if(len==0) --skip_flag; else  // duplicate
	    if(len==4) next_frameno=start[0];
	}
} else {

// check frame duplicate/drop:

float mux_frametime = (float)mux_v->h.dwScale/mux_v->h.dwRate;

if (v_timer_corr >= mux_frametime && (skip_limit<0 || skip_flag < skip_limit)) {
    v_timer_corr-=mux_frametime;
    ++skip_flag; // skip
}
while (v_timer_corr <= -mux_frametime && (skip_limit<0 || -skip_flag < skip_limit)) {
    v_timer_corr+=mux_frametime;
    --skip_flag; // dup
}

// either v_pts_corr is big, more than 2 times framerate, then we follow its advice,
// or, it cancels out v_timer_corr, in which case be happy and do nothing.

while ((v_pts_corr <= -mux_frametime && skip_flag > 0) || (v_pts_corr <= -2*mux_frametime)) {
    v_pts_corr+=mux_frametime;
    --skip_flag; // dup
}
if ((v_pts_corr >= mux_frametime && skip_flag < 0) || (v_pts_corr >= 2*mux_frametime)) {
  if (skip_flag<=0) { // we can't skip more than 1 frame now
    v_pts_corr-=mux_frametime;
    ++skip_flag; // skip
  }
}

} // demuxer2

ptimer_start = GetTimerMS();

switch(mux_v->codec){
case VCODEC_COPY:
    mux_v->buffer=frame_data.start;
    if(skip_flag<=0) muxer_write_chunk(mux_v,frame_data.in_size,(sh_video->ds->flags&1)?AVIIF_KEYFRAME:0, MP_NOPTS_VALUE, MP_NOPTS_VALUE);
    break;
case VCODEC_FRAMENO:
    mux_v->buffer=(unsigned char *)&decoded_frameno; // tricky
    if(skip_flag<=0) muxer_write_chunk(mux_v,sizeof(int),AVIIF_KEYFRAME, MP_NOPTS_VALUE, MP_NOPTS_VALUE);
    break;
default:
    // decode_video will callback down to ve_*.c encoders, through the video filters
    sh_video->vfilter->control(sh_video->vfilter, VFCTRL_SET_OSD_OBJ, osd);
    {void *decoded_frame = decode_video(sh_video,frame_data.start,frame_data.in_size,
      skip_flag>0 && (!sh_video->vfilter || sh_video->vfilter->control(sh_video->vfilter, VFCTRL_SKIP_NEXT_FRAME, 0) != CONTROL_TRUE), MP_NOPTS_VALUE);
      blit_frame = decoded_frame && filter_video(sh_video, decoded_frame, MP_NOPTS_VALUE);
      if (blit_frame) {
          struct vf_instance *vf = sh_video->vfilter;
          vf->control(vf, VFCTRL_DRAW_EOSD, NULL);
          vf->control(vf, VFCTRL_DRAW_OSD, osd);
      }
    }

    if (sh_video->vf_initialized < 0) mencoder_exit(1, NULL);

    if(!blit_frame){
      if (play_n_frames >= 0)
        play_n_frames++;
      badframes++;
      if(skip_flag<=0){
	// unwanted skipping of a frame, what to do?
        v_timer_corr-=(float)mux_v->h.dwScale/mux_v->h.dwRate;
#if 0
        // Old code apparently made under the assumption that !blit_frame means
        // decoding failed due to corruption or something.. but duplication and
        // skipping of frames should be entirely disabled when skip_limit==0,
        // and must be in order for many filters to work with -noskip.
        // Eventually this entire block should probably be removed.
	if(skip_limit==0){
	    // skipping not allowed -> write empty frame:
	    if (!encode_duplicates || !sh_video->vfilter || sh_video->vfilter->control(sh_video->vfilter, VFCTRL_DUPLICATE_FRAME, 0) != CONTROL_TRUE)
	      muxer_write_chunk(mux_v,0,0, MP_NOPTS_VALUE, MP_NOPTS_VALUE);
	} else {
	    // skipping allowed -> skip it and distriubute timer error:
	    v_timer_corr-=(float)mux_v->h.dwScale/mux_v->h.dwRate;
	}
#endif
      }
    }
}

videosamples++;
videorate+=(GetTimerMS() - ptimer_start);

if(skip_flag<0){
    // duplicate frame
	if(!quiet) mp_tmsg(MSGT_MENCODER, MSGL_WARN, "\n%d duplicate frame(s)!\n",-skip_flag);
    while(skip_flag<0){
	duplicatedframes++;
	if (!encode_duplicates || !sh_video->vfilter || sh_video->vfilter->control(sh_video->vfilter, VFCTRL_DUPLICATE_FRAME, 0) != CONTROL_TRUE)
	    muxer_write_chunk(mux_v,0,0, MP_NOPTS_VALUE, MP_NOPTS_VALUE);
	++skip_flag;
    }
} else
if(skip_flag>0){
    // skip frame
	if(!quiet) mp_tmsg(MSGT_MENCODER, MSGL_WARN, "\nSkipping frame!\n");
	skippedframes++;
    --skip_flag;
}

if(sh_audio && !demuxer2){
    float AV_delay,x;
    // A-V sync!
#if 0
    if(pts_from_bps){
        unsigned int samples=(sh_audio->audio.dwSampleSize)?
          ((ds_tell(d_audio)-sh_audio->a_in_buffer_len)/sh_audio->audio.dwSampleSize) :
          (d_audio->block_no); // <- used for VBR audio
        a_pts=samples*(float)sh_audio->audio.dwScale/(float)sh_audio->audio.dwRate;
      delay_corrected=1;
    } else
#endif
    {
      // PTS = (last timestamp) + (bytes after last timestamp)/(bytes per sec)
      a_pts=d_audio->pts;
      if(!delay_corrected) if(a_pts) delay_corrected=1;
      a_pts+=(ds_tell_pts(d_audio)-sh_audio->a_in_buffer_len)/(float)sh_audio->i_bps;
    }
    v_pts=sh_video ? sh_video->pts : d_video->pts;
    // av = compensated (with out buffering delay) A-V diff
    AV_delay=(a_pts-v_pts);
    AV_delay-=audio_delay;
    AV_delay /= opts.playback_speed;
    AV_delay-=mux_a->timer-(mux_v->timer-(v_timer_corr+v_pts_corr));
    // adjust for encoder delays
    AV_delay -= (float) mux_a->encoder_delay * mux_a->h.dwScale/mux_a->h.dwRate;
    AV_delay += (float) mux_v->encoder_delay * mux_v->h.dwScale/mux_v->h.dwRate;
	// compensate input video timer by av:
        x=AV_delay*0.1f;
        if(x<-max_pts_correction) x=-max_pts_correction; else
        if(x> max_pts_correction) x= max_pts_correction;
        if(default_max_pts_correction>=0)
          max_pts_correction=default_max_pts_correction*opts.playback_speed;
        else
          max_pts_correction=sh_video->frametime*0.10 *opts.playback_speed; // +-10% of time
	// sh_video->timer-=x;
	c_total+=x;
	v_pts_corr+=x;
}

    {	float t=(GetTimerMS()-timer_start)*0.001f;
	float len=(demuxer->movi_end-demuxer->movi_start);
	off_t pos = demuxer->filepos >= 0 ? demuxer->filepos : stream_tell(demuxer->stream);
	float p=len>1000 ? (float)(pos-demuxer->movi_start) / len :
                (demuxer_get_percent_pos(demuxer) / 100.0);
#if 0
	if(!len && sh_audio && sh_audio->audio.dwLength>100){
	    p=(sh_audio->audio.dwSampleSize? ds_tell(sh_audio->ds)/sh_audio->audio.dwSampleSize : sh_audio->ds->block_no)
	     / (float)(sh_audio->audio.dwLength);
	}
#endif
      if(!quiet) {
	if( mp_msg_test(MSGT_STATUSLINE,MSGL_V) ) {
		mp_msg(MSGT_STATUSLINE,MSGL_STATUS,"Pos:%6.1fs %6df (%2d%%) %3dfps Trem:%4dmin %3dmb  A-V:%5.3f [%d:%d] A/Vms %d/%d D/B/S %d/%d/%d \r",
	    	mux_v->timer, decoded_frameno, (int)(p*100),
	    	(t>1) ? (int)(decoded_frameno/t+0.5) : 0,
	    	(p>0.001) ? (int)((t/p-t)/60) : 0,
	    	(p>0.001) ? (int)(stream_tell(muxer->stream)/p/1024/1024) : 0,
	    	v_pts_corr,
	    	(mux_v->timer>1) ? (int)(mux_v->size/mux_v->timer/125) : 0,
	    	(mux_a && mux_a->timer>1) ? (int)(mux_a->size/mux_a->timer/125) : 0,
			audiorate/audiosamples, videorate/videosamples,
			duplicatedframes, badframes, skippedframes
		);
	} else
	mp_msg(MSGT_STATUSLINE,MSGL_STATUS,"Pos:%6.1fs %6df (%2d%%) %5.2ffps Trem:%4dmin %3dmb  A-V:%5.3f [%d:%d]\r",
	    mux_v->timer, decoded_frameno, (int)(p*100),
	    (t>1) ? (float)(decoded_frameno/t) : 0,
	    (p>0.001) ? (int)((t/p-t)/60) : 0,
	    (p>0.001) ? (int)(stream_tell(muxer->stream)/p/1024/1024) : 0,
	    v_pts_corr,
	    (mux_v->timer>1) ? (int)(mux_v->size/mux_v->timer/125) : 0,
	    (mux_a && mux_a->timer>1) ? (int)(mux_a->size/mux_a->timer/125) : 0
	);
      }
    }
        fflush(stdout);

#ifdef CONFIG_DVDREAD
// DVD sub:
 if(vobsub_writer){
     unsigned char* packet=NULL;
     int len;
     while((len=ds_get_packet_sub(d_dvdsub,&packet))>0){
	 mp_msg(MSGT_MENCODER,MSGL_V,"\rDVD sub: len=%d  v_pts=%5.3f  s_pts=%5.3f  \n",len,sh_video->pts,d_dvdsub->pts);
	     vobsub_out_output(vobsub_writer,packet,len,mux_v->timer + d_dvdsub->pts - sh_video->pts);
     }
 }
 else
#endif
     update_subtitles(NULL, &opts, sh_video, sh_video->pts, 0, d_dvdsub, 0);

 frame_data = (s_frame_data){ .start = NULL, .in_size = 0, .frame_time = 0., .already_read = 0 };

#if 0
 if(ferror(muxer_f)) {
     mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"%s: Error writing file.\n", out_filename);
     mencoder_exit(1, NULL);
 }
#endif

} // while(!at_eof)

if (!interrupted && filelist[++curfile].name != 0) {
	if (sh_video && sh_video->vfilter) { // Before uniniting sh_video and the filter chain, break apart the VE.
 		vf_instance_t * ve; // this will be the filter right before the ve.
		for (ve = sh_video->vfilter; ve->next && ve->next->next; ve = ve->next);

		if (ve->next) ve->next = NULL; // I'm telling the last filter, before the VE, there is nothing after it
		else sh_video->vfilter = NULL; // There is no chain except the VE.
	}

	if(sh_audio){ uninit_audio(sh_audio);sh_audio=NULL; }
	if(sh_video){ uninit_video(sh_video);sh_video=NULL; }
	if(demuxer) free_demuxer(demuxer);
	if(stream) free_stream(stream); // kill cache thread

	at_eof = 0;

	m_config_pop(mconfig);
	goto play_next_file;
}

/* Emit the remaining frames in the video system */
/*TODO emit frmaes delayed by decoder lag*/
if(sh_video && sh_video->vfilter){
	mp_tmsg(MSGT_MENCODER, MSGL_INFO, "\nFlushing video frames.\n");
	if (!sh_video->vfilter->fmt.have_configured)
		mp_tmsg(MSGT_MENCODER, MSGL_WARN, "Filters have not been configured! Empty file?\n");
	else
		sh_video->vfilter->control(sh_video->vfilter,
    	                                              VFCTRL_FLUSH_FRAMES, 0);
}

if(aencoder)
    if(aencoder->fixup)
        aencoder->fixup(aencoder);

if (muxer->cont_write_index) muxer_write_index(muxer);
muxer_f_size=stream_tell(muxer->stream);
stream_seek(muxer->stream,0);
if (muxer->cont_write_header) muxer_write_header(muxer); // update header
#if 0
if(ferror(muxer_f) || fclose(muxer_f) != 0) {
    mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"%s: Error writing file.\n", out_filename);
    mencoder_exit(1, NULL);
}
#endif
if(vobsub_writer)
    vobsub_out_close(vobsub_writer);

if(out_video_codec==VCODEC_FRAMENO && mux_v->timer>100){
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Recommended video bitrate for %s CD: %d\n","650MB",(int)((650*1024*1024-muxer_f_size)/mux_v->timer/125));
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Recommended video bitrate for %s CD: %d\n","700MB",(int)((700*1024*1024-muxer_f_size)/mux_v->timer/125));
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Recommended video bitrate for %s CD: %d\n","800MB",(int)((800*1024*1024-muxer_f_size)/mux_v->timer/125));
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Recommended video bitrate for %s CD: %d\n","2 x 650MB",(int)((2*650*1024*1024-muxer_f_size)/mux_v->timer/125));
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Recommended video bitrate for %s CD: %d\n","2 x 700MB",(int)((2*700*1024*1024-muxer_f_size)/mux_v->timer/125));
    mp_tmsg(MSGT_MENCODER, MSGL_INFO, "Recommended video bitrate for %s CD: %d\n","2 x 800MB",(int)((2*800*1024*1024-muxer_f_size)/mux_v->timer/125));
}

mp_tmsg(MSGT_MENCODER, MSGL_INFO, "\nVideo stream: %8.3f kbit/s  (%d B/s)  size: %"PRIu64" bytes  %5.3f secs  %d frames\n",
    (float)(mux_v->size/mux_v->timer*8.0f/1000.0f), (int)(mux_v->size/mux_v->timer), (uint64_t)mux_v->size, (float)mux_v->timer, decoded_frameno);
if(sh_audio)
mp_tmsg(MSGT_MENCODER, MSGL_INFO, "\nAudio stream: %8.3f kbit/s  (%d B/s)  size: %"PRIu64" bytes  %5.3f secs\n",
    (float)(mux_a->size/mux_a->timer*8.0f/1000.0f), (int)(mux_a->size/mux_a->timer), (uint64_t)mux_a->size, (float)mux_a->timer);

if(sh_audio){ uninit_audio(sh_audio);sh_audio=NULL; }
if(sh_video){ uninit_video(sh_video);sh_video=NULL; }
if(demuxer) free_demuxer(demuxer);
if(stream) free_stream(stream); // kill cache thread

return interrupted;
}
