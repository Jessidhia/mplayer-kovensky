
/// \file
/// \ingroup Properties Command2Property OSDMsgStack

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "config.h"
#include "talloc.h"

#if defined(__MINGW32__) || defined(__CYGWIN__)
#define _UWIN 1  /*disable Non-underscored versions of non-ANSI functions as otherwise int eof would conflict with eof()*/
#include <windows.h>
#endif
#include <string.h>
#include <unistd.h>

// #include <sys/mman.h>
#include <sys/types.h>
#ifndef __MINGW32__
#include <sys/ioctl.h>
#include <sys/wait.h>
#else
#define	SIGHUP	1	/* hangup */
#define	SIGQUIT	3	/* quit */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
#define	SIGPIPE	13	/* broken pipe */
#endif

#include <sys/time.h>
#include <sys/stat.h>

#include <signal.h>
#include <time.h>
#include <fcntl.h>
#include <limits.h>

#include <errno.h>

#include "mp_msg.h"
#include "av_log.h"

#define HELP_MP_DEFINE_STATIC
#include "help_mp.h"

#include "m_option.h"
#include "m_config.h"
#include "mplayer.h"
#include "access_mpcontext.h"
#include "m_property.h"

#include "cfg-mplayer-def.h"

#include "ffmpeg_files/intreadwrite.h"
#include "libavutil/avstring.h"

#include "subreader.h"

#include "mp_osd.h"
#include "libvo/video_out.h"

#include "libvo/font_load.h"
#include "libvo/sub.h"

#ifdef CONFIG_X11
#include "libvo/x11_common.h"
#endif

#include "libao2/audio_out.h"

#include "codec-cfg.h"

#include "edl.h"

#include "spudec.h"
#include "vobsub.h"

#include "osdep/getch2.h"
#include "osdep/timer.h"
#include "osdep/findfiles.h"

#include "input/input.h"

const int under_mencoder = 0;
int slave_mode=0;
int player_idle_mode=0;
int quiet=0;
int enable_mouse_movements=0;
float start_volume = -1;

#include "osdep/priority.h"

char *heartbeat_cmd;

#define ROUND(x) ((int)((x)<0 ? (x)-0.5 : (x)+0.5))

#ifdef HAVE_RTC
#ifdef __linux__
#include <linux/rtc.h>
#else
#include <rtc.h>
#define RTC_IRQP_SET RTCIO_IRQP_SET
#define RTC_PIE_ON   RTCIO_PIE_ON
#endif /* __linux__ */
#endif /* HAVE_RTC */

#include "stream/tv.h"
#include "stream/stream_radio.h"
#ifdef CONFIG_DVBIN
#include "stream/dvbin.h"
#endif
#include "stream/cache2.h"

//**************************************************************************//
//             Playtree
//**************************************************************************//
#include "playtree.h"
#include "playtreeparser.h"

//**************************************************************************//
//             Config
//**************************************************************************//
#include "parser-cfg.h"
#include "parser-mpcmd.h"

//**************************************************************************//
//             Config file
//**************************************************************************//

static int cfg_inc_verbose(m_option_t *conf){ ++verbose; return 0;}

#include "get_path.h"

//**************************************************************************//
//**************************************************************************//
//             Input media streaming & demultiplexer:
//**************************************************************************//

static int max_framesize=0;

#include "stream/stream.h"
#include "libmpdemux/demuxer.h"
#include "libmpdemux/stheader.h"

#ifdef CONFIG_DVDREAD
#include "stream/stream_dvd.h"
#endif
#include "stream/stream_dvdnav.h"

#include "libmpcodecs/dec_audio.h"
#include "libmpcodecs/dec_video.h"
#include "libmpcodecs/mp_image.h"
#include "libmpcodecs/vf.h"
#include "libmpcodecs/vd.h"

#include "mixer.h"

#include "mp_core.h"
#include "options.h"
#include "defaultopts.h"

#define Exit_SIGILL_RTCpuSel _(\
"- MPlayer crashed by an 'Illegal Instruction'.\n"\
"  It may be a bug in our new runtime CPU-detection code...\n"\
"  Please read DOCS/HTML/en/bugreports.html.\n")

#define Exit_SIGILL _(\
"- MPlayer crashed by an 'Illegal Instruction'.\n"\
"  It usually happens when you run it on a CPU different than the one it was\n"\
"  compiled/optimized for.\n"\
"  Verify this!\n")

#define Exit_SIGSEGV_SIGFPE _(\
"- MPlayer crashed by bad usage of CPU/FPU/RAM.\n"\
"  Recompile MPlayer with --enable-debug and make a 'gdb' backtrace and\n"\
"  disassembly. Details in DOCS/HTML/en/bugreports_what.html#bugreports_crash.\n")

#define Exit_SIGCRASH _(\
"- MPlayer crashed. This shouldn't happen.\n"\
"  It can be a bug in the MPlayer code _or_ in your drivers _or_ in your\n"\
"  gcc version. If you think it's MPlayer's fault, please read\n"\
"  DOCS/HTML/en/bugreports.html and follow the instructions there. We can't and\n"\
"  won't help unless you provide this information when reporting a possible bug.\n")

#define SystemTooSlow _("\n\n"\
"           ************************************************\n"\
"           **** Your system is too SLOW to play this!  ****\n"\
"           ************************************************\n\n"\
"Possible reasons, problems, workarounds:\n"\
"- Most common: broken/buggy _audio_ driver\n"\
"  - Try -ao sdl or use the OSS emulation of ALSA.\n"\
"  - Experiment with different values for -autosync, 30 is a good start.\n"\
"- Slow video output\n"\
"  - Try a different -vo driver (-vo help for a list) or try -framedrop!\n"\
"- Slow CPU\n"\
"  - Don't try to play a big DVD/DivX on a slow CPU! Try some of the lavdopts,\n"\
"    e.g. -vfm ffmpeg -lavdopts lowres=1:fast:skiploopfilter=all.\n"\
"- Broken file\n"\
"  - Try various combinations of -nobps -ni -forceidx -mc 0.\n"\
"- Slow media (NFS/SMB mounts, DVD, VCD etc)\n"\
"  - Try -cache 8192.\n"\
"- Are you using -cache to play a non-interleaved AVI file?\n"\
"  - Try -nocache.\n"\
"Read DOCS/HTML/en/video.html for tuning/speedup tips.\n"\
"If none of this helps you, read DOCS/HTML/en/bugreports.html.\n\n")


//**************************************************************************//
//**************************************************************************//

// Common FIFO functions, and keyboard/event FIFO code
#include "mp_fifo.h"
int noconsolecontrols=0;
//**************************************************************************//

// benchmark:
double video_time_usage=0;
double vout_time_usage=0;
static double audio_time_usage=0;
static int total_time_usage_start=0;
static int total_frame_cnt=0;
static int drop_frame_cnt=0; // total number of dropped frames
int benchmark=0;

// options:
       int auto_quality=0;
static int output_quality=0;

static int list_properties = 0;

int term_osd = 1;
static char* term_osd_esc = "\x1b[A\r\x1b[K";
static char* playing_msg = NULL;
// seek:
static double seek_to_sec;
static off_t seek_to_byte=0;
static off_t step_sec=0;

static m_time_size_t end_at = { .type = END_AT_NONE, .pos = 0 };

// A/V sync:
       int autosync=0; // 30 might be a good default value.

// codecs:
char **audio_codec_list=NULL; // override audio codec
char **video_codec_list=NULL; // override video codec
char **audio_fm_list=NULL;    // override audio codec family
char **video_fm_list=NULL;    // override video codec family

// demuxer:
extern char *demuxer_name; // override demuxer
extern char *audio_demuxer_name; // override audio demuxer
extern char *sub_demuxer_name; // override sub demuxer

// this dvdsub_id was selected via slang
// use this to allow dvdnav to follow -slang across stream resets,
// in particular the subtitle ID for a language changes
int dvdsub_lang_id;
int vobsub_id=-1;
char* audio_lang=NULL;
char* dvdsub_lang=NULL;
static char* spudec_ifo=NULL;
int forced_subs_only=0;
int file_filter=1;

// cache2:
       int stream_cache_size=-1;
#ifdef CONFIG_STREAM_CACHE
extern int cache_fill_status;

float stream_cache_min_percent=20.0;
float stream_cache_seek_min_percent=50.0;
#else
#define cache_fill_status 0
#endif

// dump:
static char *stream_dump_name="stream.dump";
       int stream_dump_type=0;

// A-V sync:
static float default_max_pts_correction=-1;//0.01f;
       float audio_delay=0;
static int ignore_start=0;

static int softsleep=0;

       double force_fps=0;
static int force_srate=0;
static int audio_output_format=-1; // AF_FORMAT_UNKNOWN
       int frame_dropping=0; // option  0=no drop  1= drop vo  2= drop decode
static int play_n_frames=-1;
static int play_n_frames_mf=-1;

// sub:
char *font_name=NULL;
char *sub_font_name=NULL;
extern int font_fontconfig;
float font_factor=0.75;
char **sub_name=NULL;
float sub_delay=0;
float sub_fps=0;
int   sub_auto = 1;
char *vobsub_name=NULL;
/*DSP!!char *dsp=NULL;*/
int   subcc_enabled=0;
int suboverlap_enabled = 1;

#include "ass_mp.h"

char* current_module=NULL; // for debugging


// ---

#ifdef CONFIG_MENU
#include "m_struct.h"
#include "libmenu/menu.h"
void vf_menu_pause_update(struct vf_instance* vf);
extern vf_info_t vf_info_menu;
static const vf_info_t* const libmenu_vfs[] = {
  &vf_info_menu,
  NULL
};
static vf_instance_t* vf_menu = NULL;
int use_menu = 0;
static char* menu_cfg = NULL;
static char* menu_root = "main";
#endif


#ifdef HAVE_RTC
static int nortc = 1;
static char* rtc_device;
#endif

edl_record_ptr edl_records = NULL; ///< EDL entries memory area
edl_record_ptr next_edl_record = NULL; ///< only for traversing edl_records
FILE* edl_fd = NULL; ///< fd to write to when in -edlout mode.
int use_filedir_conf;
int use_filename_title;

#include "mpcommon.h"
#include "command.h"

#include "metadata.h"

#define mp_basename2(s) (strrchr(s,'/')==NULL?(char*)s:(strrchr(s,'/')+1))

const void *mpctx_get_video_out(MPContext *mpctx)
{
    return mpctx->video_out;
}

const void *mpctx_get_audio_out(MPContext *mpctx)
{
    return mpctx->audio_out;
}

void *mpctx_get_demuxer(MPContext *mpctx)
{
    return mpctx->demuxer;
}

void *mpctx_get_playtree_iter(MPContext *mpctx)
{
    return mpctx->playtree_iter;
}

void *mpctx_get_mixer(MPContext *mpctx)
{
    return &mpctx->mixer;
}

int mpctx_get_global_sub_size(MPContext *mpctx)
{
    return mpctx->global_sub_size;
}

int mpctx_get_osd_function(MPContext *mpctx)
{
    return mpctx->osd_function;
}

static float get_relative_time(struct MPContext *mpctx)
{
    unsigned int new_time = GetTimer();
    unsigned int delta = new_time - mpctx->last_time;
    mpctx->last_time = new_time;
    return delta * 0.000001;
}

static int is_valid_metadata_type(struct MPContext *mpctx, metadata_t type) {
  switch (type)
  {
  /* check for valid video stream */
  case META_VIDEO_CODEC:
  case META_VIDEO_BITRATE:
  case META_VIDEO_RESOLUTION:
  {
    if (!mpctx->sh_video)
      return 0;
    break;
  }

  /* check for valid audio stream */
  case META_AUDIO_CODEC:
  case META_AUDIO_BITRATE:
  case META_AUDIO_SAMPLES:
  {
    if (!mpctx->sh_audio)
      return 0;
    break;
  }

  /* check for valid demuxer */
  case META_INFO_TITLE:
  case META_INFO_ARTIST:
  case META_INFO_ALBUM:
  case META_INFO_YEAR:
  case META_INFO_COMMENT:
  case META_INFO_TRACK:
  case META_INFO_GENRE:
  {
    if (!mpctx->demuxer)
      return 0;
    break;
  }

  default:
    break;
  }

  return 1;
}

static char *get_demuxer_info(struct MPContext *mpctx, char *tag) {
  char **info = mpctx->demuxer->info;
  int n;

  if (!info || !tag)
    return NULL;

  for (n = 0; info[2*n] != NULL ; n++)
    if (!strcasecmp (info[2*n], tag))
      break;

  return info[2*n+1] ? strdup (info[2*n+1]) : NULL;
}

char *get_metadata(struct MPContext *mpctx, metadata_t type)
{
  char *meta = NULL;
  sh_audio_t * const sh_audio = mpctx->sh_audio;
  sh_video_t * const sh_video = mpctx->sh_video;

  if (!is_valid_metadata_type(mpctx, type))
    return NULL;

  switch (type)
  {
  case META_NAME:
  {
    return strdup (mp_basename2 (mpctx->filename));
  }

  case META_VIDEO_CODEC:
  {
    if (sh_video->format == 0x10000001)
      meta = strdup ("mpeg1");
    else if (sh_video->format == 0x10000002)
      meta = strdup ("mpeg2");
    else if (sh_video->format == 0x10000004)
      meta = strdup ("mpeg4");
    else if (sh_video->format == 0x10000005)
      meta = strdup ("h264");
    else if (sh_video->format >= 0x20202020)
    {
      meta = malloc (8);
      sprintf (meta, "%.4s", (char *) &sh_video->format);
    }
    else
    {
      meta = malloc (8);
      sprintf (meta, "0x%08X", sh_video->format);
    }
    return meta;
  }

  case META_VIDEO_BITRATE:
  {
    meta = malloc (16);
    sprintf (meta, "%d kbps", (int) (sh_video->i_bps * 8 / 1024));
    return meta;
  }

  case META_VIDEO_RESOLUTION:
  {
    meta = malloc (16);
    sprintf (meta, "%d x %d", sh_video->disp_w, sh_video->disp_h);
    return meta;
  }

  case META_AUDIO_CODEC:
  {
    if (sh_audio->codec && sh_audio->codec->name)
      meta = strdup (sh_audio->codec->name);
    return meta;
  }

  case META_AUDIO_BITRATE:
  {
    meta = malloc (16);
    sprintf (meta, "%d kbps", (int) (sh_audio->i_bps * 8/1000));
    return meta;
  }

  case META_AUDIO_SAMPLES:
  {
    meta = malloc (16);
    sprintf (meta, "%d Hz, %d ch.", sh_audio->samplerate, sh_audio->channels);
    return meta;
  }

  /* check for valid demuxer */
  case META_INFO_TITLE:
    return get_demuxer_info(mpctx, "Title");

  case META_INFO_ARTIST:
    return get_demuxer_info(mpctx, "Artist");

  case META_INFO_ALBUM:
    return get_demuxer_info(mpctx, "Album");

  case META_INFO_YEAR:
    return get_demuxer_info(mpctx, "Year");

  case META_INFO_COMMENT:
    return get_demuxer_info(mpctx, "Comment");

  case META_INFO_TRACK:
    return get_demuxer_info(mpctx, "Track");

  case META_INFO_GENRE:
    return get_demuxer_info(mpctx, "Genre");

  default:
    break;
  }

  return meta;
}

/// step size of mixer changes
int volstep = 3;

#ifdef CONFIG_DVDNAV
static void mp_dvdnav_context_free(MPContext *ctx){
    if (ctx->nav_smpi) free_mp_image(ctx->nav_smpi);
    ctx->nav_smpi = NULL;
    if (ctx->nav_buffer) free(ctx->nav_buffer);
    ctx->nav_buffer = NULL;
    ctx->nav_start = NULL;
    ctx->nav_in_size = 0;
}
#endif

void uninit_player(struct MPContext *mpctx, unsigned int mask){
  mask &= mpctx->initialized_flags;

  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n*** uninit(0x%X)\n",mask);

  if(mask&INITIALIZED_ACODEC){
    mpctx->initialized_flags&=~INITIALIZED_ACODEC;
    current_module="uninit_acodec";
    if(mpctx->sh_audio) uninit_audio(mpctx->sh_audio);
    mpctx->sh_audio=NULL;
    mpctx->mixer.afilter = NULL;
  }

  if(mask&INITIALIZED_VCODEC){
    mpctx->initialized_flags&=~INITIALIZED_VCODEC;
    current_module="uninit_vcodec";
    if(mpctx->sh_video) uninit_video(mpctx->sh_video);
    mpctx->sh_video=NULL;
#ifdef CONFIG_MENU
    vf_menu=NULL;
#endif
  }

  if(mask&INITIALIZED_DEMUXER){
    mpctx->initialized_flags&=~INITIALIZED_DEMUXER;
    current_module="free_demuxer";
    if (mpctx->num_sources) {
        mpctx->demuxer = mpctx->sources[0].demuxer;
        for (int i = 1; i < mpctx->num_sources; i++) {
            free_stream(mpctx->sources[i].stream);
            free_demuxer(mpctx->sources[i].demuxer);
        }
    }
    talloc_free(mpctx->sources);
    mpctx->sources = NULL;
    mpctx->num_sources = 0;
    talloc_free(mpctx->timeline);
    mpctx->timeline = NULL;
    mpctx->num_timeline_parts = 0;
    talloc_free(mpctx->chapters);
    mpctx->chapters = NULL;
    mpctx->num_chapters = 0;
    mpctx->video_offset = 0;
    if(mpctx->demuxer){
	mpctx->stream=mpctx->demuxer->stream;
	free_demuxer(mpctx->demuxer);
    }
    mpctx->demuxer=NULL;
  }

  // kill the cache process:
  if(mask&INITIALIZED_STREAM){
    mpctx->initialized_flags&=~INITIALIZED_STREAM;
    current_module="uninit_stream";
    if(mpctx->stream) free_stream(mpctx->stream);
    mpctx->stream=NULL;
  }

  if(mask&INITIALIZED_VO){
    mpctx->initialized_flags&=~INITIALIZED_VO;
    current_module="uninit_vo";
    vo_destroy(mpctx->video_out);
    mpctx->video_out=NULL;
#ifdef CONFIG_DVDNAV
    mp_dvdnav_context_free(mpctx);
#endif
  }

  // Must be after libvo uninit, as few vo drivers (svgalib) have tty code.
  if(mask&INITIALIZED_GETCH2){
    mpctx->initialized_flags&=~INITIALIZED_GETCH2;
    current_module="uninit_getch2";
    mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n[[[uninit getch2]]]\n");
  // restore terminal:
    getch2_disable();
  }

  if(mask&INITIALIZED_VOBSUB){
    mpctx->initialized_flags&=~INITIALIZED_VOBSUB;
    current_module="uninit_vobsub";
    if(vo_vobsub) vobsub_close(vo_vobsub);
    vo_vobsub=NULL;
  }

  if (mask&INITIALIZED_SPUDEC){
    mpctx->initialized_flags&=~INITIALIZED_SPUDEC;
    current_module="uninit_spudec";
    spudec_free(vo_spudec);
    vo_spudec=NULL;
  }

  if(mask&INITIALIZED_AO){
    mpctx->initialized_flags&=~INITIALIZED_AO;
    current_module="uninit_ao";
    if (mpctx->edl_muted) mixer_mute(&mpctx->mixer);
    if (mpctx->audio_out)
        mpctx->audio_out->uninit(mpctx->stop_play != AT_END_OF_FILE);
    mpctx->audio_out=NULL;
  }

  current_module=NULL;
}

void exit_player_with_rc(struct MPContext *mpctx, exit_reason_t how, int rc)
{
  uninit_player(mpctx, INITIALIZED_ALL);
#if defined(__MINGW32__) || defined(__CYGWIN__)
  timeEndPeriod(1);
#endif
#ifdef CONFIG_X11
  vo_uninit(mpctx->x11_state);	// Close the X11 connection (if any is open).
#endif

    current_module="uninit_input";
    mp_input_uninit(mpctx->input);
#ifdef CONFIG_MENU
    if (use_menu)
      menu_uninit();
#endif

#ifdef CONFIG_FREETYPE
  current_module="uninit_font";
  if (mpctx->osd && mpctx->osd->sub_font != vo_font)
      free_font_desc(mpctx->osd->sub_font);
  free_font_desc(vo_font);
  vo_font = NULL;
  done_freetype();
#endif
  osd_free(mpctx->osd);

#ifdef CONFIG_ASS
  ass_library_done(ass_library);
#endif

  current_module="exit_player";

// free mplayer config
  if(mpctx->mconfig)
    m_config_free(mpctx->mconfig);

  if(mpctx->playtree)
    play_tree_free(mpctx->playtree, 1);

  talloc_free(mpctx->key_fifo);

  if(edl_records != NULL) free(edl_records); // free mem allocated for EDL
  switch(how) {
  case EXIT_QUIT:
    mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"\nExiting... (%s)\n","Quit");
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_EXIT=QUIT\n");
    break;
  case EXIT_EOF:
    mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"\nExiting... (%s)\n","End of file");
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_EXIT=EOF\n");
    break;
  case EXIT_ERROR:
    mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"\nExiting... (%s)\n","Fatal error");
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_EXIT=ERROR\n");
    break;
  default:
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_EXIT=NONE\n");
  }
  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"max framesize was %d bytes\n",max_framesize);

  exit(rc);
}

static void exit_player(struct MPContext *mpctx, exit_reason_t how)
{
  exit_player_with_rc(mpctx, how, 1);
}

#ifndef __MINGW32__
static void child_sighandler(int x){
  pid_t pid;
  while((pid=waitpid(-1,NULL,WNOHANG)) > 0);
}
#endif

#ifdef CONFIG_CRASH_DEBUG
static char *prog_path;
static int crash_debug = 0;
#endif

static void exit_sighandler(int x){
  static int sig_count=0;
#ifdef CONFIG_CRASH_DEBUG
  if (!crash_debug || x != SIGTRAP)
#endif
  ++sig_count;
  if(sig_count==5)
    {
      /* We're crashing bad and can't uninit cleanly :(
       * by popular request, we make one last (dirty)
       * effort to restore the user's
       * terminal. */
      getch2_disable();
      exit(1);
    }
  if(sig_count==6) exit(1);
  if(sig_count>6){
    // can't stop :(
#ifndef __MINGW32__
    kill(getpid(),SIGKILL);
#endif
  }
  mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"\n" "\nMPlayer interrupted by signal %d in module: %s\n",x,
      current_module?current_module:"unknown"
  );
  mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_SIGNAL=%d\n", x);
  if(sig_count<=1)
  switch(x){
  case SIGINT:
  case SIGQUIT:
  case SIGTERM:
  case SIGKILL:
      async_quit_request = 1;
      return;  // killed from keyboard (^C) or killed [-9]
  case SIGILL:
#if CONFIG_RUNTIME_CPUDETECT
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,Exit_SIGILL_RTCpuSel);
#else
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,Exit_SIGILL);
#endif
  case SIGFPE:
  case SIGSEGV:
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,Exit_SIGSEGV_SIGFPE);
  default:
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,Exit_SIGCRASH);
#ifdef CONFIG_CRASH_DEBUG
      if (crash_debug) {
        int gdb_pid;
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "Forking...\n");
        gdb_pid = fork();
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "Forked...\n");
        if (gdb_pid == 0) { // We are the child
          char spid[20];
          snprintf(spid, sizeof(spid), "%i", getppid());
          getch2_disable(); // allow terminal to work properly with gdb
          if (execlp("gdb", "gdb", prog_path, spid, "-ex", "bt", NULL) == -1)
            mp_msg(MSGT_CPLAYER, MSGL_ERR, "Couldn't start gdb\n");
        } else if (gdb_pid < 0)
          mp_msg(MSGT_CPLAYER, MSGL_ERR, "Couldn't fork\n");
        else {
          waitpid(gdb_pid, NULL, 0);
        }
        if (x == SIGTRAP) return;
      }
#endif
  }
  getch2_disable();
  exit(1);
}

#include "cfg-mplayer.h"

static int cfg_include(m_option_t *conf, char *filename)
{
    return m_config_parse_config_file(conf->priv, filename);
}

static void parse_cfgfiles(struct MPContext *mpctx, m_config_t* conf)
{
char *conffile;
int conffile_fd;
if (!disable_system_conf &&
    m_config_parse_config_file(conf, MPLAYER_CONFDIR "/mplayer.conf") < 0)
  exit_player(mpctx, EXIT_NONE);
if ((conffile = get_path("")) == NULL) {
  mp_tmsg(MSGT_CPLAYER,MSGL_WARN,"Cannot find HOME directory.\n");
} else {
#ifdef __MINGW32__
  mkdir(conffile);
#else
  mkdir(conffile, 0777);
#endif
  free(conffile);
  if ((conffile = get_path("config")) == NULL) {
    mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"get_path(\"config\") problem\n");
  } else {
    if ((conffile_fd = open(conffile, O_CREAT | O_EXCL | O_WRONLY, 0666)) != -1) {
      mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Creating config file: %s\n", conffile);
      write(conffile_fd, default_config, strlen(default_config));
      close(conffile_fd);
    }
    if (!disable_user_conf &&
        m_config_parse_config_file(conf, conffile) < 0)
      exit_player(mpctx, EXIT_NONE);
    free(conffile);
  }
}
}

#define PROFILE_CFG_PROTOCOL "protocol."

static void load_per_protocol_config (m_config_t* conf, const char *const file)
{
    char *str;
    char protocol[strlen (PROFILE_CFG_PROTOCOL) + strlen (file) + 1];
    m_profile_t *p;

    /* does filename actually uses a protocol ? */
    str = strstr (file, "://");
    if (!str)
        return;

    sprintf (protocol, "%s%s", PROFILE_CFG_PROTOCOL, file);
    protocol[strlen (PROFILE_CFG_PROTOCOL)+strlen(file)-strlen(str)] = '\0';
    p = m_config_get_profile (conf, protocol);
    if (p)
    {
        mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Loading protocol-related profile '%s'\n", protocol);
        m_config_set_profile(conf,p);
    }
}

#define PROFILE_CFG_EXTENSION "extension."

static void load_per_extension_config (m_config_t* conf, const char *const file)
{
    char *str;
    char extension[strlen (PROFILE_CFG_EXTENSION) + 8];
    m_profile_t *p;

    /* does filename actually have an extension ? */
    str = strrchr (file, '.');
    if (!str)
        return;

    sprintf (extension, PROFILE_CFG_EXTENSION);
    strncat (extension, ++str, 7);
    p = m_config_get_profile (conf, extension);
    if (p)
    {
      mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Loading extension-related profile '%s'\n", extension);
      m_config_set_profile(conf,p);
    }
}

#define PROFILE_CFG_VO "vo."
#define PROFILE_CFG_AO "ao."

static void load_per_output_config (m_config_t* conf, char *cfg, char *out)
{
    char profile[strlen (cfg) + strlen (out) + 1];
    m_profile_t *p;

    sprintf (profile, "%s%s", cfg, out);
    p = m_config_get_profile (conf, profile);
    if (p)
    {
      mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Loading extension-related profile '%s'\n", profile);
      m_config_set_profile(conf,p);
    }
}

/**
 * Tries to load a config file
 * @return 0 if file was not found, 1 otherwise
 */
static int try_load_config(m_config_t *conf, const char *file)
{
    struct stat st;
    if (stat(file, &st))
        return 0;
    mp_tmsg(MSGT_CPLAYER, MSGL_INFO, "Loading config '%s'\n", file);
    m_config_parse_config_file (conf, file);
    return 1;
}

static void load_per_file_config (m_config_t* conf, const char *const file)
{
    char *confpath;
    char cfg[PATH_MAX];
    char *name;

    if (strlen(file) > PATH_MAX - 14) {
        mp_msg(MSGT_CPLAYER, MSGL_WARN, "Filename is too long, can not load file or directory specific config files\n");
        return;
    }
    sprintf (cfg, "%s.conf", file);

    name = strrchr(cfg, '/');
    if (HAVE_DOS_PATHS) {
        char *tmp = strrchr(cfg, '\\');
        if (!name || tmp > name)
            name = tmp;
        tmp = strrchr(cfg, ':');
        if (!name || tmp > name)
            name = tmp;
    }
    if (!name)
	name = cfg;
    else
	name++;

    if (use_filedir_conf) {
        char dircfg[PATH_MAX];
        strcpy(dircfg, cfg);
        strcpy(dircfg + (name - cfg), "mplayer.conf");
        try_load_config(conf, dircfg);

        if (try_load_config(conf, cfg))
            return;
    }

    if ((confpath = get_path (name)) != NULL)
    {
	try_load_config(conf, confpath);

	free (confpath);
    }
}

/* When libmpdemux performs a blocking operation (network connection or
 * cache filling) if the operation fails we use this function to check
 * if it was interrupted by the user.
 * The function returns a new value for eof. */
static int libmpdemux_was_interrupted(struct MPContext *mpctx, int stop_play)
{
  mp_cmd_t* cmd;
  if((cmd = mp_input_get_cmd(mpctx->input, 0,0,0)) != NULL) {
       switch(cmd->id) {
       case MP_CMD_QUIT:
	 exit_player_with_rc(mpctx, EXIT_QUIT, (cmd->nargs > 0)? cmd->args[0].v.i : 0);
       case MP_CMD_PLAY_TREE_STEP: {
	 stop_play = (cmd->args[0].v.i > 0) ? PT_NEXT_ENTRY : PT_PREV_ENTRY;
	 mpctx->play_tree_step = (cmd->args[0].v.i == 0) ? 1 : cmd->args[0].v.i;
       } break;
       case MP_CMD_PLAY_TREE_UP_STEP: {
	 stop_play = (cmd->args[0].v.i > 0) ? PT_UP_NEXT : PT_UP_PREV;
       } break;
       case MP_CMD_PLAY_ALT_SRC_STEP: {
	 stop_play = (cmd->args[0].v.i > 0) ?  PT_NEXT_SRC : PT_PREV_SRC;
       } break;
       }
       mp_cmd_free(cmd);
  }
  return stop_play;
}

#define mp_basename(s) (strrchr(s,'\\')==NULL?(mp_basename2(s)):(strrchr(s,'\\')+1))

static int playtree_add_playlist(struct MPContext *mpctx, play_tree_t* entry)
{
  play_tree_add_bpf(entry,mpctx->filename);

  {
  if(!entry) {
    entry = mpctx->playtree_iter->tree;
    if(play_tree_iter_step(mpctx->playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
        return PT_NEXT_ENTRY;
    }
    if(mpctx->playtree_iter->tree == entry ) { // Loop with a single file
      if(play_tree_iter_up_step(mpctx->playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
	return PT_NEXT_ENTRY;
      }
    }
    play_tree_remove(entry,1,1);
    return PT_NEXT_SRC;
  }
  play_tree_insert_entry(mpctx->playtree_iter->tree,entry);
  play_tree_set_params_from(entry,mpctx->playtree_iter->tree);
  entry = mpctx->playtree_iter->tree;
  if(play_tree_iter_step(mpctx->playtree_iter,1,0) != PLAY_TREE_ITER_ENTRY) {
    return PT_NEXT_ENTRY;
  }
  play_tree_remove(entry,1,1);
  }
  return PT_NEXT_SRC;
}

void add_subtitles(struct MPContext *mpctx, char *filename, float fps, int noerr)
{
    struct MPOpts *opts = &mpctx->opts;
    sub_data *subd;
#ifdef CONFIG_ASS
    ASS_Track *asst = 0;
#endif

    if (filename == NULL || mpctx->set_of_sub_size >= MAX_SUBTITLE_FILES) {
	return;
    }

    subd = sub_read_file(filename, fps);
#ifdef CONFIG_ASS
    if (opts->ass_enabled)
#ifdef CONFIG_ICONV
        asst = ass_read_file(ass_library, filename, sub_cp);
#else
        asst = ass_read_file(ass_library, filename, 0);
#endif
    if (opts->ass_enabled && subd && !asst)
        asst = ass_read_subdata(ass_library, subd, fps);

    if (!asst && !subd)
#else
    if(!subd)
#endif
        mp_tmsg(MSGT_CPLAYER, noerr ? MSGL_WARN : MSGL_ERR, "Cannot load subtitles: %s\n",
		filename_recode(filename));
#if defined(_WIN32) && !defined(HAVE_NEW_GUI)
    if(filename) {
        static char message[MAX_PATH + 1];
        char *s = strrchr(filename, '\\');
        if (!s) s = strrchr(filename, '/');
        if (s) s++; else s = filename; 
        message[0] = 0;
        snprintf(message, MAX_PATH, "MPlayer: %s", filename_recode(s));
        message[MAX_PATH] = 0;
        SetConsoleTitle(message);
     }
#endif

#ifdef CONFIG_ASS
    if (!asst && !subd) return;
    mpctx->set_of_ass_tracks[mpctx->set_of_sub_size] = asst;
#else
    if (!subd) return;
#endif
    mpctx->set_of_subtitles[mpctx->set_of_sub_size] = subd;
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_FILE_SUB_ID=%d\n", mpctx->set_of_sub_size);
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_FILE_SUB_FILENAME=%s\n",
	   filename_recode(filename));
    ++mpctx->set_of_sub_size;
    mp_tmsg(MSGT_CPLAYER, MSGL_INFO, "SUB: Added subtitle file (%d): %s\n", mpctx->set_of_sub_size,
	    filename_recode(filename));
}

void init_vo_spudec(struct MPContext *mpctx)
{
  if (vo_spudec)
    spudec_free(vo_spudec);
  mpctx->initialized_flags &= ~INITIALIZED_SPUDEC;
  vo_spudec = NULL;
  if (spudec_ifo) {
    unsigned int palette[16], width, height;
    current_module="spudec_init_vobsub";
    if (vobsub_parse_ifo(NULL,spudec_ifo, palette, &width, &height, 1, -1, NULL) >= 0)
      vo_spudec=spudec_new_scaled(palette, width, height, NULL, 0);
  }

#ifdef CONFIG_DVDREAD
  if (vo_spudec==NULL && mpctx->stream->type==STREAMTYPE_DVD) {
    current_module="spudec_init_dvdread";
    vo_spudec=spudec_new_scaled(((dvd_priv_t *)(mpctx->stream->priv))->cur_pgc->palette,
                                mpctx->sh_video->disp_w, mpctx->sh_video->disp_h,
                                NULL, 0);
  }
#endif

#ifdef CONFIG_DVDNAV
  if (vo_spudec==NULL && mpctx->stream->type==STREAMTYPE_DVDNAV) {
    unsigned int *palette = mp_dvdnav_get_spu_clut(mpctx->stream);
    current_module="spudec_init_dvdnav";
    vo_spudec=spudec_new_scaled(palette, mpctx->sh_video->disp_w, mpctx->sh_video->disp_h, NULL, 0);
  }
#endif

  if (vo_spudec==NULL) {
    sh_sub_t *sh = (sh_sub_t *)mpctx->d_sub->sh;
    current_module="spudec_init_normal";
    vo_spudec=spudec_new_scaled(NULL, mpctx->sh_video->disp_w, mpctx->sh_video->disp_h, sh->extradata, sh->extradata_len);
    spudec_set_font_factor(vo_spudec,font_factor);
  }

  if (vo_spudec!=NULL) {
    mpctx->initialized_flags|=INITIALIZED_SPUDEC;
    mp_property_do("sub_forced_only", M_PROPERTY_SET, &forced_subs_only, mpctx);
  }
}

/*
 * In Mac OS X the SDL-lib is built upon Cocoa. The easiest way to
 * make it all work is to use the builtin SDL-bootstrap code, which
 * will be done automatically by replacing our main() if we include SDL.h.
 */
#if defined(__APPLE__) && defined(CONFIG_SDL)
#include <SDL.h>
#endif

/**
 * \brief append a formatted string
 * \param buf buffer to print into
 * \param pos position of terminating 0 in buf
 * \param len maximum number of characters in buf, not including terminating 0
 * \param format printf format string
 */
static void saddf(char *buf, unsigned *pos, int len, const char *format, ...)
{
  va_list va;
  va_start(va, format);
  *pos += vsnprintf(&buf[*pos], len - *pos, format, va);
  va_end(va);
  if (*pos >= len ) {
    buf[len] = 0;
    *pos = len;
  }
}

/**
 * \brief append time in the hh:mm:ss.f format
 * \param buf buffer to print into
 * \param pos position of terminating 0 in buf
 * \param len maximum number of characters in buf, not including terminating 0
 * \param time time value to convert/append
 */
static void sadd_hhmmssf(char *buf, unsigned *pos, int len, float time) {
  long tenths = 10 * time;
  int f1 = tenths % 10;
  int ss = (tenths /  10) % 60;
  int mm = (tenths / 600) % 60;
  int hh = tenths / 36000;
  if (time <= 0) {
    saddf(buf, pos, len, "unknown");
    return;
  }
  if (hh > 0)
    saddf(buf, pos, len, "%2d:", hh);
  if (hh > 0 || mm > 0)
    saddf(buf, pos, len, "%02d:", mm);
  saddf(buf, pos, len, "%02d.%1d", ss, f1);
}

static void print_status(struct MPContext *mpctx, double a_pos, bool at_frame)
{
    struct MPOpts *opts = &mpctx->opts;
  sh_video_t * const sh_video = mpctx->sh_video;

  if (mpctx->sh_audio && a_pos == MP_NOPTS_VALUE)
      a_pos = playing_audio_pts(mpctx);
  if (mpctx->sh_audio && sh_video && at_frame) {
      mpctx->last_av_difference = a_pos - sh_video->pts - audio_delay;
      if (mpctx->time_frame > 0)
          mpctx->last_av_difference += mpctx->time_frame * opts->playback_speed;
      if (mpctx->last_av_difference > 0.5 && drop_frame_cnt > 50
          && !mpctx->drop_message_shown) {
          mp_tmsg(MSGT_AVSYNC,MSGL_WARN,SystemTooSlow);
          mpctx->drop_message_shown = true;
      }
  }
  if (quiet)
      return;


  int width;
  char *line;
  unsigned pos = 0;
  get_screen_size();
  if (screen_width > 0)
    width = screen_width;
  else
  width = 80;
#if defined(__MINGW32__) || defined(__CYGWIN__) || defined(__OS2__)
  /* Windows command line is broken (MinGW's rxvt works, but we
   * should not depend on that). */
  width--;
#endif
  line = malloc(width + 1); // one additional char for the terminating null

  // Audio time
  if (mpctx->sh_audio) {
    saddf(line, &pos, width, "A:%6.1f ", a_pos);
    if (!sh_video) {
      float len = demuxer_get_time_length(mpctx->demuxer);
      saddf(line, &pos, width, "(");
      sadd_hhmmssf(line, &pos, width, a_pos);
      saddf(line, &pos, width, ") of %.1f (", len);
      sadd_hhmmssf(line, &pos, width, len);
      saddf(line, &pos, width, ") ");
    }
  }

  // Video time
  if (sh_video)
    saddf(line, &pos, width, "V:%6.1f ", sh_video->pts);

  // A-V sync
  if (mpctx->sh_audio && sh_video)
    saddf(line, &pos, width, "A-V:%7.3f ct:%7.3f ",
          mpctx->last_av_difference, mpctx->total_avsync_change);

  // Video stats
  if (sh_video)
    saddf(line, &pos, width, "%3d/%3d ",
      (int)sh_video->num_frames,
      (int)sh_video->num_frames_decoded);

  // CPU usage
  if (sh_video) {
    if (sh_video->timer > 0.5)
      saddf(line, &pos, width, "%2d%% %2d%% %4.1f%% ",
        (int)(100.0*video_time_usage*opts->playback_speed/(double)sh_video->timer),
        (int)(100.0*vout_time_usage*opts->playback_speed/(double)sh_video->timer),
        (100.0*audio_time_usage*opts->playback_speed/(double)sh_video->timer));
    else
      saddf(line, &pos, width, "??%% ??%% ??,?%% ");
  } else if (mpctx->sh_audio) {
    if (mpctx->delay > 0.5)
      saddf(line, &pos, width, "%4.1f%% ",
        100.0*audio_time_usage/(double)mpctx->delay);
    else
      saddf(line, &pos, width, "??,?%% ");
  }

  // VO stats
  if (sh_video)
    saddf(line, &pos, width, "%d %d ", drop_frame_cnt, output_quality);

#ifdef CONFIG_STREAM_CACHE
  // cache stats
  if (stream_cache_size > 0)
    saddf(line, &pos, width, "%d%% ", cache_fill_status);
#endif

  // other
  if (opts->playback_speed != 1)
    saddf(line, &pos, width, "%4.2fx ", opts->playback_speed);

  // end
  if (erase_to_end_of_line) {
    line[pos] = 0;
    mp_msg(MSGT_STATUSLINE, MSGL_STATUS, "%s%s\r", line, erase_to_end_of_line);
  } else {
    memset(&line[pos], ' ', width - pos);
    line[width] = 0;
    mp_msg(MSGT_STATUSLINE, MSGL_STATUS, "%s\r", line);
  }
  free(line);
}

/**
 * \brief build a chain of audio filters that converts the input format
 * to the ao's format, taking into account the current playback_speed.
 * \param sh_audio describes the requested input format of the chain.
 * \param ao_data describes the requested output format of the chain.
 */
int build_afilter_chain(struct MPContext *mpctx, sh_audio_t *sh_audio, ao_data_t *ao_data)
{
    struct MPOpts *opts = &mpctx->opts;
  int new_srate;
  int result;
  if (!sh_audio)
  {
    mpctx->mixer.afilter = NULL;
    return 0;
  }
  if(af_control_any_rev(sh_audio->afilter,
                        AF_CONTROL_PLAYBACK_SPEED | AF_CONTROL_SET,
                        &opts->playback_speed)) {
    new_srate = sh_audio->samplerate;
  } else {
    new_srate = sh_audio->samplerate * opts->playback_speed;
    if (new_srate != ao_data->samplerate) {
      // limits are taken from libaf/af_resample.c
      if (new_srate < 8000)
        new_srate = 8000;
      if (new_srate > 192000)
        new_srate = 192000;
      opts->playback_speed = (float)new_srate / (float)sh_audio->samplerate;
    }
  }
  result =  init_audio_filters(sh_audio, new_srate,
           &ao_data->samplerate, &ao_data->channels, &ao_data->format);
  mpctx->mixer.afilter = sh_audio->afilter;
  return result;
}


typedef struct mp_osd_msg mp_osd_msg_t;
struct mp_osd_msg {
    /// Previous message on the stack.
    mp_osd_msg_t* prev;
    /// Message text.
    char msg[128];
    int  id,level,started;
    /// Display duration in ms.
    unsigned  time;
};

/// OSD message stack.
static mp_osd_msg_t* osd_msg_stack = NULL;

/**
 *  \brief Add a message on the OSD message stack
 *
 *  If a message with the same id is already present in the stack
 *  it is pulled on top of the stack, otherwise a new message is created.
 *
 */

void set_osd_msg(int id, int level, int time, const char* fmt, ...) {
    mp_osd_msg_t *msg,*last=NULL;
    va_list va;
    int r;

    // look if the id is already in the stack
    for(msg = osd_msg_stack ; msg && msg->id != id ;
	last = msg, msg = msg->prev);
    // not found: alloc it
    if(!msg) {
        msg = calloc(1,sizeof(mp_osd_msg_t));
        msg->prev = osd_msg_stack;
        osd_msg_stack = msg;
    } else if(last) { // found, but it's not on top of the stack
        last->prev = msg->prev;
        msg->prev = osd_msg_stack;
        osd_msg_stack = msg;
    }
    // write the msg
    va_start(va,fmt);
    r = vsnprintf(msg->msg, 128, fmt, va);
    va_end(va);
    if(r >= 128) msg->msg[127] = 0;
    // set id and time
    msg->id = id;
    msg->level = level;
    msg->time = time;

}

/**
 *  \brief Remove a message from the OSD stack
 *
 *  This function can be used to get rid of a message right away.
 *
 */

void rm_osd_msg(int id) {
    mp_osd_msg_t *msg,*last=NULL;

    // Search for the msg
    for(msg = osd_msg_stack ; msg && msg->id != id ;
	last = msg, msg = msg->prev);
    if(!msg) return;

    // Detach it from the stack and free it
    if(last)
        last->prev = msg->prev;
    else
        osd_msg_stack = msg->prev;
    free(msg);
}

/**
 *  \brief Remove all messages from the OSD stack
 *
 */

static void clear_osd_msgs(void) {
    mp_osd_msg_t* msg = osd_msg_stack, *prev = NULL;
    while(msg) {
        prev = msg->prev;
        free(msg);
        msg = prev;
    }
    osd_msg_stack = NULL;
}

/**
 *  \brief Get the current message from the OSD stack.
 *
 *  This function decrements the message timer and destroys the old ones.
 *  The message that should be displayed is returned (if any).
 *
 */

static mp_osd_msg_t* get_osd_msg(struct MPContext *mpctx)
{
    struct MPOpts *opts = &mpctx->opts;
    mp_osd_msg_t *msg,*prev,*last = NULL;
    static unsigned last_update = 0;
    unsigned now = GetTimerMS();
    unsigned diff;
    char hidden_dec_done = 0;

    if (mpctx->osd_visible) {
	// 36000000 means max timed visibility is 1 hour into the future, if
	// the difference is greater assume it's wrapped around from below 0
        if (mpctx->osd_visible - now > 36000000) {
            mpctx->osd_visible = 0;
	    vo_osd_progbar_type = -1; // disable
	    vo_osd_changed(OSDTYPE_PROGBAR);
            mpctx->osd_function = mpctx->paused ? OSD_PAUSE : OSD_PLAY;
	}
    }
    if (mpctx->osd_show_percentage_until - now > 36000000)
        mpctx->osd_show_percentage_until = 0;

    if(!last_update) last_update = now;
    diff = now >= last_update ? now - last_update : 0;

    last_update = now;

    // Look for the first message in the stack with high enough level.
    for(msg = osd_msg_stack ; msg ; last = msg, msg = prev) {
        prev = msg->prev;
        if (msg->level > opts->osd_level && hidden_dec_done)
            continue;
        // The message has a high enough level or it is the first hidden one
        // in both cases we decrement the timer or kill it.
        if(!msg->started || msg->time > diff) {
            if(msg->started) msg->time -= diff;
            else msg->started = 1;
            // display it
            if (msg->level <= opts->osd_level)
                return msg;
            hidden_dec_done = 1;
            continue;
        }
        // kill the message
        free(msg);
        if(last) {
            last->prev = prev;
            msg = last;
        } else {
            osd_msg_stack = prev;
            msg = NULL;
        }
    }
    // Nothing found
    return NULL;
}

/**
 * \brief Display the OSD bar.
 *
 * Display the OSD bar or fall back on a simple message.
 *
 */

void set_osd_bar(struct MPContext *mpctx, int type,const char* name,double min,double max,double val) {
    struct MPOpts *opts = &mpctx->opts;
    if (opts->osd_level < 1)
        return;

    if(mpctx->sh_video) {
        mpctx->osd_visible = (GetTimerMS() + 1000) | 1;
        vo_osd_progbar_type = type;
        vo_osd_progbar_value = 256*(val-min)/(max-min);
        vo_osd_changed(OSDTYPE_PROGBAR);
        return;
    }

    set_osd_msg(OSD_MSG_BAR, 1, opts->osd_duration, "%s: %d %%",
                name, ROUND(100*(val-min)/(max-min)));
}

/**
 * \brief Display text subtitles on the OSD
 */
void set_osd_subtitle(struct MPContext *mpctx, subtitle *subs)
{
    int i;
    vo_sub = subs;
    vo_osd_changed(OSDTYPE_SUBTITLE);
    if (!mpctx->sh_video) {
        // reverse order, since newest set_osd_msg is displayed first
        for (i = SUB_MAX_TEXT - 1; i >= 0; i--) {
            if (!subs || i >= subs->lines || !subs->text[i])
                rm_osd_msg(OSD_MSG_SUB_BASE + i);
            else {
                // HACK: currently display time for each sub line except the last is set to 2 seconds.
                int display_time = i == subs->lines - 1 ? 180000 : 2000;
                set_osd_msg(OSD_MSG_SUB_BASE + i, 1, display_time, "%s", subs->text[i]);
            }
        }
    }
}

/**
 * \brief Update the OSD message line.
 *
 * This function displays the current message on the vo OSD or on the term.
 * If the stack is empty and the OSD level is high enough the timer
 * is displayed (only on the vo OSD).
 *
 */

static void update_osd_msg(struct MPContext *mpctx)
{
    struct MPOpts *opts = &mpctx->opts;
    mp_osd_msg_t *msg;
    struct osd_state *osd = mpctx->osd;
    char osd_text_timer[128];

    if (mpctx->add_osd_seek_info) {
        double percentage;
        if (mpctx->timeline && mpctx->sh_video)
            percentage = mpctx->sh_video->pts * 100 /
                mpctx->timeline[mpctx->num_timeline_parts].start;
        else
            percentage = demuxer_get_percent_pos(mpctx->demuxer);
        set_osd_bar(mpctx, 0, "Position", 0, 100, percentage);
        if (mpctx->sh_video)
            mpctx->osd_show_percentage_until = (GetTimerMS() + 1000) | 1;
        mpctx->add_osd_seek_info = false;
    }

    // Look if we have a msg
    if((msg = get_osd_msg(mpctx))) {
        if (strcmp(osd->osd_text, msg->msg)) {
            strncpy(osd->osd_text, msg->msg, 127);
            if(mpctx->sh_video) vo_osd_changed(OSDTYPE_OSD); else
            if(term_osd) mp_msg(MSGT_CPLAYER,MSGL_STATUS,"%s%s\n",term_osd_esc,msg->msg);
        }
        return;
    }

    if(mpctx->sh_video) {
        // fallback on the timer
        if (opts->osd_level >= 2) {
            int len;
            if (mpctx->timeline)
                len = mpctx->timeline[mpctx->num_timeline_parts].start;
            else
                len = demuxer_get_time_length(mpctx->demuxer);
            int percentage = -1;
            char percentage_text[10];
            int pts = demuxer_get_current_time(mpctx->demuxer);

            if (mpctx->osd_show_percentage_until)
                if (mpctx->timeline)
                    percentage = mpctx->sh_video->pts * 100 /
                        mpctx->timeline[mpctx->num_timeline_parts].start;
                else
                    percentage = demuxer_get_percent_pos(mpctx->demuxer);

            if (percentage >= 0)
                snprintf(percentage_text, 9, " (%d%%)", percentage);
            else
                percentage_text[0] = 0;

            if (opts->osd_level == 3)
                snprintf(osd_text_timer, 63,
                         "%c %02d:%02d:%02d / %02d:%02d:%02d%s",
                         mpctx->osd_function,pts/3600,(pts/60)%60,pts%60,
                         len/3600,(len/60)%60,len%60,percentage_text);
            else
                snprintf(osd_text_timer, 63, "%c %02d:%02d:%02d%s",
                         mpctx->osd_function,pts/3600,(pts/60)%60,
                         pts%60,percentage_text);
        } else
            osd_text_timer[0]=0;

        if (strcmp(osd->osd_text, osd_text_timer)) {
            strncpy(osd->osd_text, osd_text_timer, 63);
            vo_osd_changed(OSDTYPE_OSD);
        }
        return;
    }

    // Clear the term osd line
    if (term_osd && osd->osd_text[0]) {
        osd->osd_text[0] = 0;
        printf("%s\n",term_osd_esc);
    }
}

///@}
// OSDMsgStack


void reinit_audio_chain(struct MPContext *mpctx)
{
    struct MPOpts *opts = &mpctx->opts;
    if (!mpctx->sh_audio)
        return;
    current_module="init_audio_codec";
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
    if(!init_best_audio_codec(mpctx->sh_audio,audio_codec_list,audio_fm_list)){
        goto init_error;
    }
    mpctx->initialized_flags|=INITIALIZED_ACODEC;
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");


    current_module="af_preinit";
    ao_data.samplerate=force_srate;
    ao_data.channels=0;
    ao_data.format=audio_output_format;
#if 1
    // first init to detect best values
    if(!init_audio_filters(mpctx->sh_audio,   // preliminary init
                           // input:
                           mpctx->sh_audio->samplerate,
                           // output:
                           &ao_data.samplerate, &ao_data.channels, &ao_data.format)){
        mp_tmsg(MSGT_CPLAYER,MSGL_ERR, "Error at audio filter chain "
                "pre-init!\n");
        exit_player(mpctx, EXIT_ERROR);
    }
#endif
    current_module="ao2_init";
    mpctx->audio_out = init_best_audio_out(opts->audio_driver_list,
                                           0, // plugin flag
                                           ao_data.samplerate,
                                           ao_data.channels,
                                           ao_data.format, 0);
    if(!mpctx->audio_out){
        mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Could not open/initialize audio device -> no sound.\n");
        goto init_error;
    }
    mpctx->initialized_flags|=INITIALIZED_AO;
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"AO: [%s] %dHz %dch %s (%d bytes per sample)\n",
           mpctx->audio_out->info->short_name,
           ao_data.samplerate, ao_data.channels,
           af_fmt2str_short(ao_data.format),
           af_fmt2bits(ao_data.format)/8 );
    mp_msg(MSGT_CPLAYER,MSGL_V,"AO: Description: %s\nAO: Author: %s\n",
           mpctx->audio_out->info->name, mpctx->audio_out->info->author);
    if(strlen(mpctx->audio_out->info->comment) > 0)
        mp_msg(MSGT_CPLAYER,MSGL_V,"AO: Comment: %s\n", mpctx->audio_out->info->comment);
    // init audio filters:
#if 1
    current_module="af_init";
    if(!build_afilter_chain(mpctx, mpctx->sh_audio, &ao_data)) {
        mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Couldn't find matching filter/ao format!\n");
        goto init_error;
    }
#endif
    mpctx->mixer.audio_out = mpctx->audio_out;
    mpctx->mixer.volstep = volstep;
    return;

init_error:
    uninit_player(mpctx, INITIALIZED_ACODEC|INITIALIZED_AO); // close codec and possibly AO
    mpctx->sh_audio=mpctx->d_audio->sh=NULL; // -> nosound
    mpctx->d_audio->id = -2;
}


///@}
// Command2Property


// Return pts value corresponding to the end point of audio written to the
// ao so far.
static double written_audio_pts(struct MPContext *mpctx)
{
    sh_audio_t *sh_audio = mpctx->sh_audio;
    demux_stream_t *d_audio = mpctx->d_audio;
    double buffered_output;
    // first calculate the end pts of audio that has been output by decoder
    double a_pts = sh_audio->pts;
    if (a_pts != MP_NOPTS_VALUE)
	// Good, decoder supports new way of calculating audio pts.
	// sh_audio->pts is the timestamp of the latest input packet with
	// known pts that the decoder has decoded. sh_audio->pts_bytes is
	// the amount of bytes the decoder has written after that timestamp.
	a_pts += sh_audio->pts_bytes / (double) sh_audio->o_bps;
    else {
	// Decoder doesn't support new way of calculating pts (or we're
	// being called before it has decoded anything with known timestamp).
	// Use the old method of audio pts calculation: take the timestamp
	// of last packet with known pts the decoder has read data from,
	// and add amount of bytes read after the beginning of that packet
	// divided by input bps. This will be inaccurate if the input/output
	// ratio is not constant for every audio packet or if it is constant
	// but not accurately known in sh_audio->i_bps.

	a_pts = d_audio->pts;
	// ds_tell_pts returns bytes read after last timestamp from
	// demuxing layer, decoder might use sh_audio->a_in_buffer for bytes
	// it has read but not decoded
	if (sh_audio->i_bps)
	    a_pts += (ds_tell_pts(d_audio) - sh_audio->a_in_buffer_len) /
		(double)sh_audio->i_bps;
    }
    // Now a_pts hopefully holds the pts for end of audio from decoder.
    // Substract data in buffers between decoder and audio out.

    // Decoded but not filtered
    a_pts -= sh_audio->a_buffer_len / (double)sh_audio->o_bps;

    // Data buffered in audio filters, measured in bytes of "missing" output
    buffered_output = af_calc_delay(sh_audio->afilter);

    // Data that was ready for ao but was buffered because ao didn't fully
    // accept everything to internal buffers yet
    buffered_output += sh_audio->a_out_buffer_len;

    // Filters divide audio length by playback_speed, so multiply by it
    // to get the length in original units without speedup or slowdown
    a_pts -= buffered_output * mpctx->opts.playback_speed / ao_data.bps;

    return a_pts + mpctx->video_offset;
}

// Return pts value corresponding to currently playing audio.
double playing_audio_pts(struct MPContext *mpctx)
{
    return written_audio_pts(mpctx) - mpctx->opts.playback_speed *
	mpctx->audio_out->get_delay();
}

static int check_framedrop(struct MPContext *mpctx, double frame_time) {
    struct MPOpts *opts = &mpctx->opts;
	// check for frame-drop:
	current_module = "check_framedrop";
	if (mpctx->sh_audio && !mpctx->d_audio->eof) {
	    static int dropped_frames;
	    float delay = opts->playback_speed*mpctx->audio_out->get_delay();
	    float d = delay-mpctx->delay;
	    ++total_frame_cnt;
	    // we should avoid dropping too many frames in sequence unless we
	    // are too late. and we allow 100ms A-V delay here:
	    if (d < -dropped_frames*frame_time-0.100 && !mpctx->paused
                && !mpctx->update_video_immediately) {
		++drop_frame_cnt;
		++dropped_frames;
		return frame_dropping;
	    } else
		dropped_frames = 0;
	}
	return 0;
}


#ifdef HAVE_RTC
    int rtc_fd = -1;
#endif

static float timing_sleep(struct MPContext *mpctx, float time_frame)
{
#ifdef HAVE_RTC
    if (rtc_fd >= 0){
	// -------- RTC -----------
	current_module="sleep_rtc";
        while (time_frame > 0.000) {
	    unsigned long rtc_ts;
	    if (read(rtc_fd, &rtc_ts, sizeof(rtc_ts)) <= 0)
		mp_tmsg(MSGT_CPLAYER, MSGL_ERR, "Linux RTC read error: %s\n", strerror(errno));
            time_frame -= get_relative_time(mpctx);
	}
    } else
#endif
    {
	// assume kernel HZ=100 for softsleep, works with larger HZ but with
	// unnecessarily high CPU usage
	float margin = softsleep ? 0.011 : 0;
	current_module = "sleep_timer";
	while (time_frame > margin) {
	    usec_sleep(1000000 * (time_frame - margin));
            time_frame -= get_relative_time(mpctx);
	}
	if (softsleep){
	    current_module = "sleep_soft";
	    if (time_frame < 0)
		mp_tmsg(MSGT_AVSYNC, MSGL_WARN, "Warning! Softsleep underflow!\n");
	    while (time_frame > 0)
                time_frame -= get_relative_time(mpctx); // burn the CPU
	}
    }
    return time_frame;
}

static void select_subtitle(MPContext *mpctx)
{
    struct MPOpts *opts = &mpctx->opts;
  // find the best sub to use
  int vobsub_index_id = vobsub_get_index_by_id(vo_vobsub, vobsub_id);
  mpctx->global_sub_pos = -1; // no subs by default
  if (vobsub_index_id >= 0) {
    // if user asks for a vobsub id, use that first.
    mpctx->global_sub_pos = mpctx->global_sub_indices[SUB_SOURCE_VOBSUB] + vobsub_index_id;
  } else if (opts->sub_id >= 0 && mpctx->global_sub_indices[SUB_SOURCE_DEMUX] >= 0) {
    // if user asks for a dvd sub id, use that next.
    mpctx->global_sub_pos = mpctx->global_sub_indices[SUB_SOURCE_DEMUX] + opts->sub_id;
  } else if (mpctx->global_sub_indices[SUB_SOURCE_SUBS] >= 0) {
    // if there are text subs to use, use those.  (autosubs come last here)
    mpctx->global_sub_pos = mpctx->global_sub_indices[SUB_SOURCE_SUBS];
  } else if (opts->sub_id == -1 && mpctx->global_sub_indices[SUB_SOURCE_DEMUX] >= 0) {
    // finally select subs by language and container hints
    if (opts->sub_id == -1 && dvdsub_lang)
      opts->sub_id = demuxer_sub_track_by_lang(mpctx->demuxer, dvdsub_lang);
    if (opts->sub_id == -1)
      opts->sub_id = demuxer_default_sub_track(mpctx->demuxer);
    if (opts->sub_id >= 0)
      mpctx->global_sub_pos = mpctx->global_sub_indices[SUB_SOURCE_DEMUX] + opts->sub_id;
  }
  // rather than duplicate code, use the SUB_SELECT handler to init the right one.
  mpctx->global_sub_pos--;
  mp_property_do("sub",M_PROPERTY_STEP_UP,NULL, mpctx);
}

#ifdef CONFIG_DVDNAV
#ifndef FF_B_TYPE
#define FF_B_TYPE 3
#endif
/// store decoded video image
static mp_image_t * mp_dvdnav_copy_mpi(mp_image_t *to_mpi,
                                       mp_image_t *from_mpi) {
    mp_image_t *mpi;

    /// Do not store B-frames
    if (from_mpi->pict_type == FF_B_TYPE)
        return to_mpi;

    if (to_mpi &&
        to_mpi->w == from_mpi->w &&
        to_mpi->h == from_mpi->h &&
        to_mpi->imgfmt == from_mpi->imgfmt)
        mpi = to_mpi;
    else {
        if (to_mpi)
            free_mp_image(to_mpi);
        if (from_mpi->w == 0 || from_mpi->h == 0)
            return NULL;
        mpi = alloc_mpi(from_mpi->w,from_mpi->h,from_mpi->imgfmt);
    }

    copy_mpi(mpi,from_mpi);
    return mpi;
}

static void mp_dvdnav_reset_stream (MPContext *ctx) {
    struct MPOpts *opts = &ctx->opts;
    if (ctx->sh_video) {
        /// clear video pts
        ctx->d_video->pts = 0.0f;
        ctx->sh_video->pts = 0.0f;
        ctx->sh_video->i_pts = 0.0f;
        ctx->sh_video->last_pts = 0.0f;
        ctx->sh_video->num_buffered_pts = 0;
        ctx->sh_video->num_frames = 0;
        ctx->sh_video->num_frames_decoded = 0;
        ctx->sh_video->timer = 0.0f;
        ctx->sh_video->stream_delay = 0.0f;
        ctx->sh_video->timer = 0;
        ctx->demuxer->stream_pts = MP_NOPTS_VALUE;
    }

    if (ctx->sh_audio) {
        /// free audio packets and reset
        ds_free_packs(ctx->d_audio);
        audio_delay -= ctx->sh_audio->stream_delay;
        ctx->delay =- audio_delay;
        ctx->audio_out->reset();
        resync_audio_stream(ctx->sh_audio);
    }

    audio_delay = 0.0f;
    ctx->global_sub_size = ctx->global_sub_indices[SUB_SOURCE_DEMUX] + mp_dvdnav_number_of_subs(ctx->stream);
    if (dvdsub_lang && opts->sub_id == dvdsub_lang_id) {
        dvdsub_lang_id = mp_dvdnav_sid_from_lang(ctx->stream, dvdsub_lang);
        if (dvdsub_lang_id != opts->sub_id) {
            opts->sub_id = dvdsub_lang_id;
            select_subtitle(ctx);
        }
    }

    /// clear all EOF related flags
    ctx->d_video->eof = ctx->d_audio->eof = ctx->stream->eof = 0;
}

/// Restore last decoded DVDNAV (still frame)
static mp_image_t *mp_dvdnav_restore_smpi(struct MPContext *mpctx,
                                          int *in_size,
                                          unsigned char **start,
                                          mp_image_t *decoded_frame)
{
    if (mpctx->stream->type != STREAMTYPE_DVDNAV)
        return decoded_frame;

    /// a change occured in dvdnav stream
    if (mp_dvdnav_cell_has_changed(mpctx->stream,0)) {
        mp_dvdnav_read_wait(mpctx->stream, 1, 1);
        mp_dvdnav_context_free(mpctx);
        mp_dvdnav_reset_stream(mpctx);
        mp_dvdnav_read_wait(mpctx->stream, 0, 1);
        mp_dvdnav_cell_has_changed(mpctx->stream,1);
    }

    if (*in_size < 0) {
        float len;

        /// Display still frame, if any
        if (mpctx->nav_smpi && !mpctx->nav_buffer)
            decoded_frame = mpctx->nav_smpi;

        /// increment video frame : continue playing after still frame
        len = demuxer_get_time_length(mpctx->demuxer);
        if (mpctx->sh_video->pts >= len &&
            mpctx->sh_video->pts > 0.0 && len > 0.0) {
            mp_dvdnav_skip_still(mpctx->stream);
            mp_dvdnav_skip_wait(mpctx->stream);
        }
        mpctx->sh_video->pts += 1 / mpctx->sh_video->fps;

        if (mpctx->nav_buffer) {
            *start = mpctx->nav_start;
            *in_size = mpctx->nav_in_size;
            if (mpctx->nav_start)
                memcpy(*start,mpctx->nav_buffer,mpctx->nav_in_size);
        }
    }

    return decoded_frame;
}

/// Save last decoded DVDNAV (still frame)
static void mp_dvdnav_save_smpi(struct MPContext *mpctx, int in_size,
                                unsigned char *start,
                                mp_image_t *decoded_frame)
{
    if (mpctx->stream->type != STREAMTYPE_DVDNAV)
        return;

    if (mpctx->nav_buffer)
        free(mpctx->nav_buffer);

    mpctx->nav_buffer = malloc(in_size);
    mpctx->nav_start = start;
    mpctx->nav_in_size = mpctx->nav_buffer ? in_size : -1;
    if (mpctx->nav_buffer)
        memcpy(mpctx->nav_buffer,start,in_size);

    if (decoded_frame && mpctx->nav_smpi != decoded_frame)
        mpctx->nav_smpi = mp_dvdnav_copy_mpi(mpctx->nav_smpi,decoded_frame);
}
#endif /* CONFIG_DVDNAV */

/* Modify video timing to match the audio timeline. There are two main
 * reasons this is needed. First, video and audio can start from different
 * positions at beginning of file or after a seek (MPlayer starts both
 * immediately even if they have different pts). Second, the file can have
 * audio timestamps that are inconsistent with the duration of the audio
 * packets, for example two consecutive timestamp values differing by
 * one second but only a packet with enough samples for half a second
 * of playback between them.
 */
static void adjust_sync(struct MPContext *mpctx, double frame_time)
{
    current_module = "av_sync";

    if (!mpctx->sh_audio)
        return;

    double a_pts = written_audio_pts(mpctx) - mpctx->delay;
    double v_pts = mpctx->sh_video->pts;
    double av_delay = a_pts - v_pts;
    // Try to sync vo_flip() so it will *finish* at given time
    av_delay += mpctx->last_vo_flip_duration;
    av_delay -= audio_delay;   // This much pts difference is desired

    double change = av_delay * 0.1;
    double max_change = default_max_pts_correction >= 0 ?
        default_max_pts_correction : frame_time * 0.1;
    if (change < -max_change)
        change = -max_change;
    else if (change > max_change)
        change = max_change;
    mpctx->delay += change;
    mpctx->total_avsync_change += change;
}

static int fill_audio_out_buffers(struct MPContext *mpctx)
{
    struct MPOpts *opts = &mpctx->opts;
    unsigned int t;
    double tt;
    int playsize;
    int playflags=0;
    int audio_eof=0;
    int bytes_to_write;
    sh_audio_t * const sh_audio = mpctx->sh_audio;

    current_module="play_audio";

    while (1) {
	int sleep_time;
	// all the current uses of ao_data.pts seem to be in aos that handle
	// sync completely wrong; there should be no need to use ao_data.pts
	// in get_space()
	ao_data.pts = ((mpctx->sh_video?mpctx->sh_video->timer:0)+mpctx->delay)*90000.0;
	bytes_to_write = mpctx->audio_out->get_space();
	if (mpctx->sh_video || bytes_to_write >= ao_data.outburst)
	    break;

	// handle audio-only case:
	// this is where mplayer sleeps during audio-only playback
	// to avoid 100% CPU use
	sleep_time = (ao_data.outburst - bytes_to_write) * 1000 / ao_data.bps;
	if (sleep_time < 10) sleep_time = 10; // limit to 100 wakeups per second
	usec_sleep(sleep_time * 1000);
    }

    while (bytes_to_write) {
	playsize = bytes_to_write;
	if (playsize > MAX_OUTBURST)
	    playsize = MAX_OUTBURST;
	bytes_to_write -= playsize;

	// Fill buffer if needed:
	current_module="decode_audio";
	t = GetTimer();
	if (decode_audio(sh_audio, playsize) < 0) // EOF or error
	    if (mpctx->d_audio->eof) {
		audio_eof = 1;
		if (sh_audio->a_out_buffer_len == 0)
		    return 0;
	    }
	t = GetTimer() - t;
	tt = t*0.000001f; audio_time_usage+=tt;
	if (playsize > sh_audio->a_out_buffer_len) {
	    playsize = sh_audio->a_out_buffer_len;
	    if (audio_eof)
		playflags |= AOPLAY_FINAL_CHUNK;
	}
	if (!playsize)
	    break;

	// play audio:
	current_module="play_audio";

	// Is this pts value actually useful for the aos that access it?
	// They're obviously badly broken in the way they handle av sync;
	// would not having access to this make them more broken?
	ao_data.pts = ((mpctx->sh_video?mpctx->sh_video->timer:0)+mpctx->delay)*90000.0;
	playsize = mpctx->audio_out->play(sh_audio->a_out_buffer, playsize, playflags);

	if (playsize > 0) {
	    sh_audio->a_out_buffer_len -= playsize;
	    memmove(sh_audio->a_out_buffer, &sh_audio->a_out_buffer[playsize],
		    sh_audio->a_out_buffer_len);
	    mpctx->delay += opts->playback_speed*playsize/(double)ao_data.bps;
	}
	else if (audio_eof && mpctx->audio_out->get_delay() < .04) {
	    // Sanity check to avoid hanging in case current ao doesn't output
	    // partial chunks and doesn't check for AOPLAY_FINAL_CHUNK
	    mp_msg(MSGT_CPLAYER, MSGL_WARN, "Audio output truncated at end.\n");
	    sh_audio->a_out_buffer_len = 0;
	}
    }
    return 1;
}

static int sleep_until_update(struct MPContext *mpctx, float *time_frame,
                              float *aq_sleep_time)
{
    struct MPOpts *opts = &mpctx->opts;
    int frame_time_remaining = 0;
    current_module="calc_sleep_time";

    *time_frame -= get_relative_time(mpctx); // reset timer

    if (mpctx->sh_audio && !mpctx->d_audio->eof) {
	float delay = mpctx->audio_out->get_delay();
	mp_dbg(MSGT_AVSYNC, MSGL_DBG2, "delay=%f\n", delay);

	if (autosync) {
	    /*
	     * Adjust this raw delay value by calculating the expected
	     * delay for this frame and generating a new value which is
	     * weighted between the two.  The higher autosync is, the
	     * closer to the delay value gets to that which "-nosound"
	     * would have used, and the longer it will take for A/V
	     * sync to settle at the right value (but it eventually will.)
	     * This settling time is very short for values below 100.
	     */
	    float predicted = mpctx->delay / opts->playback_speed + *time_frame;
	    float difference = delay - predicted;
	    delay = predicted + difference / (float)autosync;
	}

	*time_frame = delay - mpctx->delay / opts->playback_speed;

	// delay = amount of audio buffered in soundcard/driver
	if (delay > 0.25) delay=0.25; else
	if (delay < 0.10) delay=0.10;
	if (*time_frame > delay*0.6) {
	    // sleep time too big - may cause audio drops (buffer underrun)
	    frame_time_remaining = 1;
	    *time_frame = delay*0.5;
	}
    } else {
	// If we're lagging more than 200 ms behind the right playback rate,
	// don't try to "catch up".
	// If benchmark is set always output frames as fast as possible
	// without sleeping.
	if (*time_frame < -0.2 || benchmark)
	    *time_frame = 0;
    }

    *aq_sleep_time += *time_frame;


    //============================== SLEEP: ===================================

    if (mpctx->video_out->driver->flip_page_timed)
        *time_frame -= 0.05;
    // flag 256 means: libvo driver does its timing (dvb card)
    if (*time_frame > 0.001 && !(mpctx->sh_video->output_flags&256))
	*time_frame = timing_sleep(mpctx, *time_frame);
    if (mpctx->video_out->driver->flip_page_timed)
        *time_frame += 0.05;
    return frame_time_remaining;
}

int reinit_video_chain(struct MPContext *mpctx)
{
    struct MPOpts *opts = &mpctx->opts;
    sh_video_t * const sh_video = mpctx->sh_video;
    double ar=-1.0;
    //================== Init VIDEO (codec & libvo) ==========================
    if (!opts->fixed_vo || !(mpctx->initialized_flags & INITIALIZED_VO)) {
    current_module="preinit_libvo";

    //shouldn't we set dvideo->id=-2 when we fail?
    //if((mpctx->video_out->preinit(vo_subdevice))!=0){
    if(!(mpctx->video_out=init_best_video_out(opts, mpctx->x11_state, mpctx->key_fifo, mpctx->input))){
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"Error opening/initializing the selected video_out (-vo) device.\n");
      goto err_out;
    }
    mpctx->initialized_flags|=INITIALIZED_VO;
  }

  if(stream_control(mpctx->demuxer->stream, STREAM_CTRL_GET_ASPECT_RATIO, &ar) != STREAM_UNSUPPORTED)
      mpctx->sh_video->stream_aspect = ar;
  current_module="init_video_filters";
  {
    char* vf_arg[] = { "_oldargs_", (char*)mpctx->video_out , NULL };
    sh_video->vfilter = vf_open_filter(opts, NULL,"vo",vf_arg);
  }
#ifdef CONFIG_MENU
  if(use_menu) {
    char* vf_arg[] = { "_oldargs_", menu_root, NULL };
    vf_menu = vf_open_plugin(opts,libmenu_vfs,sh_video->vfilter,"menu",vf_arg);
    if(!vf_menu) {
      mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Can't open libmenu video filter with root menu %s.\n",menu_root);
      use_menu = 0;
    }
  }
  if(vf_menu)
    sh_video->vfilter = vf_menu;
#endif

#ifdef CONFIG_ASS
  if(opts->ass_enabled) {
    int i;
    int insert = 1;
    if (opts->vf_settings)
      for (i = 0; opts->vf_settings[i].name; ++i)
        if (strcmp(opts->vf_settings[i].name, "ass") == 0) {
          insert = 0;
          break;
        }
    if (insert) {
      extern vf_info_t vf_info_ass;
      const vf_info_t* libass_vfs[] = {&vf_info_ass, NULL};
      char* vf_arg[] = {"auto", "1", NULL};
      vf_instance_t* vf_ass = vf_open_plugin(opts, libass_vfs,sh_video->vfilter,"ass",vf_arg);
      if (vf_ass)
        sh_video->vfilter = vf_ass;
      else
        mp_msg(MSGT_CPLAYER,MSGL_ERR, "ASS: cannot add video filter\n");
    }
  }
#endif

  sh_video->vfilter = append_filters(sh_video->vfilter, opts->vf_settings);

#ifdef CONFIG_ASS
  if (opts->ass_enabled)
    sh_video->vfilter->control(sh_video->vfilter, VFCTRL_INIT_EOSD, ass_library);
#endif

  current_module="init_video_codec";

  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");
  init_best_video_codec(sh_video,video_codec_list,video_fm_list);
  mp_msg(MSGT_CPLAYER,MSGL_INFO,"==========================================================================\n");

  if(!sh_video->initialized){
    if(!opts->fixed_vo) uninit_player(mpctx, INITIALIZED_VO);
    goto err_out;
  }

  mpctx->initialized_flags|=INITIALIZED_VCODEC;

  if (sh_video->codec)
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_VIDEO_CODEC=%s\n", sh_video->codec->name);

  sh_video->last_pts = MP_NOPTS_VALUE;
  sh_video->num_buffered_pts = 0;
  sh_video->next_frame_time = 0;

  if(auto_quality>0){
    // Auto quality option enabled
    output_quality=get_video_quality_max(sh_video);
    if(auto_quality>output_quality) auto_quality=output_quality;
    else output_quality=auto_quality;
    mp_msg(MSGT_CPLAYER,MSGL_V,"AutoQ: setting quality to %d.\n",output_quality);
    set_video_quality(sh_video,output_quality);
  }

  // ========== Init display (sh_video->disp_w*sh_video->disp_h/out_fmt) ============

  current_module="init_vo";

  return 1;

err_out:
  mpctx->sh_video = mpctx->d_video->sh = NULL;
  return 0;
}

static double update_video_nocorrect_pts(struct MPContext *mpctx)
{
    struct sh_video *sh_video = mpctx->sh_video;
    double frame_time = 0;
    struct vo *video_out = mpctx->video_out;
    while (!video_out->frame_loaded) {
        current_module = "filter_video";
        // In nocorrect-pts mode there is no way to properly time these frames
        if (vo_get_buffered_frame(video_out, 0) >= 0)
            break;
        if (vf_output_queued_frame(sh_video->vfilter))
            continue;
        unsigned char *packet = NULL;
        frame_time = sh_video->next_frame_time;
        if (mpctx->update_video_immediately)
            frame_time = 0;
        int in_size = video_read_frame(sh_video, &sh_video->next_frame_time,
                                       &packet, force_fps);
        if (in_size < 0) {
#ifdef CONFIG_DVDNAV
            if (mpctx->stream->type == STREAMTYPE_DVDNAV) {
                if (mp_dvdnav_is_eof(mpctx->stream))
                    return -1;
                if (mpctx->d_video)
                    mpctx->d_video->eof = 0;
                if (mpctx->d_audio)
                    mpctx->d_audio->eof = 0;
                mpctx->stream->eof = 0;
            } else
#endif
            return -1;
        }
        if (in_size > max_framesize)
            max_framesize = in_size;
        sh_video->timer += frame_time;
        if (mpctx->sh_audio)
            mpctx->delay -= frame_time;
        // video_read_frame can change fps (e.g. for ASF video)
        vo_fps = sh_video->fps;
        int framedrop_type = check_framedrop(mpctx, frame_time);
        current_module = "decode video";

        void *decoded_frame;
#ifdef CONFIG_DVDNAV
        decoded_frame = mp_dvdnav_restore_smpi(mpctx, &in_size, &packet, NULL);
        if (in_size >= 0 && !decoded_frame)
#endif
        decoded_frame = decode_video(sh_video, packet, in_size, framedrop_type,
                                     sh_video->pts);
#ifdef CONFIG_DVDNAV
        // Save last still frame for future display
        mp_dvdnav_save_smpi(mpctx, in_size, packet, decoded_frame);
#endif
        if (decoded_frame) {
            current_module = "filter video";
            if (filter_video(sh_video, decoded_frame, sh_video->pts))
                if (!video_out->config_ok)
                    break;
        }
    }
    return frame_time;
}

static void determine_frame_pts(struct MPContext *mpctx)
{
    struct sh_video *sh_video = mpctx->sh_video;
    struct MPOpts *opts = &mpctx->opts;

    if (opts->user_pts_assoc_mode) {
        sh_video->pts_assoc_mode = opts->user_pts_assoc_mode;
    } else if (sh_video->pts_assoc_mode == 0) {
        if (sh_video->codec_reordered_pts != MP_NOPTS_VALUE)
            sh_video->pts_assoc_mode = 1;
        else
            sh_video->pts_assoc_mode = 2;
    } else {
        int probcount1 = sh_video->num_reordered_pts_problems;
        int probcount2 = sh_video->num_sorted_pts_problems;
        if (sh_video->pts_assoc_mode == 2) {
            int tmp = probcount1;
            probcount1 = probcount2;
            probcount2 = tmp;
        }
        if (probcount1 >= probcount2 * 1.5 + 2) {
            sh_video->pts_assoc_mode = 3 - sh_video->pts_assoc_mode;
            mp_msg(MSGT_CPLAYER, MSGL_V, "Switching to pts association mode "
                   "%d.\n", sh_video->pts_assoc_mode);
        }
    }
    sh_video->pts = sh_video->pts_assoc_mode == 1 ?
        sh_video->codec_reordered_pts : sh_video->sorted_pts;
}

static double update_video(struct MPContext *mpctx)
{
    struct sh_video *sh_video = mpctx->sh_video;
    struct vo *video_out = mpctx->video_out;
    sh_video->vfilter->control(sh_video->vfilter, VFCTRL_SET_OSD_OBJ,
                               mpctx->osd); // hack for vf_expand
    if (!mpctx->opts.correct_pts)
        return update_video_nocorrect_pts(mpctx);

    double pts;

    bool hit_eof = false;
    while (!video_out->frame_loaded) {
        current_module = "filter_video";
        if (vo_get_buffered_frame(video_out, hit_eof) >= 0)
            break;
        // XXX Time used in this call is not counted in any performance
        // timer now, OSD time is not updated correctly for filter-added frames
        if (vf_output_queued_frame(sh_video->vfilter))
            continue;
        if (hit_eof)
            return -1;
        unsigned char *packet = NULL;
        int in_size = ds_get_packet_pts(mpctx->d_video, &packet, &pts);
        if (pts != MP_NOPTS_VALUE)
            pts += mpctx->video_offset;
        if (in_size < 0) {
            // try to extract last frames in case of decoder lag
            in_size = 0;
            pts = 1e300;
            hit_eof = true;
        }
        if (in_size > max_framesize)
            max_framesize = in_size;
        current_module = "decode video";
        int framedrop_type = check_framedrop(mpctx, sh_video->frametime);
        void *decoded_frame = decode_video(sh_video, packet, in_size,
                                           framedrop_type, pts);
        if (decoded_frame) {
            determine_frame_pts(mpctx);
            current_module = "filter video";
            if (filter_video(sh_video, decoded_frame, sh_video->pts))
                if (!video_out->config_ok)
                    break; // We'd likely hang in this loop otherwise
        }
    }

    pts = video_out->next_pts;
    if (pts == MP_NOPTS_VALUE) {
        mp_msg(MSGT_CPLAYER, MSGL_ERR, "Video pts after filters MISSING\n");
        // Try to use decoder pts from before filters
        pts = sh_video->pts;
        if (pts == MP_NOPTS_VALUE)
            pts = sh_video->last_pts;
    }
    sh_video->pts = pts;
    if (sh_video->last_pts == MP_NOPTS_VALUE)
        sh_video->last_pts = sh_video->pts;
    else if (sh_video->last_pts > sh_video->pts) {
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "Decreasing video pts: %f < %f\n",
               sh_video->pts, sh_video->last_pts);
        /* If the difference in pts is small treat it as jitter around the
         * right value (possibly caused by incorrect timestamp ordering) and
         * just show this frame immediately after the last one.
         * Treat bigger differences as timestamp resets and start counting
         * timing of later frames from the position of this one. */
        if (sh_video->last_pts - sh_video->pts > 0.5)
            sh_video->last_pts = sh_video->pts;
        else
            sh_video->pts = sh_video->last_pts;
    }
    double frame_time = sh_video->pts - sh_video->last_pts;
    sh_video->last_pts = sh_video->pts;
    sh_video->timer += frame_time;
    if (mpctx->sh_audio)
        mpctx->delay -= frame_time;
    return frame_time;
}

void pause_player(struct MPContext *mpctx)
{
    if (mpctx->paused)
        return;
    mpctx->paused = 1;
    mpctx->step_frames = 0;
    mpctx->time_frame -= get_relative_time(mpctx);

    if (mpctx->video_out && mpctx->sh_video && mpctx->video_out->config_ok)
	vo_control(mpctx->video_out, VOCTRL_PAUSE, NULL);

    if (mpctx->audio_out && mpctx->sh_audio)
	mpctx->audio_out->pause();	// pause audio, keep data if possible
}

void unpause_player(struct MPContext *mpctx)
{
    if (!mpctx->paused)
        return;
    mpctx->paused = 0;

    if (mpctx->audio_out && mpctx->sh_audio)
        mpctx->audio_out->resume();	// resume audio
    if (mpctx->video_out && mpctx->sh_video && mpctx->video_out->config_ok
        && !mpctx->step_frames)
        vo_control(mpctx->video_out, VOCTRL_RESUME, NULL);	// resume video
    (void)get_relative_time(mpctx);	// ignore time that passed during pause
}

void add_step_frame(struct MPContext *mpctx)
{
    mpctx->step_frames++;
    if (mpctx->video_out && mpctx->sh_video && mpctx->video_out->config_ok)
	vo_control(mpctx->video_out, VOCTRL_PAUSE, NULL);
    unpause_player(mpctx);
}

static void pause_loop(struct MPContext *mpctx)
{
    mp_cmd_t* cmd;
    if (!quiet) {
        // Small hack to display the pause message on the OSD line.
        // The pause string is: "\n == PAUSE == \r" so we need to
        // take the first and the last char out
	if (term_osd && !mpctx->sh_video) {
	    char msg[128] = _("\n ===== PAUSE =====\r");
	    int mlen = strlen(msg);
	    msg[mlen-1] = '\0';
	    set_osd_msg(OSD_MSG_PAUSE, 1, 0, "%s", msg+1);
	    update_osd_msg(mpctx);
	} else
	    mp_tmsg(MSGT_CPLAYER,MSGL_STATUS,"\n ===== PAUSE =====\r");
        mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_PAUSED\n");
    }

    while ( (cmd = mp_input_get_cmd(mpctx->input, 20, 1, 1)) == NULL
            || cmd->id == MP_CMD_SET_MOUSE_POS || cmd->pausing == 4) {
	if (cmd) {
	  cmd = mp_input_get_cmd(mpctx->input, 0,1,0);
	  run_command(mpctx, cmd);
	  mp_cmd_free(cmd);
	  continue;
	}
	if (mpctx->sh_video && mpctx->video_out)
	    vo_check_events(mpctx->video_out);
#ifdef CONFIG_MENU
        if (vf_menu)
            vf_menu_pause_update(vf_menu);
#endif
	usec_sleep(20000);
      update_osd_msg(mpctx);
      int hack = vo_osd_changed(0);
      vo_osd_changed(hack);
      if (hack)
          break;
    }
}


// Find the right mute status and record position for new file position
static void edl_seek_reset(MPContext *mpctx)
{
    mpctx->edl_muted = 0;
    next_edl_record = edl_records;

    while (next_edl_record) {
	if (next_edl_record->start_sec >= mpctx->sh_video->pts)
	    break;

	if (next_edl_record->action == EDL_MUTE)
	    mpctx->edl_muted = !mpctx->edl_muted;
	next_edl_record = next_edl_record->next;
    }
    if ((mpctx->user_muted | mpctx->edl_muted) != mpctx->mixer.muted)
	mixer_mute(&mpctx->mixer);
}


// Execute EDL command for the current position if one exists
static void edl_update(MPContext *mpctx)
{
    if (!next_edl_record)
	return;

    if (!mpctx->sh_video) {
	mp_tmsg(MSGT_CPLAYER, MSGL_ERR, "Cannot use EDL without video, disabling.\n");
	free_edl(edl_records);
	next_edl_record = NULL;
	edl_records = NULL;
	return;
    }

    if (mpctx->sh_video->pts >= next_edl_record->start_sec) {
	if (next_edl_record->action == EDL_SKIP) {
	    mpctx->osd_function = OSD_FFW;
	    mpctx->abs_seek_pos = 0;
	    mpctx->rel_seek_secs = next_edl_record->length_sec;
	    mp_msg(MSGT_CPLAYER, MSGL_DBG4, "EDL_SKIP: start [%f], stop "
		   "[%f], length [%f]\n", next_edl_record->start_sec,
		   next_edl_record->stop_sec, next_edl_record->length_sec);
	}
	else if (next_edl_record->action == EDL_MUTE) {
	    mpctx->edl_muted = !mpctx->edl_muted;
	    if ((mpctx->user_muted | mpctx->edl_muted) != mpctx->mixer.muted)
		mixer_mute(&mpctx->mixer);
	    mp_msg(MSGT_CPLAYER, MSGL_DBG4, "EDL_MUTE: [%f]\n",
		   next_edl_record->start_sec );
	}
	next_edl_record = next_edl_record->next;
    }
}

static void reinit_decoders(struct MPContext *mpctx)
{
    reinit_video_chain(mpctx);
    reinit_audio_chain(mpctx);
    mp_property_do("sub", M_PROPERTY_SET, &mpctx->global_sub_pos, mpctx);
}

static bool timeline_set_part(struct MPContext *mpctx, int i)
{
    struct timeline_part *p = mpctx->timeline + mpctx->timeline_part;
    struct timeline_part *n = mpctx->timeline + i;
    mpctx->timeline_part = i;
    mpctx->video_offset = n->start - n->source_start;
    if (n->source == p->source)
        return false;
    uninit_player(mpctx, INITIALIZED_VCODEC | (mpctx->opts.fixed_vo && mpctx->opts.video_id != -2 ? 0 : INITIALIZED_VO) | INITIALIZED_AO | INITIALIZED_ACODEC);
    mpctx->demuxer = n->source->demuxer;
    mpctx->d_video = mpctx->demuxer->video;
    mpctx->d_audio = mpctx->demuxer->audio;
    mpctx->d_sub = mpctx->demuxer->sub;
    mpctx->sh_video = mpctx->d_video->sh;
    mpctx->sh_audio = mpctx->d_audio->sh;
    return true;
}

// Given pts, switch playback to the corresponding part.
// Return offset within that part.
static double timeline_set_from_time(struct MPContext *mpctx, double pts,
                                     bool *need_reset)
{
    if (pts < 0)
        pts = 0;
    for (int i = 0; i < mpctx->num_timeline_parts; i++) {
        struct timeline_part *p = mpctx->timeline + i;
        if (pts < (p+1)->start) {
            *need_reset = timeline_set_part(mpctx, i);
            return pts - p->start + p->source_start;
        }
    }
    return -1;
}


// style & SEEK_ABSOLUTE == 0 means seek relative to current position, == 1 means absolute
// style & SEEK_FACTOR == 0 means amount in seconds, == 2 means fraction of file length
// return -1 if seek failed (non-seekable stream?), 0 otherwise
static int seek(MPContext *mpctx, double amount, int style)
{
    current_module = "seek";
    if (mpctx->stop_play == AT_END_OF_FILE)
        mpctx->stop_play = KEEP_PLAYING;
    if (mpctx->timeline && style & SEEK_FACTOR) {
        amount *= mpctx->timeline[mpctx->num_timeline_parts].start;
        style &= ~SEEK_FACTOR;
    }
    if ((mpctx->demuxer->accurate_seek || mpctx->timeline) && mpctx->sh_video
        && !(style & (SEEK_ABSOLUTE | SEEK_FACTOR))) {
        style |= SEEK_ABSOLUTE;
        if (amount > 0)
            style |= SEEK_FORWARD;
        else
            style |= SEEK_BACKWARD;
        amount += mpctx->sh_video->pts;
    }

    /* At least the liba52 decoder wants to read from the input stream
     * during initialization, so reinit must be done after the demux_seek()
     * call that clears possible stream EOF. */
    bool need_reset = false;
    if (mpctx->timeline) {
        amount = timeline_set_from_time(mpctx, amount, &need_reset);
        if (amount == -1) {
            mpctx->stop_play = AT_END_OF_FILE;
            // Clear audio from current position
            if (mpctx->sh_audio) {
                mpctx->audio_out->reset();
                mpctx->sh_audio->a_buffer_len = 0;
                mpctx->sh_audio->a_out_buffer_len = 0;
            }
            return -1;
        }
    }
    int seekresult = demux_seek(mpctx->demuxer, amount, audio_delay, style);
    if (need_reset)
        reinit_decoders(mpctx);
    if (seekresult == 0)
	return -1;

    if (mpctx->sh_video) {
	current_module = "seek_video_reset";
	resync_video_stream(mpctx->sh_video);
        vo_seek_reset(mpctx->video_out);
	mpctx->sh_video->num_buffered_pts = 0;
	mpctx->sh_video->last_pts = MP_NOPTS_VALUE;
	mpctx->delay = 0;
        mpctx->time_frame = 0;
        mpctx->update_video_immediately = true;
	// Not all demuxers set d_video->pts during seek, so this value
	// (which is used by at least vobsub and edl code below) may
	// be completely wrong (probably 0).
	mpctx->sh_video->pts = mpctx->d_video->pts + mpctx->video_offset;
	update_subtitles(mpctx, &mpctx->opts, mpctx->sh_video,
                         mpctx->sh_video->pts, mpctx->video_offset,
                         mpctx->d_sub, 1);
	update_teletext(mpctx->sh_video, mpctx->demuxer, 1);
    }

    if (mpctx->sh_audio) {
	current_module = "seek_audio_reset";
	mpctx->audio_out->reset(); // stop audio, throwing away buffered data
	mpctx->sh_audio->a_buffer_len = 0;
	mpctx->sh_audio->a_out_buffer_len = 0;
	if (!mpctx->sh_video)
	    update_subtitles(mpctx, &mpctx->opts, NULL, mpctx->sh_audio->pts,
                             mpctx->video_offset, mpctx->d_sub, 1);
    }

    if (vo_vobsub && mpctx->sh_video) {
	current_module = "seek_vobsub_reset";
	vobsub_seek(vo_vobsub, mpctx->sh_video->pts);
    }

    edl_seek_reset(mpctx);

    mpctx->total_avsync_change = 0;
    audio_time_usage = 0; video_time_usage = 0; vout_time_usage = 0;
    drop_frame_cnt = 0;

    current_module = NULL;
    return 0;
}

int get_current_chapter(struct MPContext *mpctx)
{
    if (!mpctx->chapters || !mpctx->sh_video)
        return demuxer_get_current_chapter(mpctx->demuxer);

    int i;
    double current_pts = mpctx->sh_video->pts;
    for (i = 1; i < mpctx->num_chapters; i++)
        if (current_pts < mpctx->chapters[i].start)
            break;
    return i - 1;
}

// currently returns a string allocated with malloc, not talloc
char *chapter_display_name(struct MPContext *mpctx, int chapter)
{
    if (!mpctx->chapters || !mpctx->sh_video)
        return demuxer_chapter_display_name(mpctx->demuxer, chapter);
    return strdup(mpctx->chapters[chapter].name);
}

int seek_chapter(struct MPContext *mpctx, int chapter, double *seek_pts,
                 char **chapter_name)
{
    if (!mpctx->chapters || !mpctx->sh_video)
        return demuxer_seek_chapter(mpctx->demuxer, chapter, seek_pts,
                                    chapter_name);
    if (chapter >= mpctx->num_chapters)
        return -1;
    if (chapter < 0)
        chapter = 0;
    *seek_pts = mpctx->chapters[chapter].start;
    if (chapter_name)
        *chapter_name = talloc_strdup(NULL, mpctx->chapters[chapter].name);
    return chapter;
}

static int find_ordered_chapter_sources(struct MPContext *mpctx,
                                        struct content_source *sources,
                                        int num_sources,
                                        unsigned char uid_map[][16])
{
    int num_filenames = 0;
    char **filenames = NULL;
    if (num_sources > 1) {
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "This file references data from "
               "other sources.\n");
        if (mpctx->stream->type != STREAMTYPE_FILE) {
            mp_msg(MSGT_CPLAYER, MSGL_WARN, "Playback source is not a "
                   "normal disk file. Will not search for related files.\n");
        } else {
            mp_msg(MSGT_CPLAYER, MSGL_INFO, "Will scan other files in the "
                   "same directory to find referenced sources.\n");
            filenames = find_files(mpctx->demuxer->filename, ".mkv",
                                   &num_filenames);
        }
    }

    int num_left = num_sources - 1;
    for (int i = 0; i < num_filenames && num_left > 0; i++) {
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "Checking file %s\n",
               filename_recode(filenames[i]));
        int format;
        struct stream *s = open_stream(filenames[i], &mpctx->opts, &format);
        if (!s)
            continue;
        struct demuxer *d = demux_open(&mpctx->opts, s, DEMUXER_TYPE_MATROSKA,
                                       mpctx->opts.audio_id,
                                       mpctx->opts.video_id,
                                       mpctx->opts.sub_id, filenames[i]);
        if (!d) {
            free_stream(s);
            continue;
        }
        if (d->file_format == DEMUXER_TYPE_MATROSKA) {
            for (int i = 1; i < num_sources; i++) {
                if (sources[i].demuxer)
                    continue;
                if (!memcmp(uid_map[i], d->matroska_data.segment_uid, 16)) {
                    mp_msg(MSGT_CPLAYER, MSGL_INFO,"Match for source %d: %s\n",
                           i, filename_recode(d->filename));
                    sources[i].stream = s;
                    sources[i].demuxer = d;
                    num_left--;
                    goto match;
                }
            }
        }
        free_demuxer(d);
        free_stream(s);
        continue;
    match:
        ;
    }
    talloc_free(filenames);
    if (num_left) {
        mp_msg(MSGT_CPLAYER, MSGL_ERR, "Failed to find ordered chapter part!\n"
               "There will be parts MISSING from the video!\n");
        for (int i = 1, j = 1; i < num_sources; i++)
            if (sources[i].demuxer) {
                sources[j] = sources[i];
                memcpy(uid_map[j], uid_map[i], 16);
                j++;
            }
    }
    return num_sources - num_left;
}

static void build_ordered_chapter_timeline(struct MPContext *mpctx)
{
    if (!mpctx->opts.ordered_chapters) {
        mp_msg(MSGT_CPLAYER, MSGL_INFO, "File uses ordered chapters, but "
               "you have disabled support for them. Ignoring.\n");
        return;
    }

    mp_msg(MSGT_CPLAYER, MSGL_INFO, "File uses ordered chapters, will build "
           "edit timeline.\n");

    struct demuxer *demuxer = mpctx->demuxer;
    struct matroska_data *m = &demuxer->matroska_data;

    // +1 because sources/uid_map[0] is original file even if all chapters
    // actually use other sources and need separate entries
    struct content_source *sources = talloc_array_ptrtype(NULL, sources,
                                                   m->num_ordered_chapters+1);
    sources[0].stream = mpctx->stream;
    sources[0].demuxer = mpctx->demuxer;
    unsigned char uid_map[m->num_ordered_chapters+1][16];
    int num_sources = 1;
    memcpy(uid_map[0], m->segment_uid, 16);

    for (int i = 0; i < m->num_ordered_chapters; i++) {
        struct matroska_chapter *c = m->ordered_chapters + i;
        if (!c->has_segment_uid)
            memcpy(c->segment_uid, m->segment_uid, 16);

        for (int j = 0; j < num_sources; j++)
            if (!memcmp(c->segment_uid, uid_map[j], 16))
                goto found1;
        memcpy(uid_map[num_sources], c->segment_uid, 16);
        sources[num_sources] = (struct content_source){};
        num_sources++;
    found1:
        ;
    }

    num_sources = find_ordered_chapter_sources(mpctx, sources, num_sources,
                                               uid_map);


    // +1 for terminating chapter with start time marking end of last real one
    struct timeline_part *timeline = talloc_array_ptrtype(NULL, timeline,
                                                  m->num_ordered_chapters + 1);
    struct chapter *chapters = talloc_array_ptrtype(NULL, chapters,
                                                    m->num_ordered_chapters);
    uint64_t starttime = 0;
    uint64_t missing_time = 0;
    int part_count = 0;
    int num_chapters = 0;
    uint64_t prev_part_offset;
    for (int i = 0; i < m->num_ordered_chapters; i++) {
        struct matroska_chapter *c = m->ordered_chapters + i;

        int j;
        for (j = 0; j < num_sources; j++) {
            if (!memcmp(c->segment_uid, uid_map[j], 16))
                goto found2;
        }
        missing_time += c->end - c->start;
        continue;
    found2:;
        chapters[num_chapters].start = starttime / 1000.;
        chapters[num_chapters].name = talloc_strdup(chapters, c->name);
        /* Only add a separate part if the time or file actually changes.
         * Matroska files have chapter divisions that are redundant from
         * timeline point of view because the same chapter structure is used
         * both to specify the timeline and for normal chapter information.
         * Removing a missing inserted external chapter can also cause this. */
        if (part_count == 0 || c->start != starttime + prev_part_offset
            || sources + j != timeline[part_count - 1].source) {
            timeline[part_count].source = sources + j;
            timeline[part_count].start = chapters[num_chapters].start;
            timeline[part_count].source_start = c->start / 1000.;
            prev_part_offset = c->start - starttime;
            part_count++;
        }
        starttime += c->end - c->start;
        num_chapters++;
    }
    timeline[part_count].start = starttime / 1000.;

    if (!part_count) {
        // None of the parts come from the file itself???
        talloc_free(sources);
        talloc_free(timeline);
        talloc_free(chapters);
        return;
    }

    mp_msg(MSGT_CPLAYER, MSGL_V, "Timeline contains %d parts from %d "
           "sources. Total length %.3f seconds.\n", part_count, num_sources,
           timeline[part_count].start);
    if (missing_time)
        mp_msg(MSGT_CPLAYER, MSGL_ERR, "There are %.3f seconds missing "
               "from the timeline!\n", missing_time / 1000.);
    mp_msg(MSGT_CPLAYER, MSGL_V, "Source files:\n");
    for (int i = 0; i < num_sources; i++)
        mp_msg(MSGT_CPLAYER, MSGL_V, "%d: %s\n", i,
               filename_recode(sources[i].demuxer->filename));
    mp_msg(MSGT_CPLAYER, MSGL_V, "Timeline parts: (number, start, "
           "source_start, source):\n");
    for (int i = 0; i < part_count; i++) {
        struct timeline_part *p = timeline + i;
        mp_msg(MSGT_CPLAYER, MSGL_V, "%3d %9.3f %9.3f %3td\n", i, p->start,
               p->source_start, p->source - sources);
    }
    mp_msg(MSGT_CPLAYER, MSGL_V, "END %9.3f\n", timeline[part_count].start);
    mpctx->sources = sources;
    mpctx->num_sources = num_sources;
    mpctx->timeline = timeline;
    mpctx->num_timeline_parts = part_count;
    mpctx->num_chapters = num_chapters;
    mpctx->chapters = chapters;

    mpctx->timeline_part = 0;
    mpctx->demuxer = timeline[0].source->demuxer;
}


static int read_keys(void *ctx, int fd)
{
    getch2(ctx);
    return mplayer_get_key(ctx, 0);
}


#ifdef PTW32_STATIC_LIB
static void detach_ptw32(void)
{
    pthread_win32_thread_detach_np();
    pthread_win32_process_detach_np();
}
#endif

/* This preprocessor directive is a hack to generate a mplayer-nomain.o object
 * file for some tools to link against. */
#ifndef DISABLE_MAIN
int main(int argc,char* argv[]){


char * mem_ptr;

// movie info:

/* Flag indicating whether MPlayer should exit without playing anything. */
int opt_exit = 0;

//float a_frame=0;    // Audio

int i;

    struct MPContext *mpctx = &(struct MPContext){
        .osd_function = OSD_PLAY,
        .begin_skip = MP_NOPTS_VALUE,
        .play_tree_step = 1,
        .global_sub_pos = -1,
        .set_of_sub_pos = -1,
        .file_format = DEMUXER_TYPE_UNKNOWN,
        .last_dvb_step = 1,
    };

#ifdef PTW32_STATIC_LIB
  pthread_win32_process_attach_np();
  pthread_win32_thread_attach_np();
  atexit(detach_ptw32);
#endif

  InitTimer();
  srand(GetTimerMS());

  mp_msg_init();
  set_av_log_callback();

#ifdef CONFIG_X11
  mpctx->x11_state = vo_x11_init_state();
#endif
  struct MPOpts *opts = &mpctx->opts;
  set_default_mplayer_options(opts);
  // Create the config context and register the options
  mpctx->mconfig = m_config_new(opts, cfg_include);
  m_config_register_options(mpctx->mconfig,mplayer_opts);
  mp_input_register_options(mpctx->mconfig);

  // Preparse the command line
  m_config_preparse_command_line(mpctx->mconfig,argc,argv);

#if (defined(__MINGW32__) || defined(__CYGWIN__)) && defined(CONFIG_WIN32DLL)
  set_path_env();
#endif

#ifdef CONFIG_TV
  stream_tv_defaults.immediate = 1;
#endif

    parse_cfgfiles(mpctx, mpctx->mconfig);

    mpctx->playtree = m_config_parse_mp_command_line(mpctx->mconfig, argc, argv);
    if(mpctx->playtree == NULL)
      opt_exit = 1;
    else {
    mpctx->playtree = play_tree_cleanup(mpctx->playtree);
    if(mpctx->playtree) {
      mpctx->playtree_iter = play_tree_iter_new(mpctx->playtree,mpctx->mconfig);
      if(mpctx->playtree_iter) {
	if(play_tree_iter_step(mpctx->playtree_iter,0,0) != PLAY_TREE_ITER_ENTRY) {
	  play_tree_iter_free(mpctx->playtree_iter);
	  mpctx->playtree_iter = NULL;
	}
	mpctx->filename = play_tree_iter_get_file(mpctx->playtree_iter,1);
      }
    }
    }
    mpctx->key_fifo = mp_fifo_create(opts);

  print_version("MPlayer");

#if defined(__MINGW32__) || defined(__CYGWIN__)
	{
		HMODULE kernel32 = GetModuleHandle("Kernel32.dll");
		BOOL WINAPI (*setDEP)(DWORD) = NULL;
		if (kernel32)
			setDEP = GetProcAddress(kernel32, "SetProcessDEPPolicy");
		if (setDEP) setDEP(3);
	}
	// stop Windows from showing all kinds of annoying error dialogs
	SetErrorMode(0x8003);
	// request 1ms timer resolution
	timeBeginPeriod(1);
#endif

#ifdef CONFIG_PRIORITY
    set_priority();
#endif

    if(opts->video_driver_list && strcmp(opts->video_driver_list[0],"help")==0){
      list_video_out();
      opt_exit = 1;
    }

    if(opts->audio_driver_list && strcmp(opts->audio_driver_list[0],"help")==0){
      list_audio_out();
      opt_exit = 1;
    }

/* Check codecs.conf. */
if(!codecs_file || !parse_codec_cfg(codecs_file)){
  if(!parse_codec_cfg(mem_ptr=get_path("codecs.conf"))){
    if(!parse_codec_cfg(MPLAYER_CONFDIR "/codecs.conf")){
      if(!parse_codec_cfg(NULL)){
        exit_player_with_rc(mpctx, EXIT_NONE, 0);
      }
      mp_tmsg(MSGT_CPLAYER,MSGL_V,"Using built-in default codecs.conf.\n");
    }
  }
  free( mem_ptr ); // release the buffer created by get_path()
}

#if 0
    if(video_codec_list){
	int i;
	video_codec=video_codec_list[0];
	for(i=0;video_codec_list[i];i++)
	    mp_msg(MSGT_FIXME,MSGL_FIXME,"vc#%d: '%s'\n",i,video_codec_list[i]);
    }
#endif
    if(audio_codec_list && strcmp(audio_codec_list[0],"help")==0){
      mp_tmsg(MSGT_CPLAYER, MSGL_INFO, "Available audio codecs:\n");
      mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_AUDIO_CODECS\n");
      list_codecs(1);
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      opt_exit = 1;
    }
    if(video_codec_list && strcmp(video_codec_list[0],"help")==0){
      mp_tmsg(MSGT_CPLAYER, MSGL_INFO, "Available video codecs:\n");
      mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_VIDEO_CODECS\n");
      list_codecs(0);
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      opt_exit = 1;
    }
    if(video_fm_list && strcmp(video_fm_list[0],"help")==0){
      vfm_help();
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      opt_exit = 1;
    }
    if(audio_fm_list && strcmp(audio_fm_list[0],"help")==0){
      afm_help();
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      opt_exit = 1;
    }
    if(af_cfg.list && strcmp(af_cfg.list[0],"help")==0){
      af_help();
      printf("\n");
      opt_exit = 1;
    }
#ifdef CONFIG_X11
    if(vo_fstype_list && strcmp(vo_fstype_list[0],"help")==0){
      fstype_help();
      mp_msg(MSGT_FIXME, MSGL_FIXME, "\n");
      opt_exit = 1;
    }
#endif
    if((demuxer_name && strcmp(demuxer_name,"help")==0) ||
       (audio_demuxer_name && strcmp(audio_demuxer_name,"help")==0) ||
       (sub_demuxer_name && strcmp(sub_demuxer_name,"help")==0)){
      demuxer_help();
      mp_msg(MSGT_CPLAYER, MSGL_INFO, "\n");
      opt_exit = 1;
    }
    if(list_properties) {
      property_print_help();
      opt_exit = 1;
    }

    if(opt_exit)
      exit_player(mpctx, EXIT_NONE);

    if(!mpctx->filename && !player_idle_mode){
	// no file/vcd/dvd -> show HELP:
	mp_msg(MSGT_CPLAYER, MSGL_INFO, help_text);
        exit_player_with_rc(mpctx, EXIT_NONE, 0);
    }

    /* Display what configure line was used */
    mp_msg(MSGT_CPLAYER, MSGL_V, "Configuration: " CONFIGURATION "\n");

    // Many users forget to include command line in bugreports...
    if( mp_msg_test(MSGT_CPLAYER,MSGL_V) ){
      mp_tmsg(MSGT_CPLAYER, MSGL_INFO, "CommandLine:");
      for(i=1;i<argc;i++)mp_msg(MSGT_CPLAYER, MSGL_INFO," '%s'",argv[i]);
      mp_msg(MSGT_CPLAYER, MSGL_INFO, "\n");
    }

//------ load global data first ------

  mpctx->osd = osd_create();

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
       if(!vo_font) mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Cannot load bitmap font: %s\n",
		filename_recode(font_name));
  } else {
      // try default:
       vo_font=read_font_desc( mem_ptr=get_path("font/font.desc"),font_factor,verbose>1);
       free(mem_ptr); // release the buffer created by get_path()
       if(!vo_font)
       vo_font=read_font_desc(MPLAYER_DATADIR "/font/font.desc",font_factor,verbose>1);
  }
  if (sub_font_name)
    mpctx->osd->sub_font = read_font_desc(sub_font_name, font_factor, verbose>1);
  else
    mpctx->osd->sub_font = vo_font;
#endif
#ifdef CONFIG_FONTCONFIG
  }
#endif

#ifdef CONFIG_ASS
  ass_library = ass_init();
#endif

#ifdef HAVE_RTC
  if(!nortc)
  {
    // seteuid(0); /* Can't hurt to try to get root here */
    if ((rtc_fd = open(rtc_device ? rtc_device : "/dev/rtc", O_RDONLY)) < 0)
	mp_tmsg(MSGT_CPLAYER, MSGL_WARN, "Failed to open %s: %s (it should be readable by the user.)\n",
	    rtc_device ? rtc_device : "/dev/rtc", strerror(errno));
     else {
	unsigned long irqp = 1024; /* 512 seemed OK. 128 is jerky. */

	if (ioctl(rtc_fd, RTC_IRQP_SET, irqp) < 0) {
    	    mp_tmsg(MSGT_CPLAYER, MSGL_WARN, "Linux RTC init error in ioctl (rtc_irqp_set %lu): %s\n", irqp, strerror(errno));
    	    mp_tmsg(MSGT_CPLAYER, MSGL_HINT, "Try adding \"echo %lu > /proc/sys/dev/rtc/max-user-freq\" to your system startup scripts.\n", irqp);
   	    close (rtc_fd);
    	    rtc_fd = -1;
	} else if (ioctl(rtc_fd, RTC_PIE_ON, 0) < 0) {
	    /* variable only by the root */
    	    mp_tmsg(MSGT_CPLAYER, MSGL_ERR, "Linux RTC init error in ioctl (rtc_pie_on): %s\n", strerror(errno));
    	    close (rtc_fd);
	    rtc_fd = -1;
	} else
	    mp_tmsg(MSGT_CPLAYER, MSGL_V, "Using Linux hardware RTC timing (%ldHz).\n", irqp);
    }
  }
    if(rtc_fd<0)
#endif /* HAVE_RTC */
      mp_msg(MSGT_CPLAYER, MSGL_V, "Using %s timing\n",
	     softsleep?"software":timer_name);

#ifdef HAVE_TERMCAP
  load_termcap(NULL); // load key-codes
#endif

// ========== Init keyboard FIFO (connection to libvo) ============

// Init input system
current_module = "init_input";
 mpctx->input = mp_input_init(&opts->input);
 mp_input_add_key_fd(mpctx->input, -1,0,mplayer_get_key,NULL, mpctx->key_fifo);
if(slave_mode)
    mp_input_add_cmd_fd(mpctx->input, 0,USE_SELECT,MP_INPUT_SLAVE_CMD_FUNC,NULL);
else if(!noconsolecontrols)
    mp_input_add_key_fd(mpctx->input, 0, 1, read_keys, NULL, mpctx->key_fifo);
// Set the libstream interrupt callback
stream_set_interrupt_callback(mp_input_check_interrupt, mpctx->input);

#ifdef CONFIG_MENU
 if(use_menu) {
     if(menu_cfg && menu_init(mpctx, mpctx->mconfig, mpctx->input, menu_cfg))
     mp_tmsg(MSGT_CPLAYER, MSGL_V, "Menu initialized: %s\n", menu_cfg);
   else {
     menu_cfg = get_path("menu.conf");
     if(menu_init(mpctx, mpctx->mconfig, mpctx->input, menu_cfg))
       mp_tmsg(MSGT_CPLAYER, MSGL_V, "Menu initialized: %s\n", menu_cfg);
     else {
         if(menu_init(mpctx, mpctx->mconfig, mpctx->input,
                      MPLAYER_CONFDIR "/menu.conf"))
         mp_tmsg(MSGT_CPLAYER, MSGL_V, "Menu initialized: %s\n", MPLAYER_CONFDIR"/menu.conf");
       else {
         mp_tmsg(MSGT_CPLAYER, MSGL_ERR, "Menu init failed.\n");
         use_menu = 0;
       }
     }
   }
 }
#endif

current_module = NULL;

  /// Catch signals
#ifndef __MINGW32__
  signal(SIGCHLD,child_sighandler);
#endif

#ifdef CONFIG_CRASH_DEBUG
  prog_path = argv[0];
#endif
  //========= Catch terminate signals: ================
  // terminate requests:
  signal(SIGTERM,exit_sighandler); // kill
  signal(SIGHUP,exit_sighandler);  // kill -HUP  /  xterm closed

  signal(SIGINT,exit_sighandler);  // Interrupt from keyboard

  signal(SIGQUIT,exit_sighandler); // Quit from keyboard
  signal(SIGPIPE,exit_sighandler); // Some window managers cause this
#ifdef CONFIG_SIGHANDLER
  // fatal errors:
  signal(SIGBUS,exit_sighandler);  // bus error
  signal(SIGSEGV,exit_sighandler); // segfault
  signal(SIGILL,exit_sighandler);  // illegal instruction
  signal(SIGFPE,exit_sighandler);  // floating point exc.
  signal(SIGABRT,exit_sighandler); // abort()
#ifdef CONFIG_CRASH_DEBUG
  if (crash_debug)
    signal(SIGTRAP,exit_sighandler);
#endif
#endif

// ******************* Now, let's see the per-file stuff ********************

play_next_file:

  // init global sub numbers
  mpctx->global_sub_size = 0;
  { int i; for (i = 0; i < SUB_SOURCES; i++) mpctx->global_sub_indices[i] = -1; }

  if (mpctx->filename) {
    load_per_protocol_config (mpctx->mconfig, mpctx->filename);
    load_per_extension_config (mpctx->mconfig, mpctx->filename);
    load_per_file_config (mpctx->mconfig, mpctx->filename);
  }

  if (opts->video_driver_list)
    load_per_output_config (mpctx->mconfig, PROFILE_CFG_VO, opts->video_driver_list[0]);
  if (opts->audio_driver_list)
    load_per_output_config (mpctx->mconfig, PROFILE_CFG_AO, opts->audio_driver_list[0]);

// We must enable getch2 here to be able to interrupt network connection
// or cache filling
if(!noconsolecontrols && !slave_mode){
  if(mpctx->initialized_flags&INITIALIZED_GETCH2)
    mp_tmsg(MSGT_CPLAYER,MSGL_WARN,"WARNING: getch2_init called twice!\n");
  else
    getch2_enable();  // prepare stdin for hotkeys...
  mpctx->initialized_flags|=INITIALIZED_GETCH2;
  mp_msg(MSGT_CPLAYER,MSGL_DBG2,"\n[[[init getch2]]]\n");
}

// =================== GUI idle loop (STOP state) ===========================
while (player_idle_mode && !mpctx->filename) {
    play_tree_t * entry = NULL;
    mp_cmd_t * cmd;
    while (!(cmd = mp_input_get_cmd(mpctx->input, 0,1,0))) { // wait for command
        if (mpctx->video_out)
	    vo_check_events(mpctx->video_out);
        usec_sleep(20000);
    }
    switch (cmd->id) {
        case MP_CMD_LOADFILE:
            // prepare a tree entry with the new filename
            entry = play_tree_new();
            play_tree_add_file(entry, cmd->args[0].v.s);
            // The entry is added to the main playtree after the switch().
            break;
        case MP_CMD_LOADLIST:
            entry = parse_playlist_file(mpctx->mconfig, cmd->args[0].v.s);
            break;
        case MP_CMD_QUIT:
            exit_player_with_rc(mpctx, EXIT_QUIT, (cmd->nargs > 0)? cmd->args[0].v.i : 0);
            break;
        case MP_CMD_GET_PROPERTY:
        case MP_CMD_SET_PROPERTY:
        case MP_CMD_STEP_PROPERTY:
            run_command(mpctx, cmd);
            break;
    }

    mp_cmd_free(cmd);

    if (entry) { // user entered a command that gave a valid entry
        if (mpctx->playtree) // the playtree is always a node with one child. let's clear it
            play_tree_free_list(mpctx->playtree->child, 1);
        else mpctx->playtree=play_tree_new(); // .. or make a brand new playtree

        if (!mpctx->playtree) continue; // couldn't make playtree! wait for next command

        play_tree_set_child(mpctx->playtree, entry);

        /* Make iterator start at the top the of tree. */
        mpctx->playtree_iter = play_tree_iter_new(mpctx->playtree, mpctx->mconfig);
        if (!mpctx->playtree_iter) continue;

        // find the first real item in the tree
        if (play_tree_iter_step(mpctx->playtree_iter,0,0) != PLAY_TREE_ITER_ENTRY) {
            // no items!
            play_tree_iter_free(mpctx->playtree_iter);
            mpctx->playtree_iter = NULL;
            continue; // wait for next command
        }
        mpctx->filename = play_tree_iter_get_file(mpctx->playtree_iter, 1);
    }
}
//---------------------------------------------------------------------------

    if (mpctx->filename) {
	mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"\nPlaying %s.\n",
		filename_recode(mpctx->filename));
        if(use_filename_title && vo_wintitle == NULL)
            vo_wintitle = strdup(mp_basename2(mpctx->filename));
    }

if (edl_filename) {
    if (edl_records) free_edl(edl_records);
    next_edl_record = edl_records = edl_parse_file();
}
if (edl_output_filename) {
    if (edl_fd) fclose(edl_fd);
    if ((edl_fd = fopen(edl_output_filename, "w")) == NULL)
    {
        mp_tmsg(MSGT_CPLAYER, MSGL_ERR, "Can't open EDL file [%s] for writing.\n",
		filename_recode(edl_output_filename));
    }
}

//==================== Open VOB-Sub ============================

    current_module="vobsub";
    if (vobsub_name){
      vo_vobsub=vobsub_open(vobsub_name,spudec_ifo,1,&vo_spudec);
      if(vo_vobsub==NULL)
        mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Cannot load subtitles: %s\n",
		filename_recode(vobsub_name));
    } else if (sub_auto && mpctx->filename){
      /* try to autodetect vobsub from movie filename ::atmos */
      char *buf = strdup(mpctx->filename), *psub;
      char *pdot = strrchr(buf, '.');
      char *pslash = strrchr(buf, '/');
#if defined(__MINGW32__) || defined(__CYGWIN__)
      if (!pslash) pslash = strrchr(buf, '\\');
#endif
      if (pdot && (!pslash || pdot > pslash))
        *pdot = '\0';
      vo_vobsub=vobsub_open(buf,spudec_ifo,0,&vo_spudec);
      /* try from ~/.mplayer/sub */
      if(!vo_vobsub && (psub = get_path( "sub/" ))) {
          char *bname;
          int l;
          bname = strrchr(buf,'/');
#if defined(__MINGW32__) || defined(__CYGWIN__)
          if(!bname) bname = strrchr(buf,'\\');
#endif
          if(bname) bname++;
          else bname = buf;
          l = strlen(psub) + strlen(bname) + 1;
          psub = realloc(psub,l);
          strcat(psub,bname);
          vo_vobsub=vobsub_open(psub,spudec_ifo,0,&vo_spudec);
          free(psub);
      }
      free(buf);
    }
    if(vo_vobsub){
      mpctx->initialized_flags|=INITIALIZED_VOBSUB;
      vobsub_set_from_lang(vo_vobsub, dvdsub_lang);
      mp_property_do("sub_forced_only", M_PROPERTY_SET, &forced_subs_only, mpctx);

      // setup global sub numbering
      mpctx->global_sub_indices[SUB_SOURCE_VOBSUB] = mpctx->global_sub_size; // the global # of the first vobsub.
      mpctx->global_sub_size += vobsub_get_indexes_count(vo_vobsub);
    }

//============ Open & Sync STREAM --- fork cache2 ====================

  mpctx->stream=NULL;
  mpctx->demuxer=NULL;
  if (mpctx->d_audio) {
    //free_demuxer_stream(mpctx->d_audio);
    mpctx->d_audio=NULL;
  }
  if (mpctx->d_video) {
    //free_demuxer_stream(d_video);
    mpctx->d_video=NULL;
  }
  mpctx->sh_audio=NULL;
  mpctx->sh_video=NULL;

  current_module="open_stream";
  mpctx->stream = open_stream(mpctx->filename, opts, &mpctx->file_format);
  if(!mpctx->stream) { // error...
      mpctx->stop_play = libmpdemux_was_interrupted(mpctx, PT_NEXT_ENTRY);
    goto goto_next_file;
  }
  mpctx->initialized_flags|=INITIALIZED_STREAM;

  if(mpctx->file_format == DEMUXER_TYPE_PLAYLIST) {
      mp_msg(MSGT_CPLAYER, MSGL_ERR, "\nThis looks like a playlist, but "
             "playlist support will not be used automatically.\n"
             "MPlayer's playlist code is unsafe and should only be used with "
             "trusted sources.\nPlayback will probably fail.\n\n");
#if 0
    play_tree_t* entry;
    // Handle playlist
    current_module="handle_playlist";
    mp_msg(MSGT_CPLAYER,MSGL_V,"Parsing playlist %s...\n",
	    filename_recode(mpctx->filename));
    entry = parse_playtree(mpctx->stream, mpctx->mconfig, 0);
    mpctx->eof=playtree_add_playlist(mpctx, entry);
    goto goto_next_file;
#endif
  }
  mpctx->stream->start_pos+=seek_to_byte;

if(stream_dump_type==5){
  unsigned char buf[4096];
  int len;
  FILE *f;
  current_module="dumpstream";
  stream_reset(mpctx->stream);
  stream_seek(mpctx->stream,mpctx->stream->start_pos);
  f=fopen(stream_dump_name,"wb");
  if(!f){
    mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"Cannot open dump file.\n");
    exit_player(mpctx, EXIT_ERROR);
  }
  if (dvd_chapter > 1) {
    int chapter = dvd_chapter - 1;
    stream_control(mpctx->stream, STREAM_CTRL_SEEK_TO_CHAPTER, &chapter);
  }
  while(!mpctx->stream->eof && !async_quit_request){
      len=stream_read(mpctx->stream,buf,4096);
      if(len>0) {
        if(fwrite(buf,len,1,f) != 1) {
          mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"%s: Error writing file.\n",stream_dump_name);
          exit_player(mpctx, EXIT_ERROR);
        }
      }
      if(dvd_last_chapter > 0) {
        int chapter = -1;
        if (stream_control(mpctx->stream, STREAM_CTRL_GET_CURRENT_CHAPTER,
                           &chapter) == STREAM_OK && chapter + 1 > dvd_last_chapter)
          break;
      }
  }
  if(fclose(f)) {
    mp_tmsg(MSGT_MENCODER,MSGL_FATAL,"%s: Error writing file.\n",stream_dump_name);
    exit_player(mpctx, EXIT_ERROR);
  }
  mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Core dumped ;)\n");
  exit_player_with_rc(mpctx, EXIT_EOF, 0);
}

#ifdef CONFIG_DVDREAD
if(mpctx->stream->type==STREAMTYPE_DVD){
  current_module="dvd lang->id";
  if(opts->audio_id==-1) opts->audio_id=dvd_aid_from_lang(mpctx->stream,audio_lang);
  if(dvdsub_lang && opts->sub_id==-1) opts->sub_id=dvd_sid_from_lang(mpctx->stream,dvdsub_lang);
  // setup global sub numbering
  mpctx->global_sub_indices[SUB_SOURCE_DEMUX] = mpctx->global_sub_size; // the global # of the first demux-specific sub.
  mpctx->global_sub_size += dvd_number_of_subs(mpctx->stream);
  current_module=NULL;
}
#endif

#ifdef CONFIG_DVDNAV
if(mpctx->stream->type==STREAMTYPE_DVDNAV){
  current_module="dvdnav lang->id";
  if(opts->audio_id==-1) opts->audio_id=mp_dvdnav_aid_from_lang(mpctx->stream,audio_lang);
  dvdsub_lang_id = -3;
  if(dvdsub_lang && opts->sub_id==-1)
    dvdsub_lang_id = opts->sub_id = mp_dvdnav_sid_from_lang(mpctx->stream,dvdsub_lang);
  // setup global sub numbering
  mpctx->global_sub_indices[SUB_SOURCE_DEMUX] = mpctx->global_sub_size; // the global # of the first demux-specific sub.
  mpctx->global_sub_size += mp_dvdnav_number_of_subs(mpctx->stream);
  current_module=NULL;
}
#endif

// CACHE2: initial prefill: 20%  later: 5%  (should be set by -cacheopts)
goto_enable_cache:
if(stream_cache_size>0){
  current_module="enable_cache";
  if(!stream_enable_cache(mpctx->stream,stream_cache_size*1024,
                          stream_cache_size*1024*(stream_cache_min_percent / 100.0),
                          stream_cache_size*1024*(stream_cache_seek_min_percent / 100.0)))
      if((mpctx->stop_play = libmpdemux_was_interrupted(mpctx, PT_NEXT_ENTRY))) goto goto_next_file;
}

//============ Open DEMUXERS --- DETECT file type =======================
current_module="demux_open";

mpctx->demuxer=demux_open(opts, mpctx->stream,mpctx->file_format,opts->audio_id,opts->video_id,opts->sub_id,mpctx->filename);

// HACK to get MOV Reference Files working

if (mpctx->demuxer && mpctx->demuxer->type==DEMUXER_TYPE_PLAYLIST)
{
  unsigned char* playlist_entry;
  play_tree_t *list = NULL, *entry = NULL;

  current_module="handle_demux_playlist";
  while (ds_get_packet(mpctx->demuxer->video,&playlist_entry)>0)
  {
    char *temp, *bname;

    mp_msg(MSGT_CPLAYER,MSGL_V,"Adding file %s to element entry.\n",
	    filename_recode(playlist_entry));

    bname=mp_basename(playlist_entry);
    if ((strlen(bname)>10) && !strncmp(bname,"qt",2) && !strncmp(bname+3,"gateQT",6))
        continue;

    if (!strcmp(playlist_entry, mpctx->filename)) // ignoring self-reference
        continue;

    entry = play_tree_new();

    if (mpctx->filename && !strcmp(mp_basename(playlist_entry),playlist_entry)) // add reference path of current file
    {
      temp=malloc((strlen(mpctx->filename)-strlen(mp_basename(mpctx->filename))+strlen(playlist_entry)+1));
      if (temp)
      {
	strncpy(temp, mpctx->filename, strlen(mpctx->filename)-strlen(mp_basename(mpctx->filename)));
	temp[strlen(mpctx->filename)-strlen(mp_basename(mpctx->filename))]='\0';
	strcat(temp, playlist_entry);
	if (!strcmp(temp, mpctx->filename)) {
	  free(temp);
	  continue;
	}
	play_tree_add_file(entry,temp);
	mp_msg(MSGT_CPLAYER,MSGL_V,"Resolving reference to %s.\n",temp);
	free(temp);
      }
    }
    else
      play_tree_add_file(entry,playlist_entry);

    if(!list)
      list = entry;
    else
      play_tree_append_entry(list,entry);
  }
  free_demuxer(mpctx->demuxer);
  mpctx->demuxer = NULL;

  if (list)
  {
    entry = play_tree_new();
    play_tree_set_child(entry,list);
    mpctx->stop_play = playtree_add_playlist(mpctx, entry);
    goto goto_next_file;
  }
}

if(!mpctx->demuxer)
  goto goto_next_file;

 if (mpctx->demuxer->matroska_data.ordered_chapters)
     build_ordered_chapter_timeline(mpctx);

 if (!mpctx->sources) {
     mpctx->sources = talloc_ptrtype(NULL, mpctx->sources);
     *mpctx->sources = (struct content_source){.stream = mpctx->stream,
                                               .demuxer = mpctx->demuxer};
     mpctx->num_sources = 1;
 }

if(dvd_chapter>1) {
  double pts;
  if (seek_chapter(mpctx, dvd_chapter-1, &pts, NULL) >= 0 && pts > -1.0)
    seek(mpctx, pts, SEEK_ABSOLUTE);
}

mpctx->initialized_flags|=INITIALIZED_DEMUXER;

if (mpctx->stream->type != STREAMTYPE_DVD && mpctx->stream->type != STREAMTYPE_DVDNAV) {
  int i;
  int maxid = -1;
  // setup global sub numbering
  mpctx->global_sub_indices[SUB_SOURCE_DEMUX] = mpctx->global_sub_size; // the global # of the first demux-specific sub.
  for (i = 0; i < MAX_S_STREAMS; i++)
    if (mpctx->demuxer->s_streams[i])
      maxid = FFMAX(maxid, mpctx->demuxer->s_streams[i]->sid);
  mpctx->global_sub_size += maxid + 1;
}
// Make opts->sub_id always selectable if set.
if (mpctx->global_sub_size <= mpctx->global_sub_indices[SUB_SOURCE_DEMUX] + opts->sub_id)
  mpctx->global_sub_size = mpctx->global_sub_indices[SUB_SOURCE_DEMUX] + opts->sub_id + 1;

#ifdef CONFIG_ASS
if (opts->ass_enabled && ass_library) {
    for (int j = 0; j < mpctx->num_sources; j++) {
        struct demuxer *d = mpctx->sources[j].demuxer;
        for (int i = 0; i < d->num_attachments; i++) {
            struct demux_attachment *att = d->attachments + i;
            if (extract_embedded_fonts
                && att->name && att->type && att->data && att->data_size
                && (strcmp(att->type, "application/x-truetype-font") == 0
                    || strcmp(att->type, "application/x-font") == 0))
                ass_add_font(ass_library, att->name, att->data, att->data_size);
        }
    }
}
#endif

current_module="demux_open2";

//file_format=demuxer->file_format;

mpctx->d_audio=mpctx->demuxer->audio;
mpctx->d_video=mpctx->demuxer->video;
mpctx->d_sub=mpctx->demuxer->sub;

if (ts_prog) {
  int tmp = ts_prog;
  mp_property_do("switch_program", M_PROPERTY_SET, &tmp, mpctx);
}
// select audio stream
select_audio(mpctx->demuxer, opts->audio_id, audio_lang);

// DUMP STREAMS:
if((stream_dump_type)&&(stream_dump_type<4)){
  FILE *f;
  demux_stream_t *ds=NULL;
  current_module="dump";
  // select stream to dump
  switch(stream_dump_type){
  case 1: ds=mpctx->d_audio;break;
  case 2: ds=mpctx->d_video;break;
  case 3: ds=mpctx->d_sub;break;
  }
  if(!ds){
      mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"dump: FATAL: Selected stream missing!\n");
      exit_player(mpctx, EXIT_ERROR);
  }
  // disable other streams:
  if(mpctx->d_audio && mpctx->d_audio!=ds) {ds_free_packs(mpctx->d_audio); mpctx->d_audio->id=-2; }
  if(mpctx->d_video && mpctx->d_video!=ds) {ds_free_packs(mpctx->d_video); mpctx->d_video->id=-2; }
  if(mpctx->d_sub && mpctx->d_sub!=ds) {ds_free_packs(mpctx->d_sub); mpctx->d_sub->id=-2; }
  // let's dump it!
  f=fopen(stream_dump_name,"wb");
  if(!f){
    mp_tmsg(MSGT_CPLAYER,MSGL_FATAL,"Cannot open dump file.\n");
    exit_player(mpctx, EXIT_ERROR);
  }
  while(!ds->eof){
    unsigned char* start;
    int in_size=ds_get_packet(ds,&start);
    if( (mpctx->demuxer->file_format==DEMUXER_TYPE_AVI || mpctx->demuxer->file_format==DEMUXER_TYPE_ASF || mpctx->demuxer->file_format==DEMUXER_TYPE_MOV)
	&& stream_dump_type==2) fwrite(&in_size,1,4,f);
    if(in_size>0) fwrite(start,in_size,1,f);
    if(dvd_last_chapter>0) {
      int cur_chapter = demuxer_get_current_chapter(mpctx->demuxer);
      if(cur_chapter!=-1 && cur_chapter+1>dvd_last_chapter)
        break;
    }
  }
  fclose(f);
  mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Core dumped ;)\n");
  exit_player_with_rc(mpctx, EXIT_EOF, 0);
}

mpctx->sh_audio=mpctx->d_audio->sh;
mpctx->sh_video=mpctx->d_video->sh;

if(mpctx->sh_video){

  current_module="video_read_properties";
  if(!video_read_properties(mpctx->sh_video)) {
    mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"Video: Cannot read properties.\n");
    mpctx->sh_video=mpctx->d_video->sh=NULL;
  } else {
    mp_tmsg(MSGT_CPLAYER,MSGL_V,"[V] filefmt:%d fourcc:0x%X size:%dx%d fps:%5.3f ftime:=%6.4f\n",
	   mpctx->demuxer->file_format,mpctx->sh_video->format, mpctx->sh_video->disp_w,mpctx->sh_video->disp_h,
	   mpctx->sh_video->fps,mpctx->sh_video->frametime
	   );

    /* need to set fps here for output encoders to pick it up in their init */
    if(force_fps){
      mpctx->sh_video->fps=force_fps;
      mpctx->sh_video->frametime=1.0f/mpctx->sh_video->fps;
    }
    vo_fps = mpctx->sh_video->fps;

    if(!mpctx->sh_video->fps && !force_fps){
      mp_tmsg(MSGT_CPLAYER,MSGL_ERR,"FPS not specified in the header or invalid, use the -fps option.\n");
      mpctx->sh_video=mpctx->d_video->sh=NULL;
    }
  }

}

if(!mpctx->sh_video && !mpctx->sh_audio){
    mp_tmsg(MSGT_CPLAYER,MSGL_FATAL, "No stream found.\n");
#ifdef CONFIG_DVBIN
	if(mpctx->stream->type == STREAMTYPE_DVB)
	{
		int dir;
		int v = mpctx->last_dvb_step;
		if(v > 0)
			dir = DVB_CHANNEL_HIGHER;
		else
			dir = DVB_CHANNEL_LOWER;

		if(dvb_step_channel(mpctx->stream, dir)) {
                    mpctx->stop_play = PT_NEXT_ENTRY;
                    mpctx->dvbin_reopen = 1;
                }
	}
#endif
    goto goto_next_file; // exit_player(_("Fatal error"));
}

/* display clip info */
demux_info_print(mpctx->demuxer);

//================== Read SUBTITLES (DVD & TEXT) ==========================
if(vo_spudec==NULL && mpctx->sh_video &&
     (mpctx->stream->type==STREAMTYPE_DVD || mpctx->stream->type == STREAMTYPE_DVDNAV)){
  init_vo_spudec(mpctx);
}

// after reading video params we should load subtitles because
// we know fps so now we can adjust subtitle time to ~6 seconds AST
// check .sub
  current_module="read_subtitles_file";
  double sub_fps = mpctx->sh_video ? mpctx->sh_video->fps : 25;
  if(sub_name){
    for (i = 0; sub_name[i] != NULL; ++i)
        add_subtitles(mpctx, sub_name[i], sub_fps, 0);
  }
  if(sub_auto) { // auto load sub file ...
    char *psub = get_path( "sub/" );
    char **tmp = sub_filenames((psub ? psub : ""), mpctx->filename);
    int i = 0;
    free(psub); // release the buffer created by get_path() above
    while (tmp[i]) {
        add_subtitles(mpctx, tmp[i], sub_fps, 1);
        free(tmp[i++]);
    }
    free(tmp);
  }
  if (mpctx->set_of_sub_size > 0)  {
      // setup global sub numbering
      mpctx->global_sub_indices[SUB_SOURCE_SUBS] = mpctx->global_sub_size; // the global # of the first sub.
      mpctx->global_sub_size += mpctx->set_of_sub_size;
  }


if (mpctx->global_sub_size) {
  select_subtitle(mpctx);
  if(subdata)
    switch (stream_dump_type) {
        case 3: list_sub_file(subdata); break;
        case 4: dump_mpsub(subdata, mpctx->sh_video->fps); break;
        case 6: dump_srt(subdata, mpctx->sh_video->fps); break;
        case 7: dump_microdvd(subdata, mpctx->sh_video->fps); break;
        case 8: dump_jacosub(subdata, mpctx->sh_video->fps); break;
        case 9: dump_sami(subdata, mpctx->sh_video->fps); break;
    }
}

  mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_FILENAME=%s\n",
	  filename_recode(mpctx->filename));
  mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_DEMUXER=%s\n", mpctx->demuxer->desc->name);
  if (mpctx->sh_video) {
    /* Assume FOURCC if all bytes >= 0x20 (' ') */
    if (mpctx->sh_video->format >= 0x20202020)
	mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_FORMAT=%.4s\n", (char *)&mpctx->sh_video->format);
    else
	mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_FORMAT=0x%08X\n", mpctx->sh_video->format);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_BITRATE=%d\n", mpctx->sh_video->i_bps*8);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_WIDTH=%d\n", mpctx->sh_video->disp_w);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_HEIGHT=%d\n", mpctx->sh_video->disp_h);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_FPS=%5.3f\n", mpctx->sh_video->fps);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_VIDEO_ASPECT=%1.4f\n", mpctx->sh_video->aspect);
  }
  if (mpctx->sh_audio) {
    /* Assume FOURCC if all bytes >= 0x20 (' ') */
    if (mpctx->sh_audio->format >= 0x20202020)
      mp_msg(MSGT_IDENTIFY,MSGL_INFO, "ID_AUDIO_FORMAT=%.4s\n", (char *)&mpctx->sh_audio->format);
    else
      mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_AUDIO_FORMAT=%d\n", mpctx->sh_audio->format);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_AUDIO_BITRATE=%d\n", mpctx->sh_audio->i_bps*8);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_AUDIO_RATE=%d\n", mpctx->sh_audio->samplerate);
    mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_AUDIO_NCH=%d\n", mpctx->sh_audio->channels);
  }
  mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_LENGTH=%.2f\n", mpctx->timeline ?
         mpctx->timeline[mpctx->num_timeline_parts].start :
         demuxer_get_time_length(mpctx->demuxer));
  mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_SEEKABLE=%d\n",
         mpctx->stream->seek && (!mpctx->demuxer || mpctx->demuxer->seekable));
  if (mpctx->demuxer) {
      if (mpctx->demuxer->num_chapters == 0)
          stream_control(mpctx->demuxer->stream, STREAM_CTRL_GET_NUM_CHAPTERS, &mpctx->demuxer->num_chapters);
      mp_msg(MSGT_IDENTIFY,MSGL_INFO,"ID_CHAPTERS=%d\n", mpctx->demuxer->num_chapters);
  }

if(!mpctx->sh_video) goto main; // audio-only

if(!reinit_video_chain(mpctx)) {
  if(!mpctx->sh_video){
    if(!mpctx->sh_audio) goto goto_next_file;
    goto main; // exit_player(_("Fatal error"));
  }
}

   if(mpctx->sh_video->output_flags & 0x08 && vo_spudec)
      spudec_set_hw_spu(vo_spudec,mpctx->video_out);

#ifdef CONFIG_FREETYPE
   force_load_font = 1;
#endif

//================== MAIN: ==========================
main:
current_module="main";

    if(playing_msg) {
        char* msg = property_expand_string(mpctx, playing_msg);
        mp_msg(MSGT_CPLAYER,MSGL_INFO,"%s",msg);
        free(msg);
    }


// Disable the term OSD in verbose mode
if(verbose) term_osd = 0;

{
//int frame_corr_num=0;   //
//float v_frame=0;    // Video
//float num_frames=0;      // number of frames played

int frame_time_remaining=0; // flag
int blit_frame=0;

// Make sure old OSD does not stay around,
// e.g. with -fixed-vo and same-resolution files
clear_osd_msgs();
update_osd_msg(mpctx);

//================ SETUP AUDIO ==========================

if(mpctx->sh_audio){
  reinit_audio_chain(mpctx);
  if (mpctx->sh_audio && mpctx->sh_audio->codec)
    mp_msg(MSGT_IDENTIFY,MSGL_INFO, "ID_AUDIO_CODEC=%s\n", mpctx->sh_audio->codec->name);
}

current_module="av_init";

if(mpctx->sh_video){
  mpctx->sh_video->timer=0;
  if (! ignore_start)
    audio_delay += mpctx->sh_video->stream_delay;
}
if(mpctx->sh_audio){
  if (start_volume >= 0)
    mixer_setvolume(&mpctx->mixer, start_volume, start_volume);
  if (! ignore_start)
    audio_delay -= mpctx->sh_audio->stream_delay;
  mpctx->delay=-audio_delay;
}

if(!mpctx->sh_audio){
  mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Audio: no sound\n");
  mp_msg(MSGT_CPLAYER,MSGL_V,"Freeing %d unused audio chunks.\n",mpctx->d_audio->packs);
  ds_free_packs(mpctx->d_audio); // free buffered chunks
  //mpctx->d_audio->id=-2;         // do not read audio chunks
  //uninit_player(mpctx, INITIALIZED_AO); // close device
}
if(!mpctx->sh_video){
   mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Video: no video\n");
   mp_msg(MSGT_CPLAYER,MSGL_V,"Freeing %d unused video chunks.\n",mpctx->d_video->packs);
   ds_free_packs(mpctx->d_video);
   mpctx->d_video->id=-2;
   //if(!fixed_vo) uninit_player(mpctx, INITIALIZED_VO);
}

if (!mpctx->sh_video && !mpctx->sh_audio)
    goto goto_next_file;

//if(demuxer->file_format!=DEMUXER_TYPE_AVI) pts_from_bps=0; // it must be 0 for mpeg/asf!
if(force_fps && mpctx->sh_video){
  vo_fps = mpctx->sh_video->fps=force_fps;
  mpctx->sh_video->frametime=1.0f/mpctx->sh_video->fps;
  mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"FPS forced to be %5.3f (ftime: %5.3f).\n",mpctx->sh_video->fps,mpctx->sh_video->frametime);
}

 mp_input_set_section(mpctx->input, NULL);
//TODO: add desired (stream-based) sections here
 if (mpctx->stream->type==STREAMTYPE_TV) mp_input_set_section(mpctx->input, "tv");
 if (mpctx->stream->type==STREAMTYPE_DVDNAV) mp_input_set_section(mpctx->input, "dvdnav");

//==================== START PLAYING =======================

if(opts->loop_times>1) opts->loop_times--; else
if(opts->loop_times==1) opts->loop_times = -1;

mp_tmsg(MSGT_CPLAYER,MSGL_INFO,"Starting playback...\n");

total_time_usage_start=GetTimer();
audio_time_usage=0; video_time_usage=0; vout_time_usage=0;
total_frame_cnt=0; drop_frame_cnt=0; // fix for multifile fps benchmark
play_n_frames=play_n_frames_mf;

if(play_n_frames==0){
  mpctx->stop_play=PT_NEXT_ENTRY; goto goto_next_file;
}

// If there's a timeline force an absolute seek to initialize state
if (seek_to_sec || mpctx->timeline) {
    seek(mpctx, seek_to_sec, SEEK_ABSOLUTE);
    end_at.pos += seek_to_sec;
}

if (end_at.type == END_AT_SIZE) {
    mp_tmsg(MSGT_CPLAYER, MSGL_WARN, "Option -endpos in MPlayer does not yet support size units.\n");
    end_at.type = END_AT_NONE;
}

#ifdef CONFIG_DVDNAV
mp_dvdnav_context_free(mpctx);
if (mpctx->stream->type == STREAMTYPE_DVDNAV) {
    mp_dvdnav_read_wait(mpctx->stream, 0, 1);
    mp_dvdnav_cell_has_changed(mpctx->stream,1);
}
#endif

 get_relative_time(mpctx); // reset current delta
 mpctx->time_frame = 0;
 mpctx->drop_message_shown = 0;
 mpctx->update_video_immediately = true;
 mpctx->total_avsync_change = 0;
 // Make sure VO knows current pause state
 if (mpctx->sh_video)
     vo_control(mpctx->video_out, mpctx->paused ? VOCTRL_PAUSE : VOCTRL_RESUME,
                NULL);

while(!mpctx->stop_play){
    float aq_sleep_time=0;

if(dvd_last_chapter>0) {
  int cur_chapter = get_current_chapter(mpctx);
  if(cur_chapter!=-1 && cur_chapter+1>dvd_last_chapter)
    goto goto_next_file;
}

if(!mpctx->sh_audio && mpctx->d_audio->sh) {
  mpctx->sh_audio = mpctx->d_audio->sh;
  mpctx->sh_audio->ds = mpctx->d_audio;
  reinit_audio_chain(mpctx);
}

/*========================== PLAY AUDIO ============================*/

if (mpctx->sh_audio && !mpctx->paused)
    if (!fill_audio_out_buffers(mpctx))
	// at eof, all audio at least written to ao
	if (!mpctx->sh_video)
	    mpctx->stop_play = AT_END_OF_FILE;


if(!mpctx->sh_video) {
  // handle audio-only case:
  double a_pos=0;
  // sh_audio can be NULL due to video stream switching
  // TODO: handle this better
  if (mpctx->sh_audio)
    a_pos = playing_audio_pts(mpctx);

  print_status(mpctx, a_pos, false);

  if(end_at.type == END_AT_TIME && end_at.pos < a_pos)
    mpctx->stop_play = PT_NEXT_ENTRY;
  update_subtitles(mpctx, &mpctx->opts, NULL, a_pos, mpctx->video_offset,
                   mpctx->d_sub, 0);
  update_osd_msg(mpctx);

} else {

/*========================== PLAY VIDEO ============================*/

  vo_pts=mpctx->sh_video->timer*90000.0;
  vo_fps=mpctx->sh_video->fps;

  blit_frame = mpctx->video_out->frame_loaded;
  if (!blit_frame) {
      double frame_time = update_video(mpctx);
      blit_frame = mpctx->video_out->frame_loaded;
      mp_dbg(MSGT_AVSYNC,MSGL_DBG2,"*** ftime=%5.3f ***\n",frame_time);
      if (mpctx->sh_video->vf_initialized < 0) {
	  mp_tmsg(MSGT_CPLAYER,MSGL_FATAL, "\nFATAL: Could not initialize video filters (-vf) or video output (-vo).\n");
	  mpctx->stop_play = PT_NEXT_ENTRY;
          goto goto_next_file;
      }
      if (blit_frame) {
          struct sh_video *sh_video = mpctx->sh_video;
          update_subtitles(mpctx, &mpctx->opts, sh_video, sh_video->pts,
                           mpctx->video_offset, mpctx->d_sub, 0);
          update_teletext(sh_video, mpctx->demuxer, 0);
          update_osd_msg(mpctx);
          struct vf_instance *vf = mpctx->sh_video->vfilter;
          vf->control(vf, VFCTRL_DRAW_EOSD, NULL);
          vf->control(vf, VFCTRL_DRAW_OSD, mpctx->osd);
          vo_osd_changed(0);
      }
      if (frame_time < 0)
	  mpctx->stop_play = AT_END_OF_FILE;
      else {
          if (mpctx->update_video_immediately) {
              // Show this frame immediately, rest normally
              mpctx->update_video_immediately = false;
          } else {
              mpctx->time_frame += frame_time / opts->playback_speed;
              adjust_sync(mpctx, frame_time);
          }
      }
  }
  if (mpctx->timeline) {
      struct timeline_part *next = mpctx->timeline + mpctx->timeline_part + 1;
      if (mpctx->sh_video->pts >= next->start
          || mpctx->stop_play == AT_END_OF_FILE
          && mpctx->timeline_part + 1 < mpctx->num_timeline_parts) {
          seek(mpctx, next->start, SEEK_ABSOLUTE);
          continue;
      }
  }

// ==========================================================================

//    current_module="draw_osd";
//    if(vo_config_count) mpctx->video_out->draw_osd();

    current_module="vo_check_events";
    vo_check_events(mpctx->video_out);

#ifdef CONFIG_X11
    if (stop_xscreensaver) {
	current_module = "stop_xscreensaver";
	xscreensaver_heartbeat(mpctx->x11_state);
    }
#endif
    if (heartbeat_cmd) {
        static unsigned last_heartbeat;
        unsigned now = GetTimerMS();
        if (now - last_heartbeat > 30000) {
            last_heartbeat = now;
            system(heartbeat_cmd);
        }
    }

    frame_time_remaining = sleep_until_update(mpctx, &mpctx->time_frame, &aq_sleep_time);

//====================== FLIP PAGE (VIDEO BLT): =========================

        current_module="flip_page";
        if (!frame_time_remaining && blit_frame) {
	   unsigned int t2=GetTimer();
           unsigned int pts_us = mpctx->last_time + mpctx->time_frame * 1e6;
           int duration = -1;
           double pts2 = mpctx->video_out->next_pts2;
           if (pts2 != MP_NOPTS_VALUE && opts->correct_pts) {
               // expected A/V sync correction is ignored
               double diff = (pts2 - mpctx->sh_video->pts);
               diff /= opts->playback_speed;
               if (mpctx->time_frame < 0)
                   diff += mpctx->time_frame;
               if (diff < 0)
                   diff = 0;
               if (diff > 10)
                   diff = 10;
               duration = diff * 1e6;
           }
           vo_flip_page(mpctx->video_out, pts_us|1, duration);

           mpctx->last_vo_flip_duration = (GetTimer() - t2) * 0.000001;
           vout_time_usage += mpctx->last_vo_flip_duration;
           if (mpctx->video_out->driver->flip_page_timed) {
               // No need to adjust sync based on flip speed
               mpctx->last_vo_flip_duration = 0;
               // For print_status - VO call finishing early is OK for sync
               mpctx->time_frame -= get_relative_time(mpctx);
           }
           print_status(mpctx, MP_NOPTS_VALUE, true);
        }
        else
            print_status(mpctx, MP_NOPTS_VALUE, false);

//============================ Auto QUALITY ============================

/*Output quality adjustments:*/
if(auto_quality>0){
  current_module="autoq";
//  float total=0.000001f * (GetTimer()-aq_total_time);
//  if(output_quality<auto_quality && aq_sleep_time>0.05f*total)
  if(output_quality<auto_quality && aq_sleep_time>0)
      ++output_quality;
  else
//  if(output_quality>0 && aq_sleep_time<-0.05f*total)
  if(output_quality>1 && aq_sleep_time<0)
      --output_quality;
  else
  if(output_quality>0 && aq_sleep_time<-0.050f) // 50ms
      output_quality=0;
//  printf("total: %8.6f  sleep: %8.6f  q: %d\n",(0.000001f*aq_total_time),aq_sleep_time,output_quality);
  set_video_quality(mpctx->sh_video,output_quality);
}

 if (!frame_time_remaining && blit_frame) {
     if (play_n_frames >= 0) {
         --play_n_frames;
         if (play_n_frames <= 0)
             mpctx->stop_play = PT_NEXT_ENTRY;
     }
     if (mpctx->step_frames > 0) {
         mpctx->step_frames--;
         if (mpctx->step_frames == 0)
             pause_player(mpctx);
     }
 }


// FIXME: add size based support for -endpos
 if (end_at.type == END_AT_TIME &&
         !frame_time_remaining && end_at.pos <= mpctx->sh_video->pts)
     mpctx->stop_play = PT_NEXT_ENTRY;

} // end if(mpctx->sh_video)

#ifdef CONFIG_DVDNAV
 if (mpctx->stream->type == STREAMTYPE_DVDNAV) {
   nav_highlight_t hl;
   mp_dvdnav_get_highlight (mpctx->stream, &hl);
   osd_set_nav_box (hl.sx, hl.sy, hl.ex, hl.ey);
   vo_osd_changed (OSDTYPE_DVDNAV);

   if (mp_dvdnav_stream_has_changed(mpctx->stream)) {
     double ar = -1.0;
     if (stream_control (mpctx->demuxer->stream,
                         STREAM_CTRL_GET_ASPECT_RATIO, &ar)
         != STREAM_UNSUPPORTED)
       mpctx->sh_video->stream_aspect = ar;
   }
 }
#endif

//================= Keyboard events, SEEKing ====================

  current_module="key_events";

{
  while (1) {
  mp_cmd_t* cmd;
  while ((cmd = mp_input_get_cmd(mpctx->input, 0,0,0)) != NULL) {
      run_command(mpctx, cmd);
      mp_cmd_free(cmd);
      if (mpctx->stop_play)
          break;
  }
  if (!mpctx->paused || mpctx->stop_play || mpctx->rel_seek_secs
      || mpctx->abs_seek_pos)
      break;
  if (mpctx->sh_video) {
      update_osd_msg(mpctx);
      int hack = vo_osd_changed(0);
      vo_osd_changed(hack);
      if (hack)
          if (redraw_osd(mpctx->sh_video, mpctx->osd) < 0) {
              add_step_frame(mpctx);
              break;
          }
          else
              vo_osd_changed(0);
  }
  pause_loop(mpctx);
  }
}

// handle -sstep
if (step_sec > 0 && !mpctx->paused) {
	mpctx->osd_function=OSD_FFW;
	mpctx->rel_seek_secs+=step_sec;
}

 edl_update(mpctx);

  /* Looping. */
  if(mpctx->stop_play==AT_END_OF_FILE && opts->loop_times>=0) {
    mp_msg(MSGT_CPLAYER,MSGL_V,"loop_times = %d\n", opts->loop_times);

    if(opts->loop_times>1) opts->loop_times--; else
    if(opts->loop_times==1) opts->loop_times=-1;
    play_n_frames=play_n_frames_mf;
    mpctx->stop_play=0;
    mpctx->abs_seek_pos=SEEK_ABSOLUTE; mpctx->rel_seek_secs=seek_to_sec;
  }

if(mpctx->rel_seek_secs || mpctx->abs_seek_pos){
    seek(mpctx, mpctx->rel_seek_secs, mpctx->abs_seek_pos);

  mpctx->rel_seek_secs=0;
  mpctx->abs_seek_pos=0;
}

} // while(!mpctx->stop_play)

mp_msg(MSGT_GLOBAL,MSGL_V,"EOF code: %d  \n",mpctx->stop_play);

#ifdef CONFIG_DVBIN
if(mpctx->dvbin_reopen)
{
  mpctx->stop_play = 0;
  uninit_player(mpctx, INITIALIZED_ALL-(INITIALIZED_STREAM|INITIALIZED_GETCH2|(opts->fixed_vo?INITIALIZED_VO:0)));
  cache_uninit(mpctx->stream);
  mpctx->dvbin_reopen = 0;
  goto goto_enable_cache;
}
#endif
}

goto_next_file:  // don't jump here after ao/vo/getch initialization!

mp_msg(MSGT_CPLAYER,MSGL_INFO,"\n");

if(benchmark){
    double tot=video_time_usage+vout_time_usage+audio_time_usage;
    double total_time_usage;
    total_time_usage_start=GetTimer()-total_time_usage_start;
    total_time_usage = (float)total_time_usage_start*0.000001;
    mp_msg(MSGT_CPLAYER,MSGL_INFO,"\nBENCHMARKs: VC:%8.3fs VO:%8.3fs A:%8.3fs Sys:%8.3fs = %8.3fs\n",
           video_time_usage,vout_time_usage,audio_time_usage,
           total_time_usage-tot,total_time_usage);
    if(total_time_usage>0.0)
        mp_msg(MSGT_CPLAYER,MSGL_INFO,"BENCHMARK%%: VC:%8.4f%% VO:%8.4f%% A:%8.4f%% Sys:%8.4f%% = %8.4f%%\n",
               100.0*video_time_usage/total_time_usage,
               100.0*vout_time_usage/total_time_usage,
               100.0*audio_time_usage/total_time_usage,
               100.0*(total_time_usage-tot)/total_time_usage,
               100.0);
    if(total_frame_cnt && frame_dropping)
        mp_msg(MSGT_CPLAYER,MSGL_INFO,"BENCHMARKn: disp: %d (%3.2f fps)  drop: %d (%d%%)  total: %d (%3.2f fps)\n",
               total_frame_cnt-drop_frame_cnt,
               (total_time_usage>0.5)?((total_frame_cnt-drop_frame_cnt)/total_time_usage):0,
               drop_frame_cnt,
               100*drop_frame_cnt/total_frame_cnt,
               total_frame_cnt,
               (total_time_usage>0.5)?(total_frame_cnt/total_time_usage):0);
}

// time to uninit all, except global stuff:
uninit_player(mpctx, INITIALIZED_ALL-(opts->fixed_vo?INITIALIZED_VO:0));

if(mpctx->set_of_sub_size > 0) {
    current_module="sub_free";
    for(i = 0; i < mpctx->set_of_sub_size; ++i) {
        sub_free(mpctx->set_of_subtitles[i]);
#ifdef CONFIG_ASS
        if(mpctx->set_of_ass_tracks[i])
            ass_free_track( mpctx->set_of_ass_tracks[i] );
#endif
    }
    mpctx->set_of_sub_size = 0;
}
vo_sub_last = vo_sub=NULL;
subdata=NULL;
#ifdef CONFIG_ASS
ass_track = NULL;
if(ass_library)
    ass_clear_fonts(ass_library);
#endif

 if (!mpctx->stop_play)  // In case some goto jumped here...
     mpctx->stop_play = PT_NEXT_ENTRY;

int playtree_direction = 1;

if(mpctx->stop_play == PT_NEXT_ENTRY || mpctx->stop_play == PT_PREV_ENTRY) {
    if(play_tree_iter_step(mpctx->playtree_iter,mpctx->play_tree_step,0) != PLAY_TREE_ITER_ENTRY) {
        play_tree_iter_free(mpctx->playtree_iter);
        mpctx->playtree_iter = NULL;
    }
    mpctx->play_tree_step = 1;
} else if(mpctx->stop_play == PT_UP_NEXT || mpctx->stop_play == PT_UP_PREV) {
    int direction = mpctx->stop_play == PT_UP_NEXT ? 1 : -1;
    if(mpctx->playtree_iter) {
        if(play_tree_iter_up_step(mpctx->playtree_iter,direction,0) != PLAY_TREE_ITER_ENTRY) {
            play_tree_iter_free(mpctx->playtree_iter);
            mpctx->playtree_iter = NULL;
        }
    }
} else if (mpctx->stop_play == PT_STOP) {
    play_tree_iter_free(mpctx->playtree_iter);
    mpctx->playtree_iter = NULL;
} else { // NEXT PREV SRC
    playtree_direction = mpctx->stop_play == PT_PREV_SRC ? -1 : 1;
}

while(mpctx->playtree_iter != NULL) {
    mpctx->filename = play_tree_iter_get_file(mpctx->playtree_iter, playtree_direction);
    if(mpctx->filename == NULL) {
        if(play_tree_iter_step(mpctx->playtree_iter, playtree_direction, 0) != PLAY_TREE_ITER_ENTRY) {
            play_tree_iter_free(mpctx->playtree_iter);
            mpctx->playtree_iter = NULL;
        };
    } else
        break;
}

if(mpctx->playtree_iter != NULL || player_idle_mode){
    if(!mpctx->playtree_iter) mpctx->filename = NULL;
    mpctx->stop_play = 0;
    goto play_next_file;
}


exit_player_with_rc(mpctx, EXIT_EOF, 0);

return 1;
}
#endif /* DISABLE_MAIN */
