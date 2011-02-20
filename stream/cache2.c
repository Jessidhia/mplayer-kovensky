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

#include "config.h"

// Initial draft of my new cache system...
// Note it runs in 2 processes (using fork()), but doesn't require locking!!
// TODO: seeking, data consistency checking

#define READ_SLEEP_TIME 10
// These defines are used to reduce the cost of many successive
// seeks (e.g. when a file has no index) by spinning quickly at first.
#define INITIAL_FILL_USLEEP_TIME 1000
#define INITIAL_FILL_USLEEP_COUNT 10
#define FILL_USLEEP_TIME 50000
#define PREFILL_SLEEP_TIME 200
#define CONTROL_SLEEP_TIME 0

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "libavutil/avutil.h"
#include "osdep/shmem.h"
#include "osdep/timer.h"
#if defined(__MINGW32__)
#include <windows.h>
static void ThreadProc( void *s );
#elif defined(__OS2__)
#define INCL_DOS
#include <os2.h>
static void ThreadProc( void *s );
#elif defined(PTHREAD_CACHE)
#include <pthread.h>
static void *ThreadProc(void *s);
#else
#include <sys/wait.h>
#define FORKED_CACHE 1
#endif
#ifndef FORKED_CACHE
#define FORKED_CACHE 0
#endif

#include "mp_msg.h"

#include "stream.h"
#include "cache2.h"

typedef struct {
  // constats:
  unsigned char *buffer;      // base pointer of the allocated buffer memory
  int buffer_size; // size of the allocated buffer memory
  int sector_size; // size of a single sector (2048/2324)
  int back_size;   // we should keep back_size amount of old bytes for backward seek
  int fill_limit;  // we should fill buffer only if space>=fill_limit
  int seek_limit;  // keep filling cache if distance is less that seek limit
  // filler's pointers:
  int eof;
  off_t min_filepos; // buffer contain only a part of the file, from min-max pos
  off_t max_filepos;
  off_t offset;      // filepos <-> bufferpos  offset value (filepos of the buffer's first byte)
  // reader's pointers:
  off_t read_filepos;
  // commands/locking:
//  int seek_lock;   // 1 if we will seek/reset buffer, 2 if we are ready for cmd
//  int fifo_flag;  // 1 if we should use FIFO to notice cache about buffer reads.
  // callback
  stream_t* stream;
  volatile int control;
  volatile unsigned control_uint_arg;
  volatile double control_double_arg;
  volatile int control_res;
  volatile off_t control_new_pos;
  volatile double stream_time_length;
} cache_vars_t;

static int min_fill=0;

static void cache_wakeup(stream_t *s)
{
#if FORKED_CACHE
  // signal process to wake up immediately
  kill(s->cache_pid, SIGUSR1);
#endif
}

static int cache_read(cache_vars_t *s, unsigned char *buf, int size)
{
  int total=0;
  int sleep_count = 0;
  int last_max = s->max_filepos;
  while(size>0){
    int pos,newb,len;

  //printf("CACHE2_READ: 0x%X <= 0x%X <= 0x%X  \n",s->min_filepos,s->read_filepos,s->max_filepos);

    if(s->read_filepos>=s->max_filepos || s->read_filepos<s->min_filepos){
	// eof?
	if(s->eof) break;
	if (s->max_filepos == last_max) {
	    if (sleep_count++ == 10)
	        mp_msg(MSGT_CACHE, MSGL_WARN, "Cache not filling, consider increasing -cache and/or -cache-min!\n");
	} else {
	    last_max = s->max_filepos;
	    sleep_count = 0;
	}
	// waiting for buffer fill...
	if (stream_check_interrupt(READ_SLEEP_TIME)) {
	    s->eof = 1;
	    break;
	}
	continue; // try again...
    }
    sleep_count = 0;

    newb=s->max_filepos-s->read_filepos; // new bytes in the buffer
    if(newb<min_fill) min_fill=newb; // statistics...

//    printf("*** newb: %d bytes ***\n",newb);

    pos=s->read_filepos - s->offset;
    if(pos<0) pos+=s->buffer_size; else
    if(pos>=s->buffer_size) pos-=s->buffer_size;

    if(newb>s->buffer_size-pos) newb=s->buffer_size-pos; // handle wrap...
    if(newb>size) newb=size;

    // check:
    if(s->read_filepos<s->min_filepos) mp_msg(MSGT_CACHE,MSGL_ERR,"Ehh. s->read_filepos<s->min_filepos !!! Report bug...\n");

    // len=write(mem,newb)
    //printf("Buffer read: %d bytes\n",newb);
    memcpy(buf,&s->buffer[pos],newb);
    buf+=newb;
    len=newb;
    // ...

    s->read_filepos+=len;
    size-=len;
    total+=len;

  }
  return total;
}

static int cache_fill(cache_vars_t *s)
{
  int back,back2,newb,space,len,pos;
  off_t read=s->read_filepos;
  int read_chunk;

  if(read<s->min_filepos || read>s->max_filepos){
      // seek...
      mp_msg(MSGT_CACHE,MSGL_DBG2,"Out of boundaries... seeking to 0x%"PRIX64"  \n",(int64_t)read);
      // drop cache contents only if seeking backward or too much fwd.
      // This is also done for on-disk files, since it loses the backseek cache.
      // That in turn can cause major bandwidth increase and performance
      // issues with e.g. mov or badly interleaved files
      if(read<s->min_filepos || read>=s->max_filepos+s->seek_limit)
      {
        s->offset= // FIXME!?
        s->min_filepos=s->max_filepos=read; // drop cache content :(
        if(s->stream->eof) stream_reset(s->stream);
        stream_seek_internal(s->stream,read);
        mp_msg(MSGT_CACHE,MSGL_DBG2,"Seek done. new pos: 0x%"PRIX64"  \n",(int64_t)stream_tell(s->stream));
      }
  }

  // calc number of back-bytes:
  back=read - s->min_filepos;
  if(back<0) back=0; // strange...
  if(back>s->back_size) back=s->back_size;

  // calc number of new bytes:
  newb=s->max_filepos - read;
  if(newb<0) newb=0; // strange...

  // calc free buffer space:
  space=s->buffer_size - (newb+back);

  // calc bufferpos:
  pos=s->max_filepos - s->offset;
  if(pos>=s->buffer_size) pos-=s->buffer_size; // wrap-around

  if(space<s->fill_limit){
//    printf("Buffer is full (%d bytes free, limit: %d)\n",space,s->fill_limit);
    return 0; // no fill...
  }

//  printf("### read=0x%X  back=%d  newb=%d  space=%d  pos=%d\n",read,back,newb,space,pos);

  // reduce space if needed:
  if(space>s->buffer_size-pos) space=s->buffer_size-pos;

  // limit one-time block size
  read_chunk = s->stream->read_chunk;
  if (!read_chunk) read_chunk = 4*s->sector_size;
  space = FFMIN(space, read_chunk);

#if 1
  // back+newb+space <= buffer_size
  back2=s->buffer_size-(space+newb); // max back size
  if(s->min_filepos<(read-back2)) s->min_filepos=read-back2;
#else
  s->min_filepos=read-back; // avoid seeking-back to temp area...
#endif

  len = stream_read_internal(s->stream, &s->buffer[pos], space);
  s->eof= !len;

  s->max_filepos+=len;
  if(pos+len>=s->buffer_size){
      // wrap...
      s->offset+=s->buffer_size;
  }

  return len;

}

static int cache_execute_control(cache_vars_t *s) {
  static unsigned last;
  int quit = s->control == -2;
  if (quit || !s->stream->control) {
    s->stream_time_length = 0;
    s->control_new_pos = 0;
    s->control_res = STREAM_UNSUPPORTED;
    s->control = -1;
    return !quit;
  }
  if (GetTimerMS() - last > 99) {
    double len;
    if (s->stream->control(s->stream, STREAM_CTRL_GET_TIME_LENGTH, &len) == STREAM_OK)
      s->stream_time_length = len;
    else
      s->stream_time_length = 0;
    last = GetTimerMS();
  }
  if (s->control == -1) return 1;
  switch (s->control) {
    case STREAM_CTRL_GET_CURRENT_TIME:
    case STREAM_CTRL_SEEK_TO_TIME:
    case STREAM_CTRL_GET_ASPECT_RATIO:
      s->control_res = s->stream->control(s->stream, s->control, &s->control_double_arg);
      break;
    case STREAM_CTRL_SEEK_TO_CHAPTER:
    case STREAM_CTRL_GET_NUM_CHAPTERS:
    case STREAM_CTRL_GET_CURRENT_CHAPTER:
    case STREAM_CTRL_GET_NUM_ANGLES:
    case STREAM_CTRL_GET_ANGLE:
    case STREAM_CTRL_SET_ANGLE:
      s->control_res = s->stream->control(s->stream, s->control, &s->control_uint_arg);
      break;
    default:
      s->control_res = STREAM_UNSUPPORTED;
      break;
  }
  s->control_new_pos = s->stream->pos;
  s->control = -1;
  return 1;
}

static void *shared_alloc(int size) {
#if FORKED_CACHE
    return shmem_alloc(size);
#else
    return malloc(size);
#endif
}

static void shared_free(void *ptr, int size) {
#if FORKED_CACHE
    shmem_free(ptr, size);
#else
    free(ptr);
#endif
}

static cache_vars_t* cache_init(int size,int sector){
  int num;
  cache_vars_t* s=shared_alloc(sizeof(cache_vars_t));
  if(s==NULL) return NULL;

  memset(s,0,sizeof(cache_vars_t));
  num=size/sector;
  if(num < 16){
     num = 16;
  }//32kb min_size
  s->buffer_size=num*sector;
  s->sector_size=sector;
  s->buffer=shared_alloc(s->buffer_size);

  if(s->buffer == NULL){
    shared_free(s, sizeof(cache_vars_t));
    return NULL;
  }

  s->fill_limit=8*sector;
  s->back_size=s->buffer_size/2;
  return s;
}

void cache_uninit(stream_t *s) {
  cache_vars_t* c = s->cache_data;
  if(s->cache_pid) {
#if !FORKED_CACHE
    cache_do_control(s, -2, NULL);
#else
    kill(s->cache_pid,SIGKILL);
    waitpid(s->cache_pid,NULL,0);
#endif
    s->cache_pid = 0;
  }
  if(!c) return;
  shared_free(c->buffer, c->buffer_size);
  c->buffer = NULL;
  c->stream = NULL;
  shared_free(s->cache_data, sizeof(cache_vars_t));
  s->cache_data = NULL;
}

static void exit_sighandler(int x){
  // close stream
  exit(0);
}

static void dummy_sighandler(int x) {
}

/**
 * Main loop of the cache process or thread.
 */
static void cache_mainloop(cache_vars_t *s) {
    int sleep_count = 0;
#if FORKED_CACHE
    struct sigaction sa = { .sa_handler = SIG_IGN };
    sigaction(SIGUSR1, &sa, NULL);
#endif
    do {
        if (!cache_fill(s)) {
#if FORKED_CACHE
            // Let signal wake us up, we cannot leave this
            // enabled since we do not handle EINTR in most places.
            // This might need extra code to work on BSD.
            sa.sa_handler = dummy_sighandler;
            sigaction(SIGUSR1, &sa, NULL);
#endif
            if (sleep_count < INITIAL_FILL_USLEEP_COUNT) {
                sleep_count++;
                usec_sleep(INITIAL_FILL_USLEEP_TIME);
            } else
                usec_sleep(FILL_USLEEP_TIME); // idle
#if FORKED_CACHE
            sa.sa_handler = SIG_IGN;
            sigaction(SIGUSR1, &sa, NULL);
#endif
        } else
            sleep_count = 0;
    } while (cache_execute_control(s));
}

/**
 * \return 1 on success, 0 if the function was interrupted and -1 on error
 */
int stream_enable_cache(stream_t *stream,int size,int min,int seek_limit){
  int ss = stream->sector_size ? stream->sector_size : STREAM_BUFFER_SIZE;
  int res = -1;
  cache_vars_t* s;

  if (stream->flags & STREAM_NON_CACHEABLE) {
    mp_msg(MSGT_CACHE,MSGL_STATUS,"\rThis stream is non-cacheable\n");
    return 1;
  }

  s=cache_init(size,ss);
  if(s == NULL) return -1;
  stream->cache_data=s;
  s->stream=stream; // callback
  s->seek_limit=seek_limit;


  //make sure that we won't wait from cache_fill
  //more data than it is allowed to fill
  if (s->seek_limit > s->buffer_size - s->fill_limit ){
     s->seek_limit = s->buffer_size - s->fill_limit;
  }
  if (min > s->buffer_size - s->fill_limit) {
     min = s->buffer_size - s->fill_limit;
  }
  // to make sure we wait for the cache process/thread to be active
  // before continuing
  if (min <= 0)
    min = 1;

#if FORKED_CACHE
  if((stream->cache_pid=fork())){
    if ((pid_t)stream->cache_pid == -1)
      stream->cache_pid = 0;
#else
  {
    stream_t* stream2=malloc(sizeof(stream_t));
    memcpy(stream2,s->stream,sizeof(stream_t));
    s->stream=stream2;
#if defined(__MINGW32__)
    stream->cache_pid = _beginthread( ThreadProc, 0, s );
#elif defined(__OS2__)
    stream->cache_pid = _beginthread( ThreadProc, NULL, 256 * 1024, s );
#else
    {
    pthread_t tid;
    pthread_create(&tid, NULL, ThreadProc, s);
    stream->cache_pid = 1;
    }
#endif
#endif
    if (!stream->cache_pid) {
        mp_msg(MSGT_CACHE, MSGL_ERR,
               "Starting cache process/thread failed: %s.\n", strerror(errno));
        goto err_out;
    }
    // wait until cache is filled at least prefill_init %
    mp_msg(MSGT_CACHE,MSGL_V,"CACHE_PRE_INIT: %"PRId64" [%"PRId64"] %"PRId64"  pre:%d  eof:%d  \n",
	(int64_t)s->min_filepos,(int64_t)s->read_filepos,(int64_t)s->max_filepos,min,s->eof);
    while(s->read_filepos<s->min_filepos || s->max_filepos-s->read_filepos<min){
	mp_tmsg(MSGT_CACHE,MSGL_STATUS,"\rCache fill: %5.2f%% (%"PRId64" bytes)   ",
	    100.0*(float)(s->max_filepos-s->read_filepos)/(float)(s->buffer_size),
	    (int64_t)s->max_filepos-s->read_filepos
	);
	if(s->eof) break; // file is smaller than prefill size
	if(stream_check_interrupt(PREFILL_SLEEP_TIME)) {
	  res = 0;
	  goto err_out;
        }
    }
    mp_msg(MSGT_CACHE,MSGL_STATUS,"\n");
    return 1; // parent exits

err_out:
    cache_uninit(stream);
    return res;
  }

#if FORKED_CACHE
  signal(SIGTERM,exit_sighandler); // kill
  cache_mainloop(s);
  // make sure forked code never leaves this function
  exit(0);
#endif
}

#if !FORKED_CACHE
#if defined(__MINGW32__) || defined(__OS2__)
static void ThreadProc( void *s ){
  cache_mainloop(s);
  _endthread();
}
#else
static void *ThreadProc( void *s ){
  cache_mainloop(s);
  return NULL;
}
#endif
#endif

int cache_stream_fill_buffer(stream_t *s){
  int len;
  int sector_size;
  if(!s->cache_pid) return stream_fill_buffer(s);

  if(s->pos!=((cache_vars_t*)s->cache_data)->read_filepos) mp_msg(MSGT_CACHE,MSGL_ERR,"!!! read_filepos differs!!! report this bug...\n");
  sector_size = ((cache_vars_t*)s->cache_data)->sector_size;
  if (sector_size > STREAM_MAX_SECTOR_SIZE) {
    mp_msg(MSGT_CACHE, MSGL_ERR, "Sector size %i larger than maximum %i\n", sector_size, STREAM_MAX_SECTOR_SIZE);
    sector_size = STREAM_MAX_SECTOR_SIZE;
  }

  len=cache_read(s->cache_data,s->buffer, sector_size);
  //printf("cache_stream_fill_buffer->read -> %d\n",len);

  if(len<=0){ s->eof=1; s->buf_pos=s->buf_len=0; return 0; }
  s->eof=0;
  s->buf_pos=0;
  s->buf_len=len;
  s->pos+=len;
//  printf("[%d]",len);fflush(stdout);
  if (s->capture_file)
    stream_capture_do(s);
  return len;

}

int cache_fill_status(stream_t *s) {
  cache_vars_t *cv;
  if (!s || !s->cache_data)
    return -1;
  cv = s->cache_data;
  return (cv->max_filepos-cv->read_filepos)/(cv->buffer_size / 100);
}

int cache_stream_seek_long(stream_t *stream,off_t pos){
  cache_vars_t* s;
  off_t newpos;
  if(!stream->cache_pid) return stream_seek_long(stream,pos);

  s=stream->cache_data;
//  s->seek_lock=1;

  mp_msg(MSGT_CACHE,MSGL_DBG2,"CACHE2_SEEK: 0x%"PRIX64" <= 0x%"PRIX64" (0x%"PRIX64") <= 0x%"PRIX64"  \n",s->min_filepos,pos,s->read_filepos,s->max_filepos);

  newpos=pos/s->sector_size; newpos*=s->sector_size; // align
  stream->pos=s->read_filepos=newpos;
  s->eof=0; // !!!!!!!
  cache_wakeup(stream);

  cache_stream_fill_buffer(stream);

  pos-=newpos;
  if(pos>=0 && pos<=stream->buf_len){
    stream->buf_pos=pos; // byte position in sector
    return 1;
  }

//  stream->buf_pos=stream->buf_len=0;
//  return 1;

  mp_msg(MSGT_CACHE,MSGL_V,"cache_stream_seek: WARNING! Can't seek to 0x%"PRIX64" !\n",(int64_t)(pos+newpos));
  return 0;
}

int cache_do_control(stream_t *stream, int cmd, void *arg) {
  int sleep_count = 0;
  cache_vars_t* s = stream->cache_data;
  switch (cmd) {
    case STREAM_CTRL_SEEK_TO_TIME:
      s->control_double_arg = *(double *)arg;
      s->control = cmd;
      break;
    case STREAM_CTRL_SEEK_TO_CHAPTER:
    case STREAM_CTRL_SET_ANGLE:
      s->control_uint_arg = *(unsigned *)arg;
      s->control = cmd;
      break;
// the core might call these every frame, they are too slow for this...
    case STREAM_CTRL_GET_TIME_LENGTH:
//    case STREAM_CTRL_GET_CURRENT_TIME:
      *(double *)arg = s->stream_time_length;
      return s->stream_time_length ? STREAM_OK : STREAM_UNSUPPORTED;
    case STREAM_CTRL_GET_NUM_CHAPTERS:
    case STREAM_CTRL_GET_CURRENT_CHAPTER:
    case STREAM_CTRL_GET_ASPECT_RATIO:
    case STREAM_CTRL_GET_NUM_ANGLES:
    case STREAM_CTRL_GET_ANGLE:
    case -2:
      s->control = cmd;
      break;
    default:
      return STREAM_UNSUPPORTED;
  }
  cache_wakeup(stream);
  while (s->control != -1) {
    if (sleep_count++ == 1000)
      mp_msg(MSGT_CACHE, MSGL_WARN, "Cache not responding!\n");
    if (stream_check_interrupt(CONTROL_SLEEP_TIME)) {
      s->eof = 1;
      return STREAM_UNSUPPORTED;
    }
  }
  switch (cmd) {
    case STREAM_CTRL_GET_TIME_LENGTH:
    case STREAM_CTRL_GET_CURRENT_TIME:
    case STREAM_CTRL_GET_ASPECT_RATIO:
      *(double *)arg = s->control_double_arg;
      break;
    case STREAM_CTRL_GET_NUM_CHAPTERS:
    case STREAM_CTRL_GET_CURRENT_CHAPTER:
    case STREAM_CTRL_GET_NUM_ANGLES:
    case STREAM_CTRL_GET_ANGLE:
      *(unsigned *)arg = s->control_uint_arg;
      break;
    case STREAM_CTRL_SEEK_TO_CHAPTER:
    case STREAM_CTRL_SEEK_TO_TIME:
    case STREAM_CTRL_SET_ANGLE:
      if (s->control_res != STREAM_UNSUPPORTED)
          stream->pos = s->read_filepos = s->control_new_pos;
      break;
  }
  return s->control_res;
}
