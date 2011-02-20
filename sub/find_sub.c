/*
 * .SUB
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

#include "config.h"

#include <stdio.h>

#include "libvo/video_out.h"
#include "sub.h"
#include "subreader.h"

#include "mp_msg.h"
#include "mpcommon.h"
#include "mplayer.h"

static int current_sub=0;

//static subtitle* subtitles=NULL;
static int nosub_range_start=-1;
static int nosub_range_end=-1;
static const sub_data *last_sub_data = NULL;

void step_sub(sub_data *subd, float pts, int movement) {
    subtitle *subs;
    int key;

    if (subd == NULL) return;
    subs = subd->subtitles;
    key = (pts+sub_delay) * (subd->sub_uses_time ? 100 : sub_fps);

    /* Tell the OSD subsystem that the OSD contents will change soon */
    vo_osd_changed(OSDTYPE_SUBTITLE);

    /* If we are moving forward, don't count the next (current) subtitle
     * if we haven't displayed it yet. Same when moving other direction.
     */
    if (movement > 0 && key < subs[current_sub].start)
    	movement--;
    if (movement < 0 && key >= subs[current_sub].end)
    	movement++;

    /* Never move beyond first or last subtitle. */
    if (current_sub+movement < 0)
    	movement = 0-current_sub;
    if (current_sub+movement >= subd->sub_num)
    	movement = subd->sub_num - current_sub - 1;

    current_sub += movement;
    sub_delay = subs[current_sub].start / (subd->sub_uses_time ? 100 : sub_fps) - pts;
}

void find_sub(struct MPContext *mpctx, sub_data* subd,int key){
    subtitle *subs;
    subtitle *new_sub = NULL;
    int i,j;

    if ( !subd || subd->sub_num == 0) return;
    subs = subd->subtitles;

    if (last_sub_data != subd) {
        // Sub data changed, reset nosub range.
        last_sub_data = subd;
        nosub_range_start = -1;
        nosub_range_end = -1;
    }

    if(vo_sub){
      if(key>=vo_sub->start && key<=vo_sub->end) return; // OK!
    } else {
      if(key>nosub_range_start && key<nosub_range_end) return; // OK!
    }
    // sub changed!

    /* Tell the OSD subsystem that the OSD contents will change soon */
    vo_osd_changed(OSDTYPE_SUBTITLE);

    if(key<=0){
      // no sub here
      goto update;
    }

//    printf("\r---- sub changed ----\n");

    // check next sub.
    if(current_sub>=0 && current_sub+1 < subd->sub_num){
      if(key>subs[current_sub].end && key<subs[current_sub+1].start){
          // no sub
          nosub_range_start=subs[current_sub].end;
          nosub_range_end=subs[current_sub+1].start;
          goto update;
      }
      // next sub?
      ++current_sub;
      new_sub=&subs[current_sub];
      if(key>=new_sub->start && key<=new_sub->end) goto update; // OK!
    }

//    printf("\r---- sub log search... ----\n");

    // use logarithmic search:
    i=0;
    j = subd->sub_num - 1;
//    printf("Searching %d in %d..%d\n",key,subs[i].start,subs[j].end);
    while(j>=i){
        current_sub=(i+j+1)/2;
        new_sub=&subs[current_sub];
        if(key<new_sub->start) j=current_sub-1;
        else if(key>new_sub->end) i=current_sub+1;
        else goto update; // found!
    }
//    if(key>=new_sub->start && key<=new_sub->end) return; // OK!

    // check where are we...
    if(key<new_sub->start){
      if(current_sub<=0){
          // before the first sub
          nosub_range_start=key-1; // tricky
          nosub_range_end=new_sub->start;
//          printf("FIRST...  key=%d  end=%d  \n",key,new_sub->start);
          new_sub=NULL;
          goto update;
      }
      --current_sub;
      if(key>subs[current_sub].end && key<subs[current_sub+1].start){
          // no sub
          nosub_range_start=subs[current_sub].end;
          nosub_range_end=subs[current_sub+1].start;
//          printf("No sub... 1 \n");
          new_sub=NULL;
          goto update;
      }
      printf("HEH????  ");
    } else {
      if(key<=new_sub->end) printf("JAJJ!  "); else
      if(current_sub+1 >= subd->sub_num){
          // at the end?
          nosub_range_start=new_sub->end;
          nosub_range_end=0x7FFFFFFF; // MAXINT
//          printf("END!?\n");
          new_sub=NULL;
          goto update;
      } else
      if(key>subs[current_sub].end && key<subs[current_sub+1].start){
          // no sub
          nosub_range_start=subs[current_sub].end;
          nosub_range_end=subs[current_sub+1].start;
//          printf("No sub... 2 \n");
          new_sub=NULL;
          goto update;
      }
    }

    mp_msg(MSGT_FIXME,MSGL_FIXME,"SUB ERROR:  %d  ?  %d --- %d  [%d]  \n",key,(int)new_sub->start,(int)new_sub->end,current_sub);

    new_sub=NULL; // no sub here
update:
    set_osd_subtitle(mpctx, new_sub);
}
