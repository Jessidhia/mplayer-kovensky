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
#include "mp_msg.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libmpcodecs/img_format.h"
#include "libmpcodecs/mp_image.h"

#include "m_struct.h"
#include "m_option.h"
#include "menu.h"

#include "sub/font_load.h"
#include "input/keycodes.h"

struct menu_priv_s {
  char** lines;
  int num_lines;
  int cur_line;
  int disp_lines;
  int minb;
  int hspace;
  char* file;
};

static struct menu_priv_s cfg_dflt = {
  NULL,
  0,
  0,
  0,
  0,
  3,
  NULL
};

#define ST_OFF(m) M_ST_OFF(struct menu_priv_s,m)

static const m_option_t cfg_fields[] = {
  { "minbor", ST_OFF(minb), CONF_TYPE_INT, M_OPT_MIN, 0, 0, NULL },
  { "hspace", ST_OFF(hspace), CONF_TYPE_INT, M_OPT_MIN, 0, 0, NULL },
  { "file", ST_OFF(file), CONF_TYPE_STRING, 0, 0, 0, NULL },
  { NULL, NULL, NULL, 0,0,0,NULL }
};

#define mpriv (menu->priv)

static void read_cmd(menu_t* menu,int cmd) {
  switch(cmd) {
  case MENU_CMD_UP:
    mpriv->cur_line -= mpriv->disp_lines / 2;
    if(mpriv->cur_line < 0)
      mpriv->cur_line = 0;
    break;
  case MENU_CMD_DOWN:
  case MENU_CMD_OK:
    mpriv->cur_line += mpriv->disp_lines / 2;
    if(mpriv->cur_line >= mpriv->num_lines)
      mpriv->cur_line = mpriv->num_lines - 1;
    break;
  case MENU_CMD_LEFT:
  case MENU_CMD_CANCEL:
    menu->show = 0;
    menu->cl = 1;
    break;
  case MENU_CMD_HOME:
    mpriv->cur_line = 0;
    break;
  case MENU_CMD_END:
    mpriv->cur_line = mpriv->num_lines - 1;
    break;
  case MENU_CMD_PAGE_UP:
    mpriv->cur_line =  mpriv->cur_line > mpriv->disp_lines ?
      mpriv->cur_line - mpriv->disp_lines : 0;
    break;
  case MENU_CMD_PAGE_DOWN:
    mpriv->cur_line = mpriv->cur_line + mpriv->disp_lines > mpriv->num_lines - 1 ? mpriv->num_lines - 1 : mpriv->cur_line + mpriv->disp_lines;
    break;
  }
}


static void draw(menu_t* menu,mp_image_t* mpi) {
  int x = mpriv->minb;
  int y = mpriv->minb;
  //int th = 2*mpriv->hspace + vo_font->height;
  int i,end;

  if(x < 0) x = 8;
  if(y < 0) y = 8;

  mpriv->disp_lines = (mpi->h + mpriv->hspace  - 2*mpriv->minb) / (  vo_font->height + mpriv->hspace);
  if(mpriv->num_lines - mpriv->cur_line < mpriv->disp_lines) {
    i = mpriv->num_lines - 1 - mpriv->disp_lines;
    if(i < 0) i = 0;
    end = mpriv->num_lines - 1;
  } else {
    i = mpriv->cur_line;
    end = i + mpriv->disp_lines;
    if(end >= mpriv->num_lines) end = mpriv->num_lines - 1;
  }

  for( ; i < end ; i++) {
    menu_draw_text(mpi,mpriv->lines[i],x,y);
    y += vo_font->height + mpriv->hspace;
  }

}

#define BUF_SIZE 1024

static int open_txt(menu_t* menu, char* args) {
  FILE* fd;
  char buf[BUF_SIZE];
  char *l;
  int s;
  int pos = 0, r = 0;
  args = NULL; // Warning kill

  menu->draw = draw;
  menu->read_cmd = read_cmd;

  if(!mpriv->file) {
    mp_tmsg(MSGT_GLOBAL,MSGL_WARN,"[MENU] Text menu needs a textfile name (parameter file).\n");
    return 0;
  }

  fd = fopen(mpriv->file,"r");
  if(!fd) {
    mp_tmsg(MSGT_GLOBAL,MSGL_WARN,"[MENU] Can't open %s.\n",mpriv->file);
    return 0;
  }

  while(1) {
    r = fread(buf+pos,1,BUF_SIZE-pos-1,fd);
    if(r <= 0) {
      if(pos > 0) {
	mpriv->lines = realloc(mpriv->lines,(mpriv->num_lines + 1)*sizeof(char*));
	mpriv->lines[mpriv->num_lines] = strdup(buf);
	mpriv->num_lines++;
      }
      fclose(fd);
      break;
    }
    pos += r;
    buf[pos] = '\0';

    while((l = strchr(buf,'\n')) != NULL) {
      s = l-buf;
      mpriv->lines = realloc(mpriv->lines,(mpriv->num_lines + 1)*sizeof(char*));
      mpriv->lines[mpriv->num_lines] = malloc(s+1);
      memcpy(mpriv->lines[mpriv->num_lines],buf,s);
      mpriv->lines[mpriv->num_lines][s] = '\0';
      pos -= s + 1;
      if(pos > 0)
	memmove(buf,l+1,pos);
      buf[pos] = '\0';
      mpriv->num_lines++;
    }
    if(pos >= BUF_SIZE-1) {
      mp_tmsg(MSGT_GLOBAL,MSGL_WARN,"[MENU] Warning, line too long. Splitting it.\n");
      mpriv->lines = realloc(mpriv->lines,(mpriv->num_lines + 1)*sizeof(char*));
      mpriv->lines[mpriv->num_lines] = strdup(buf);
      mpriv->num_lines++;
      pos = 0;
    }
  }

  mp_tmsg(MSGT_GLOBAL,MSGL_INFO,"[MENU] Parsed %d lines.\n",mpriv->num_lines);

  return 1;
}

const menu_info_t menu_info_txt = {
  "Text file viewer",
  "txt",
  "Albeu",
  "",
  {
    "txt_cfg",
    sizeof(struct menu_priv_s),
    &cfg_dflt,
    cfg_fields
  },
  open_txt,
};
