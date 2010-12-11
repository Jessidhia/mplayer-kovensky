/*
 * CD Info
 *
 * Copyright (C) 2002 Bertrand Baudet <bertrand_baudet@yahoo.com>
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
#include <stdlib.h>
#include <string.h>
#include "mp_msg.h"
#include "cdd.h"

/*******************************************************************************************************************
 *
 * xmcd parser, cd info list
 *
 *******************************************************************************************************************/

cd_info_t*
cd_info_new(void) {
	cd_info_t *cd_info = NULL;

	cd_info = malloc(sizeof(cd_info_t));
	if( cd_info==NULL ) {
		mp_tmsg(MSGT_DEMUX, MSGL_ERR, "Memory allocation failed.\n");
		return NULL;
	}

	memset(cd_info, 0, sizeof(cd_info_t));

	return cd_info;
}

void
cd_info_free(cd_info_t *cd_info) {
	cd_track_t *cd_track, *cd_track_next;
	if( cd_info==NULL ) return;
	free(cd_info->artist);
	free(cd_info->album);
	free(cd_info->genre);

	cd_track_next = cd_info->first;
	while( cd_track_next!=NULL ) {
		cd_track = cd_track_next;
		cd_track_next = cd_track->next;
		free(cd_track->name);
		free(cd_track);
	}
}

cd_track_t*
cd_info_add_track(cd_info_t *cd_info, char *track_name, unsigned int track_nb, unsigned int min, unsigned int sec, unsigned int msec, unsigned long frame_begin, unsigned long frame_length) {
	cd_track_t *cd_track;

	if( cd_info==NULL || track_name==NULL ) return NULL;

	cd_track = malloc(sizeof(cd_track_t));
	if( cd_track==NULL ) {
		mp_tmsg(MSGT_DEMUX, MSGL_ERR, "Memory allocation failed.\n");
		return NULL;
	}
	memset(cd_track, 0, sizeof(cd_track_t));

	cd_track->name = malloc(strlen(track_name)+1);
	if( cd_track->name==NULL ) {
		mp_tmsg(MSGT_DEMUX, MSGL_ERR, "Memory allocation failed.\n");
		free(cd_track);
		return NULL;
	}
	strcpy(cd_track->name, track_name);
	cd_track->track_nb = track_nb;
	cd_track->min = min;
	cd_track->sec = sec;
	cd_track->msec = msec;
	cd_track->frame_begin = frame_begin;
	cd_track->frame_length = frame_length;

	if( cd_info->first==NULL ) {
		cd_info->first = cd_track;
	}
	if( cd_info->last!=NULL ) {
		cd_info->last->next = cd_track;
	}

	cd_track->prev = cd_info->last;

	cd_info->last = cd_track;
	cd_info->current = cd_track;

	cd_info->nb_tracks++;

	return cd_track;
}

cd_track_t*
cd_info_get_track(cd_info_t *cd_info, unsigned int track_nb) {
	cd_track_t *cd_track=NULL;

	if( cd_info==NULL ) return NULL;

	cd_track = cd_info->first;
	while( cd_track!=NULL ) {
		if( cd_track->track_nb==track_nb ) {
			return cd_track;
		}
		cd_track = cd_track->next;
	}
	return NULL;
}

void
cd_info_debug(cd_info_t *cd_info) {
	cd_track_t *current_track;
	mp_msg(MSGT_DEMUX, MSGL_INFO, "================ CD INFO === start =========\n");
	if( cd_info==NULL ) {
		mp_msg(MSGT_DEMUX, MSGL_INFO, "cd_info is NULL\n");
		return;
	}
	mp_msg(MSGT_DEMUX, MSGL_INFO, " artist=[%s]\n", cd_info->artist);
	mp_msg(MSGT_DEMUX, MSGL_INFO, " album=[%s]\n", cd_info->album);
	mp_msg(MSGT_DEMUX, MSGL_INFO, " genre=[%s]\n", cd_info->genre);
	mp_msg(MSGT_DEMUX, MSGL_INFO, " nb_tracks=%d\n", cd_info->nb_tracks);
	mp_msg(MSGT_DEMUX, MSGL_INFO, " length= %2d:%02d.%02d\n", cd_info->min, cd_info->sec, cd_info->msec);

	mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_ARTIST=%s\n", cd_info->artist);
	mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_ALBUM=%s\n", cd_info->album);
	mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_GENRE=%s\n", cd_info->genre);
	mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_LENGTH_MSF=%02d:%02d.%02d\n", cd_info->min, cd_info->sec, cd_info->msec);
	mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_TRACKS=%d\n", cd_info->nb_tracks);

	current_track = cd_info->first;
	while( current_track!=NULL ) {
		mp_msg(MSGT_DEMUX, MSGL_INFO, "  #%2d %2d:%02d.%02d @ %7ld\t[%s] \n", current_track->track_nb, current_track->min, current_track->sec, current_track->msec, current_track->frame_begin, current_track->name);
		mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_TRACK_%d_NAME=%s\n", current_track->track_nb, current_track->name);
		mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_CDDB_INFO_TRACK_%d_MSF=%02d:%02d.%02d\n", current_track->track_nb, current_track->min, current_track->sec, current_track->msec);
		current_track = current_track->next;
	}
	mp_msg(MSGT_DEMUX, MSGL_INFO, "================ CD INFO ===  end  =========\n");
}
