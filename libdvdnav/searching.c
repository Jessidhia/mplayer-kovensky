/*
 * Copyright (C) 2000 Rich Wareham <richwareham@users.sourceforge.net>
 *
 * This file is part of libdvdnav, a DVD navigation library.
 *
 * libdvdnav is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * libdvdnav is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with libdvdnav; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/time.h>
#include "dvdnav/dvdnav.h"
#include <dvdread/nav_types.h>
#include <dvdread/ifo_types.h>
#include "remap.h"
#include "vm/decoder.h"
#include "vm/vm.h"
#include "dvdnav_internal.h"

/*
#define LOG_DEBUG
*/

/* Searching API calls */

/* Scan the ADMAP for a particular block number. */
/* Return placed in vobu. */
/* Returns error status */
/* FIXME: Maybe need to handle seeking outside current cell. */
static dvdnav_status_t dvdnav_scan_admap(dvdnav_t *this, int32_t domain, uint32_t seekto_block, uint32_t *vobu) {
  vobu_admap_t *admap = NULL;

#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: Seeking to target %u ...\n", seekto_block);
#endif
  *vobu = -1;

  /* Search through the VOBU_ADMAP for the nearest VOBU
   * to the target block */
  switch(domain) {
  case FP_DOMAIN:
  case VMGM_DOMAIN:
    admap = this->vm->vmgi->menu_vobu_admap;
    break;
  case VTSM_DOMAIN:
    admap = this->vm->vtsi->menu_vobu_admap;
    break;
  case VTS_DOMAIN:
    admap = this->vm->vtsi->vts_vobu_admap;
    break;
  default:
    fprintf(MSG_OUT, "libdvdnav: Error: Unknown domain for seeking.\n");
  }
  if(admap) {
    uint32_t address = 0;
    uint32_t vobu_start, next_vobu;
    int admap_entries = (admap->last_byte + 1 - VOBU_ADMAP_SIZE)/VOBU_ADMAP_SIZE;

    /* Search through ADMAP for best sector */
    vobu_start = SRI_END_OF_CELL;
    /* FIXME: Implement a faster search algorithm */
    while(address < admap_entries) {
      next_vobu = admap->vobu_start_sectors[address];

      /* fprintf(MSG_OUT, "libdvdnav: Found block %u\n", next_vobu); */

      if(vobu_start <= seekto_block && next_vobu > seekto_block)
        break;
      vobu_start = next_vobu;
      address++;
    }
    *vobu = vobu_start;
    return DVDNAV_STATUS_OK;
  }
  fprintf(MSG_OUT, "libdvdnav: admap not located\n");
  return DVDNAV_STATUS_ERR;
}

/* FIXME: right now, this function does not use the time tables but interpolates
   only the cell times */
dvdnav_status_t dvdnav_time_search(dvdnav_t *this,
				   uint64_t time) {

  uint64_t target = time;
  uint64_t length = 0;
  uint32_t first_cell_nr, last_cell_nr, cell_nr;
  int32_t found;
  cell_playback_t *cell;
  dvd_state_t *state;

  if(this->position_current.still != 0) {
    printerr("Cannot seek in a still frame.");
    return DVDNAV_STATUS_ERR;
  }

  pthread_mutex_lock(&this->vm_lock);
  state = &(this->vm->state);
  if(!state->pgc) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }


  this->cur_cell_time = 0;
  if (this->pgc_based) {
    first_cell_nr = 1;
    last_cell_nr = state->pgc->nr_of_cells;
  } else {
    /* Find start cell of program. */
    first_cell_nr = state->pgc->program_map[state->pgN-1];
    /* Find end cell of program */
    if(state->pgN < state->pgc->nr_of_programs)
      last_cell_nr = state->pgc->program_map[state->pgN] - 1;
    else
      last_cell_nr = state->pgc->nr_of_cells;
  }

  found = 0;
  for(cell_nr = first_cell_nr; (cell_nr <= last_cell_nr) && !found; cell_nr ++) {
    cell =  &(state->pgc->cell_playback[cell_nr-1]);
    if(cell->block_type == BLOCK_TYPE_ANGLE_BLOCK && cell->block_mode != BLOCK_MODE_FIRST_CELL)
      continue;
    length = dvdnav_convert_time(&cell->playback_time);
    if (target >= length) {
      target -= length;
    } else {
      /* FIXME: there must be a better way than interpolation */
      target = target * (cell->last_sector - cell->first_sector + 1) / length;
      target += cell->first_sector;

      found = 1;
      break;
    }
  }

  if(found) {
    uint32_t vobu;
#ifdef LOG_DEBUG
    fprintf(MSG_OUT, "libdvdnav: Seeking to cell %i from choice of %i to %i\n",
	    cell_nr, first_cell_nr, last_cell_nr);
#endif
    if (dvdnav_scan_admap(this, state->domain, target, &vobu) == DVDNAV_STATUS_OK) {
      uint32_t start = state->pgc->cell_playback[cell_nr-1].first_sector;

      if (vm_jump_cell_block(this->vm, cell_nr, vobu - start)) {
#ifdef LOG_DEBUG
        fprintf(MSG_OUT, "libdvdnav: After cellN=%u blockN=%u target=%x vobu=%x start=%x\n" ,
          state->cellN, state->blockN, target, vobu, start);
#endif
        this->vm->hop_channel += HOP_SEEK;
        pthread_mutex_unlock(&this->vm_lock);
        return DVDNAV_STATUS_OK;
      }
    }
  }

  fprintf(MSG_OUT, "libdvdnav: Error when seeking\n");
  printerr("Error when seeking.");
  pthread_mutex_unlock(&this->vm_lock);
  return DVDNAV_STATUS_ERR;
}

dvdnav_status_t dvdnav_sector_search(dvdnav_t *this,
				     uint64_t offset, int32_t origin) {
  uint32_t target = 0;
  uint32_t length = 0;
  uint32_t first_cell_nr, last_cell_nr, cell_nr;
  int32_t found;
  cell_playback_t *cell;
  dvd_state_t *state;
  dvdnav_status_t result;

  if(this->position_current.still != 0) {
    printerr("Cannot seek in a still frame.");
    return DVDNAV_STATUS_ERR;
  }

  result = dvdnav_get_position(this, &target, &length);
  if(!result) {
    return DVDNAV_STATUS_ERR;
  }

  pthread_mutex_lock(&this->vm_lock);
  state = &(this->vm->state);
  if(!state->pgc) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }
#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: seeking to offset=%lu pos=%u length=%u\n", offset, target, length);
  fprintf(MSG_OUT, "libdvdnav: Before cellN=%u blockN=%u\n", state->cellN, state->blockN);
#endif

  switch(origin) {
   case SEEK_SET:
    if(offset >= length) {
      printerr("Request to seek behind end.");
      pthread_mutex_unlock(&this->vm_lock);
      return DVDNAV_STATUS_ERR;
    }
    target = offset;
    break;
   case SEEK_CUR:
    if(target + offset >= length) {
      printerr("Request to seek behind end.");
      pthread_mutex_unlock(&this->vm_lock);
      return DVDNAV_STATUS_ERR;
    }
    target += offset;
    break;
   case SEEK_END:
    if(length < offset) {
      printerr("Request to seek before start.");
      pthread_mutex_unlock(&this->vm_lock);
      return DVDNAV_STATUS_ERR;
    }
    target = length - offset;
    break;
   default:
    /* Error occured */
    printerr("Illegal seek mode.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }

  this->cur_cell_time = 0;
  if (this->pgc_based) {
    first_cell_nr = 1;
    last_cell_nr = state->pgc->nr_of_cells;
  } else {
    /* Find start cell of program. */
    first_cell_nr = state->pgc->program_map[state->pgN-1];
    /* Find end cell of program */
    if(state->pgN < state->pgc->nr_of_programs)
      last_cell_nr = state->pgc->program_map[state->pgN] - 1;
    else
      last_cell_nr = state->pgc->nr_of_cells;
  }

  found = 0;
  for(cell_nr = first_cell_nr; (cell_nr <= last_cell_nr) && !found; cell_nr ++) {
    cell =  &(state->pgc->cell_playback[cell_nr-1]);
    if(cell->block_type == BLOCK_TYPE_ANGLE_BLOCK && cell->block_mode != BLOCK_MODE_FIRST_CELL)
      continue;
    length = cell->last_sector - cell->first_sector + 1;
    if (target >= length) {
      target -= length;
    } else {
      /* convert the target sector from Cell-relative to absolute physical sector */
      target += cell->first_sector;
      found = 1;
      break;
    }
  }

  if(found) {
    uint32_t vobu;
#ifdef LOG_DEBUG
    fprintf(MSG_OUT, "libdvdnav: Seeking to cell %i from choice of %i to %i\n",
	    cell_nr, first_cell_nr, last_cell_nr);
#endif
    if (dvdnav_scan_admap(this, state->domain, target, &vobu) == DVDNAV_STATUS_OK) {
      int32_t start = state->pgc->cell_playback[cell_nr-1].first_sector;

      if (vm_jump_cell_block(this->vm, cell_nr, vobu - start)) {
#ifdef LOG_DEBUG
        fprintf(MSG_OUT, "libdvdnav: After cellN=%u blockN=%u target=%x vobu=%x start=%x\n" ,
          state->cellN, state->blockN, target, vobu, start);
#endif
        this->vm->hop_channel += HOP_SEEK;
        pthread_mutex_unlock(&this->vm_lock);
        return DVDNAV_STATUS_OK;
      }
    }
  }

  fprintf(MSG_OUT, "libdvdnav: Error when seeking\n");
  fprintf(MSG_OUT, "libdvdnav: FIXME: Implement seeking to location %u\n", target);
  printerr("Error when seeking.");
  pthread_mutex_unlock(&this->vm_lock);
  return DVDNAV_STATUS_ERR;
}

dvdnav_status_t dvdnav_part_search(dvdnav_t *this, int32_t part) {
  int32_t title, old_part;

  if (dvdnav_current_title_info(this, &title, &old_part) == DVDNAV_STATUS_OK)
    return dvdnav_part_play(this, title, part);
  return DVDNAV_STATUS_ERR;
}

dvdnav_status_t dvdnav_prev_pg_search(dvdnav_t *this) {
  pthread_mutex_lock(&this->vm_lock);
  if(!this->vm->state.pgc) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }

#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: previous chapter\n");
#endif
  if (!vm_jump_prev_pg(this->vm)) {
    fprintf(MSG_OUT, "libdvdnav: previous chapter failed.\n");
    printerr("Skip to previous chapter failed.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }
  this->cur_cell_time = 0;
  this->position_current.still = 0;
  this->vm->hop_channel++;
#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: previous chapter done\n");
#endif
  pthread_mutex_unlock(&this->vm_lock);

  return DVDNAV_STATUS_OK;
}

dvdnav_status_t dvdnav_top_pg_search(dvdnav_t *this) {
  pthread_mutex_lock(&this->vm_lock);
  if(!this->vm->state.pgc) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }

#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: top chapter\n");
#endif
  if (!vm_jump_top_pg(this->vm)) {
    fprintf(MSG_OUT, "libdvdnav: top chapter failed.\n");
    printerr("Skip to top chapter failed.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }
  this->cur_cell_time = 0;
  this->position_current.still = 0;
  this->vm->hop_channel++;
#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: top chapter done\n");
#endif
  pthread_mutex_unlock(&this->vm_lock);

  return DVDNAV_STATUS_OK;
}

dvdnav_status_t dvdnav_next_pg_search(dvdnav_t *this) {
  vm_t *try_vm;

  pthread_mutex_lock(&this->vm_lock);
  if(!this->vm->state.pgc) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }

#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: next chapter\n");
#endif
  /* make a copy of current VM and try to navigate the copy to the next PG */
  try_vm = vm_new_copy(this->vm);
  if (!vm_jump_next_pg(try_vm) || try_vm->stopped) {
    vm_free_copy(try_vm);
    /* next_pg failed, try to jump at least to the next cell */
    try_vm = vm_new_copy(this->vm);
    vm_get_next_cell(try_vm);
    if (try_vm->stopped) {
      vm_free_copy(try_vm);
      fprintf(MSG_OUT, "libdvdnav: next chapter failed.\n");
      printerr("Skip to next chapter failed.");
      pthread_mutex_unlock(&this->vm_lock);
      return DVDNAV_STATUS_ERR;
    }
  }
  this->cur_cell_time = 0;
  /* merge changes on success */
  vm_merge(this->vm, try_vm);
  vm_free_copy(try_vm);
  this->position_current.still = 0;
  this->vm->hop_channel++;
#ifdef LOG_DEBUG
  fprintf(MSG_OUT, "libdvdnav: next chapter done\n");
#endif
  pthread_mutex_unlock(&this->vm_lock);

  return DVDNAV_STATUS_OK;
}

dvdnav_status_t dvdnav_menu_call(dvdnav_t *this, DVDMenuID_t menu) {
  vm_t *try_vm;

  pthread_mutex_lock(&this->vm_lock);
  if(!this->vm->state.pgc) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }

  this->cur_cell_time = 0;
  /* make a copy of current VM and try to navigate the copy to the menu */
  try_vm = vm_new_copy(this->vm);
  if ( (menu == DVD_MENU_Escape) && (this->vm->state.domain != VTS_DOMAIN)) {
    /* Try resume */
    if (vm_jump_resume(try_vm) && !try_vm->stopped) {
        /* merge changes on success */
        vm_merge(this->vm, try_vm);
        vm_free_copy(try_vm);
        this->position_current.still = 0;
        this->vm->hop_channel++;
        pthread_mutex_unlock(&this->vm_lock);
        return DVDNAV_STATUS_OK;
    }
  }
  if (menu == DVD_MENU_Escape) menu = DVD_MENU_Root;

  if (vm_jump_menu(try_vm, menu) && !try_vm->stopped) {
    /* merge changes on success */
    vm_merge(this->vm, try_vm);
    vm_free_copy(try_vm);
    this->position_current.still = 0;
    this->vm->hop_channel++;
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_OK;
  } else {
    vm_free_copy(try_vm);
    printerr("No such menu or menu not reachable.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }
}

dvdnav_status_t dvdnav_get_position(dvdnav_t *this, uint32_t *pos,
				    uint32_t *len) {
  uint32_t cur_sector;
  int32_t cell_nr, first_cell_nr, last_cell_nr;
  cell_playback_t *cell;
  dvd_state_t *state;

  if(!this->started) {
    printerr("Virtual DVD machine not started.");
    return DVDNAV_STATUS_ERR;
  }

  pthread_mutex_lock(&this->vm_lock);
  state = &(this->vm->state);
  if(!state->pgc || this->vm->stopped) {
    printerr("No current PGC.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }
  if (this->position_current.hop_channel  != this->vm->hop_channel ||
      this->position_current.domain       != state->domain         ||
      this->position_current.vts          != state->vtsN           ||
      this->position_current.cell_restart != state->cell_restart) {
    printerr("New position not yet determined.");
    pthread_mutex_unlock(&this->vm_lock);
    return DVDNAV_STATUS_ERR;
  }

  /* Get current sector */
  cur_sector = this->vobu.vobu_start + this->vobu.blockN;

  if (this->pgc_based) {
    first_cell_nr = 1;
    last_cell_nr = state->pgc->nr_of_cells;
  } else {
    /* Find start cell of program. */
    first_cell_nr = state->pgc->program_map[state->pgN-1];
    /* Find end cell of program */
    if(state->pgN < state->pgc->nr_of_programs)
      last_cell_nr = state->pgc->program_map[state->pgN] - 1;
    else
      last_cell_nr = state->pgc->nr_of_cells;
  }

  *pos = -1;
  *len = 0;
  for (cell_nr = first_cell_nr; cell_nr <= last_cell_nr; cell_nr++) {
    cell = &(state->pgc->cell_playback[cell_nr-1]);
    if (cell_nr == state->cellN) {
      /* the current sector is in this cell,
       * pos is length of PG up to here + sector's offset in this cell */
      *pos = *len + cur_sector - cell->first_sector;
    }
    *len += cell->last_sector - cell->first_sector + 1;
  }

  assert((signed)*pos != -1);

  pthread_mutex_unlock(&this->vm_lock);

  return DVDNAV_STATUS_OK;
}

dvdnav_status_t dvdnav_get_position_in_title(dvdnav_t *this,
					     uint32_t *pos,
					     uint32_t *len) {
  uint32_t cur_sector;
  uint32_t first_cell_nr;
  uint32_t last_cell_nr;
  cell_playback_t *first_cell;
  cell_playback_t *last_cell;
  dvd_state_t *state;

  state = &(this->vm->state);
  if(!state->pgc) {
    printerr("No current PGC.");
    return DVDNAV_STATUS_ERR;
  }

  /* Get current sector */
  cur_sector = this->vobu.vobu_start + this->vobu.blockN;

  /* Now find first and last cells in title. */
  first_cell_nr = state->pgc->program_map[0];
  first_cell = &(state->pgc->cell_playback[first_cell_nr-1]);
  last_cell_nr = state->pgc->nr_of_cells;
  last_cell = &(state->pgc->cell_playback[last_cell_nr-1]);

  *pos = cur_sector - first_cell->first_sector;
  *len = last_cell->last_sector - first_cell->first_sector;

  return DVDNAV_STATUS_OK;
}

uint32_t dvdnav_describe_title_chapters(dvdnav_t *this, int32_t title, uint64_t **times, uint64_t *duration) {
  int32_t retval=0;
  uint16_t parts, i;
  title_info_t *ptitle = NULL;
  ptt_info_t *ptt = NULL;
  ifo_handle_t *ifo = NULL;
  pgc_t *pgc;
  cell_playback_t *cell;
  uint64_t length, *tmp=NULL;

  *times = NULL;
  *duration = 0;
  pthread_mutex_lock(&this->vm_lock);
  if(!this->vm->vmgi) {
    printerr("Bad VM state or missing VTSI.");
    goto fail;
  }
  if(!this->started) {
    /* don't report an error but be nice */
    vm_start(this->vm);
    this->started = 1;
  }
  ifo = vm_get_title_ifo(this->vm, title);
  if(!ifo || !ifo->vts_pgcit) {
    printerr("Couldn't open IFO for chosen title, exit.");
    retval = 0;
    goto fail;
  }

  ptitle = &this->vm->vmgi->tt_srpt->title[title-1];
  parts = ptitle->nr_of_ptts;
  ptt = ifo->vts_ptt_srpt->title[ptitle->vts_ttn-1].ptt;

  tmp = calloc(1, sizeof(uint64_t)*parts);
  if(!tmp)
    goto fail;

  length = 0;
  for(i=0; i<parts; i++) {
    uint32_t cellnr, endcellnr;
    pgc = ifo->vts_pgcit->pgci_srp[ptt[i].pgcn-1].pgc;
    if(ptt[i].pgn > pgc->nr_of_programs) {
      printerr("WRONG part number.");
      goto fail;
    }

    cellnr = pgc->program_map[ptt[i].pgn-1];
    if(ptt[i].pgn < pgc->nr_of_programs)
      endcellnr = pgc->program_map[ptt[i].pgn];
    else
      endcellnr = 0;

    do {
      cell = &pgc->cell_playback[cellnr-1];
      if(!(cell->block_type == BLOCK_TYPE_ANGLE_BLOCK &&
           cell->block_mode != BLOCK_MODE_FIRST_CELL
      ))
      {
        tmp[i] = length + dvdnav_convert_time(&cell->playback_time);
        length = tmp[i];
      }
      cellnr++;
    } while(cellnr < endcellnr);
  }
  *duration = length;
  vm_ifo_close(ifo);
  ifo = NULL;
  retval = parts;
  *times = tmp;

fail:
  pthread_mutex_unlock(&this->vm_lock);
  if(!retval && ifo)
    vm_ifo_close(ifo);
  if(!retval && tmp)
    free(tmp);
  return retval;
}
