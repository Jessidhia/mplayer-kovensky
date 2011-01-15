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

#include <stdlib.h>

#include "libmpdemux/stheader.h"
#include "sd.h"
#include "dec_sub.h"
#include "options.h"

extern const struct sd_functions sd_ass;

void sub_init(struct sh_sub *sh, struct osd_state *osd)
{
    struct MPOpts *opts = sh->opts;
    if (opts->ass_enabled)
        sh->sd_driver = &sd_ass;
    if (sh->sd_driver)
        sh->sd_driver->init(sh, osd);
}

void sub_decode(struct sh_sub *sh, struct osd_state *osd, void *data,
                int data_len, double pts, double duration)
{
    sh->sd_driver->decode(sh, osd, data, data_len, pts, duration);
}
