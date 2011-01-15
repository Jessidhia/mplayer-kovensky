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
#include <ass/ass.h>
#include <assert.h>

#include "talloc.h"

#include "mpcommon.h"
#include "libmpdemux/stheader.h"
#include "libvo/sub.h"
#include "ass_mp.h"
#include "sd.h"

struct priv {
    struct ass_track *ass_track;
};

static void init(struct sh_sub *sh, struct osd_state *osd)
{
    struct priv *ctx = talloc_zero(NULL, struct priv);
    sh->context = ctx;
    if (sh->type == 'a') {
        ctx->ass_track = ass_new_track(ass_library);
        if (sh->extradata)
            ass_process_codec_private(ctx->ass_track, sh->extradata,
                                      sh->extradata_len);
    } else
        ctx->ass_track = ass_default_track(ass_library);

    assert(osd->ass_track == NULL);
    osd->ass_track = ctx->ass_track;
}

static void decode(struct sh_sub *sh, struct osd_state *osd, void *data,
                   int data_len, double pts, double duration)
{
    struct priv *ctx = sh->context;

    if (sh->type == 'a') { // ssa/ass subs with libass
        ass_process_chunk(ctx->ass_track, data, data_len,
                          (long long)(pts*1000 + 0.5),
                          (long long)(duration*1000 + 0.5));
    } else { // plaintext subs with libass
        if (pts != MP_NOPTS_VALUE) {
            subtitle tmp_subs = {0};
            if (duration <= 0)
                duration = 3;
            sub_add_text(&tmp_subs, data, data_len, pts + duration);
            tmp_subs.start = pts * 100;
            tmp_subs.end = (pts + duration) * 100;
            ass_process_subtitle(ctx->ass_track, &tmp_subs);
            sub_clear_text(&tmp_subs, MP_NOPTS_VALUE);
        }
    }
}


const struct sd_functions sd_ass = {
    .init = init,
    .decode = decode,
};
