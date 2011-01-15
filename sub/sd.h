#ifndef MPLAYER_SD_H
#define MPLAYER_SD_H

struct osd_state;
struct sh_sub;

struct sd_functions {
    void (*init)(struct sh_sub *sh, struct osd_state *osd);
    void (*decode)(struct sh_sub *sh, struct osd_state *osd,
                   void *data, int data_len, double pts, double duration);
};

#endif
