#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>

#include "config.h"
#include "mp_msg.h"

#include "img_format.h"
#include "mp_image.h"
#include "vf.h"

struct vf_priv_s {
    char *tcv2filename;
    FILE *tcv2file;
    int noptswarn;
};

//===========================================================================//
static int config(struct vf_instance* vf,
        int width, int height, int d_width, int d_height,
	unsigned int flags, unsigned int outfmt){
    return vf_next_config(vf,width,height,d_width,d_height,flags,outfmt);
}

static int put_image(struct vf_instance* vf, mp_image_t *mpi, double pts){
    mp_image_t *dmpi;
    struct vf_priv_s *priv = (struct vf_priv_s*) vf->priv;

    // hope we'll get DR buffer:
    if(mpi->flags&MP_IMGFLAG_DIRECT){
        dmpi=(mp_image_t*)mpi->priv;
    } else {
        dmpi=vf_get_image(vf->next, mpi->imgfmt, MP_IMGTYPE_EXPORT, 0, mpi->w, mpi->h);

        dmpi->planes[0]=mpi->planes[0];
        dmpi->planes[1]=mpi->planes[1];
        dmpi->planes[2]=mpi->planes[2];
        dmpi->stride[0]=mpi->stride[0];
        dmpi->stride[1]=mpi->stride[1];
        dmpi->stride[2]=mpi->stride[2];
        dmpi->width=mpi->width;
        dmpi->height=mpi->height;
    }
    if (pts == MP_NOPTS_VALUE) {
        if (!priv->noptswarn) {
            priv->noptswarn = 1;
            mp_msg(MSGT_VFILTER, MSGL_WARN, "Stream contains frames with no PTS, timecode file not valid (if already written)\n");
        }
    } else {
        if (!priv->tcv2file && priv->tcv2filename) {
            priv->tcv2file = fopen(priv->tcv2filename,"w");
            if (!priv->tcv2file) {
                mp_msg(MSGT_VFILTER, MSGL_WARN, "Unable to open timecode file: %s\n",
                    strerror(errno));
                free(priv->tcv2filename);
                priv->tcv2filename = NULL;
            } else
                fprintf(priv->tcv2file, "# timecode format v2\n");
        }
        if (priv->tcv2file) {
            fprintf(priv->tcv2file, "%.2f\n", pts*1000);
        }
    }
    return vf_next_put_image(vf,dmpi, pts);
}

static void uninit(struct vf_instance *vf) {
    struct vf_priv_s *priv = (struct vf_priv_s*) vf->priv;
    if (priv->tcv2file)
        fclose(priv->tcv2file);
    if (priv->tcv2filename)
        free (priv->tcv2filename);
    free (priv);
}

//===========================================================================//

static int open(vf_instance_t *vf, char* args){
    vf->config=config;
    vf->put_image=put_image;
    vf->uninit=uninit;
    vf->priv=malloc(sizeof(struct vf_priv_s));
    vf->priv->tcv2filename = strdup(args ? args : "timecodesv2.txt");
    vf->priv->tcv2file = NULL;
    vf->priv->noptswarn = 0;
    return 1;
}

vf_info_t vf_info_tcdump = {
    "dump frame timecodes to file",
    "tcdump",
    "Andrew",
    "",
    open,
    NULL
};
