/*
 * This file is part of mplayer2.
 *
 * mplayer2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * mplayer2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with mplayer2; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef MPLAYER_FILTER_KERNELS_H
#define MPLAYER_FILTER_KERNELS_H

struct filter_kernel {
    const char *name;
    // Number of coefficients; equals the rounded up radius multiplied with 2.
    int size;
    double (*weight)(struct filter_kernel *kernel, double x);
};

extern const struct filter_kernel mp_filter_kernels[];

struct filter_kernel *mp_find_filter_kernel(const char *name);
void mp_compute_weights(struct filter_kernel *filter, double f, float *out_w);
void mp_compute_lut(struct filter_kernel *filter, int count, float *out_array);


#endif /* MPLAYER_FILTER_KERNELS_H */
