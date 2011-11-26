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

// Note that this file is not directly passed as shader, but run through
// text2header, and in fact contains multiple vertex and fragment shaders.

#!section shader_prelude
// inserted at the beginning of all shaders
#version 140

#!section vertex_shader
uniform mat3 transform;

in vec2 vertex_position;
in vec4 vertex_color;
out vec4 color;
in vec2 vertex_texcoord;
out vec2 texcoord;

void main() {
    gl_Position = vec4(transform * vec3(vertex_position, 1), 1);
    color = vertex_color;
    texcoord = vertex_texcoord;
}

#!section frag_shader_eosd
uniform sampler2D texture1;

in vec2 texcoord;
in vec4 color;
out vec4 out_color;

void main() {
    out_color = vec4(color.rgb, texture2D(texture1, texcoord).a);
}

#!section frag_shader_osd
uniform sampler2D texture1;

in vec2 texcoord;
out vec4 out_color;

void main() {
    out_color = texture2D(texture1, texcoord).rrra;
}

#!section frag_shader_video
uniform sampler2D texture1;
uniform sampler2D texture2;
uniform sampler2D texture3;
uniform sampler1D texture_lanczos2_weights;
uniform sampler2D texture_lanczos3_weights;
uniform mat4x3 colormatrix;
uniform vec3 gamma;
uniform float filter_strength;

in vec2 texcoord;
out vec4 out_color;

vec4 sample_bilinear(sampler2D tex, vec2 texcoord) {
    return texture(tex, texcoord);
}

// Explanation how bicubic scaling with only 4 texel fetches is done:
//   http://www.mate.tue.nl/mate/pdfs/10318.pdf
//   'Efficient GPU-Based Texture Interpolation using Uniform B-Splines'
// Explanation why cubic scaling normally always blurs, even with unit scaling:
//   http://bigwww.epfl.ch/preprints/ruijters1001p.pdf
//   'GPU Prefilter for Accurate Cubic B-spline Interpolation'
vec4 calcweights(float s) {
    vec4 t = vec4(-0.5, 0.1666, 0.3333, -0.3333) * s + vec4(1, 0, -0.5, 0.5);
    t = t * s + vec4(0, 0, -0.5, 0.5);
    t = t * s + vec4(-0.6666, 0, 0.8333, 0.1666);
    vec2 a = vec2(1 / t.z, 1 / t.w);
    t.xy = t.xy * a + vec2(1, 1);
    t.x = t.x + s;
    t.y = t.y - s;
    return t;
}

vec4 sample_bicubic(sampler2D tex, vec2 texcoord) {
    vec2 texsize = textureSize(tex, 0);
    vec2 pt = 1 / texsize;
    vec2 fcoord = fract(texcoord * texsize + vec2(0.5, 0.5));
    vec4 parmx = calcweights(fcoord.x);
    vec4 parmy = calcweights(fcoord.y);
    vec4 cdelta;
    cdelta.xz = parmx.rg * vec2(-pt.x, pt.x);
    cdelta.yw = parmy.rg * vec2(-pt.y, pt.y);
    // first y-interpolation
    vec4 ar = texture(tex, texcoord + cdelta.xy);
    vec4 ag = texture(tex, texcoord + cdelta.xw);
    vec4 ab = mix(ag, ar, parmy.b);
    // second y-interpolation
    vec4 br = texture(tex, texcoord + cdelta.zy);
    vec4 bg = texture(tex, texcoord + cdelta.zw);
    vec4 aa = mix(bg, br, parmy.b);
    // x-interpolation
    return mix(aa, ab, parmx.b);
}

#define CONVOLUTION_N_TAPS(NAME, N)                                          \
    vec4 NAME(sampler2D tex, vec2 texcoord, vec2 pt, float taps_x[N],        \
              float taps_y[N]) {                                             \
        vec4 res = vec4(0);                                                  \
        for (int y = 0; y < N; y++) {                                        \
            vec4 line = vec4(0);                                             \
            for (int x = 0; x < N; x++)                                      \
                line += taps_x[x] * texture(tex, texcoord + pt * vec2(x, y));\
            res += taps_y[y] * line;                                         \
        }                                                                    \
        return res;                                                          \
    }

CONVOLUTION_N_TAPS(convolution4taps, 4)
CONVOLUTION_N_TAPS(convolution6taps, 6)

// (assumes -a < x < a, normally the function should be 0 otherwise)
float lanczos_L(float a, float x) {
    float pix = 3.1415926 * x;
    return (x == 0) ? 1 : (a * sin(pix) * sin(pix / a) / (pix * pix));
}

// Calculate the Lanczos filtering kernel for 4 sample points.
// f = x0 - abs(x0)
float[4] lanczos2_weights(float f) {
    float[4] taps;
    float sum = 0;
    float a = 2;
    for (int n = 0; n < 4; n++) {
        float x = f - (n - a + 1);
        taps[n] = lanczos_L(a, x);
        sum += taps[n];
    }
    //normalize
    for (int n = 0; n < 4; n++)
        taps[n] /= sum;
    return taps;
}

vec4 sample_lanczos2_nolookup(sampler2D tex, vec2 texcoord) {
    vec2 texsize = textureSize(tex, 0);
    vec2 pt = 1 / texsize;
    vec2 fcoord = fract(texcoord * texsize - 0.5);
    vec2 base = texcoord - fcoord * pt;
    return convolution4taps(tex, base - pt, pt,
                            lanczos2_weights(fcoord.x),
                            lanczos2_weights(fcoord.y));
}

float[4] lookup_weights4(sampler1D lookup, float f) {
    vec4 c = texture(lookup, f);
    return float[4](c.r, c.g, c.b, c.a);
}

vec4 sample_convolution4x4(sampler1D lookup, sampler2D tex, vec2 texcoord) {
    vec2 texsize = textureSize(tex, 0);
    vec2 pt = 1 / texsize;
    vec2 fcoord = fract(texcoord * texsize - 0.5);
    vec2 base = texcoord - fcoord * pt;
    return convolution4taps(tex, base - pt, pt,
                            lookup_weights4(lookup, fcoord.x),
                            lookup_weights4(lookup, fcoord.y));
}

vec4 sample_lanczos2(sampler2D tex, vec2 texcoord) {
    return sample_convolution4x4(texture_lanczos2_weights, tex, texcoord);
}

float[6] lookup_weights6(sampler2D lookup, float f) {
    vec4 c1 = texture(lookup, vec2(0.25, f));
    vec4 c2 = texture(lookup, vec2(0.75, f));
    return float[6](c1.r, c1.g, c1.b, c2.r, c2.g, c2.b);
}

vec4 sample_convolution6x6(sampler2D lookup, sampler2D tex, vec2 texcoord) {
    vec2 texsize = textureSize(tex, 0);
    vec2 pt = 1 / texsize;
    vec2 fcoord = fract(texcoord * texsize - 0.5);
    vec2 base = texcoord - fcoord * pt;
    return convolution6taps(tex, base - pt * 2, pt,
                            lookup_weights6(lookup, fcoord.x),
                            lookup_weights6(lookup, fcoord.y));
}

vec4 sample_lanczos3(sampler2D tex, vec2 texcoord) {
    return sample_convolution6x6(texture_lanczos3_weights, tex, texcoord);
}

vec4 sample_unsharp3x3(sampler2D tex, vec2 texcoord) {
    vec2 texsize = textureSize(tex, 0);
    vec2 pt = 1 / texsize;
    vec2 st = pt * 0.5;
    vec4 p = texture(tex, texcoord);
    vec4 sum = texture(tex, texcoord + st * vec2(+1, +1))
             + texture(tex, texcoord + st * vec2(+1, -1))
             + texture(tex, texcoord + st * vec2(-1, +1))
             + texture(tex, texcoord + st * vec2(-1, -1));
    return p + (p - 0.25 * sum) * filter_strength;
}

vec4 sample_unsharp5x5(sampler2D tex, vec2 texcoord) {
    vec2 texsize = textureSize(tex, 0);
    vec2 pt = 1 / texsize;
    vec2 st1 = pt * 1.2;
    vec4 p = texture(tex, texcoord);
    vec4 sum1 = texture(tex, texcoord + st1 * vec2(+1, +1))
              + texture(tex, texcoord + st1 * vec2(+1, -1))
              + texture(tex, texcoord + st1 * vec2(-1, +1))
              + texture(tex, texcoord + st1 * vec2(-1, -1));
    vec2 st2 = pt * 1.5;
    vec4 sum2 = texture(tex, texcoord + st2 * vec2(+1,  0))
              + texture(tex, texcoord + st2 * vec2( 0, +1))
              + texture(tex, texcoord + st2 * vec2(-1,  0))
              + texture(tex, texcoord + st2 * vec2( 0, -1));
    vec4 t = p * 0.859375 + sum2 * -0.1171875 + sum1 * -0.09765625;
    return p + t * filter_strength;
}

void main() {
#if USE_PLANAR
    vec3 color = vec3(SAMPLE_L(texture1, texcoord).r,
                      SAMPLE_C(texture2, texcoord).r,
                      SAMPLE_C(texture3, texcoord).r);
#else
    vec3 color = SAMPLE_L(texture1, texcoord).rgb;
#endif
#if USE_COLORMATRIX
    color = mat3(colormatrix) * color + colormatrix[3];
#endif
#if USE_GAMMA_POW
    color = pow(color, gamma);
#endif
    out_color = vec4(color, 1);
}

