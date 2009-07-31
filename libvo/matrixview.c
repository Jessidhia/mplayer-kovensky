/*
 * Copyright (C) 2003 Alex Zolotov <nightradio@knoppix.ru>
 * Mucked with by Tugrul Galatali <tugrul@galatali.com>
 *
 * MatrixView is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 2 as 
 * published by the Free Software Foundation.
 *
 * MatrixView is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/**
 * Ported to an MPlayer video out plugin by Pigeon <pigeon at pigeond.net>
 * August 2006
 */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <GL/gl.h>

#ifdef _WIN32
#define random rand
#endif

extern unsigned char matrixview_font[];
extern int matrixview_font_w;
extern int matrixview_font_h;
extern int matrixview_font_size;

#include "matrixview.h"

static float matrix_contrast = 1.5;
static float matrix_brightness = 1.0;

// Settings for our light.  Try playing with these (or add more lights).
static float Light_Ambient[] = { 0.1f, 0.1f, 0.1f, 1.0f };
static float Light_Diffuse[] = { 1.2f, 1.2f, 1.2f, 1.0f };
static float Light_Position[] = { 2.0f, 2.0f, 0.0f, 1.0f };

static unsigned char *font = NULL;

static unsigned char flare[16] = {
        0, 0, 0, 0,
	0, 180, 0, 0,
	0, 0, 0, 0,
	0, 0, 0, 0
};

#define MAX_TEXT_X 320
#define MAX_TEXT_Y 240
static int text_x = 0;
static int text_y = 0;
#define _text_x text_x/2
#define _text_y text_y/2

// Scene position
#define Z_Off -128.0f
#define Z_Depth 8

static unsigned char speed[MAX_TEXT_X];
static unsigned char text[MAX_TEXT_X * MAX_TEXT_Y + MAX_TEXT_X];
static unsigned char text_light[MAX_TEXT_X * MAX_TEXT_Y + MAX_TEXT_X];	//255 - white; 254 - none;
static float text_depth[MAX_TEXT_X * MAX_TEXT_Y + MAX_TEXT_X];

static unsigned char *pic = NULL;
static float bump_pic[MAX_TEXT_X * MAX_TEXT_Y + MAX_TEXT_X];

static void draw_char (long num, float light, float x, float y, float z)
{
	float tx, ty;
	long num2, num3;

	num &= 63;
	//light = light / 255;	//light=7-light;num+=(light*60);
	light = light / 255 * matrix_brightness;
	num2 = num / 10;
	num3 = num - (num2 * 10);
	ty = (float)num2 / 7;
	tx = (float)num3 / 10;
	glNormal3f (0.0f, 0.0f, 1.0f);	// Needed for lighting
	glColor4f (0.9, 0.4, 0.3, light);	// Basic polygon color

	glTexCoord2f (tx, ty);
	glVertex3f (x, y, z);
	glTexCoord2f (tx + 0.1, ty);
	glVertex3f (x + 1, y, z);
	glTexCoord2f (tx + 0.1, ty + 0.166);
	glVertex3f (x + 1, y - 1, z);
	glTexCoord2f (tx, ty + 0.166);
	glVertex3f (x, y - 1, z);
}

static void draw_illuminatedchar (long num, float x, float y, float z)
{
	float tx, ty;
	long num2, num3;

	num2 = num / 10;
	num3 = num - (num2 * 10);
	ty = (float)num2 / 7;
	tx = (float)num3 / 10;
	glNormal3f (0.0f, 0.0f, 1.0f);	// Needed for lighting
	glColor4f (0.9, 0.4, 0.3, .5);	// Basic polygon color

	glTexCoord2f (tx, ty);
	glVertex3f (x, y, z);
	glTexCoord2f (tx + 0.1, ty);
	glVertex3f (x + 1, y, z);
	glTexCoord2f (tx + 0.1, ty + 0.166);
	glVertex3f (x + 1, y - 1, z);
	glTexCoord2f (tx, ty + 0.166);
	glVertex3f (x, y - 1, z);
}

static void draw_flare (float x, float y, float z)	//flare
{
	glNormal3f (0.0f, 0.0f, 1.0f);	// Needed for lighting
	glColor4f (0.9, 0.4, 0.3, .8);	// Basic polygon color

	glTexCoord2f (0, 0);
	glVertex3f (x - 1, y + 1, z);
	glTexCoord2f (0.75, 0);
	glVertex3f (x + 2, y + 1, z);
	glTexCoord2f (0.75, 0.75);
	glVertex3f (x + 2, y - 2, z);
	glTexCoord2f (0, 0.75);
	glVertex3f (x - 1, y - 2, z);
}

static void draw_text ()
{
	int x, y;
	long p = 0;
	int c, c_pic;
        int pic_fade = 255;

	for (y = _text_y; y > -_text_y; y--) {
		for (x = -_text_x; x < _text_x; x++) {
			c = text_light[p] - (text[p] >> 1);
			c += pic_fade;
			if (c > 255)
				c = 255;

			if (pic) {

                                // Original code
				//c_pic = pic[p] * matrix_contrast - (255 - pic_fade);
                                
				c_pic = (255 - pic[p]) * matrix_contrast - (255 - pic_fade);

				if (c_pic < 0)
					c_pic = 0;

				c -= c_pic;

				if (c < 0)
					c = 0;

				bump_pic[p] = (255.0f - c_pic) / (256 / Z_Depth);
			} else {
				bump_pic[p] = Z_Depth;
			}

			if (c > 10)
				if (text[p])
					draw_char (text[p] + 1, c, x, y, text_depth[p] + bump_pic[p]);

			if (text_depth[p] < 0.1)
				text_depth[p] = 0;
			else
				text_depth[p] /= 1.1;

			p++;
		}
	}
}

static void draw_illuminatedtext (void)
{
	float x, y;
	long p = 0;

	for (y = _text_y; y > -_text_y; y--) {
		for (x = -_text_x; x < _text_x; x++) {
			if (text_light[p] > 128)
				if (text_light[p + text_x] < 10)
					draw_illuminatedchar (text[p] + 1, x, y, text_depth[p] + bump_pic[p]);
			p++;
		}
	}
}

static void draw_flares (void)
{
	float x, y;
	long p = 0;

	for (y = _text_y; y > -_text_y; y--) {
		for (x = -_text_x; x < _text_x; x++) {
			if (text_light[p] > 128)
				if (text_light[p + text_x] < 10)
					draw_flare (x, y, text_depth[p] + bump_pic[p]);
			p++;
		}
	}
}

static void scroll (double dCurrentTime)
{
	int a, s, polovina;
	//static double dLastCycle = -1;
	static double dLastMove = -1;

	if (dCurrentTime - dLastMove > 1.0 / (text_y / 1.5)) {
		dLastMove = dCurrentTime;

		polovina = (text_x * text_y) / 2;
		s = 0;
		for (a = (text_x * text_y) + text_x - 1; a > text_x; a--) {
			if (speed[s]) {
				text_light[a] = text_light[a - text_x];	//scroll light table down
			}
			s++;
			if (s >= text_x)
				s = 0;
		}

		//============================
		//for (a = (text_x * text_y) + text_x - 1; a > text_x; a--) {
		//      text_light[a] = text_light[a - text_x]; //scroll light table down
		//}
		memmove ((void *)(&text_light[0] + text_x), (void *)&text_light, (text_x * text_y) - 1);

		//for (a = 0; a < text_x; a++)
		//      text_light[a] = 253;    //clear top line (in light table)
		memset ((void *)&text_light, 253, text_x);

		s = 0;
		for (a = polovina; a < (text_x * text_y); a++) {
			if (text_light[a] == 255)
				text_light[s] = text_light[s + text_x] >> 1;	//make black bugs in top line

			s++;

			if (s >= text_x)
				s = 0;
		}
	}
}

static void make_change (double dCurrentTime)
{
	int r = random () & 0xFFFF;

	r >>= 3;
	if (r < (text_x * text_y))
		text[r] += 133;	//random bugs

	r = random () & 0xFFFF;
	r >>= 7;
	if (r < text_x)
		if (text_light[r] != 0)
			text_light[r] = 255;	//white bugs

	scroll (dCurrentTime);
}


static void load_texture ()
{
	long a;

        font = matrixview_font;

	for (a = 0; a < matrixview_font_size; a++) {
		if ((a >> 9) & 2) {
			//font[a] = font[a];
                } else {
			font[a] = font[a] >> 1;
                }
	}
}

static void make_text ()
{
	long a;
	unsigned int r;

	for (a = 0; a < (text_x * text_y); a++) {
		r = random () & 0xFFFF;
		text[a] = r;
	}

	for (a = 0; a < text_x; a++)
		speed[a] = random () & 1;
}

static void ourBuildTextures ()
{

	glBindTexture (GL_TEXTURE_2D, 1);
        glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, matrixview_font_w, matrixview_font_h, 0, GL_GREEN, GL_UNSIGNED_BYTE, font);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	glBindTexture (GL_TEXTURE_2D, 2);
        glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, matrixview_font_w, matrixview_font_h, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, font);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	glBindTexture (GL_TEXTURE_2D, 3);
        glTexImage2D (GL_TEXTURE_2D, 0, GL_RGBA8, 4, 4, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, flare);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	// Some pretty standard settings for wrapping and filtering.
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf (GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	// We start with GL_DECAL mode.
	glTexEnvf (GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
}

void matrixview_init (int w, int h)
{
        if (font == NULL) {
            load_texture ();
        }
	make_text ();

	ourBuildTextures ();

	// Color to clear color buffer to.
	glClearColor (0.0f, 0.0f, 0.0f, 0.0f);

	// Depth to clear depth buffer to; type of test.
	glClearDepth (1.0);
	glDepthFunc (GL_LESS);

	// Enables Smooth Color Shading; try GL_FLAT for (lack of) fun.
	glShadeModel (GL_SMOOTH);

	// Set up a light, turn it on.
	glLightfv (GL_LIGHT1, GL_POSITION, Light_Position);
	glLightfv (GL_LIGHT1, GL_AMBIENT, Light_Ambient);
	glLightfv (GL_LIGHT1, GL_DIFFUSE, Light_Diffuse);
	glEnable (GL_LIGHT1);

	// A handy trick -- have surface material mirror the color.
	glColorMaterial (GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable (GL_COLOR_MATERIAL);

	matrixview_reshape (w, h);
}


void matrixview_reshape (int w, int h)
{
	glViewport (0, 0, w, h);

	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	glFrustum(-_text_x, _text_x, -_text_y, _text_y, -Z_Off - Z_Depth, -Z_Off);

	glMatrixMode (GL_MODELVIEW);
}


void matrixview_draw (int w, int h, double currentTime, float frameTime,
        unsigned char *data)
{
        pic = data;

	glBindTexture (GL_TEXTURE_2D, 1);
	glEnable (GL_BLEND);
	glEnable (GL_TEXTURE_2D);

	glDisable (GL_LIGHTING);
	glBlendFunc (GL_SRC_ALPHA, GL_ONE);
	glDisable (GL_DEPTH_TEST);

	glMatrixMode (GL_MODELVIEW);
	glLoadIdentity ();
	glTranslatef (0.0f, 0.0f, Z_Off);

	// Clear the color and depth buffers.
	glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// OK, let's start drawing our planer quads.
	glBegin (GL_QUADS);
	draw_text ();
	glEnd ();

	glBindTexture (GL_TEXTURE_2D, 2);
	glBegin (GL_QUADS);
	draw_illuminatedtext ();
	glEnd ();

	glBindTexture (GL_TEXTURE_2D, 3);
	glBegin (GL_QUADS);
	draw_flares ();
	glEnd ();

	make_change (currentTime);

	glLoadIdentity ();
	glMatrixMode (GL_PROJECTION);

}

void matrixview_contrast_set(float contrast)
{
    matrix_contrast = contrast;
}

void matrixview_brightness_set(float brightness)
{
    matrix_brightness = brightness;
}


void matrixview_matrix_resize(int w, int h)
{
    if(w > MAX_TEXT_X)
    {
        w = MAX_TEXT_X;
    }
    if(h > MAX_TEXT_Y)
    {
        h = MAX_TEXT_Y;
    }
    text_x = w;
    text_y = h;
    make_text();
}

