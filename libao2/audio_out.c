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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "audio_out.h"

#include "mp_msg.h"

// there are some globals:
ao_data_t ao_data={0,0,0,0,OUTBURST,-1,0};
char *ao_subdevice = NULL;

extern const ao_functions_t audio_out_oss;
extern const ao_functions_t audio_out_coreaudio;
extern const ao_functions_t audio_out_arts;
extern const ao_functions_t audio_out_esd;
extern const ao_functions_t audio_out_pulse;
extern const ao_functions_t audio_out_jack;
extern const ao_functions_t audio_out_openal;
extern const ao_functions_t audio_out_null;
extern const ao_functions_t audio_out_alsa5;
extern const ao_functions_t audio_out_alsa;
extern const ao_functions_t audio_out_nas;
extern const ao_functions_t audio_out_sdl;
extern const ao_functions_t audio_out_sun;
extern const ao_functions_t audio_out_sgi;
extern const ao_functions_t audio_out_win32;
extern const ao_functions_t audio_out_dsound;
extern const ao_functions_t audio_out_kai;
extern const ao_functions_t audio_out_dart;
extern const ao_functions_t audio_out_ivtv;
extern const ao_functions_t audio_out_v4l2;
extern const ao_functions_t audio_out_mpegpes;
extern const ao_functions_t audio_out_pcm;
extern const ao_functions_t audio_out_pss;

const ao_functions_t* const audio_out_drivers[] =
{
// native:
#ifdef CONFIG_DIRECTX
        &audio_out_dsound,
#endif
#ifdef CONFIG_WIN32WAVEOUT
        &audio_out_win32,
#endif
#ifdef CONFIG_KAI
        &audio_out_kai,
#endif
#ifdef CONFIG_DART
        &audio_out_dart,
#endif
#ifdef CONFIG_COREAUDIO
        &audio_out_coreaudio,
#endif
#ifdef CONFIG_OSS_AUDIO
        &audio_out_oss,
#endif
#ifdef CONFIG_ALSA
        &audio_out_alsa,
#endif
#ifdef CONFIG_ALSA5
        &audio_out_alsa5,
#endif
#ifdef CONFIG_SGI_AUDIO
        &audio_out_sgi,
#endif
#ifdef CONFIG_SUN_AUDIO
        &audio_out_sun,
#endif
// wrappers:
#ifdef CONFIG_ARTS
        &audio_out_arts,
#endif
#ifdef CONFIG_ESD
        &audio_out_esd,
#endif
#ifdef CONFIG_PULSE
        &audio_out_pulse,
#endif
#ifdef CONFIG_JACK
        &audio_out_jack,
#endif
#ifdef CONFIG_NAS
        &audio_out_nas,
#endif
#ifdef CONFIG_SDL
        &audio_out_sdl,
#endif
#ifdef CONFIG_OPENAL
        &audio_out_openal,
#endif
        &audio_out_mpegpes,
#ifdef CONFIG_IVTV
        &audio_out_ivtv,
#endif
#ifdef CONFIG_V4L2_DECODER
        &audio_out_v4l2,
#endif
        &audio_out_null,
// should not be auto-selected:
        &audio_out_pcm,
        NULL
};

void list_audio_out(void){
    int i=0;
    mp_tmsg(MSGT_AO, MSGL_INFO, "Available audio output drivers:\n");
    mp_msg(MSGT_IDENTIFY, MSGL_INFO, "ID_AUDIO_OUTPUTS\n");
    while (audio_out_drivers[i]) {
        const ao_info_t *info = audio_out_drivers[i++]->info;
        mp_msg(MSGT_GLOBAL, MSGL_INFO,"\t%s\t%s\n", info->short_name, info->name);
    }
    mp_msg(MSGT_GLOBAL, MSGL_INFO,"\n");
}

const ao_functions_t* init_best_audio_out(char** ao_list,int use_plugin,int rate,int channels,int format,int flags){
    int i;
    // first try the preferred drivers, with their optional subdevice param:
    if(ao_list && ao_list[0])
      while(ao_list[0][0]){
        char* ao=ao_list[0];
        int ao_len;
        free(ao_subdevice);
        ao_subdevice = NULL;
        ao_subdevice=strchr(ao,':');
        if(ao_subdevice){
            ao_len = ao_subdevice - ao;
            ao_subdevice = strdup(&ao[ao_len + 1]);
        }
        else
            ao_len = strlen(ao);

        mp_tmsg(MSGT_AO, MSGL_V, "Trying preferred audio driver '%.*s', options '%s'\n",
               ao_len, ao, ao_subdevice ? ao_subdevice : "[none]");

        for(i=0;audio_out_drivers[i];i++){
            const ao_functions_t* audio_out=audio_out_drivers[i];
            if(!strncmp(audio_out->info->short_name,ao,ao_len)){
                // name matches, try it
                if(audio_out->init(rate,channels,format,flags))
                    return audio_out; // success!
                else
                    mp_tmsg(MSGT_AO, MSGL_WARN, "Failed to initialize audio driver '%s'\n", ao);
                break;
            }
        }
	if (!audio_out_drivers[i]) // we searched through the entire list
            mp_tmsg(MSGT_AO, MSGL_WARN, "No such audio driver '%.*s'\n", ao_len, ao);
        // continue...
        ++ao_list;
        if(!(ao_list[0])) return NULL; // do NOT fallback to others
      }
    free(ao_subdevice);
    ao_subdevice = NULL;

    mp_tmsg(MSGT_AO, MSGL_V, "Trying every known audio driver...\n");

    // now try the rest...
    for(i=0;audio_out_drivers[i];i++){
        const ao_functions_t* audio_out=audio_out_drivers[i];
//        if(audio_out->control(AOCONTROL_QUERY_FORMAT, (int)format) == CONTROL_TRUE)
        if(audio_out->init(rate,channels,format,flags))
            return audio_out; // success!
    }
    return NULL;
}
