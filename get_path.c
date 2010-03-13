/*
 * Get path to config dir/file.
 *
 * Return Values:
 *   Returns the pointer to the ALLOCATED buffer containing the
 *   zero terminated path string. This buffer has to be FREED
 *   by the caller.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config.h"
#include "mp_msg.h"
#include "get_path.h"

#ifdef CONFIG_MACOSX_BUNDLE
#include <CoreFoundation/CoreFoundation.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#elif defined(_WIN32)
#define _WIN32_IE 0x0400
#include <windows.h>
#include <shlobj.h>
#include <sys/types.h>
#include <sys/stat.h>
#if defined(__CYGWIN__)
#include <sys/cygwin.h>
#endif
#elif defined(__OS2__)
#define INCL_DOS
#include <os2.h>
#endif

char *get_path(const char *filename){
	char *homedir;
	char *buff;
#ifdef _WIN32
	static char *config_dir = "/mplayer";
#else
	static char *config_dir = "/.mplayer";
#endif
	int len;
#ifdef CONFIG_MACOSX_BUNDLE
	struct stat dummy;
	CFIndex maxlen=256;
	CFURLRef res_url_ref=NULL;
	CFURLRef bdl_url_ref=NULL;
	char *res_url_path = NULL;
	char *bdl_url_path = NULL;
#endif

	if ((homedir = getenv("MPLAYER_HOME")) != NULL)
		config_dir = "";
	else if ((homedir = getenv("HOME")) == NULL)
#if _WIN32
	/* Hack to get fonts etc. loaded outside of Cygwin environment. */
	{
		int i,imax=0;
		struct _stat statBuffer;
		char exedir[4096], config_path[4096], appdata[4096];
		if (SHGetSpecialFolderPathA(NULL, appdata, CSIDL_APPDATA, 1)) {
			strncpy(exedir, appdata, strlen(appdata));
		} else {
			GetModuleFileNameA(NULL, exedir, 4096);
		}
		int len = strlen(exedir);
		for (i=0; i< len; i++)
			if (exedir[i] =='\\')
				exedir[i]='/';
		
		sprintf(config_path, "%s%s", exedir, config_dir);
		int stat_res;
		if ((stat_res = stat(config_path, &statBuffer)) < 0 ||
				!(statBuffer.st_mode & S_IFDIR)) {
			GetModuleFileNameA(NULL, exedir, 4096);
			len = strlen(exedir);
			for (i=0; i< len; i++)
				if (exedir[i] =='\\')
					{exedir[i]='/'; imax=i;}
			exedir[imax]='\0';
		}
		
		homedir = exedir;
	}
#elif defined(__OS2__)
    {
        PPIB ppib;
        char path[260];

        // Get process info blocks
        DosGetInfoBlocks(NULL, &ppib);

        // Get full path of the executable
        DosQueryModuleName(ppib->pib_hmte, sizeof( path ), path);

        // Truncate name part including last backslash
        *strrchr(path, '\\') = 0;

        // Convert backslash to slash
        _fnslashify(path);

        homedir = path;
    }
#else
	return NULL;
#endif
	len = strlen(homedir) + strlen(config_dir) + 1;
	if (filename == NULL) {
		if ((buff = malloc(len)) == NULL)
			return NULL;
		sprintf(buff, "%s%s", homedir, config_dir);
	} else {
		len += strlen(filename) + 1;
		if ((buff = malloc(len)) == NULL)
			return NULL;
		sprintf(buff, "%s%s/%s", homedir, config_dir, filename);
	}

#ifdef CONFIG_MACOSX_BUNDLE
	if (stat(buff, &dummy)) {

		res_url_ref=CFBundleCopyResourcesDirectoryURL(CFBundleGetMainBundle());
		bdl_url_ref=CFBundleCopyBundleURL(CFBundleGetMainBundle());

		if (res_url_ref&&bdl_url_ref) {

			res_url_path=malloc(maxlen);
			bdl_url_path=malloc(maxlen);

			while (!CFURLGetFileSystemRepresentation(res_url_ref, true, res_url_path, maxlen)) {
				maxlen*=2;
				res_url_path=realloc(res_url_path, maxlen);
			}
			CFRelease(res_url_ref);

			while (!CFURLGetFileSystemRepresentation(bdl_url_ref, true, bdl_url_path, maxlen)) {
				maxlen*=2;
				bdl_url_path=realloc(bdl_url_path, maxlen);
			}
			CFRelease(bdl_url_ref);

			if (strcmp(res_url_path, bdl_url_path) == 0)
				res_url_path = NULL;
		}

		if (res_url_path&&filename) {
			if ((strlen(filename)+strlen(res_url_path)+2)>maxlen) {
				maxlen=strlen(filename)+strlen(res_url_path)+2;
			}
			free(buff);
			buff = malloc(maxlen);
			strcpy(buff, res_url_path);

			strcat(buff,"/");
			strcat(buff, filename);
		}
	}
#endif
	mp_msg(MSGT_GLOBAL,MSGL_V,"get_path('%s') -> '%s'\n",filename,buff);
	return buff;
}

#if (defined(__MINGW32__) || defined(__CYGWIN__)) && defined(CONFIG_WIN32DLL)
void set_path_env(void)
{
	/*make our codec dirs available for LoadLibraryA()*/
	char tmppath[MAX_PATH*2 + 1];
	char win32path[MAX_PATH];
	char realpath[MAX_PATH];
#ifdef __CYGWIN__
	cygwin_conv_to_full_win32_path(WIN32_PATH,win32path);
	strcpy(tmppath,win32path);
#ifdef CONFIG_REALCODECS
	cygwin_conv_to_full_win32_path(REALCODEC_PATH,realpath);
	sprintf(tmppath,"%s;%s",win32path,realpath);
#endif /*CONFIG_REALCODECS*/
#else /*__CYGWIN__*/
	/* Expand to absolute path unless it's already absolute */
	if (!strstr(WIN32_PATH,":") && WIN32_PATH[0] != '\\'){
		GetModuleFileNameA(NULL, win32path, MAX_PATH);
		strcpy(strrchr(win32path, '\\') + 1, WIN32_PATH);
	}
	else strcpy(win32path,WIN32_PATH);
	strcpy(tmppath,win32path);
#ifdef CONFIG_REALCODECS
	/* Expand to absolute path unless it's already absolute */
	if (!strstr(REALCODEC_PATH,":") && REALCODEC_PATH[0] != '\\'){
		GetModuleFileNameA(NULL, realpath, MAX_PATH);
		strcpy(strrchr(realpath, '\\') + 1, REALCODEC_PATH);
	}
	else strcpy(realpath,REALCODEC_PATH);
	sprintf(tmppath,"%s;%s",win32path,realpath);
#endif /*CONFIG_REALCODECS*/
#endif /*__CYGWIN__*/
	mp_msg(MSGT_WIN32, MSGL_V,"Setting PATH to %s\n",tmppath);
	if (!SetEnvironmentVariableA("PATH", tmppath))
		mp_msg(MSGT_WIN32, MSGL_WARN, "Cannot set PATH!");
}
#endif /* (defined(__MINGW32__) || defined(__CYGWIN__)) && defined(CONFIG_WIN32DLL) */
