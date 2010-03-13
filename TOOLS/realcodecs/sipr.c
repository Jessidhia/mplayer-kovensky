/*
 * This is a small DLL that works as a wrapper for the actual realsipr.so.6.0
 * DLL from RealPlayer 8.0.
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

/*
   Assuming that RACloseCodec is the last call.

0000000000001b8c g    DF .text	000000000000002b  G2          RASetPwd
0000000000001c6c g    DF .text	000000000000003e  G2          RAInitEncoder
0000000000001a8c g    DF .text	0000000000000039  G2          RAOpenCodec2
0000000000001a50 g    DF .text	0000000000000039  G2          RAOpenCodec
0000000000001ac8 g    DF .text	000000000000002c  G2          RACloseCodec
0000000000001af4 g    DF .text	000000000000000a  G2          RAGetNumberOfFlavors
0000000000001bdc g    DF .text	0000000000000032  G2          RADecode
0000000000001cac g    DF .text	0000000000000029  G2          RAEncode
0000000000001cd8 g    DF .text	0000000000000023  G2          RAFreeEncoder
000000000000138c g    DF .text	0000000000000029  G2          SetDLLAccessPath
0000000000001c48 g    DF .text	0000000000000023  G2          RAFreeDecoder
0000000000001c10 g    DF .text	0000000000000035  G2          RAFlush
0000000000001b00 g    DF .text	0000000000000052  G2          RAGetFlavorProperty
0000000000001b54 g    DF .text	0000000000000038  G2          RASetFlavor
0000000000001bb8 g    DF .text	0000000000000023  G2          RAInitDecoder

*/

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include <sys/time.h>

typedef unsigned long ulong;

ulong (*raCloseCodec)(ulong);
ulong (*raDecode)(ulong,ulong,ulong,ulong,ulong,ulong);
ulong (*raEncode)(ulong,ulong,ulong);
ulong (*raFlush)(ulong,ulong,ulong);
ulong (*raFreeDecoder)(ulong);
ulong (*raFreeEncoder)(ulong);
ulong (*raGetFlavorProperty)(ulong,ulong,ulong,ulong);
ulong (*raGetNumberOfFlavors)(void);
ulong (*raGetNumberOfFlavors2)(void);
ulong (*raInitDecoder)(ulong,ulong);
ulong (*raInitEncoder)(ulong,ulong);
ulong (*raOpenCodec)(ulong);
ulong (*raOpenCodec2)(ulong);
ulong (*raSetFlavor)(ulong,ulong,ulong);
void  (*raSetDLLAccessPath)(ulong);
/* the following symbol will be _dlsym()ed by rarender.so,
   but at least doesn't exist in cook.so
*/
ulong (*raSetPwd)(ulong,ulong);

int b_dlOpened=0;
void *handle=NULL;

/* exits program when failure */
void loadSyms(void) {
	fputs("loadSyms()\n", stderr);
	if (!b_dlOpened) {
		char *error;

//		fputs("opening dll...\n");
		handle = dlopen ("/usr/local/RealPlayer8/Codecs/realsipr.so.6.0", RTLD_LAZY);
		if (!handle) {
			fputs (dlerror(), stderr);
			exit(1);
		}

		raCloseCodec = dlsym(handle, "RACloseCodec");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RACloseCodec): %s\n", error);
			exit(1);
		}
		raDecode = dlsym(handle, "RADecode");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RADecode): %s\n", error);
			exit(1);
		}
		raEncode = dlsym(handle, "RAEncode");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAEncode): %s\n", error);
			exit(1);
		}
		raFlush = dlsym(handle, "RAFlush");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAFlush): %s\n", error);
			exit(1);
		}
		raFreeDecoder = dlsym(handle, "RAFreeDecoder");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAFreeDecoder): %s\n", error);
			exit(1);
		}
		raFreeEncoder = dlsym(handle, "RAFreeEncoder");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAFreeEncoder): %s\n", error);
			exit(1);
		}
		raGetFlavorProperty = dlsym(handle, "RAGetFlavorProperty");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAGetFlavorProperty): %s\n", error);
			exit(1);
		}
		raGetNumberOfFlavors = dlsym(handle, "RAGetNumberOfFlavors");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAGetNumberOfFlavors): %s\n", error);
			exit(1);
			}
//		raGetNumberOfFlavors2 = dlsym(handle, "RAGetNumberOfFlavors2");
//		if ((error = dlerror()) != NULL)  {
//			fprintf (stderr, "dlsym(RAGetNumberOfFlavors2): %s\n", error);
//			exit(1);
//		}
		raInitDecoder = dlsym(handle, "RAInitDecoder");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAInitDecoder): %s\n", error);
			exit(1);
		}
		raInitEncoder = dlsym(handle, "RAInitEncoder");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAInitEncoder): %s\n", error);
			exit(1);
		}
		raOpenCodec = dlsym(handle, "RAOpenCodec");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAOpenCodec): %s\n", error);
			exit(1);
		}
		raOpenCodec2 = dlsym(handle, "RAOpenCodec2");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RAOpenCodec2): %s\n", error);
			exit(1);
		}
		raSetFlavor = dlsym(handle, "RASetFlavor");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RASetFlavor): %s\n", error);
			exit(1);
		}
		raSetDLLAccessPath = dlsym(handle, "SetDLLAccessPath");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(SetDLLAccessPath): %s\n", error);
			exit(1);
		}
		raSetPwd = dlsym(handle, "RASetPwd");
		if ((error = dlerror()) != NULL)  {
			fprintf (stderr, "dlsym(RASetPwd): %s\n", error);
			exit(1);
		}
		b_dlOpened=1;
	}
}

void closeDll(void) {
	if (handle) {
		b_dlOpened=0;
		dlclose(handle);
		handle=NULL;
	}
}

void _init(void) {
	loadSyms();
}

struct timezone tz;
struct timeval tv1, tv2;

void tic(void) {
	gettimeofday(&tv1, &tz);
}

void toc(void) {
	long secs, usecs;
	gettimeofday(&tv2, &tz);
	secs=tv2.tv_sec-tv1.tv_sec;
	usecs=tv2.tv_usec-tv1.tv_usec;
	if (usecs<0) {
		usecs+=1000000;
		--secs;
	}
	fprintf(stderr, "Duration: %ld.%0.6lds\n", secs, usecs);
}


void hexdump(void *pos, int len) {
	unsigned char *cpos=pos, *cpos1;
	int lines=(len+15)>>4;
	while(lines--) {
		int len1=len, i;
		fprintf(stderr, "%0x  ", cpos);
		cpos1=cpos;
		for (i=0;i<16;i++) {
			if (len1>0) {
				fprintf(stderr, "%02x ", *(cpos++));
			} else {
				fprintf(stderr, "   ");
			}
			len1--;
		}
		fputs("  ", stderr);
		cpos=cpos1;
		for (i=0;i<16;i++) {
			if (len>0) {
				unsigned char ch=(*(cpos++));
				if ((ch<32)||(ch>127)) ch='.';
				fputc(ch, stderr);
			}
			len--;
		}
		fputs("\n", stderr);
	}
	fputc('\n', stderr);
}


ulong RACloseCodec(ulong p1) {
	ulong result;
	fprintf(stderr, "RACloseCodec(ulong p1=0x%0lx(%ld))\n", p1, p1);
	result=(*raCloseCodec)(p1);
//	closeDll();
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

static int pkno=0;

ulong RADecode(ulong p1,ulong p2,ulong p3,ulong p4,ulong* p5,ulong p6) {
	ulong result;
	int x,y;

	fprintf(stderr, "RADecode(ulong ctx=0x%0lx, ", p1);
	fprintf(stderr, "ulong src=0x%0lx,\n", p2);
	fprintf(stderr, "ulong len=0x%0lx,", p3);
	fprintf(stderr, "ulong dst=0x%0lx,\n", p4);
	fprintf(stderr, "ulong dstcnt=0x%0x, ",p5);
	fprintf(stderr, "ulong p6=%ld)\n", p6);
//	hexdump((void*)p1, 44);
//	hexdump((void*)p2, p3);
//	hexdump((void*)p4, 80);
//	hexdump((void*)p5, 16);
//	tic();

#if 0
	hexdump(p2, 0x128);
#else
	    fprintf(stderr,"\n#CRC[%3d]",pkno++);
	for(y=0;y<(0x128*2);y+=37){
	    unsigned short crc=0;
	    unsigned char* p=p2;
//	    p+=y;
	    for(x=0;x<37;x++){
		int i=y+x;
		int ib=p[(i>>1)];
		if(i&1) ib>>=4; else ib&=15;
		crc+=ib<<(x&7);
	    }
	    fprintf(stderr," %04X",crc);
//	    fprintf(stderr," %02X",p[0]);
	}
	    fprintf(stderr,"\n");
#endif

	{ FILE *f=fopen("sipr.dump","a");
	  fwrite(p2,p3,1,f);
	  fclose(f);
	}

	result=(*raDecode)(p1,p2,p3,p4,p5,p6);
//	toc();
//	hexdump((void*)p1, 44);
//	hexdump((void*)p4, 80);
//	hexdump((void*)p5, 16);
	fprintf(stderr, "--> 0x%0lx(%ld)  decoded: %ld  \n\n\n", result, result, p5[0]);
	return result;
}

ulong RAEncode(ulong p1,ulong p2,ulong p3) {
	ulong result;
	fprintf(stderr, "RAEncode(ulong p1=0x%0lx(%ld), ", p1, p1);
	fprintf(stderr, "ulong p2=0x%0lx(%ld),\n", p2, p2);
	fprintf(stderr, "ulong p3=0x%0lx(%ld))\n", p3, p3);
	result=(*raEncode)(p1,p2,p3);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAFlush(ulong p1,ulong p2,ulong p3) {
	ulong result;
	fprintf(stderr, "RAFlush(ulong p1=0x%0lx(%ld), ", p1, p1);
	fprintf(stderr, "ulong p2=0x%0lx(%ld),\n", p2, p2);
	fprintf(stderr, "ulong p3=0x%0lx(%ld))\n", p3, p3);
	result=(*raFlush)(p1,p2,p3);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAFreeDecoder(ulong p1) {
	ulong result;
	fprintf(stderr, "RAFreeDecoder(ulong p1=0x%0lx(%ld))\n", p1, p1);
	hexdump((void*)p1, 44);
	result=(*raFreeDecoder)(p1);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAFreeEncoder(ulong p1) {
	ulong result;
	fprintf(stderr, "RAFreeEncoder(ulong p1=0x%0lx(%ld))\n", p1, p1);
	result=(*raFreeEncoder)(p1);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAGetFlavorProperty(ulong p1,ulong p2,ulong p3, ulong p4) {
	ulong result;
	fprintf(stderr, "RAGetFlavorProperty(ulong p1=0x%0lx(%ld), ", p1, p1);
	fprintf(stderr, "ulong p2=0x%0lx(%ld),\n", p2, p2);
	fprintf(stderr, "ulong p3=0x%0lx(%ld), ", p3, p3);
	fprintf(stderr, "ulong p4=0x%0lx(%ld))\n", p4, p4);
	hexdump((void*)p4/*(void*)(*((void**)p4))*/,p2);
	hexdump((void*)p1, 44);
	tic();
	result=(*raGetFlavorProperty)(p1,p2,p3,p4);
	toc();
	fprintf(stderr, "*p4=0x%0lx\n", *((ulong*)p4));
	hexdump((void*)p4/*(void*)(*((void**)p4))*/,p2);
	hexdump((void*)p1, 44);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAGetNumberOfFlavors(void) {
	ulong result;
	fprintf(stderr, "RAGetNumberOfFlavors(void)\n");
	result=(*raGetNumberOfFlavors)();
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAInitDecoder(ulong p1,ulong p2) {
	ulong result;
//	int temp[256];
//	unsigned char temp2[256];
	fprintf(stderr, "RAInitDecoder(ulong p1=0x%0lx(%ld), ", p1, p1);
	fprintf(stderr, "ulong p2=0x%0lx(%ld))\n", p2, p2);
	hexdump((void*)p2, 4*7);
//	hexdump((void*)p1, 44);

#if 1
	result=(*raInitDecoder)(p1,p2);
#else
	memset(temp,0x77,256*4);
	memcpy(temp,p2,4*7);
	hexdump((void*)temp[6], 32);

	memset(temp2,0x77,256);
	memcpy(temp2,temp[6],16);
	temp[6]=temp2;

	result=(*raInitDecoder)(p1,temp);
	hexdump((void*)temp[6], 32);
#endif

//	memcpy(p2,temp,4*11);
//	hexdump((void*)p1, 44);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAInitEncoder(ulong p1,ulong p2) {
	ulong result;
	fprintf(stderr, "RAInitEncoder(ulong p1=0x%0lx(%ld), ", p1, p1);
	fprintf(stderr, "ulong p2=0x%0lx(%ld))\n", p2, p2);
	result=(*raInitEncoder)(p1,p2);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAOpenCodec(ulong p1) {
	ulong result;
//	loadSyms();
	fprintf(stderr, "RAOpenCodec(ulong p1=0x%0lx(%ld))\n", p1, p1);
	result=(*raOpenCodec)(p1);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RAOpenCodec2(ulong p1) {
	ulong result;
//	loadSyms();
	fprintf(stderr, "RAOpenCodec2(ulong p1=0x%0lx(%ld))\n", p1, p1);
	hexdump((void*)p1, 44);
	result=(*raOpenCodec2)(p1);
	hexdump((void*)p1, 44);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);
	return result;
}

ulong RASetFlavor(ulong p1,ulong p2,ulong p3) {
	ulong result, numprop=0, result1=0;
	ulong numflavors, flavor;
	unsigned short property;
	fprintf(stderr, "RASetFlavor(ulong p1=0x%0lx(%ld), ", p1, p1);
	fprintf(stderr, "ulong p2=0x%0lx(%ld))\n", p2, p2);
//	hexdump((void*)p1, 44);
//	hexdump((void*)p1, 44);
	result=(*raSetFlavor)(p1,p2,p3);
	fprintf(stderr, "--> 0x%0lx(%ld)\n\n\n", result, result);

	fputs("######################## FLAVOR PROPERTIES ###################\n\n", stderr);
	numflavors=3;//raGetNumberOfFlavors();
	flavor=0;
	while (flavor<numflavors) {
		fprintf(stderr, "************ Flavor %ld *************\n\n", flavor);
		numprop=0;
		while (numprop<32) {
			result1=raGetFlavorProperty(p1, flavor, numprop, (ulong)&property);
			fprintf(stderr, "property %ld=%d, result=0x%0lx\n\n",
				numprop, property, result1);
			hexdump((void*)result1, property);
			numprop++;
		}
		flavor++;
	}

	fputs("######################## FLAVOR PROPERTIES ###################\n\n", stderr);

	return result;
}

void  SetDLLAccessPath(ulong p1,ulong p2) {
//	loadSyms();
	char* ize=p1;
	fprintf(stderr, "SetDLLAccessPath(ulong p1=0x%0lx(%ld) p2=%p)\n", p1, p1,p2);
	while(*ize){
	    fprintf(stderr,"%s\n",ize);
	    ize+=strlen(ize)+1;
	}
	//hexdump((void*)p1, 244);
	(*raSetDLLAccessPath)(p1);
//	hexdump((void*)p1, 44);
	fprintf(stderr, "--> void\n\n\n");
}

static char pwdtemp[1000];

void  RASetPwd(ulong p1,ulong p2) {
//	loadSyms();
	fprintf(stderr, "RASetPwd(ulong p1=0x%0lx(%ld),ulong p2=0x%0lx(%ld))\n", p1, p1, p2, p2);
//	hexdump((void*)p1, 44);
//	hexdump((void*)p2, 44);
	memset(pwdtemp,0x77,1000);
	hexdump((void*)pwdtemp, 44);
	(*raSetPwd)(pwdtemp,"Ardubancel Quazanga"); // set password... lol.
	hexdump((void*)pwdtemp, 1000);
	strcpy(p1,p2);
//	hexdump((void*)p1, 44);
//	hexdump((void*)p2, 44);
//	hexdump((void*)p1, 44);
	fprintf(stderr, "--> void\n\n\n");
}
