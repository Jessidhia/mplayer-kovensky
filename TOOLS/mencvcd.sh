#!/bin/bash
#
# Version:          0.2.2
#
# Licence:          GPL
#
# 2002/09/21        Jürgen Hammelmann <juergen.hammelmann@gmx.de>
#
# Script:           MPlayer Sources (DVD) to (S)VCD ripping and burning
#
# requires:         mplayer
#                   mjpegtools
#                   vcdimager
#                   cdrdao
#                   lame    (optionally)
#                   toolame (optionally)
#                   sox     (optionally)

for exe in mplayer mpeg2enc mp2enc mplex yuvscaler ; do
	if [ -z "`which $exe`" ]; then
		echo "ERROR: $exe must be in your path $PATH!"
		exit 1
	fi
done
for exe in vcdimager lame cdrdao yuvdenoise sox toolame normalize; do
        if [ -z "`which $exe`" ]; then
                echo "WARNING: $exe is not found in your path $PATH!"
	fi
done

################################################################################
#
# 2002/04/11        v0.1.0: first version
# 2002/04/12        v0.1.1:
# 2002/04/14        v0.1.2:
#                     - handles now multiple vcd's
#                     - support's mp3 audio as option
#                     - use of mp2enc/lame instead of toolame because of support
#                       of resampling and mp3
# 2002/04/16        v0.1.3:
#                     - new option "-burnonly"
#                     - new option "-vbr"
#                     - removes mpg file after mastering cd image
#                       (mplayer can read bin images!)
#                     - bugfixes
# 2002/04/22        v0.1.4
#                     - bugfixes / changes
#                     - more options
# 2002/05/06        v0.1.5
#                     - new option "-norm"
# 2002/05/08        v0.1.6
#                     - bugfixes
#                     - new option "-vfr"
#                     - "-norm" renamed to "-vnorm"
# 2002/05/15        v0.1.7
#                     - bugfixes
#                     - more help
# 2002/05/26        v0.1.8
#                     - new option "-sync"
# 2002/06/19        v0.1.9
#                     - new option "-mpgonly"
# 2002/09/21        v0.1.10
#                     - bug fixes
#                     - tests now, if the commands are in path and give warnings
#                       and errors.
# 2002/11/01        v0.2
#                     - bug fixes
#                     - new options: "-sox", "-volume", "-toolame"
#                     - renamed "-mpg" to "-mpegsrc"
#                     - default cd size changed to 800mb
# 2002/11/18        v0.2.1
#                     - aspect ratio for vcd's, too
#                     - pulldown mode...
# 2002/12/01        v0.2.2
#                     - -normalize
#
################################################################################
#
# global config section, change them to your needs!

TMPDIR="." # path to directory for creating temporary files, recommended 2-3GB space

CDDRV="generic-mmc"    # cdrdao: cdwriter driver
CDDEV="--device 0,1,0" # or comment out and create link /dev/cdrecorder
                       # to your cdwriter dev
CDMAXSIZE=800

################################################################################
AUDIO="audiodump.wav"
VIDEO="stream.yuv"
VCDMODE=2
SVCDMODE=5
################################################################################

usage() {
   echo "Usage: $HOWCALLED <basename> [$HOWCALLED options] [mplayer options]"
   echo
   echo "   <basename> is used as basename of the resulting mpg/bin files."
   echo
   echo "For mplayer options see mplayer help and manual."
   echo
   echo "$HOWCALLED options:"
   echo
   echo "-?|-h           help."
   echo "-a <n>          sets the aspect ratio (for SVCD):"
   echo "                1 - 1:1 display"
   echo "                2 - 4:3 display"
   echo "                3 - 16:9 display"
   echo "                4 - 2.21:1 display"
   echo "-abr <n>        output audio bitrate in kbs [224]."
   echo "-asr <n>        output audio sample rate in Hz [44100]."
   echo "-blank          cleans cd-rw before burning."
   echo "-burnonly       burn only the premastered <name>*.cue/<name>*.bin images."
   echo "-cdsize <n>     maximal size of cd images [800 = 80min raw cd]."
   echo "-denoise        denoises mpeg stream."
   echo "-mp3            outputs audio in mp3 instead of mp2 format."
   echo "-mpegsrc        don't encode from source, multiplex/burn the "
   echo "                encoded mpg stream."
   echo "-mpgonly        do only encoding to mpeg 1/2 format."
   echo "-noburn         disables burning."
   echo "-normalize      use 'normalize'."
   echo "-overburn       enables overburning a cd."
   echo "-pulldown       enable pulldown mode in output."
   echo "-ratio <s>      output ratio size of frames, see yuvscaler (1)."
   echo "-size <X>x<Y>   sets output size of frames."
   echo "-sox            use sox for audio resampling."
   echo "-svcdout        encode to SVCD format [VCD default]."
   echo "-sync <n>       set the presentation timestamp offset of video"
   echo "                stream w.r.t. audio stream (video-audio) in mSec."
   echo "-toolame        use toolame instead of mp2enc."
   echo "-v <volume>     change  amplitude  (floating  point); less than 1.0"
   echo "                decreases, greater than 1.0 increases. Use that only"
   echo "                together with sox!"
   echo "-vbr <n>        output video bitrate in kbs [VCD:1152, SVCD:2500]."
   echo "-vfr <n>        sets the frame-rate of the output-stream. Currently"
   echo "                only the standard MPEG rates are supported:"
   echo "                0 - illegal"
   echo "                1 - 24000.0/1001.0 (NTSC 3:2 pulldown converted FILM)"
   echo "                2 - 24.0 (NATIVE FILM)"
   echo "                3 - 25.0 (PAL/SECAM VIDEO / converted FILM)"
   echo "                4 - 30000.0/1001.0 (NTSC VIDEO)"
   echo "                5 - 30.0"
   echo "                6 - 50.0 (PAL FIELD RATE)"
   echo "                7 - 60000.0/1001.0 (NTSC FIELD RATE)"
   echo "                8 - 60.0"
   echo "-vnorm <p|n|s>  sets the input video norm p, n or s:"
   echo "                forces the input stream to be treated as PAL|NTSC|SECAM"
   echo "                regardless  of  what the stream header might suggest."
   echo "-w              outputs in wide screen format 16:9."
   echo
   echo "examples:"
   echo
   echo "'$HOWCALLED teneriffa teneriffa.avi'"
   echo "this creates a VCD from an avi file."
   echo
   echo "'$HOWCALLED crazy dvd://3 -w':"
   echo "encodes and burns dvd title 3 to VCD in 16:9."
   echo
   echo "'$HOWCALLED carter dvd://1 -sid 0 -aid 128 -ss 3:00 -frames 1500 \\"
   echo "        -noburn -vnorm n -vfr 1':"
   echo "this encodes 1500 frames of dvd title 1 with subtitle 0 and audio stream"
   echo "128 starting from 3:00 with frame rate 29.97 fps and input video norm NTSC"
   echo "and output frame rate 23.98 fps because of the 3:2 pulldown method in NTSC."
   echo
   echo "'$HOWCALLED bj -size 640x272 -vf expand=640:480:0:104:0 \\"
   echo "        -cdsize 645 -noburn bj.avi'"
   echo "this example above shows you, how to deal with movies which are not"
   echo "created with aspect ratios of 4:3 or 16:9."
}

HOWCALLED=`basename $0`
if [ $# -le 1 ]; then
   usage
   exit 1
fi
case $1 in
   -*)
      usage
      exit 1
   ;;
   *)
      NAME=$1
      shift 1
   ;;
esac

cd $TMPDIR
rm -f $VIDEO
rm -f $AUDIO
# create a named pipe for video stream
mkfifo -m 660 $VIDEO

# some inits
sub=""
size=""
aratio=""
ratio=""
params=""
wide=""
blank=0
burn=1
burnonly=0
mp3=0
mpgonly=0
mkstream=1
abr=224
abrset=0
asr=44100
vbr=1152
vbrset=0
denoise="cat -"
vnorm="VCD"
mplexnorm="-f $VCDMODE -m 1 -V -b 46"
imaget="-t vcd2"
yuvin=""
framerate=""
sync=""
sox=0
toolame=0
volume="1.0"
overburn=""
pd=""
norm=0

while [ "$1"x != "x" ]; do
   case $1 in
      -a)
        aratio="-a $2"
	shift 1
        ;;
      -w)
      	wide="-M WIDE2STD"
	;;
      -h|-?)
        usage
        exit 0
	;;
      -abr)
      	abr=$2
        abrset=1
	shift 1
	;;
      -asr)
        asr=$2
        shift 1
        ;;
      -cdsize)
        CDMAXSIZE=$2
        shift 1
        ;;
      -blank)
      	blank=1
	;;
      -noburn)
        burn=0
	;;
      -burnonly)
        burnonly=1
	burn=1
	;;
      -overburn)
      	overburn="--overburn"
	;;
      -pulldown)
        pd="-p"
        ;;
      -vfr)
        framerate="-F $2"
	shift 1
	;;
      -mp3)
        mp3=1
        ;;
      -mpegsrc)
      	mkstream=0
	;;
      -mpgonly)
        mpgonly=1
	;;
      -normalize)
        norm=1
	;;
      -vnorm)
        yuvin="-n $2"
	shift 1
	;;
      -volume)
        volume="$2"
	shift 1
	;;
      -denoise)
        denoise="yuvdenoise"
        ;;
      -ratio)
        ratio=$2
        shift 1
        ;;
      -sid) # mplayer option: have to set vf expand, too!!!
        sub="-vf pp,expand=-1:-1:-1:-1:1 -sid $2"
        shift 1
        ;;
      -size)
        size=$2
        shift 1
        ;;
      -svcdout)
        vnorm="SVCD"
	;;
      -sync)
        sync="-O $2"
	shift 1
	;;
      -sox)
        sox=1
	;;
      -toolame)
        toolame=1
	;;
      -vbr)
        vbr=$2
        vbrset=1
	shift 1
	;;
      *)
	params="$params $1"
	;;
   esac
   shift 1
done

# some configs
mpegnorm="-f $VCDMODE -b $vbr -B 260 -V 46"
if [ "$vnorm" = "SVCD" ]; then
   [ $vbrset -eq 0 ] && vbr=2500
   mplexnorm="-f $SVCDMODE -m 2 -V -b 230"
   mpegnorm="-f $SVCDMODE -b $vbr -B 260 -V 230"
   imaget="-t svcd"
fi

# ratio overwrites wide sizing, size overwrites default frame size
[ -n "$ratio" ] && wide="-M $ratio"
[ -n "$size" ] && size="-O SIZE_$size"

# with mp3 audio set the default audio bitrate to 128 kbs
[ $mp3 -eq 1 -a $abrset -eq 0 ] && abr=128

# audio sample rate in kHz
a=$(($a=$asr / 1000))
b=$(($b=$asr % 1000))
[ $b -le 9 ] && b="00$b00"
[ $b -le 99 ] && b="0$b00"
kasr="$a.$b"

# start de-/encoding
if [ $burnonly -eq 0 ]; then
   # encode streams
   if [ $mkstream -eq 1 ]; then
      # start mplayer
      command="mplayer -noframedrop -vo yuv4mpeg -ao pcm:waveheader -v -osdlevel 0 $sub $params"
      echo "$command"
      $command &

      # mjpegtools
      ($denoise < $VIDEO | \
         yuvscaler -v 0 $wide -O $vnorm $size $yuvin | \
         mpeg2enc -v 0 -s $mpegnorm $aratio -S $CDMAXSIZE -g 6 -G 15 -r 16 \
	          $pd $framerate $yuvin -4 2 -2 1 -o $NAME.mpv) &

      # wait for finishing the subprocesses
      wait

      # normalize sound
      [ $norm -eq 1 ] && (echo "normalizing sound..."; normalize $AUDIO)

      # do resampling with sox
      if [ $sox -ne 0 ]; then
	echo "wait, do resampling with sox..."
	sox $AUDIO -v $volume -r $asr $NAME.wav resample -qs
	mv -f $NAME.wav $AUDIO
      fi

      if [ $toolame -ne 0 -a $mp3 -eq 0 ]; then
	# do mp2 encoding with toolame
	echo "wait, encoding to mp2 audio with toolame..."
	toolame -b $abr $AUDIO
	mv -f `basename $AUDIO .wav`.mp2 $NAME.mpa
      elif [ $toolame -eq 0 -a $mp3 -eq 0 ]; then
        # mp2enc/lame can't read audiodump.wav directly from named pipe,
        # we have to read the whole file.
	echo "wait, encoding to mp2 audio with mp2enc..."
        mp2enc -b $abr -r $asr -o $NAME.mpa < $AUDIO
      elif [ $mp3 -ne 0 ]; then
	echo "wait, encoding to mp3 audio with lame..."
        lame -b $abr --resample $kasr - $NAME.mpa < $AUDIO
      fi
   fi

   # remove wav file, won't need anymore!
   rm -f $AUDIO

   # multiplex streams
   [ -f $NAME.mpv -a -f $NAME.mpa ] || exit 1
   rm -f ${NAME}*.mpg
   mplex $mplexnorm $sync $NAME.mpv $NAME.mpa -o ${NAME}%d.mpg

   # remove pipe, won't need anymore!
   rm -f $VIDEO

   # should i create only the mpeg file?
   [ $mpgonly -eq 1 ] && exit 0

   # create cd images
   for mpg in ${NAME}*.mpg; do
      [ -f $mpg ] || exit 1
      cue="`basename $mpg .mpg`.cue"
      bin="`basename $mpg .mpg`.bin"
      rm -f $cue $bin
      vcdimager $imaget -c $cue -b $bin $mpg
      [ -f $bin -a -f $cue ] && rm -f $mpg
   done

# end of streaming/mastering
fi

# burn the (s)vcd's
[ $burn -eq 0 ] && exit 0

for cue in ${NAME}*.cue; do
   bin="`basename $cue .cue`.bin"
   [ -f $bin -a -f $cue ] || exit 1

   echo "please insert a cd in your cdwriter, after a keypress we start:"
   read -n 1 i

   if [ $blank -eq 1 ]; then
      cdrdao blank --reload $CDDEV --driver $CDDRV --blank-mode minimal
   fi
   cdrdao write $overburn --reload $CDDEV --driver $CDDRV $cue
done
exit 0
