mplayer: ffmpeg libass mplayer-config
	$(MAKE) -C ..

mplayer-config: ffmpeg libass
	script/mplayer-config

ffmpeg-config:
	script/ffmpeg-config

ffmpeg: ffmpeg-config
	$(MAKE) -C ffmpeg_build install

# libass doesn't really depend on ffmpeg, but added so configure runs are in order
libass-config: ffmpeg
	script/libass-config

libass: libass-config
	$(MAKE) -C libass install

noconfig:
	$(MAKE) -C ffmpeg_build install
	$(MAKE) -C libass install
	$(MAKE) -C ..

install:
	$(MAKE) -C .. install

.PHONY: mplayer-config mplayer ffmpeg-config ffmpeg libass-config libass noconfig install
