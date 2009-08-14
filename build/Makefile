mplayer: ffmpeg libass mplayer-config
	$(MAKE) -C ..

mplayer-config:
	script/mplayer-config

ffmpeg-config:
	script/ffmpeg-config

ffmpeg: ffmpeg-config
	$(MAKE) -C ffmpeg_build install

libass-config:
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
