mplayer: libav libass mplayer-config
	$(MAKE) -C ..

mplayer-config: libav libass
	script/mplayer-config

libav-config:
	script/libav-config

libav: libav-config
	$(MAKE) -C libav_build install

# libass doesn't really depend on libav, but added so configure runs are in order
libass-config: libav
	script/libass-config

libass: libass-config
	$(MAKE) -C libass install

noconfig:
	$(MAKE) -C libav_build install
	$(MAKE) -C libass install
	$(MAKE) -C ..

install:
	$(MAKE) -C .. install

.PHONY: mplayer-config mplayer libav-config libav libass-config libass noconfig install
