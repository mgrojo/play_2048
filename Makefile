MAIN = play_2048
DESTDIR = /opt/$(MAIN)

.PHONY: all install AppImage

all:
	alr build

install: all
	alr exec -P -- gprinstall --prefix=$(DESTDIR) --create-missing-dirs --mode=usage -f
	cp -p themes/1/icon.png $(DESTDIR)/$(MAIN).png

AppImage:
	rm -rf AppDir
	make install DESTDIR=AppDir/usr
	wget -nv -c https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
	chmod +x linuxdeploy-x86_64.AppImage
	./linuxdeploy-x86_64.AppImage \
	--executable bin/$(MAIN) \
	--desktop-file $(MAIN).desktop --icon-file=AppDir/usr/$(MAIN).png \
	--appdir AppDir --output appimage
