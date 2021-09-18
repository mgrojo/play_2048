MAIN = play_2048
DESTDIR = /opt/$(MAIN)

.PHONY: all install AppImage

all:
	alr build --no-tty

install: all
	mkdir -p $(DESTDIR)
	cp -rp $(MAIN).desktop $(MAIN) themes $(DESTDIR)
	cp -p themes/1/icon.png $(DESTDIR)/$(MAIN).png

AppImage:
	rm -rf AppDir
	make install DESTDIR=AppDir
	wget -nv -c https://github.com/linuxdeploy/linuxdeploy/releases/download/continuous/linuxdeploy-x86_64.AppImage
	chmod +x linuxdeploy-x86_64.AppImage
	./linuxdeploy-x86_64.AppImage \
	--executable $(MAIN) \
	--desktop-file $(MAIN).desktop --icon-file=AppDir/$(MAIN).png \
	--appdir AppDir --output appimage
