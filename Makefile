.SUFFIX:
.PHONEY: all clean install

APP := obj/adaview

prefix :=/usr/local
localedir :=${prefix}/share/locale
DESTDIR :=
LIBRARY_TYPE := "relocatable"

all:
	@if ! [ -d obj ]; then \
	    mkdir obj; \
	fi
	@./generate_locale_path.sh "${localedir}"
	@gprbuild -P adaview.gpr -XLIBRARY_TYPE=$(LIBRARY_TYPE)

clean:
	-rm obj/*

install: all
	@echo "Installing to $(DESTDIR)$(prefix)"
	@if ! [ -d "$(DESTDIR)$(prefix)/bin" ]; then \
	    mkdir -p $(DESTDIR)$(prefix)/bin; \
	fi
	@cp $(APP) $(DESTDIR)$(prefix)/bin
