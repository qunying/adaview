.SUFFIX:
.PHONEY: all clean install

APP := obj/adaview

prefix :=/usr/local
DESTDIR :=

all:
	@if ! [ -d obj ]; then \
	    mkdir obj; \
	fi
	@gprbuild -P adaview.gpr

clean:
	-rm obj/*

install: all
	@echo "Installing to $(DESTDIR)$(prefix)"
	@if ! [ -d "$(DESTDIR)$(prefix)/bin" ]; then \
	    mkdir -p $(DESTDIR)$(prefix)/bin; \
	fi
	@cp $(APP) $(DESTDIR)$(prefix)/bin
