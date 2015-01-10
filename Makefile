.SUFFIX:
.PHONEY: all clean install

APP := obj/adaview

prefix :=/usr/local
localedir :=${prefix}/share/locale
DESTDIR :=

all:
	@./generate_locale_path.sh "${localedir}"
	@gnatmake -d -Padaview.gpr

clean:
	@echo "Cleaning build trees"
	@gprclean -r -P adaview.gpr

install: all
	@echo "Installing to $(DESTDIR)$(prefix)"
	@if ! [ -d "$(DESTDIR)$(prefix)/bin" ]; then \
	    mkdir -p $(DESTDIR)$(prefix)/bin; \
	fi
	@cp $(APP) $(DESTDIR)$(prefix)/bin
