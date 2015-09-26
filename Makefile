.SUFFIX:
.PHONEY: all clean install

APP := obj/adaview

prefix :=/usr/local
localedir :=${prefix}/share/locale
DESTDIR :=
.PHONY: all clean po

all:
	@./generate_locale_path.sh "${localedir}"
	@gprbuild -d -Padaview.gpr -XLIBRARY_TYPE=relocatable

clean:
	@echo "Cleaning build trees"
	@gprclean -r -P adaview.gpr -XLIBRARY_TYPE=relocatable

po:
	@$(MAKE) -C po

install: all
	@echo "Installing to $(DESTDIR)$(prefix)"
	@if ! [ -d "$(DESTDIR)$(prefix)/bin" ]; then \
	    mkdir -p $(DESTDIR)$(prefix)/bin; \
	fi
	@cp $(APP) $(DESTDIR)$(prefix)/bin
