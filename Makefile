.SUFFIX:
.PHONEY: all clean install

APP := obj/adaview

prefix :=/usr/local

DESTDIR :=
.PHONY: all clean po

all:
	@./generate_path.sh "${prefix}"
	@gprbuild -d -Padaview.gpr -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable

clean:
	@echo "Cleaning build trees"
	@gprclean -r -q -P adaview.gpr -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable

po:
	@$(MAKE) -C po

install: all
	@echo "Installing to $(DESTDIR)$(prefix)"
	@if ! [ -d "$(DESTDIR)$(prefix)/bin" ]; then \
	    mkdir -p $(DESTDIR)$(prefix)/bin; \
	fi
	@cp $(APP) $(DESTDIR)$(prefix)/bin
