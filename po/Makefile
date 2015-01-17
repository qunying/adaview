LANGS = zh_CN
MKDIR = mkdir -p
CP=cp
MV=mv
prg=adaview
## Refresh all translations by extracting the strings from the current sources
all:
	@$(MAKE) refresh
	@$(MAKE) ${LANGS}.gmo
refresh:
	@./build_skeleton.pl ${prg} 2014-2015 "Zhu qun-Ying" "zhu.qunying@gmail.com" > ${prg}.pot
	@${foreach lang,${LANGS}, \
	if [ -f ${lang}.po ]; then true; else ${CP} ${prg}.pot ${lang}.po; fi; \
	msgmerge --no-wrap --update ${lang}.po ${prg}.pot} >& /dev/null

%.gmo:%.po
	-@msgfmt -o $@ $^ --statistics --check-header --check-format --check-domain

clean:
	-$(RM) *.gmo 
