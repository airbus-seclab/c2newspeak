include Makefile.distrib

.PHONY: check
DISTDIR=newspeak-$(VERSION)
DISTFILE=$(DISTDIR).tgz
TESTSDIR=$(addprefix tests/,npksimplify mult-files mem_opt npkstats npkcheck npk2bytesz)
CLEANFILES+=src/version.ml $(DISTDIR) $(DISTFILE) \
            $(addsuffix /*.no,$(TESTSDIR)) \
            $(addsuffix /*.npk,$(TESTSDIR)) \
            $(addsuffix /*~,$(TESTSDIR)) \
            tests/npksimplify/*.checked \
            tests/npk2bytesz/*.checked \
            tests/newspeak/*.no tests/newspeak/result \
            tests/newspeak/*.checked tests/newspeak/*~ \
            tests/newspeak/002.npk tests/newspeak/*.bak \
            tests/newspeak/*.cmi tests/newspeak/*.cmo tests/newspeak/read \
            tests/mult-files/*.checked \
            tests/mem_opt/000 \
            tests/*.no tests/*.checked tests/*~ tests/*.npk \
            $(addprefix tests/mult-files/,000 001 002 003 004) \
            $(addprefix tests/npkstats/,000 001 002) \
            $(addprefix tests/npkcheck/,000) \
            tests/newspeak/*.exe

genversion=\
hg parents --template 'let date = "{date|shortdate}"\n' > src/version.ml; \
hg parents --template 'let version = "$(VERSION)"\n' >> src/version.ml; \
hg parents --template 'let revision = "{node|short}"\n' >> src/version.ml

#Version number generation
src/version.ml:
	$(genversion)

distrib: $(DISTDIR)

$(DISTDIR): clean-all
	$(genversion)
	-mkdir $(DISTDIR)
	$(CP) -r doc $(DISTDIR)
	$(CP) -r src $(DISTDIR)
	$(CP) -r lib $(DISTDIR)
	$(CP) -r cil $(DISTDIR)
	$(CP) -r INSTALL.txt lgpl.txt limitations.txt  $(DISTDIR)
	$(CP) -r Makefile.distrib $(DISTDIR)/Makefile
	tar czf $(DISTFILE) $(DISTDIR)

check:
	$(MAKE) -C tests

check-all: check $(DISTDIR)
	cd $(DISTDIR); $(MAKE) install