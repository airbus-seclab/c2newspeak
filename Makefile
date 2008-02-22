include Makefile.distrib

DISTDIR=newspeak-$(VERSION)
DISTFILE=$(DISTDIR).tgz
CLEANFILES+=src/version.ml $(DISTDIR) $(DISTFILE)

genversion=\
hg parents --template 'let date = "{date|shortdate}"\n' > src/version.ml; \
hg parents --template 'let version = "$(VERSION)"\n' >> src/version.ml; \
hg parents --template 'let revision = "{node|short}"\n' >> src/version.ml

#Version number generation
src/version.ml:
	$(genversion)

distrib: 
	$(genversion)
	-mkdir $(DISTDIR)
	$(CP) -r bin $(DISTDIR)
	$(CP) -r doc $(DISTDIR)
	$(CP) -r src $(DISTDIR)
	$(CP) -r tests $(DISTDIR)
	$(CP) -r INSTALL.txt lgpl.txt limitations.txt  $(DISTDIR)
	$(CP) -r Makefile.distrib $(DISTDIR)/Makefile
	tar czf $(DISTFILE) $(DISTDIR)
