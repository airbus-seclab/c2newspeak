## C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
# well-suited for static analysis.
# Copyright (C) 2007  Charles Hymans, Olivier Levillain
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#
#
# Charles Hymans
# EADS Innovation Works - SE/CS
# 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
# email: charles.hymans@penjili.org
#

include distrib.Makefile

.PHONY: check check.clean
DISTDIR=newspeak-$(VERSION)
DISTFILE=$(DISTDIR).tar.gz
VERSION.FILE=src/version.ml
CLEANFILES+=$(VERSION.FILE) $(DISTDIR) $(DISTFILE)

NEWSPEAK_HASH=$(shell md5sum src/newspeak/newspeak.ml | cut -c 1-32)

genversion=\
hg parents --template 'let date = "{date|shortdate}"\n' > $(VERSION.FILE); \
hg parents --template 'let version = "$(VERSION)"\n' >> $(VERSION.FILE); \
hg parents --template 'let revision = "{node|short}"\n' >> $(VERSION.FILE); \
echo "let newspeak_hash=\"$(NEWSPEAK_HASH)\"" >> $(VERSION.FILE)

#Version number generation
$(VERSION.FILE): src/newspeak/newspeak.ml
	@echo "Creating version file       "$(VERSION.FILE)
	@$(genversion)

distrib: $(DISTDIR)

$(DISTDIR): clean-all
	$(genversion)
	-mkdir $(DISTDIR)
	$(CP) -r doc $(DISTDIR)
	$(CP) -r src $(DISTDIR)
	$(CP) -r lib $(DISTDIR)
	$(CP) -r INSTALL.txt lgpl.txt limitations.txt  $(DISTDIR)
	$(CP) -r distrib.Makefile $(DISTDIR)/Makefile
	tar czf $(DISTFILE) $(DISTDIR)

check:
	$(MAKE) -C tests

check-all: check $(DISTDIR)
	cd $(DISTDIR); $(MAKE) all

clean: check.clean

check.clean: 
	@$(MAKE) -s -C tests clean
