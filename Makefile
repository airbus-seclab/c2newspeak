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

.PHONY: check
DISTDIR=newspeak-$(VERSION)
DISTFILE=$(DISTDIR).tar.gz
TESTSDIR=$(addprefix tests/,npksimplify mult-files mem_opt npkstats npkstrip npkcheck npk2bytesz npkbugfind npkdiff ada2newspeak)
VERSION.FILE=src/newspeak/version.ml
CLEANFILES+=$(VERSION.FILE) $(DISTDIR) $(DISTFILE) \
            $(addsuffix /*.no,$(TESTSDIR)) \
            $(addsuffix /*.npk,$(TESTSDIR)) \
            $(addsuffix /*~,$(TESTSDIR)) \
            $(addsuffix /*.ok,$(TESTSDIR)) \
            tests/*.no tests/*.checked tests/*~ tests/*.npk tests/*.bak \
            tests/*.res \
            tests/newspeak/*~ tests/newspeak/*.ok tests/newspeak/*.exe \
            tests/newspeak/*_check tests/newspeak/*.cmi tests/newspeak/*.cmo \
            tests/newspeak/*.no tests/newspeak/002.npk tests/newspeak/003.npk \
            tests/newspeak/005.npk

genversion=\
hg parents --template 'let date = "{date|shortdate}"\n' > $(VERSION.FILE); \
hg parents --template 'let version = "$(VERSION)"\n' >> $(VERSION.FILE); \
hg parents --template 'let revision = "{node|short}"\n' >> $(VERSION.FILE)

#Version number generation
$(VERSION.FILE):
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
	$(CP) -r distrib.Makefile $(DISTDIR)/Makefile
	tar czf $(DISTFILE) $(DISTDIR)

check:
	$(MAKE) -C tests

check-all: check $(DISTDIR)
	cd $(DISTDIR); $(MAKE) all
