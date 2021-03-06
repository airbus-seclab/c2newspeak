#
# C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
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

#TODO: think about it, but npkcontext should not be part of INSTALL.FILES
INSTALL.FILES=newspeak/temps.cmi newspeak/conf.cmi newspeak/eBigInt.cmi \
	      newspeak/newspeak.cmi \
	      utils/standardApplication.cmi newspeak/npkcontext.cmi \
              newspeak/lowspeak.cmi newspeak/npk2lpk.cmi newspeak/npkil.cmi \
		newspeak.cma newspeak.cmxa newspeak.o c2newspeak/typedC.cmi

all: $(INSTALL.FILES)
	$(CP) $(INSTALL.FILES) ../bin

FILES=version newspeak/temps newspeak/conf newspeak/eBigInt \
      utils/listUtils newspeak/newspeak utils/standardApplication \
      newspeak/npkcontext newspeak/lowspeak newspeak/npk2lpk newspeak/npkil c2newspeak/csyntax c2newspeak/typedC
FILES.CMO=$(addsuffix .cmo,$(FILES))
FILES.CMX=$(addsuffix .cmx,$(FILES))

newspeak.cma: $(FILES.CMO)
	@echo "Building library            "newspeak.cma
	@$(OCAMLC) nums.cma str.cma -a $(FILES.CMO) -o newspeak.cma

newspeak.cmxa: $(FILES.CMX)
	@echo "Building library "newspeak.cmxa
	@$(OCAMLOPT) -a $(FILES.CMX) -o newspeak.cmxa

newspeak.o: $(FILES.CMX)
	@$(OCAMLOPT) -output-obj nums.cmxa str.cmxa $(FILES.CMX) -o newspeak.o

install:
	ocamlfind install newspeak newspeak/META newspeak.a newspeak.o newspeak.cmxa newspeak.cma ../bin/newspeak.cmi c2newspeak/typedC.cmi

package:
	rm -rf ../pkg/ && mkdir ../pkg/
	ocamlfind install -destdir ../pkg/ newspeak newspeak/META newspeak.a newspeak.o newspeak.cmxa newspeak.cma ../bin/newspeak.cmi c2newspeak/typedC.cmi c2newspeak/typedC.o c2newspeak/typedC.cmx

CLEANFILES=newspeak.cmxa newspeak.cma newspeak.o
TARGET=newspeak
DIRS=utils/ newspeak/ c2newspeak/
LIBX=nums.cmxa str.cmxa

include common.Makefile
