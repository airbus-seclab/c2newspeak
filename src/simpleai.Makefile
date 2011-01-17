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
TARGET=simpleai
DIRS=newspeak/ utils/ simpleai/
LIBX=nums.cmxa str.cmxa

all: ../bin/$(TARGET)

FILES=version utils/strSet \
      newspeak/temps newspeak/config newspeak/eBigInt newspeak/newspeak \
      utils/standardApplication \
      newspeak/npkcontext newspeak/lowspeak newspeak/npk2lpk \
      simpleai/context simpleai/simple \
      simpleai/filter simpleai/sigs \
      simpleai/unrelState simpleai/cst \
      simpleai/solver simpleai/simpleai

FILES.CMO=$(addsuffix .cmo,$(FILES))
FILES.CMX=$(addsuffix .cmx,$(FILES))


CLEANFILES=$(FILES.CMO)



include common.Makefile
