#
# ptrtype: do finer typechecks on C pointers
# Copyright (C) 2011  Etienne Millon
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
# Etienne Millon <etienne.millon@eads.net>
# EADS Innovation Works - SE/IT
# 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France

TARGET=ptrtype
DIRS=newspeak/ utils/ ptrtype/

MODS=options utils tyspeak types env unification \
     check infer npk2tpk ptrtype

FILES=version newspeak/temps newspeak/conf newspeak/eBigInt \
     newspeak/newspeak utils/standardApplication newspeak/npkcontext \
     newspeak/lowspeak  newspeak/npk2lpk $(addprefix ptrtype/, $(MODS))
LIBX=nums.cmxa str.cmxa

include common.Makefile
