#
# C2Newspeak: compiles C code into Newspeak. Newspeak is a minimal language 
# well-suited for static analysis.
# Copyright (C) 2007, 2011  Charles Hymans, Olivier Levillain, Sarah Zennou
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
# email: charles.hymans@penjili.org
#
# Sarah Zennou
# EADS Innovation Works - SE/IT
# 12, rue Pasteur - BP 76 - 92152 Suresnes Cedex - France
# email: sarah(dot)zennou(at)eads(dot)net
#

DIR=c2newspeak mult-files lowspeak npkstats mem_opt npkcheck \
    newspeak npkstrip ada2newspeak npkpointer options \
    simpleai npksolver npkmerger ulimit

.PHONY: $(DIR)

all: $(DIR)

clean: $(DIR)

#WARNING: do not remove the $(MAKCMDGOALS) variable, it is useful to propagate
#the exact rule
$(DIR):
	@$(MAKE) -s -C $@ $(MAKECMDGOALS)

