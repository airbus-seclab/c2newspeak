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

OPTION=--print-newspeak
COMMAND=$(ADA2NEWSPEAK) $(OPTION) $*.adb
PREREQ=%.adb $(ADA2NEWSPEAK)
CLEANFILES=*.bak

TESTS=t001 t002 t003 t004 t005 t006 t007 t008 t009 t010 \
      t011 t012 t013 t014 t015 t016 t017 t018 t019 t020 \
      t021 t022 t023 t024 t025 t026 t027 t028 t029 t030 \
      t031 t032 t033 t034 t035 t036 t037 t038 t039 t040 \
      t041 t042 t043 t044 t045 t046 t047 t048 t049 t050 \
      t051 t052 t053 t054 t055 t056 t057 t058 t059 t060 \
      t061 t062 t063 t064 t065 t066 t067 t068 t069 t070 \
      t071 t072 t073 t074 t075 t077 t078 t079 t080      \
      t081 t082 t083 t084 t085 t086 t087 t088 t089 t090 \
      t091 t092 t093 t094 t095 t096 t097 t098 t099 t100 \
      t101 t102 t103 t104 t105 t106 t107 t108 t109 t110 \
      t111 t112 t113 t114 t115 t116 t117 t118 t119 t120 \
      t121 t122 t123 t124 t125 t126 t127 t128 t129 t130 \
      t131 t132 t133 t134 t135 t136 t137                \
      t201 t202 t203      t205 t206 t207 t208 t209 t210 \
      t211           t214      t216 t217 t218 t219 t220 \
      t221 t222 t223 t224 t225 t226 t227 t228 t229 t230 \
      t231 t232      t234 t235 t236 t237 t238 t239 t240 \
      t241 t242 t243 t244 t245 t246 t247 t248 t249      \
			     t252 t253 t254 t255 t256 t257 t258 t259 t260 \
		  t261 t262	t263			t265 t266	t267 t268 t269 t270 \
			t271 t272	t273 t274           t277

include ../common.Makefile

t033: t210
t036: t211
t043: t090
t044: t091
t045: t092
t046: t093
t047: t094
t050: t095
t053: t097 t098 t096 t099
t058: t100
t059: t101
t060: t102
t071: t103
t115: t095
