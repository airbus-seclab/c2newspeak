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
      t051 t052     t055 t056 t057 t059 t060 \
      t061 t062 t063 t064 t065 t066 t067 t068 t069 t070 \
      t071 t072 t073 t074 t075      t077 t078 t079 t080 \
      t081 t082 t083 t084 t085 t086 t087 t088 t089 t090 \
      t091 t092 t093 t094 t095 t096     t099 t100 \
      t101 t102 t103 t104 t105 t106           t109 t110 \
      t111 t112 t113           t116      t118 t119 t120 \
      t121 t123 t124 t125 t126 t127 t128 t129 t130 \
      t131           t134 t135 t136 t137                \
           t202 t203           t206 t207 t208 t209 t210 \
      t211                          t217 t218 t219 t220 \
      t221 t222 t223 t224 t225 t226 t227 t228 t229 t230 \
      t231 t232      t234 t235 t236 t237 t238 t239 t240 \
      t241 t242 t243 t244 t245 t246 t247 t248 t249 t250 \
           t252 t253 t254 t255 t256 t257 t258 t259 t260 \
      t261 t262 t263      t265  t267 t268 t269 t270 \
      t271 t272 t273 t274 t275 t276 t277 t278           \
           t282 t283    t286 t287 t288 t289 t290 \
      t291 t292 t293 t294 t295 t296 t297 t298 t299 t300 \
      t301 t302 t303 t304 t305 t306 t307 t308 t309 t310 \
      t311 t312 t313 t314      t316 t317 t318 t319 t320 \
      t321      t323 t324 t325 t326 t327 t328 t329 t330 \
      t331 t332 t333 t334 t335 t336 t337 t338 t339 t340 \
      t341 t342 t343 t344 t345 t346 t347 t348 t349      \
      t351 t352 t353 t354 t355 t356 t357 t358 t359 t360 \
      t361 t362 t363 t364 t365 t366 t367 t368 t369 t370 \
      t371 t372 t373 t374 t375 t376 t377 t378 t379 t380 \
      t381 t382 t383 t384 t385 t386 t387 t388 t389 t390 \
      t391 t392 t393                          t399 t400 \
                          t405 t406 t407      t409 t410 \
      t411 t412 t413 t414 t415 t416 t417 t418 t419 t420 \
      t421      t423           t425 t426 \
           t432 t433 t434 t435 t436 t437 t438 t439 t440 \
      t441           t444           t447      t449 t450 \
                     t454 t455 t456 t457 t458 t459 t460 \
      t461 t462 t463 t464 t465 t466           t469 t470 \
      t471 t472 t473 t474 t475      t477 t478 t479 t480 \
      t481 t483 t484 t485 t486 t487    t489 t490 \
      t491

include ../common.Makefile

# add   t053 t098 t266 t284 t401     
# to do t428 t482
# clean warning in t434 

t033: t210 t316
t036: t211
t043: t090
t044: t091
t045: t092
t046: t093
t047: t094
t050: t095
#t053: t097 t098 t122 t096 t099
t058: t100 t317
t059: t101
t060: t102
t071: t103
t115: t095
t279: t281
t034: t286
t017: t287
t020: t288
t035: t292
t064: t303
t079: t305
t019: t308
t006: t309
t087: t310
t005: t311
t311: t312
t011: t313
t108: t315
t072: t318
t202: t319 t324
t012: t320
t302: t323
t324: t325
t116: t326
t352: t353
#t362: t364
