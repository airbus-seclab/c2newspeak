# commands
# TODO: defining functions/macros ?
RM = rm -f
FLAG = --experimental
FLAG.CIL = 
C2N = ../bin/c2newspeak --newspeak
C2NEWSPEAK = $(C2N) $(FLAG)
C2NEWSPEAK.CIL = $(C2N) $(FLAG.CIL)

DIR=mult-files npksimplify mem_opt npk2bytesz newspeak

TESTS = 000 001 002 003 004 005 006 007 008 009 \
        010 011 012 013 014 015 016 017 018 019 \
	020 021 022 023 024 025 026 027 028 029 \
	030 031 032 033 034 035 036 037 038 039 \
	040 041 042 043 044 045 046 047 048 049 \
	050 051 052 053 054 055 056 057 058 059 \
	060 061 062 063 064 065 066 067 068 069 \
	070 071 072 073 074 075 076 077 078 079 \
	080 081 082 083 084 085 086 087 088 089 \
	090 091 092 093 094 095 096 097 098 099 \
	100 101 102 103 104 105 106 107 108 109 \
	110 111 112 113 114 115 116 117 118 119 \
	120 121 122 123 124 125 126 127 128 129 \
	130 131 132 133 134 135 136 137 138 139 \
	140 141 142 143 144 145 146 147 148 149 \
	150 151 152 153 154 155 156 157 158 159 \
	160 161 162 163 164 165 166 167 168 169 \
	170 171 172 173 174 175 176 177 178 179 \
	180 181 182 183 184 185 186 187 188 189 \
	190 191 192 193 194 195 196 197 198 199 \
	200 201 202 203 204 205 206 207 208 209 \
	210 211 212 213 214 215 216 217 218 219 \
	220 221 222 223 224 225 226 227 228 229 \
	230 231 232 233 234 235 236 237 238 239 \
	240 241 242 243 244 245 246 247 248 249 \
	250 251 252 253 254 255 256 257 258 259 \
	260 261 262 263 264 265 266 267 268 269 \
	270 271 272 273 274 275 276 277 278 279 \
	280 281 282 283 284 285 286 287

CHECK=$(addsuffix .checked, $(TESTS))
TESTS.SPEC = $(addsuffix .spec, $(TESTS))
TESTS.CIL = $(addsuffix .cil, $(TESTS))
TESTS.CIL.SPEC = $(addsuffix .spec, $(TESTS.CIL))

.SILENT: check $(CHECK)
.PHONY: $(TESTS) $(TESTS.SPEC) $(TESTS.CIL.SPEC)

check: ../bin/c2newspeak $(CHECK) $(addsuffix .check, $(DIR))

%.check: ../bin/c2newspeak
	make -C $*

001: 026 028 176 177
000: 030 031
003: 032
005: 233
008: 200
009: 217
011: 064
018: 035
021: 036 037
022: 038
025: 058 145 262
026: 027 187
033: 039 040 041 042
037: 218
050: 051
052: 126
055: 188
060: 189
063: 066 219
066: 080
068: 110 213
070: 259
076: 146
095: 222 223
114: 115
194: 224
196: 225
197: 198 199
213: 220
216: 226

$(TESTS): %: %.checked

%.checked: CIL_SPEC=$(if $(shell [ -e $*.cil.spec ] && echo true), \
	  $*.cil.spec,$*.spec)
%.checked: %.c
	eval $(C2NEWSPEAK) `[ -e $*.in ] && cat $*.in` $*.c &> $*.bak; true
	dos2unix $*.bak 2&> /dev/null; true
	if [ -e $*.spec ]; \
	then \
	  if diff $*.spec $*.bak > result; \
	  then true; \
	  else cat result; false; \
	  fi; \
	else \
	  echo "$*.spec does not exist; printing output instead of diffing"; \
	  cat $*.bak; false; \
	fi
	$(RM) $*.bak result
	eval $(C2NEWSPEAK.CIL) `[ -e $*.in ] && cat $*.in` \
	$*.c &> $*.bak; true
	dos2unix $*.bak 2&> /dev/null; true 
	if [ -e $(CIL_SPEC) ]; \
	then \
	  if diff $(CIL_SPEC) $*.bak > result; \
	  then true; \
	  else cat result; false; \
	  fi; \
	else \
	  echo "$(CIL_SPEC) does not exist; printing output instead of diffing"; \
	  cat $*.bak; false; \
	fi;
	$(RM) $*.bak result
	touch $*.checked
	echo $*

$(TESTS.SPEC): %.spec: %.c
	eval $(C2NEWSPEAK) `[ -e $*.in ] && cat $*.in` $*.c &> $*.spec; \
	dos2unix $*.spec; \
	cat $*.spec

$(TESTS.CIL.SPEC): %.cil.spec: %.c
	eval $(C2NEWSPEAK.CIL) `[ -e $*.in ] && cat $*.in` $*.c &> $*.cil.spec; \
	dos2unix $*.cil.spec; \
	cat $*.cil.spec


../bin/c2newspeak: 
	make -C .. install

clean: 
	$(RM) $(TESTS)
	$(RM) *~ *.bak result *.no *.npk
