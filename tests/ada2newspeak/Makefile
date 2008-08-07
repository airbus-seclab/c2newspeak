COMPILER=../../bin/ada2newspeak
OPTION=--npko --newspeak

DIR = . t040 t041 t042 t043 t044 t045 t046 t047 t048 t049 t050 t051 t053 \
	t054 t057 t058 t059 t060 t071 t072 t077 t078 t079 t080 t081 t082

TESTS = t001 t002 t003 t004 t005 t006 t007 t008 t009 t010 t011 t012 \
	t013 t014 t015 t016 t017 t018 t019 t020 t021 t022 t023 t024 \
	t025 t026 t027 t028 t029 t030 t031 t032 t033 t034 t035 t036 \
	t037 t038 t039 t040/t040 t041/t041 t042/t042 t043/t043 \
	t043/t043a t044/t044 t044/t044a t045/t045 t045/t045a \
	t046/t046 t046/t046a t047/t047 t047/t047a t048/t048 t049/t049 \
	t050/t050a t050/t050 t051/t051 t052 t053/a t053/p t053/p1 t053/p2 \
	t053/main t054/t054 t055 t056 t057/t057 t058/t058a t058/t058 \
	t059/t059a t059/t059 t060/t060a t060/t060 t061 t062 t063 t064 \
	t065 t066 t067 t068 t069 t070 t071/p2 t071/proc t072/main t073 \
	t074 t075 t077/p2 t078/p2 t079/p2 t080/p2 t081/p2 t082/p2 \
	t083 t084 t085 t086 t087 t088 t089

TESTS.REF = $(addsuffix .ref, $(TESTS))

DIRCLEAN = $(addsuffix /*.checked, $(DIR)) $(addsuffix /*.o, $(DIR)) \
	   $(addsuffix /*.no, $(DIR)) $(addsuffix /*.ali, $(DIR)) \
	   $(addsuffix /*.npk, $(DIR)) $(addsuffix /*~, $(DIR))

DIRCLEANREF = $(addsuffix /*.ref, $(DIR))

.PHONY: clean $(TESTS) check

all: check

check: $(COMPILER) $(TESTS)

t043/t043: t043/t043a
t044/t044: t044/t044a
t045/t045: t045/t045a
t046/t046: t046/t046a
t047/t047: t047/t047a
t050/t050: t050/t050a
t053/main: t053/p1 t053/p2 t053/a t053/p
t058/t058: t058/t058a
t059/t059: t059/t059a
t060/t060: t060/t060a
t071/proc: t071/p2

$(TESTS): %: %.checked

%.ref: %.adb
	@$(COMPILER) $(OPTION) $< >$@

%.checked: %.adb %.ref
	@$(COMPILER) $(OPTION) $< >$@
	@diff $@ $*.ref
	@echo $*
	@rm $@ $*.no

ref:	$(TESTS.REF)

$(COMPILER) :
	make bin/ada2newspeak -C ../..

clean :
	rm -f $(DIRCLEAN)
	rm -f $(TESTS)

cleanref : clean
	rm -f $(DIRCLEANREF)

