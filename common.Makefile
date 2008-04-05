RM=rm -rf

OCAMLC=ocamlc -w Ael -warn-error Ael
OCAMLOPT=ocamlopt -w Ael -warn-error Ael -inline 100 -noassert -unsafe
OCAMLDEP=ocamldep

LIB=nums.cma
LIBX=unix.cmxa str.cmxa nums.cmxa

FULLDIRS=$(addprefix src/,$(DIRS))
INCLUDE=$(addprefix -I ,src $(FULLDIRS))

FULLFILES=$(addprefix src/,$(FILES))
MLI=$(addsuffix .mli,$(FULLFILES))
ML=$(addsuffix .ml,$(FULLFILES))
CMX=$(addsuffix .cmx,$(FULLFILES))

all: $(CMX)
	$(OCAMLOPT) $(INCLUDE) $(LIBX) $(CMX) -o bin/$(TARGET)

#Implicit rules
%.cmi: %.mli
	$(OCAMLC) $(INCLUDE) $(LIB) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(INCLUDE) $(LIBX) -c $<

$(TARGET).depend: $(MLI) $(ML)
	@mkdir bin 2> /dev/null; true
	@$(OCAMLDEP) $(INCLUDE) $(MLI) $(ML) > $(TARGET).depend

include $(TARGET).depend

clean:
	$(RM) $(TARGET).depend
	$(RM) $(addsuffix /*.cmi, $(FULLDIRS))
	$(RM) $(addsuffix /*.cmx, $(FULLDIRS))
	$(RM) $(addsuffix /*.o, $(FULLDIRS))
	$(RM) $(addsuffix /*~, $(FULLDIRS))
	$(RM) bin/$(TARGET)*
