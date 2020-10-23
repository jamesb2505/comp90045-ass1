
HC      = ghc
HCFLAGS = -O2
HAPPY   = happy
ALEX    = alex

EXE = Roo

APP = app
SRC = src

_MAIN = Main.hs
MAIN  = $(patsubst %,$(APP)/%,$(_MAIN))
_GEN  = RooLexer.hs RooParser.hs
GEN   = $(patsubst %,$(SRC)/%,$(_GEN))
_DEPS = RooAST.hs PrettyRoo.hs RooSymbolTable.hs RooOzCodeGen.hs OzCode.hs RooCTrans.hs RooPyTrans.hs
DEPS  = $(patsubst %,$(SRC)/%,$(_DEPS)) $(GEN)

.PHONY: all gen clean cleanly clobber

$(EXE): $(DEPS) $(MAIN)
	$(HC) $(HCFLAGS) $^ -o $@ -Wall

$(SRC)/RooLexer.hs: $(SRC)/RooLexer.x
	$(ALEX) $< -o $@

$(SRC)/RooParser.hs: $(SRC)/RooParser.y
	$(HAPPY) $< -o $@

all: $(EXE)

gen: $(GEN)

clean:
	rm -f $(SRC)/*.o $(SRC)/*.hi $(APP)/*.o $(APP)/*.hi $(GEN)

cleanly: all clean

clobber: clean
	rm -f $(EXE)
