
HC    = ghc
HAPPY = happy
ALEX  = alex
EXE   = Roo

APP = app
SRC = src

_MAIN = Main.hs
MAIN  = $(patsubst %,$(APP)/%,$(_MAIN))
_GEN  = RooLexer.hs RooParser.hs
GEN   = $(patsubst %,$(SRC)/%,$(_GEN))
_DEPS = RooAST.hs PrettyRoo.hs 
DEPS  = $(patsubst %,$(SRC)/%,$(_DEPS)) $(GEN)

.PHONY: all gen clean CLEAN cleangen

all: $(EXE)

gen: $(GEN)

clean:
	rm -f $(SRC)/*.o $(SRC)/*.hi $(APP)/*.o $(APP)/*.hi

CLEAN: clean
	rm -f $(EXE)

cleangen: 
	rm -f $(GEN)

cleanly: all clean

$(EXE): $(DEPS)
	$(HC) $(MAIN) $^ -o $(EXE)

$(SRC)/RooLexer.hs: $(SRC)/RooLexer.x
	$(ALEX) $<

$(SRC)/RooParser.hs: $(SRC)/RooParser.y
	$(HAPPY) $<
