
HC    = ghc
HAPPY = happy
ALEX  = alex
EXE   = Roo

APP = app
SRC = src

_MAIN = Main.hs
MAIN  = $(patsubst %,$(APP)/%,$(_MAIN))
_OBJ  = RooLexer.hs RooParser.hs
OBJ   = $(patsubst %,$(SRC)/%,$(_OBJ))
_DEPS = RooAST.hs PrettyRoo.hs 
DEPS  = $(patsubst %,$(SRC)/%,$(_DEPS)) $(OBJ)

.PHONY: all clean CLEAN

all: $(EXE)

clean:
	rm -f $(SRC)/*.o $(SRC)/*.hi $(APP)/*.o $(APP)/*.hi
	rm -f $(OBJ)

CLEAN:
	clean
	rm -f $(EXE)

Roo: $(DEPS)
	$(HC) $(MAIN) $^ -o $(EXE)

$(SRC)/RooLexer.hs: $(SRC)/RooLexer.x
	$(ALEX) $<

$(SRC)/RooParser.hs: $(SRC)/RooParser.y
	$(HAPPY) $<
