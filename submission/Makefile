
HC      = ghc
HCFLAGS = -O2 -Wall -fno-warn-unused-imports
HAPPY   = happy
ALEX    = alex

EXE = Roo

MAIN = Main.hs
GEN  = RooLexer.hs RooParser.hs
DEPS = RooAST.hs PrettyRoo.hs RooSymbolTable.hs RooOzCodeGen.hs OzCode.hs RooCTrans.hs RooPyTrans.hs $(GEN)

.PHONY: all gen clean cleanly clobber

$(EXE): $(DEPS) $(MAIN)
	$(HC) $(HCFLAGS) $^ -o $@

RooLexer.hs: RooLexer.x
	$(ALEX) $< -o $@

RooParser.hs: RooParser.y
	$(HAPPY) $< -o $@

all: $(EXE)

gen: $(GEN)

clean:
	rm -f *.o *.hi $(GEN)

cleanly: all clean

clobber: clean
	rm -f $(EXE)
