
HC      = ghc
HCFLAGS = -O2
HAPPY   = happy
ALEX    = alex
EXE     = Roo

APP = app
SRC = src

MAIN = Main.hs
GEN  = RooLexer.hs RooParser.hs
DEPS = RooAST.hs PrettyRoo.hs $(GEN)

.PHONY: all gen clean CLEAN cleangen

all: $(EXE)

gen: $(GEN)

clean:
	rm -f *.o *.hi *.o *.hi

CLEAN: clean
	rm -f $(EXE)

cleangen: 
	rm -f $(GEN)

cleanly: all clean

$(EXE): $(DEPS)
	$(HC) $(HCFLAGS) $(MAIN) $^ -o $(EXE)

RooLexer.hs: RooLexer.x
	$(ALEX) $<

RooParser.hs: RooParser.y
	$(HAPPY) $<
