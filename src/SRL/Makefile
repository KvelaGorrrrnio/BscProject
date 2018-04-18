SRC = Main.hs
HLP = *.hs
EXE = ../bin/srevl
HIDIR = hi
ODIR  = o

default : compile

compile : $(EXE)

$(EXE) : $(SRC) $(HLP)
	@mkdir -p ../bin
	@ghc -XLambdaCase -i../ $(SRC) -o $(EXE) -odir $(ODIR) -hidir $(HIDIR)

clean :
	@rm -rf *.srlog $(ODIR) $(HIDIR) $(EXE) programs/*.srlog

.PHONY: default compile clean
