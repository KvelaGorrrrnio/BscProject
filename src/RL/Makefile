SRC = Main.hs
HLP = *.hs ../Common/*.hs
EXE = ../bin/rl
HIDIR = hi
ODIR  = o

default : compile

compile : $(EXE)

$(EXE) : $(SRC) $(HLP)
	@mkdir -p ../bin
	@ghc -XLambdaCase -i../ $(SRC) -o $(EXE) -odir $(ODIR) -hidir $(HIDIR)

clean :
	@rm -rf *.rlog $(ODIR) $(HIDIR) $(EXE) programs/*.rlog

.PHONY: default compile clean
