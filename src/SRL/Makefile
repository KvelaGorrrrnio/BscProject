SRC = Main.hs
HLP = HandleArgs.hs SrlAST.hs Error.hs Parser.hs Translation.hs
EXE = ../bin/srevl

default : compile

compile : $(EXE)

$(EXE) : $(SRC) $(HLP)
	@ghc $(SRC) -i../ -o $(EXE) -odir xyz -hidir xyz

clean :
	@rm -rf *.rlog xyz $(EXE) programs/*.srlog
