SRC = Main.hs
HLP = HandleArgs.hs Interp.hs AST.hs Error.hs Inversion.hs
EXE = ../bin/revl

default : compile

compile : $(EXE)

$(EXE) : $(SRC) $(HLP)
	ghc $(SRC) -o $(EXE) -odir xyz -hidir xyz

run : $(EXE)
	@./$(EXE) fib.rl

runhelp : $(EXE)
	@./$(EXE) fib.rl --h

clean :
	@rm -rf *.rlog xyz $(EXE)


