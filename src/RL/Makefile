SRC = Main.hs
HLP = HandleArgs.hs Interp.hs AST.hs Error.hs Inversion.hs
EXE = revl

default : compile

compile : $(EXE)

$(EXE) : $(SRC) $(HLP)
	ghc $(SRC) -o $(EXE) -odir xyz -hidir xyz

run : $(EXE)
	@./$(EXE) -l fib.rl

clean :
	@rm -rf *.rlog xyz $(EXE)


