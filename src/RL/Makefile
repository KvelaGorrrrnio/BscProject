SRC = Main.hs
HLP = HandleArgs.hs Interp.hs AST.hs Error.hs Inversion.hs Parser.hs
EXE = ../bin/revl

default : compile

compile : $(EXE)

$(EXE) : $(SRC) $(HLP)
	@ghc -XLambdaCase $(SRC) -o $(EXE) -odir xyz -hidir xyz

run : $(EXE)
	@./$(EXE) programs/fib.rl

runhelp : $(EXE)
	@./$(EXE) programs/fib.rl --h

runlog : $(EXE)
	@./$(EXE) programs/fib.rl --log-stdout

clean :
	@rm -rf *.rlog xyz $(EXE) programs/*.rlog

zip : clean
	@zip -r rl.zip .

.PHONY: default compile run runhelp runlog clean zip
