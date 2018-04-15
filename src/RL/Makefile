SRC = Main.hs
EXE = interp

default : compile

compile : $(EXE)

$(EXE) : $(SRC)
	ghc $(SRC) -o $(EXE) -odir xyz -hidir xyz

run : $(EXE)
	./$(EXE)

clean :
	rm -rf *.rlog xyz $(EXE)


