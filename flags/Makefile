SRC = Main.hs
EXE = revl

default : compile

compile : $(EXE)

$(EXE) : $(SRC)
	ghc $(SRC) -o $(EXE) -odir xyz -hidir xyz

run : $(EXE)
	./$(EXE)

clean :
	rm -rf xyz $(EXE)


