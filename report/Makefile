main    := document
outfile := thesis
outdir  := out
compile := pdflatex --output-directory=$(outdir) -file-line-error \
           -jobname $(outfile) $(main)
bibtex  := bibtex $(outfile)

default: run

run: compile
	cd $(outdir) && $(bibtex)
	$(compile)

compile:
	mkdir -p $(outdir)
	cp *.{bst,cls,bib} ./out
	$(compile)
	#cd $(outdir) && $(bibtex)
	$(compile)

clean:
	rm -rf $(outdir)
