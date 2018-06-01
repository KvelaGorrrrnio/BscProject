main    := thesis
outfile := thesis
outdir  := out

compile := pdflatex --output-directory=$(outdir) -shell-escape -file-line-error \
           -jobname $(outfile) $(main)
#bibtex  := bibtex $(outdir)/$(outfile)
biber   := biber $(outdir)/$(outfile)

default: compile #run

compile:
	mkdir -p $(outdir)
	$(compile)
	$(compile)
	$(biber)
	$(compile)

clean:
	rm -rf $(outdir)
