main    := thesis
outfile := thesis
outdir  := out

compile := pdflatex --output-directory=$(outdir) -shell-escape -file-line-error \
           -jobname $(outfile) $(main)
bibtex  := bibtex $(outdir)/$(outfile)

default: compile #run

compile:
	#mkdir -p $(outdir)
	$(compile)
	$(compile)
	#$(bibtex)       - Commented out because it's slow and stuff
	#$(compile)      - Commented out because it's slow and stuff

clean:
	rm -rf $(outdir)
