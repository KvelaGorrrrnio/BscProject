main    := thesis
outfile := thesis
outdir  := out

compile := pdflatex --output-directory=$(outdir) -shell-escape -file-line-error \
           -jobname $(outfile) $(main)
biber   := biber $(outdir)/$(outfile)

default: compile #run

compile:
	mkdir -p $(outdir)
	$(compile)
	$(biber)
	$(compile)
	$(compile)

clean:
	rm -rf $(outdir)
