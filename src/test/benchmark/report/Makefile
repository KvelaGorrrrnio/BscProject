main    := thesis
outfile := thesis
outdir  := out

compile := pdflatex --output-directory=$(outdir) -shell-escape -file-line-error \
           -jobname $(outfile) $(main)

default: compile #run

compile:
	mkdir -p $(outdir)
	$(compile)
	#$(compile)

clean:
	rm -rf $(outdir)
