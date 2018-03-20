cc=ghc
mod=$(notdir ${CURDIR})
modlc:=$(shell echo '$(mod)' | tr A-Z a-z)
pfiles=$(wildcard *.hs)
ofile=../bin/${modlc}i
main=../${modlc}.hs
odir=o
hidir=hi

build: ${ofile}
${ofile}: ${pfiles}
	@echo "Building ${mod}..."
	@mkdir -p "$(dir ${ofile})"
	@${cc} "${main}" -o "${ofile}" -odir "${odir}" -hidir "${hidir}" -i=../

clean:
	@echo "Cleaning ${mod}..."
	@rm -rf ${odir} ${hidir} ${ofile}

.PHONY: build clean
