cc=ghc
mod=$(notdir ${CURDIR})
cfiles=$(wildcard ../common/*.hs)
ifiles=$(notdir ${cfiles})
pfiles=$(wildcard *.hs)
ofile=../bin/${mod}
main=Main.hs
odir=o
hidir=hi

build: ${ofile}
${ofile}: ${ifiles} ${pfiles}
	@echo "Building ${mod}..."
	@mkdir -p "$(dir ${ofile})"
	@${cc} "${main}" -o "${ofile}" -odir "${odir}" -hidir "${hidir}"

link: ${ifiles}
${ifiles}: ${cfiles}
	@echo "Linking common files with ${mod}..."
	@cp $^ .

clean:
	@echo "Cleaning ${mod}..."
	@rm -rf ${ifiles} ${odir} ${hidir} ${ofile}

.PHONY: build link clean
