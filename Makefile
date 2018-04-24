zipfile=robin.zip

zip: clean
	@echo "Compressing project into ${zipfile}..."
	@zip -r ${zipfile} src/Makefile src/bin src/RL src/SRL src/Common # > /dev/null

clean:
	@$(MAKE) --no-print-directory -C src clean
	@rm -rf ${zipfile}

.PHONY: clean zip
