zipfile=robin.zip
nottest=Makefile srl rl common

build: rl srl

rl:
	@$(MAKE) -C rl

srl:
	@$(MAKE) -C srl

clean:
	@$(MAKE) -C rl clean
	@$(MAKE) -C srl clean
	@rm -rf ${zipfile} bin

test:
	@./test/run_tests.sh -d

test-rl:
	@./test/run_tests.sh rl -d

test-srl:
	@./test/run_tests.sh srl -d

zip: clean
	@echo "Compressing project into ${zipfile}..."
	@zip -r ${zipfile} . > /dev/null

release: clean
	@echo "Compressing release of project into release_${zipfile}..."
	@zip -r "release_${zipfile}" ${nottest} > /dev/null

.PHONY: build rl srl clean test test-srl test-rl zip release
