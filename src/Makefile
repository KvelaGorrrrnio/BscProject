zipfile=robin.zip
nottest=Makefile rl.hs srl.hs SRL RL

build: rl srl

rl:
	@$(MAKE) -C RL

srl:
	@$(MAKE) -C SRL

clean:
	@$(MAKE) -C RL clean
	@$(MAKE) -C SRL clean
	@rm -rf ${zipfile} bin

test:
	@./test/run_tests.sh -d

test-rl:
	@./test/run_tests.sh RL -d

test-srl:
	@./test/run_tests.sh SRL -d

zip: clean
	@echo "Compressing project into ${zipfile}..."
	@zip -r ${zipfile} . > /dev/null

release: clean
	@echo "Compressing release of project into release_${zipfile}..."
	@zip -r "release_${zipfile}" ${nottest} > /dev/null

.PHONY: build rl srl clean test test-srl test-rl zip release
