zipfile=robin.zip
nottest=Makefile SRL RL

build: rl srl

rl:
	@$(MAKE) --no-print-directory -C RL

srl:
	@$(MAKE) --no-print-directory -C SRL

clean:
	@$(MAKE) --no-print-directory -C RL clean
	@$(MAKE) --no-print-directory -C SRL clean
	@rm -rf ${zipfile}

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

runrl:
	runhaskell rl.hs programs/rl/fib.rl
runsrl:
	runhaskell srl.hs programs/srl/fib.srl

.PHONY: build rl srl clean test test-srl test-rl zip release
