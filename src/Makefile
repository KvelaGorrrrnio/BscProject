zipfile=robin.zip

build: rl srl

rl:
	@$(MAKE) -C rl

srl:
	@$(MAKE) -C srl

clean:
	@$(MAKE) -C rl clean
	@$(MAKE) -C srl clean
	@rm -rf ${zipfile} bin

test: rl srl
	@echo "Not testing..."

zip: clean
	@echo "Compressing project into ${zipfile}..."
	@zip -r ${zipfile} . > /dev/null

.PHONY: build rl srl clean test zip
