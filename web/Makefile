build: build-client build-server
build-client:
	@make -C client build
build-server:
	@make -C server build

clean: clean-client clean-server
clean-client:
	@make -C client clean
clean-server:
	@make -C server clean

serve:
	@make -C server serve

.PHONY: build build-client build-server clean clean-client clean-server serve
