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

server:
	@make -C server server
cserver:
	@make -C client server

test: test-client test-server
test-client:
	@make -C client test
test-server:
	@make -C server test

.PHONY: build build-client build-server clean clean-client clean-server server cserver test test-client test-server
