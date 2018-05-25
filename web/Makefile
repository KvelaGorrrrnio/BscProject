build: build-server
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

.PHONY: build build-client build-server clean clean-client clean-server server cserver
