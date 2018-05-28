all: src web
src:
	@make -C src build
web: src
	@make -C web build

clean: clean-src clean-web
clean-src:
	@make -C src clean
clean-web:
	@make -C web clean

zip: zip-src zip-web
	zip bsc.zip src.zip web.zip
zip-src: clean-src
	@zip -r src.zip src
zip-web: clean-web
	@zip -r web.zip web

install:
	@make -C src install

server:
	@make -C web server

cserver:
	@make -C web cserver

release:
	@zip -r src.zip release

test:
	@make -C src test

.PHONY: all src web install server release clean clean-src clean-web zip zip-src zip-web test cserver
