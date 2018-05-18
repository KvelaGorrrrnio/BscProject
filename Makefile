all: src web
src:
	@make -C src build
web:
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

serve:
	@make -C web serve

release:
	@zip -r src.zip release

.PHONY: all src web install serve release clean clean-src clean-web zip zip-src zip-web
