build: install
	@echo "Building web server..."
	@mkdir -p ./client && cp -r ../client/build/* ./client
	@make -C ../../src && cp -r ../../src/bin .
	@make -C ../docs && rm -rf ./docs && cp -rf ../docs/site ./docs
	@echo "Web server has been built."

install:
	@npm install

server:
	@npm start

clean:
	@echo "Cleaning web server..."
	@rm -rf bin client node_modules docs

test:
	@npm test

.PHONY: build install server clean test
