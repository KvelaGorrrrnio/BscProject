build: install
	@echo "Building web server..."
	@mkdir -p ./client && cp -r ../client/build/* ./client
	@make -C ../../src && cp -r ../../src/bin .
	@echo "Web server has been build."

install:
	@npm install

server:
	@npm start

clean:
	@echo "Cleaning web server..."
	@rm -rf bin client node_modules

.PHONY: build install server clean
