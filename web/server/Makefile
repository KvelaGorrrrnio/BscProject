build: install
	@echo "Building web server..."
	@make -C ../client clean
	@npm run build

install:
	@npm install

server:
	@npm start

clean:
	@echo "Cleaning web server..."
	@rm -rf bin client node_modules

.PHONY: build install serve clean
