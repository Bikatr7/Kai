PREFIX ?= $(HOME)/.local
BIN_DIR ?= $(PREFIX)/bin

.PHONY: install build-install uninstall test site

install:
	@mkdir -p "$(BIN_DIR)"
	@cp -f scripts/kai "$(BIN_DIR)/kai"
	@chmod +x "$(BIN_DIR)/kai"
	@echo "Installed runner to $(BIN_DIR)/kai"
	@echo "Ensure $(BIN_DIR) is on your PATH."

build-install:
	stack install

uninstall:
	@rm -f "$(BIN_DIR)/kai"
	@echo "Removed $(BIN_DIR)/kai"

test:
	stack test --fast --test-arguments "--format progress"

site:
	bash scripts/export-site.sh

