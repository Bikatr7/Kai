PREFIX ?= $(HOME)/.local
BIN_DIR ?= $(PREFIX)/bin

.PHONY: install build-install uninstall test site

install:
	@mkdir -p "$(BIN_DIR)"
	@cp -f scripts/kai "$(BIN_DIR)/kai"
	@chmod +x "$(BIN_DIR)/kai"
	@echo "Installed runner to $(BIN_DIR)/kai"
	@if ! grep -qxF 'export PATH="$(BIN_DIR):$$PATH"' ~/.bashrc; then \
		echo "Adding $(BIN_DIR) to PATH in ~/.bashrc..."; \
		echo 'export PATH="$(BIN_DIR):$$PATH"' >> ~/.bashrc; \
	else \
		echo "$(BIN_DIR) is already in PATH in ~/.bashrc."; \
	fi
	@echo "Please restart your shell or run 'source ~/.bashrc' for changes to take effect."

build-install:
	stack install

uninstall:
	@rm -f "$(BIN_DIR)/kai"
	@echo "Removed $(BIN_DIR)/kai"

test:
	stack test --fast --test-arguments "--format progress"

site:
	bash scripts/export-site.sh
