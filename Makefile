PREFIX ?= $(HOME)/.local
BIN_DIR ?= $(PREFIX)/bin

.PHONY: install build-install uninstall test site

install:
	@mkdir -p "$(BIN_DIR)"
	@cp -f scripts/kai "$(BIN_DIR)/kai"
	@chmod +x "$(BIN_DIR)/kai"
	@echo "Installed runner to $(BIN_DIR)/kai"
	@if [ -f ~/.bashrc ] && ! grep -qxF 'export PATH="$(BIN_DIR):$$PATH"' ~/.bashrc; then \
		echo "Adding $(BIN_DIR) to PATH in ~/.bashrc..."; \
		echo 'export PATH="$(BIN_DIR):$$PATH"' >> ~/.bashrc; \
		echo "Please restart your shell or run 'source ~/.bashrc' for changes to take effect."; \
	elif [ -f ~/.zshrc ] && ! grep -qxF 'export PATH="$(BIN_DIR):$$PATH"' ~/.zshrc; then \
		echo "Adding $(BIN_DIR) to PATH in ~/.zshrc..."; \
		echo 'export PATH="$(BIN_DIR):$$PATH"' >> ~/.zshrc; \
		echo "Please restart your shell or run 'source ~/.zshrc' for changes to take effect."; \
	elif [ -f ~/.config/fish/config.fish ] && ! grep -qxF 'fish_add_path $(BIN_DIR)' ~/.config/fish/config.fish; then \
		echo "Adding $(BIN_DIR) to PATH in ~/.config/fish/config.fish..."; \
		echo 'fish_add_path $(BIN_DIR)' >> ~/.config/fish/config.fish; \
		echo "Please restart your shell for changes to take effect."; \
	else \
		echo "$(BIN_DIR) is already in PATH or no supported shell config found."; \
		echo "Manually add 'export PATH=\"$(BIN_DIR):\$$PATH\"' to your shell config."; \
	fi

build-install:
	stack install

uninstall:
	@rm -f "$(BIN_DIR)/kai"
	@echo "Removed $(BIN_DIR)/kai"

test:
	stack test --fast --test-arguments "--format progress"

site:
	bash scripts/export-site.sh
