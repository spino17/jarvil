# Makefile

BINARY_NAME := jarvil
INSTALL_DIR := /Users/bhavyabhatt/.jarvil
VERSION := 0.1.0
APP_NAME := jarvil ${VERSION}

.PHONY: all build install uninstall clean

all: build

build:
	@echo "Building ${APP_NAME} ..."
	@cargo build --release

install: build
	@echo "Installing ${APP_NAME} ..."
	@install -d $(INSTALL_DIR)/bin
	@install -m 755 target/release/$(BINARY_NAME) $(INSTALL_DIR)/bin
	@echo "Updating shell script path variable..."
	@echo 'export PATH="$(INSTALL_DIR)/bin:$$PATH"' >> ~/.bashrc
	@echo 'export PATH="$(INSTALL_DIR)/bin:$$PATH"' >> ~/.bash_profile
	@echo "${APP_NAME} installed successfully"

uninstall:
	@echo "Uninstalling ${APP_NAME} ..."
	@rm -f $(INSTALL_DIR)/bin/$(BINARY_NAME)
	@rmdir $(INSTALL_DIR)/bin
	@rmdir $(INSTALL_DIR)
	@echo "${APP_NAME} uninstalled successfully"

clean:
	@echo "Cleaning the project..."
	@cargo
