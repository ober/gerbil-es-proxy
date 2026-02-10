##
# Build es-proxy (Gerbil Scheme)
#
# @file
# @version 0.2

pkg := es-proxy
Pkg := es-proxy
version := 0.2.0
PREFIX ?= /usr/local
OPENSSL_RPATH = /home/linuxbrew/.linuxbrew/opt/openssl@3/lib

.PHONY: default help build release debug test test-functional test-memory test-stress install install-dev uninstall deps deps-update clean ensure-docker build-rpm build-deb deploy rpm deb

default: help

help:
	@echo "$(Pkg) Makefile targets:"
	@echo ""
	@echo "  Building:"
	@echo "    make build        Build the $(pkg) binary"
	@echo "    make release      Build (release alias)"
	@echo "    make debug        Build (debug alias)"
	@echo ""
	@echo "  Testing:"
	@echo "    make test              Run all tests"
	@echo "    make test-functional   Run functional tests (mock ES)"
	@echo "    make test-memory       Run memory leak detection test"
	@echo "    make test-stress       Run extended stress test (5 minutes)"
	@echo ""
	@echo "  Installation:"
	@echo "    make install      Install binary to $(PREFIX)/bin"
	@echo "    make install-dev  Install from source (no compile)"
	@echo "    make uninstall    Remove from $(PREFIX)/bin"
	@echo ""
	@echo "  Dependencies:"
	@echo "    make deps         Install package dependencies"
	@echo "    make deps-update  Update package dependencies"
	@echo ""
	@echo "  Packaging:"
	@echo "    make rpm          Build RPM package"
	@echo "    make deb          Build DEB package"
	@echo "    make deploy       Deploy RPM package"
	@echo ""
	@echo "  Other:"
	@echo "    make clean        Remove build artifacts"
	@echo "    make help         Show this help"

build: .gerbil/bin/$(pkg)

release: build

debug: build

# Test targets
test:
	@echo "Running $(Pkg) tests"
	@rm -rf .gerbil/lib/es-proxy ~/.gerbil/lib/es-proxy ~/.gerbil/lib/static/es-proxy__*
	GERBIL_LOADPATH=$(CURDIR) gerbil test es-proxy-test.ss

test-functional: test

test-memory:
	@echo "Running $(Pkg) memory leak tests"
	@rm -rf .gerbil/lib/es-proxy ~/.gerbil/lib/es-proxy ~/.gerbil/lib/static/es-proxy__*
	GERBIL_LOADPATH=$(CURDIR) gerbil test es-proxy-test.ss

test-stress: build
	@echo "Running $(Pkg) extended stress test (5 minutes, compiled)"
	.gerbil/bin/es-proxy-stress-test

# Install/uninstall targets
install: build
	@echo "Installing $(Pkg) to $(DESTDIR)$(PREFIX)/bin"
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 .gerbil/bin/$(pkg) $(DESTDIR)$(PREFIX)/bin/$(pkg)

install-dev:
	@echo "Installing $(Pkg) from source to $(DESTDIR)$(PREFIX)/bin"
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 $(CURDIR)/es-proxy.ss $(DESTDIR)$(PREFIX)/bin/$(pkg)

uninstall:
	@echo "Removing $(Pkg) from $(DESTDIR)$(PREFIX)/bin"
	rm -f $(DESTDIR)$(PREFIX)/bin/$(pkg)

# Dependencies
deps:
	@echo "Installing dependencies"
	gerbil pkg install

deps-update:
	@echo "Updating dependencies"
	gerbil pkg update

# Build rules
.gerbil/bin/$(pkg): es-proxy.ss mock-es.ss es-proxy-stress-test.ss build.ss gerbil.pkg
	@echo "Building $(Pkg) Binary"
	gerbil build
	patchelf --set-rpath $(OPENSSL_RPATH) .gerbil/bin/$(pkg)
	patchelf --set-rpath $(OPENSSL_RPATH) .gerbil/bin/es-proxy-stress-test

clean:
	@echo "Make clean:"
	rm -rf .gerbil/bin .gerbil/lib
	rm -rf ~/.gerbil/lib/es-proxy ~/.gerbil/lib/static/es-proxy__*

# Packaging
ensure-docker:
	@echo "Ensuring docker is installed"
	@command -v docker || (echo "Docker is not installed" && exit 1)

build-rpm: ensure-docker build
	@echo "Generating $(Pkg) rpm package"
	@echo "RPM packaging not yet configured for Gerbil build"

build-deb: ensure-docker build
	@echo "Generating $(Pkg) deb package"
	@echo "DEB packaging not yet configured for Gerbil build"

deploy: build-rpm
	@echo "Deploying $(Pkg) rpm"
	@echo "RPM deployment not yet configured for Gerbil build"

rpm: build-rpm
deb: build-deb
