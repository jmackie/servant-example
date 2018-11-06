SHELL      = /usr/bin/env bash
BUILD_DIR ?= bin

define GHCID_COMMAND_FLAGS
	--ghci-options=-fno-break-on-exception \
	--ghci-options=-fno-break-on-error \
	--ghci-options=-ferror-spans \
	--ghci-options=-j
endef


.PHONY: build
build: ## Run swagger-redoc then stack-build
build: swagger-redoc stack-build


.PHONY: test
test: ## Alias for stack-test
test: stack-test


.PHONY: stack-build
stack-build: ## Build the server binary to $BUILD_DIR (default: ./bin)
stack-build:
	@[ -d $(BUILD_DIR) ] || mkdir -p $(BUILD_DIR)
	@stack build servant-example:exe:server \
        --copy-bins \
        --local-bin-path $(BUILD_DIR)


.PHONY: stack-test
stack-test: ## Run stack tests
stack-test:
	@stack test


.PHONY: ghcid
ghcd: ## Start ghcid
ghcid:
	@-ghcid \
		--command='stack ghci . \
				   --main-is servant-example:exe:server \
				   $(GHCID_COMMAND_FLAGS)' \
        --test=':main --config config.json' \
		--restart=swagger/dist/index.html \
		--reload=config.json


.PHONY: ghcid-test
ghcid-test: ## Start ghcid with tests
ghcid-test:
	@-ghcid \
		--command='stack ghci . \
				   --package hspec \
				   --package hspec-wai \
				   --package process \
				   --main-is servant-example:test:unit \
				   $(GHCID_COMMAND_FLAGS)' \
        --test=':main' \
		--restart=config.json


.PHONY: swagger-vanilla
swagger-vanilla: ## Build swagger-ui (vanilla)
swagger-vanilla:
	@# Clean up first
	@find swagger/dist -type f -not -name index.html -exec rm {} \;
	@cd ./swagger/vanilla && \
		./node_modules/.bin/parcel build \
			--public-url /docs/ \
			--out-dir ../dist \
			--target browser \
			index.html


.PHONY: swagger-redoc
swagger-redoc: ## Build swagger-ui (ReDoc)
swagger-redoc:
	@# Clean up first
	@find swagger/dist -type f -not -name index.html -exec rm {} \;
	@cd ./swagger/redoc && \
		./node_modules/.bin/parcel build \
			--public-url /docs/ \
			--out-dir ../dist \
			--target browser \
			--log-level debug \
			index.html

.PHONY: help
help: ## Print this help message
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
    	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36;1m%-30s\033[0m %s\n", $$1, $$2}'

