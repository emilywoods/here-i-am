.DEFAULT_GOAL := help

.PHONY: install
install: 
	cabal update && cabal install


.PHONY: format
format: install
	stylish-haskell -i src/Main.hs


.PHONY: run
run: install
	cabal run

.PHONY: docker_build
docker_build:
	docker build -t "emilywoods/resume:latest" . 

.PHONY: docker_run
docker_run: docker_build
	docker run -it emilywoods/resume:latest 

.PHONY: help
help:
	@echo ""
	@echo "Available targets:"
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$'


