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


