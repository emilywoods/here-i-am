.DEFAULT_GOAL := help

.PHONY: install
install: 
	stack init
	stack build


.PHONY: format
format: install
	stylish-haskell -i src/Main.hs

.PHONY: run
run: install
	stack exec here-i-am

.PHONY: docker_build
docker_build:
	docker build -t "emilywoods/resume:latest" . 

.PHONY: docker_run
docker_run: docker_build
	docker run -it emilywoods/resume:latest 


