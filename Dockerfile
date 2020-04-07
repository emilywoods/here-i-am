FROM haskell:8

WORKDIR /resume
RUN cabal update
COPY . /resume
RUN cabal install

CMD ["cli-resume"]