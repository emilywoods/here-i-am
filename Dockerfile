FROM haskell:8.8.4 as builder

WORKDIR /app
COPY . /app

RUN stack setup
RUN stack build --copy-bins

FROM haskell:8.8.4 as runtime

COPY --from=builder /root/.local/bin/here-i-am .

CMD ["./here-i-am"]
