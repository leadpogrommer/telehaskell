FROM haskell:8.10

RUN apt-get update && apt-get install -y libpcre3-dev wait-for-it \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install --lib HTTP http-server aeson mongoDB pcre-heavy
ADD . /dist

WORKDIR /dist

RUN cabal build

ENTRYPOINT [ "wait-for-it", "db:27017", "--", "cabal", "run" ]

