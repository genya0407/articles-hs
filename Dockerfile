FROM haskell:8.10.4

WORKDIR /app
RUN cabal update
ADD articles.cabal .
RUN cabal build --only-dependencies -j4
ADD . .
RUN cabal install

CMD ["articles-exe"]
