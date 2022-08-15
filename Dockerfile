FROM debian:bullseye as build-env
ARG PACKAGES="curl upx-ucl build-essential binutils libncurses-dev "
RUN apt-get update -y \
    && apt-get install -y -q ${PACKAGES} \
    && curl -sSL https://get.haskellstack.org/ | sh

ARG STACK_RESOLVER=lts-18.28
RUN stack setup --resolver $STACK_RESOLVER

FROM build-env as build-deps-only
COPY stack.yaml stack.yaml.lock package.yaml bricktrader.cabal README.md ChangeLog.md /bricktrader/
WORKDIR /bricktrader
RUN stack install record-dot-preprocessor
RUN stack test --only-dependencies
RUN stack build --only-dependencies

FROM build-deps-only as build
COPY app ./app
COPY src ./src
COPY test ./test
RUN stack test
RUN stack build
RUN stack --local-bin-path ./dist install
RUN strip -s dist/bricktrader-exe
RUN upx dist/bricktrader-exe
