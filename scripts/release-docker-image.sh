#!/bin/sh

set -eu

if [ -z "$1" ]; then
  echo 'Please specify image version.'
  exit 1
fi

docker build . -f Dockerfile.static -t docker.genya0407.net/articles-static:$1
docker build . -t docker.genya0407.net/articles:$1

docker push docker.genya0407.net/articles:$1
docker push docker.genya0407.net/articles-static:$1
