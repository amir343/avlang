#!/bin/bash

IMAGE="erlang:17"
MOUNTS="$PWD:/opt/terlang"
CMD="cd /opt/terlang && make $@"

docker run \
       --rm \
       -v $MOUNTS \
       -t $IMAGE \
       bin/bash -c "$CMD"
