#/usr/bin/env bash

# A small wrapper for calling the checker CLI image corresponding to the
# current master branch revision. Arguments are passed directly to the
# `checker deploy` command.

set -e

if [ ! -f "$PWD/my-account.json" ]
then
    echo "No account file found at $PWD/my-account.json. Make sure that your account is available at this location."
    exit 1
fi
if [ -z "$NODE_ADDRESS" ]
then
    echo 'Environment variable $NODE_ADDRESS must be set.'
    exit 1
fi
if [ -z "$NODE_PORT" ]
then
    echo 'Environment variable $NODE_PORT must be set.'
    exit 1
fi
if [ -z "$VERSION" ]
then
    echo '$VERSION not set. Using default "master" as the docker image tag'
    VERSION="master"
else
    echo "Using version ${VERSION} as the docker image tag"
fi


#FIXME: --pull always / image name
docker run --rm \
      -v $PWD/my-account.json:/my-account.json \
      checker-client:$VERSION \
        checker \
        deploy \
        --address $NODE_ADDRESS \
        --port $NODE_PORT \
        --key /my-account.json \
        $@
