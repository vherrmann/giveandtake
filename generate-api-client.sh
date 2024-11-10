#!/bin/sh

set -e

cd ./server
rm swagger.json || true;
direnv exec . cabal run giveandtake-generate-typescript -- swagger.json
direnv exec . openapi-generator-cli generate -i swagger.json -g typescript-axios  -o ~/repos/giveandtake/client/src/api/autogen --additional-properties=useSingleRequestParameter=true,disallowAdditionalPropertiesIfNotPresent=false
