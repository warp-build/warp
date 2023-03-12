#!/bin/bash

mix escript.install hex protobuf --force

export PATH="$PATH:$HOME/.mix/escripts"

OUTDIR=${PWD}/lib/_proto
mkdir -p ${OUTDIR}

mkdir -p priv/protos
pushd priv/protos

for file in $(find -L . -name "*.proto"); do
  echo $file
  protoc \
    --elixir_out=inline_docs=true,one_file_per_module=true,gen_descriptors=true,plugins=grpc:${OUTDIR} \
    $file
done

popd
