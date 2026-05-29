#!/usr/bin/env sh
set -e
cd "$(dirname "$0")"
mkdir -p build
cd build
PKG_CONFIG=/usr/bin/false cmake ..
cmake --build . --config Release
mkdir -p ../../priv
cp lexbor_port ../../priv/
echo "Built priv/lexbor_port"
