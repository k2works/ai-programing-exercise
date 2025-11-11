#!/bin/bash
# Cakeビルドスクリプトラッパー

if [ "$1" = "" ]; then
    dotnet cake
else
    dotnet cake --target="$1" "${@:2}"
fi
