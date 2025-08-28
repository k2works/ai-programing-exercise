#!/bin/bash
# Cake Build Script Runner

if [ $# -eq 0 ]; then
    echo "使用可能なタスク:"
    dotnet cake build.cake --showdescription
else
    dotnet cake build.cake --target="$1"
fi