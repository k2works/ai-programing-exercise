#!/bin/bash

# C#/.NET用の自動実行スクリプト（Guard相当）
# ファイルの変更を監視して、テストとフォーマットを自動実行

echo "Starting .NET file watcher (Guard equivalent)..."
echo "Monitoring *.cs files for changes..."
echo "Press Ctrl+C to exit"

# 初回実行
echo "Running initial test and format..."
dotnet format
dotnet test --collect:"XPlat Code Coverage" --results-directory ./coverage
if [ $? -eq 0 ]; then
    reportgenerator -reports:"coverage/*/coverage.cobertura.xml" -targetdir:"coverage" -reporttypes:Html
fi

# inotifywaitを使ったファイル監視（Linuxの場合）
if command -v inotifywait >/dev/null 2>&1; then
    while inotifywait -r -e modify,create,delete --include='.*\.cs$' .; do
        echo "File change detected, running tests and format..."
        dotnet format
        dotnet test --collect:"XPlat Code Coverage" --results-directory ./coverage
        if [ $? -eq 0 ]; then
            reportgenerator -reports:"coverage/*/coverage.cobertura.xml" -targetdir:"coverage" -reporttypes:Html
        fi
    done
else
    echo "inotifywait not available. Install inotify-tools for file watching."
    echo "Running dotnet watch test instead..."
    dotnet watch test
fi
