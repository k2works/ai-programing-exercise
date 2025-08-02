#!/bin/bash
cd /workspaces/ai-programing-exercise/app
#!/bin/bash
# テスト実行スクリプト
# テスト駆動開発から始めるProlog入門2

echo "FizzBuzzテストスイートを実行中..."
swipl -g "consult('test_fizzbuzz.pl'), run_tests, halt."
