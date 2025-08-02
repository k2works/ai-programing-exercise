#!/bin/bash
# FizzBuzz実行スクリプト
# テスト駆動開発から始めるProlog入門3

echo "=== Prolog OO FizzBuzz ==="

# srcディレクトリに移動
cd "$(dirname "$0")/../src"

echo "対話モードでFizzBuzzを開始します..."
echo "使用方法:"
echo "  ?- fizzbuzz_print(1, 100, 1).  % タイプ1"
echo "  ?- halt.                      % 終了"
echo ""

swipl -g "consult('fizzbuzz.pl')"
