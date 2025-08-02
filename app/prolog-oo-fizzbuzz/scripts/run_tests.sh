#!/bin/bash
# テスト実行スクリプト
# テスト駆動開発から始めるProlog入門3

echo "=== Prologテストスイート実行 ==="

# testsディレクトリに移動
cd "$(dirname "$0")/../tests"

# 各テストファイルを実行
for test_file in test_*.pl; do
    if [ -f "$test_file" ]; then
        echo ""
        echo "実行中: $test_file"
        echo "----------------------------------------"
        swipl -g "consult('$test_file'), run_tests, halt." || exit 1
    fi
done

echo ""
echo "=== 全テスト完了 ==="
