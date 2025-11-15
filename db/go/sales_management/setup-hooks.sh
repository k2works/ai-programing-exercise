#!/bin/bash
# Gitフックをセットアップするスクリプト

echo "Gitフックをセットアップ中..."

# プロジェクトルートディレクトリに移動
cd "$(dirname "$0")"

# .git/hooksディレクトリが存在しない場合は作成
if [ ! -d ".git/hooks" ]; then
    echo ".git/hooksディレクトリが見つかりません。Gitリポジトリを初期化してください。"
    exit 1
fi

# pre-commitフックをコピー
cp .githooks/pre-commit .git/hooks/pre-commit

# 実行権限を付与
chmod +x .git/hooks/pre-commit

echo "✅ Gitフックのセットアップが完了しました"
echo "コミット前に自動で品質チェックが実行されます"
