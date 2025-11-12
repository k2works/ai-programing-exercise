#!/bin/bash
# OpenAPI ドキュメント生成スクリプト

echo "OpenAPI ドキュメントを生成中..."

# プロジェクトディレクトリ
PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
DLL_PATH="$PROJECT_DIR/bin/Debug/net9.0/SalesManagement.Api.dll"
OUTPUT_FILE="$PROJECT_DIR/openapi.json"
PORT=5123

# DLL が存在するか確認
if [ ! -f "$DLL_PATH" ]; then
    echo "エラー: $DLL_PATH が見つかりません"
    exit 1
fi

# アプリケーションをバックグラウンドで起動
echo "アプリケーションを起動中..."
ASPNETCORE_ENVIRONMENT=Development dotnet "$DLL_PATH" --urls "http://localhost:$PORT" > /dev/null 2>&1 &
APP_PID=$!

# クリーンアップ関数
cleanup() {
    if [ ! -z "$APP_PID" ]; then
        kill $APP_PID 2>/dev/null || true
        wait $APP_PID 2>/dev/null || true
    fi
}
trap cleanup EXIT

# アプリケーションが起動するまで待機（最大30秒）
echo "アプリケーションの起動を待機中..."
for i in {1..30}; do
    if curl -s "http://localhost:$PORT/swagger/v1/swagger.json" > /dev/null 2>&1; then
        echo "アプリケーションが起動しました"
        break
    fi
    if [ $i -eq 30 ]; then
        echo "エラー: アプリケーションの起動がタイムアウトしました"
        exit 1
    fi
    sleep 1
done

# OpenAPI ドキュメントをダウンロード
echo "OpenAPI ドキュメントをダウンロード中..."
if curl -s "http://localhost:$PORT/swagger/v1/swagger.json" -o "$OUTPUT_FILE"; then
    # OpenAPI バージョンを 3.0.1 に修正（IDE との互換性のため）
    if command -v sed &> /dev/null; then
        sed -i '' 's/"openapi": "3\.0\.[0-9]*"/"openapi": "3.0.1"/' "$OUTPUT_FILE"
        echo "OpenAPI バージョンを 3.0.1 に修正しました"
    fi
    echo "OpenAPI ドキュメントを生成しました: $OUTPUT_FILE"
else
    echo "エラー: OpenAPI ドキュメントのダウンロードに失敗しました"
    exit 1
fi
