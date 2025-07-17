#!/bin/bash
# ファイル変更監視スクリプト（Guardの代替）

echo "Haskell FizzBuzz Auto Runner"
echo "ファイル変更を監視してテストを自動実行します..."
echo "Ctrl+Cで終了"

# 無限ループでファイルの変更を監視
while true; do
    # ファイルの変更をチェック（簡易版）
    find src test app -name "*.hs" -newer .last_run 2>/dev/null && {
        echo ""
        echo "=== ファイル変更を検出 ==="
        echo "$(date): テストを実行中..."
        
        # タイムスタンプファイルを更新
        touch .last_run
        
        # 全自動化タスクを実行
        make all
        
        echo "=== 実行完了 ==="
        echo ""
    }
    
    # 2秒待機
    sleep 2
done
