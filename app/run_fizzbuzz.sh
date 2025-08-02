#!/bin/bash
cd /workspaces/ai-programing-exercise/app
#!/bin/bash
# FizzBuzz実行スクリプト
# テスト駆動開発から始めるProlog入門2

# デフォルト実行
if [ $# -eq 0 ]; then
    echo "デフォルト設定でFizzBuzzを実行中（1-100, タイプ1）..."
    swipl -g "consult('app.pl'), main, halt."
# 引数付き実行
else
    echo "引数付きでFizzBuzzを実行中: $@"
    # 引数をProlog形式に変換
    args="["
    for arg in "$@"; do
        if [ "$args" != "[" ]; then
            args="$args, "
        fi
        args="$args'$arg'"
    done
    args="$args]"
    
    swipl -g "consult('app.pl'), main($args), halt."
fi
