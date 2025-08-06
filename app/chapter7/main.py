"""Cursed Cavernsデモアプリケーション

このファイルは、テスト駆動開発で作成されたCursed Cavernsゲームの
デモアプリケーションです。

実行方法:
    python main.py
    または
    uv run python main.py

操作方法:
    - 矢印キー: プレイヤー移動
    - スペースキー: ジャンプ
    - ENTER: ゲーム開始/リスタート
    - ESC: ゲーム終了

ゲームのルール:
    - 宝石（黄色）を集めてスコアを獲得
    - キノコ（緑）でジャンプ力アップ
    - トゲ（赤）に触れるとゲームオーバー
    - 敵キャラクターに触れるとゲームオーバー
    - より高いスコアを目指そう！
"""

from lib.main import main

if __name__ == "__main__":
    print("=== Cursed Caverns ===")
    print("テスト駆動開発で作成されたアクションゲーム")
    print("操作: ↑↓←→移動, SPACE ジャンプ, ENTER 開始, ESC 終了")
    print("開始中...")

    try:
        main()
    except KeyboardInterrupt:
        print("\nゲームを終了しました。")
    except Exception as e:
        print(f"エラーが発生しました: {e}")
        print("開発者に報告してください。")
