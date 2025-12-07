#!/usr/bin/env python3
"""
線形探索と2分探索の比較回数を確認するPythonコード
（F#/Haskellと同じロジックで実装）
"""

import math

def linear_search(arr, target):
    """
    線形探索（前から順番に探す）

    たとえ話: 本棚で本を探す
    [本1] [本2] [本3] [本4] [本5]...
      ↑     ↑     ↑
    1冊目 2冊目 3冊目... と順番に見ていく
    """
    count = 0
    for i, val in enumerate(arr):
        count += 1  # 比較するたびに+1
        if val == target:
            return (i, count)  # (見つかった位置, 比較回数)
    return (None, count)  # 見つからなかった

def binary_search(arr, target):
    """
    2分探索（半分に分けながら探す）

    たとえ話: 数当てゲーム
    「50？」→「大きい」→ 右半分だけ見る
    「75？」→「小さい」→ 左半分だけ見る
    ...どんどん半分になるから速い！

    ※データが小さい順に並んでいることが必要！
    """
    count = 0
    left = 0
    right = len(arr) - 1

    while left <= right:
        count += 1  # 比較するたびに+1
        mid = (left + right) // 2

        if arr[mid] == target:
            return (mid, count)  # 見つかった！
        elif arr[mid] < target:
            left = mid + 1  # 右半分を探す
        else:
            right = mid - 1  # 左半分を探す

    return (None, count)  # 見つからなかった

def theoretical_linear(n):
    """線形探索の理論値"""
    best = 1
    worst = n
    average = (n + 1) / 2
    return (best, average, worst)

def theoretical_binary(n):
    """2分探索の理論値"""
    best = 1
    log_n = math.log2(n)
    average = int(log_n)  # floor(log₂n)
    worst = int(log_n) + 1  # floor(log₂n) + 1
    return (best, average, worst)

def visualize_binary_search(arr, target):
    """2分探索の動きを可視化"""
    print(f"【{target} を探す】\n")

    left = 0
    right = len(arr) - 1
    step = 1

    while left <= right:
        mid = (left + right) // 2
        mid_val = arr[mid]

        # 配列を可視化
        print(f"ステップ{step}: [", end="")
        for i, val in enumerate(arr):
            if i < left or i > right:
                print(" . ", end="")
            elif i == mid:
                print(f"({val:2d})", end="")
            else:
                print(f" {val:2d} ", end="")
        print("]")

        print(f"          真ん中の値 = {mid_val}")

        if mid_val == target:
            print(f"          → 見つかった！ {step}回で発見\n")
            return
        elif mid_val < target:
            print(f"          → {mid_val} < {target} なので右半分を探す\n")
            left = mid + 1
        else:
            print(f"          → {mid_val} > {target} なので左半分を探す\n")
            right = mid - 1

        step += 1

    print("見つからなかった...\n")

def main():
    print("==========================================")
    print("線形探索と2分探索の比較回数")
    print("==========================================")
    print()

    # テスト用データ（1から100まで）
    data = list(range(1, 101))
    n = len(data)

    print(f"データ数: {n} 個")
    print()

    # 理論値を計算
    lin_best, lin_avg, lin_worst = theoretical_linear(n)
    bin_best, bin_avg, bin_worst = theoretical_binary(n)

    print("【理論値】")
    print("┌─────────────┬────────┬────────┬────────┐")
    print("│ 探索方法    │ 最良   │ 平均   │ 最悪   │")
    print("├─────────────┼────────┼────────┼────────┤")
    print(f"│ 線形探索    │ {lin_best:4d}回 │ {lin_avg:5.1f}回│ {lin_worst:4d}回 │")
    print(f"│ 2分探索     │ {bin_best:4d}回 │ {bin_avg:5d}回 │ {bin_worst:4d}回 │")
    print("└─────────────┴────────┴────────┴────────┘")
    print()

    # 実験
    print("【実験結果】")
    print()

    # 最初の要素を探す
    target1 = 1
    _, lin_count1 = linear_search(data, target1)
    _, bin_count1 = binary_search(data, target1)
    print(f"■ {target1} を探す（最初にある）:")
    print(f"  線形探索: {lin_count1}回")
    print(f"  2分探索: {bin_count1}回")
    print()

    # 真ん中を探す
    target2 = 50
    _, lin_count2 = linear_search(data, target2)
    _, bin_count2 = binary_search(data, target2)
    print(f"■ {target2} を探す（真ん中あたり）:")
    print(f"  線形探索: {lin_count2}回")
    print(f"  2分探索: {bin_count2}回")
    print()

    # 最後の要素を探す
    target3 = 100
    _, lin_count3 = linear_search(data, target3)
    _, bin_count3 = binary_search(data, target3)
    print(f"■ {target3} を探す（最後にある）:")
    print(f"  線形探索: {lin_count3}回")
    print(f"  2分探索: {bin_count3}回")
    print()

    # 全要素を探して平均を計算
    all_linear_counts = [linear_search(data, t)[1] for t in data]
    all_binary_counts = [binary_search(data, t)[1] for t in data]

    avg_linear_actual = sum(all_linear_counts) / n
    avg_binary_actual = sum(all_binary_counts) / n

    print("■ 全ての要素を探した場合の平均比較回数:")
    print(f"  線形探索: {avg_linear_actual:.1f}回（理論値: {lin_avg:.1f}回）")
    print(f"  2分探索: {avg_binary_actual:.1f}回（理論値: 約{bin_avg}回）")
    print()

    # データサイズ別の比較
    print("==========================================")
    print("データ数が増えるとどうなる？")
    print("==========================================")
    print()
    print("┌──────────┬────────────────┬────────────────┐")
    print("│ データ数 │ 線形探索(最悪) │ 2分探索(最悪)  │")
    print("├──────────┼────────────────┼────────────────┤")

    for size in [10, 100, 1000, 10000, 100000, 1000000]:
        _, _, lin_w = theoretical_linear(size)
        _, _, bin_w = theoretical_binary(size)
        print(f"│ {size:8d} │ {lin_w:12d}回 │ {bin_w:12d}回 │")

    print("└──────────┴────────────────┴────────────────┘")
    print()
    print("→ 2分探索は、データが100万個でも約20回で見つかる！")
    print()

    # 2分探索の可視化
    print("==========================================")
    print("2分探索の動きを見てみよう")
    print("==========================================")
    print()
    visualize_binary_search(list(range(1, 17)), 13)

if __name__ == "__main__":
    main()
