"""ハイパーパラメーターチューニングのデモンストレーション"""

import numpy as np
import matplotlib.pyplot as plt
import time
import sys
import os

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from ml.lib.hyperparameter_tuner import (
    GridSearchTuner, RandomSearchTuner, BayesianTuner,
    HyperparameterTuningManager, HyperparameterConfig,
    create_objective_function_example
)


def demo_enhanced_preprocessing():
    """拡張前処理機能のデモ"""
    print("=== 拡張前処理機能デモ ===")
    
    from ml.lib.game_state_extractor import GameStateExtractor
    from lib.player import Player
    from lib.enemy import Enemy
    from lib.bullet import Bullet
    
    # GameStateExtractorを初期化
    extractor = GameStateExtractor(120, 160)
    
    # テスト用オブジェクト
    player = Player(60, 80)
    enemies = [Enemy(40, 50, Enemy.TYPE_A), Enemy(90, 70, Enemy.TYPE_B)]
    enemy_bullets = [Bullet(Bullet.SIDE_ENEMY, 50, 60, 90, 3.0)]
    
    print("1. 基本状態ベクトル抽出:")
    basic_state = extractor.extract_vector_state(player, enemies, [], enemy_bullets)
    print(f"   基本状態次元: {len(basic_state)}")
    print(f"   サンプル値: {basic_state[:5]}")
    
    print("\n2. 拡張状態ベクトル抽出:")
    enhanced_state = extractor.extract_enhanced_vector_state(
        player, enemies, [], enemy_bullets
    )
    print(f"   拡張状態次元: {len(enhanced_state)}")
    print(f"   拡張特徴量数: {len(enhanced_state) - len(basic_state)}")
    
    print("\n3. 正規化機能:")
    # Z-score正規化
    normalized_zscore = extractor.normalize_state(enhanced_state)
    print(f"   Z-score正規化後の平均: {np.mean(normalized_zscore):.4f}")
    print(f"   Z-score正規化後の標準偏差: {np.std(normalized_zscore):.4f}")
    
    # Min-Max正規化
    normalized_minmax = extractor.min_max_normalize(enhanced_state)
    print(f"   Min-Max正規化後の範囲: [{np.min(normalized_minmax):.4f}, {np.max(normalized_minmax):.4f}]")
    
    print("\n4. 時系列特徴量:")
    extractor.configure_features(temporal_features=True)
    
    # 複数フレームシミュレーション
    temporal_states = []
    for i in range(5):
        player.x = 60 + i * 2  # プレイヤーを移動
        state = extractor.extract_enhanced_vector_state(player, enemies, [], enemy_bullets)
        temporal_states.append(len(state))
        print(f"   フレーム{i+1}: 状態次元 = {len(state)}")
    
    print("\n拡張前処理機能デモ完了!\n")


def demo_grid_search():
    """グリッドサーチデモ"""
    print("=== グリッドサーチデモ ===")
    
    # パラメーターグリッドを設定
    param_grid = {
        'learning_rate': [1e-4, 3e-4, 1e-3],
        'batch_size': [32, 64, 128],
        'gamma': [0.95, 0.99]
    }
    
    tuner = GridSearchTuner(param_grid)
    
    # 簡易目的関数（実際の訓練の代わり）
    def simple_objective(hyperparams):
        # 最適値を設定（lr=3e-4, bs=64, gamma=0.99）
        lr_optimal = 3e-4
        bs_optimal = 64
        gamma_optimal = 0.99
        
        # 各パラメーターからの距離に基づくスコア
        lr_penalty = abs(hyperparams['learning_rate'] - lr_optimal) * 10000
        bs_penalty = abs(hyperparams['batch_size'] - bs_optimal) * 0.1
        gamma_penalty = abs(hyperparams['gamma'] - gamma_optimal) * 1000
        
        base_score = 1000
        score = base_score - lr_penalty - bs_penalty - gamma_penalty
        score += np.random.normal(0, 10)  # ノイズ追加
        
        metrics = {
            'lr_penalty': lr_penalty,
            'bs_penalty': bs_penalty,
            'gamma_penalty': gamma_penalty
        }
        
        return score, metrics
    
    print("グリッドサーチ実行中...")
    start_time = time.time()
    best_params = tuner.tune(simple_objective, max_evaluations=15)
    end_time = time.time()
    
    print(f"\n結果:")
    print(f"実行時間: {end_time - start_time:.2f}秒")
    print(f"評価回数: {len(tuner.results)}")
    print(f"最適パラメーター: {best_params}")
    print(f"最高スコア: {max(r.score for r in tuner.results):.2f}")
    
    print("グリッドサーチデモ完了!\n")
    return tuner.results


def demo_random_search():
    """ランダムサーチデモ"""
    print("=== ランダムサーチデモ ===")
    
    # パラメーター設定
    param_configs = {
        'learning_rate': HyperparameterConfig(
            name='learning_rate',
            min_value=1e-5,
            max_value=1e-2,
            is_log_scale=True
        ),
        'batch_size': HyperparameterConfig(
            name='batch_size',
            discrete_values=[16, 32, 64, 128, 256],
            is_integer=True
        ),
        'gamma': HyperparameterConfig(
            name='gamma',
            min_value=0.9,
            max_value=0.999
        )
    }
    
    tuner = RandomSearchTuner(param_configs)
    
    # 同じ目的関数を使用
    def random_objective(hyperparams):
        lr_optimal = 3e-4
        bs_optimal = 64
        gamma_optimal = 0.99
        
        lr_penalty = abs(hyperparams['learning_rate'] - lr_optimal) * 10000
        bs_penalty = abs(hyperparams['batch_size'] - bs_optimal) * 0.1
        gamma_penalty = abs(hyperparams['gamma'] - gamma_optimal) * 1000
        
        base_score = 1000
        score = base_score - lr_penalty - bs_penalty - gamma_penalty
        score += np.random.normal(0, 10)
        
        metrics = {
            'lr_penalty': lr_penalty,
            'bs_penalty': bs_penalty,
            'gamma_penalty': gamma_penalty
        }
        
        return score, metrics
    
    print("ランダムサーチ実行中...")
    start_time = time.time()
    best_params = tuner.tune(random_objective, max_evaluations=20, random_seed=42)
    end_time = time.time()
    
    print(f"\n結果:")
    print(f"実行時間: {end_time - start_time:.2f}秒")
    print(f"評価回数: {len(tuner.results)}")
    print(f"最適パラメーター: {best_params}")
    print(f"最高スコア: {max(r.score for r in tuner.results):.2f}")
    
    print("ランダムサーチデモ完了!\n")
    return tuner.results


def demo_bayesian_optimization():
    """ベイジアン最適化デモ"""
    print("=== ベイジアン最適化デモ ===")
    
    # パラメーター設定（連続値で設定）
    param_configs = {
        'x': HyperparameterConfig(
            name='x',
            min_value=-5.0,
            max_value=5.0
        ),
        'y': HyperparameterConfig(
            name='y',
            min_value=-5.0,
            max_value=5.0
        )
    }
    
    tuner = BayesianTuner(param_configs)
    
    # 2次元最適化問題（x=1, y=2で最大）
    def bayesian_objective(hyperparams):
        x, y = hyperparams['x'], hyperparams['y']
        
        # 複雑な関数（複数の局所最適解を持つ）
        score = -(x - 1)**2 - (y - 2)**2 + 10
        score += 3 * np.exp(-((x - 3)**2 + (y - 1)**2))  # 局所最適解
        score += np.random.normal(0, 0.5)  # ノイズ
        
        metrics = {
            'distance_from_global': np.sqrt((x - 1)**2 + (y - 2)**2),
            'x_value': x,
            'y_value': y
        }
        
        return score, metrics
    
    print("ベイジアン最適化実行中...")
    start_time = time.time()
    best_params = tuner.tune(bayesian_objective, max_evaluations=30, random_seed=42)
    end_time = time.time()
    
    print(f"\n結果:")
    print(f"実行時間: {end_time - start_time:.2f}秒")
    print(f"評価回数: {len(tuner.results)}")
    print(f"最適パラメーター: {best_params}")
    print(f"グローバル最適解との距離: {np.sqrt((best_params['x'] - 1)**2 + (best_params['y'] - 2)**2):.4f}")
    print(f"最高スコア: {max(r.score for r in tuner.results):.2f}")
    
    print("ベイジアン最適化デモ完了!\n")
    return tuner.results


def demo_tuning_manager():
    """チューニングマネージャーデモ"""
    print("=== チューニングマネージャーデモ ===")
    
    manager = HyperparameterTuningManager()
    
    print("1. DQN用パラメーターグリッド:")
    dqn_grid = manager.create_dqn_param_grid()
    for name, values in dqn_grid.items():
        print(f"   {name}: {values}")
    
    print("\n2. PPO用パラメーター設定:")
    ppo_configs = manager.create_ppo_param_configs()
    for name, config in ppo_configs.items():
        if config.discrete_values:
            print(f"   {name}: 離散値 {config.discrete_values}")
        else:
            print(f"   {name}: 連続値 [{config.min_value}, {config.max_value}]"
                  + (" (対数)" if config.is_log_scale else ""))
    
    print("\n3. 結果分析デモ:")
    # 模擬結果を生成
    from ml.lib.hyperparameter_tuner import TuningResult
    
    mock_results = []
    for i in range(10):
        hyperparams = {
            'learning_rate': np.random.uniform(1e-4, 1e-2),
            'batch_size': np.random.choice([32, 64, 128]),
            'gamma': np.random.uniform(0.95, 0.99)
        }
        score = np.random.uniform(800, 1000)
        result = TuningResult(hyperparams, score, 10.0, {'test': score * 0.1})
        mock_results.append(result)
    
    analysis = manager.analyze_results(mock_results)
    print(f"   最高スコア: {analysis['best_score']:.2f}")
    print(f"   平均スコア: {analysis['mean_score']:.2f} ± {analysis['std_score']:.2f}")
    print(f"   パラメーター重要度:")
    for param, importance in analysis['parameter_importance'].items():
        print(f"     {param}: {importance:.4f}")
    
    print("チューニングマネージャーデモ完了!\n")


def visualize_tuning_comparison(grid_results, random_results, bayesian_results):
    """チューニング結果の比較可視化"""
    print("=== チューニング結果比較 ===")
    
    try:
        # スコアの推移をプロット
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
        
        # 収束性の比較
        grid_scores = [r.score for r in grid_results]
        random_scores = [r.score for r in random_results]
        bayesian_scores = [r.score for r in bayesian_results]
        
        ax1.plot(range(1, len(grid_scores) + 1), 
                np.maximum.accumulate(grid_scores), 'b-o', label='Grid Search', markersize=4)
        ax1.plot(range(1, len(random_scores) + 1), 
                np.maximum.accumulate(random_scores), 'r-s', label='Random Search', markersize=4)
        ax1.plot(range(1, len(bayesian_scores) + 1), 
                np.maximum.accumulate(bayesian_scores), 'g-^', label='Bayesian Opt.', markersize=4)
        
        ax1.set_xlabel('評価回数')
        ax1.set_ylabel('最高スコア')
        ax1.set_title('チューニング手法の収束性比較')
        ax1.legend()
        ax1.grid(True)
        
        # スコア分布の比較
        ax2.hist([grid_scores, random_scores, bayesian_scores], 
                bins=10, alpha=0.7, label=['Grid', 'Random', 'Bayesian'])
        ax2.set_xlabel('スコア')
        ax2.set_ylabel('頻度')
        ax2.set_title('スコア分布比較')
        ax2.legend()
        ax2.grid(True)
        
        plt.tight_layout()
        
        # 画像ファイルとして保存
        output_path = 'tuning_comparison.png'
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        print(f"比較グラフを保存: {output_path}")
        
        plt.show()
        
    except Exception as e:
        print(f"可視化でエラーが発生しました（matplotlib未インストール？）: {e}")
    
    # 統計的な比較
    print("\n統計的比較:")
    methods = ['Grid Search', 'Random Search', 'Bayesian Opt.']
    results_lists = [grid_results, random_results, bayesian_results]
    
    for method, results in zip(methods, results_lists):
        scores = [r.score for r in results]
        times = [r.training_time for r in results]
        print(f"{method:15s}: 最高={max(scores):6.2f}, "
              f"平均={np.mean(scores):6.2f}±{np.std(scores):5.2f}, "
              f"時間={sum(times):5.2f}s")


def main():
    """メインデモ実行"""
    print("Mega Wing ML ハイパーパラメーターチューニング & 前処理拡張デモ")
    print("=" * 70)
    
    # 1. 拡張前処理機能のデモ
    demo_enhanced_preprocessing()
    
    # 2. 各チューニング手法のデモ
    grid_results = demo_grid_search()
    random_results = demo_random_search()
    bayesian_results = demo_bayesian_optimization()
    
    # 3. チューニングマネージャーのデモ
    demo_tuning_manager()
    
    # 4. 結果比較と可視化
    visualize_tuning_comparison(grid_results, random_results, bayesian_results)
    
    print("\n" + "=" * 70)
    print("全デモ完了! 拡張機能が正常に動作しています。")
    print("\n実用的な使用法:")
    print("1. 拡張前処理: ゲーム状態の特徴量エンジニアリングで学習性能向上")
    print("2. ハイパーパラメーターチューニング: 最適なML設定の自動探索")
    print("3. 統計的正規化: 学習の安定性と収束性向上")
    print("4. 時系列特徴量: ゲームの動的な要素の活用")


if __name__ == "__main__":
    main()