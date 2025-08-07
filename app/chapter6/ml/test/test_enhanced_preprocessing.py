"""拡張前処理機能のテスト"""

import unittest
import numpy as np
import tempfile
import os
import sys

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..'))

from ml.lib.game_state_extractor import GameStateExtractor
from ml.lib.hyperparameter_tuner import (
    GridSearchTuner, RandomSearchTuner, BayesianTuner, 
    HyperparameterConfig, HyperparameterTuningManager,
    create_objective_function_example
)
from lib.player import Player
from lib.enemy import Enemy
from lib.bullet import Bullet


class TestEnhancedGameStateExtractor(unittest.TestCase):
    """拡張GameStateExtractorのテスト"""
    
    def setUp(self):
        self.extractor = GameStateExtractor(120, 160)
        self.player = Player(60, 80)
        self.enemies = [
            Enemy(40, 40, Enemy.TYPE_A),
            Enemy(80, 60, Enemy.TYPE_B)
        ]
        self.player_bullets = []
        self.enemy_bullets = [
            Bullet(Bullet.SIDE_ENEMY, 30, 70, 90, 2.0)
        ]
    
    def test_enhanced_vector_state_extraction(self):
        """拡張状態ベクトル抽出のテスト"""
        state = self.extractor.extract_enhanced_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # 基本状態（20次元）＋拡張特徴量が含まれることを確認
        self.assertGreater(len(state), 20)
        self.assertTrue(all(np.isfinite(state)))
    
    def test_velocity_features(self):
        """速度特徴量のテスト"""
        # 前フレームの状態を模擬
        previous_state = np.array([0.4, 0.4])  # 前フレーム位置（正規化済み）
        
        state = self.extractor.extract_enhanced_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets,
            previous_state=previous_state
        )
        
        # 速度特徴量が含まれていることを確認
        self.assertGreater(len(state), 20)
    
    def test_distance_features(self):
        """距離特徴量のテスト"""
        # 距離特徴量を有効化
        self.extractor.configure_features(include_distance_features=True)
        
        state = self.extractor.extract_enhanced_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # 距離特徴量が含まれていることを確認
        self.assertGreater(len(state), 20)
    
    def test_angle_features(self):
        """角度特徴量のテスト"""
        # 角度特徴量を有効化
        self.extractor.configure_features(include_angle_features=True)
        
        state = self.extractor.extract_enhanced_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # 角度特徴量が含まれていることを確認（sin, cosなので4次元追加）
        self.assertGreater(len(state), 20)
    
    def test_threat_assessment_features(self):
        """脅威評価特徴量のテスト"""
        # 脅威評価を有効化
        self.extractor.configure_features(include_threat_assessment=True)
        
        state = self.extractor.extract_enhanced_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # 脅威評価特徴量が含まれていることを確認
        self.assertGreater(len(state), 20)
    
    def test_normalization(self):
        """正規化のテスト"""
        state = self.extractor.extract_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # Z-score正規化
        normalized_zscore = self.extractor.normalize_state(state, update_stats=True)
        self.assertEqual(len(normalized_zscore), len(state))
        
        # Min-Max正規化
        normalized_minmax = self.extractor.min_max_normalize(state, update_stats=False)
        self.assertEqual(len(normalized_minmax), len(state))
        self.assertTrue(all(0 <= x <= 1 for x in normalized_minmax))
    
    def test_temporal_features(self):
        """時系列特徴量のテスト"""
        self.extractor.configure_features(temporal_features=True)
        
        # 複数フレームの状態を生成
        states = []
        for i in range(3):
            self.player.x = 60 + i * 5  # プレイヤーを少し移動
            state = self.extractor.extract_enhanced_vector_state(
                self.player, self.enemies, self.player_bullets, self.enemy_bullets
            )
            states.append(state)
        
        # 時系列特徴量が追加されることを確認
        # （履歴が蓄積されると変化率情報が追加される）
        self.assertGreaterEqual(len(states[-1]), len(states[0]))
    
    def test_stats_save_load(self):
        """統計情報の保存・読み込みテスト"""
        state = self.extractor.extract_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # 統計を初期化
        self.extractor.normalize_state(state, update_stats=True)
        
        # 一時ファイルに保存
        with tempfile.NamedTemporaryFile(suffix='.npz', delete=False) as tmp_file:
            temp_path = tmp_file.name
        
        try:
            self.extractor.save_stats(temp_path)
            
            # 統計をリセット
            self.extractor.reset_stats()
            self.assertFalse(self.extractor.state_stats['is_initialized'])
            
            # 統計を読み込み
            self.extractor.load_stats(temp_path)
            self.assertTrue(self.extractor.state_stats['is_initialized'])
        finally:
            # 一時ファイルを削除
            try:
                os.unlink(temp_path)
            except (OSError, PermissionError):
                pass  # Windows環境での削除エラーを無視
    
    def test_feature_configuration(self):
        """特徴量設定のテスト"""
        # 全ての拡張特徴量を無効化
        self.extractor.configure_features(
            include_velocity=False,
            include_distance_features=False,
            include_angle_features=False,
            include_threat_assessment=False,
            temporal_features=False
        )
        
        state = self.extractor.extract_enhanced_vector_state(
            self.player, self.enemies, self.player_bullets, self.enemy_bullets
        )
        
        # 基本状態のみ（20次元）であることを確認
        self.assertEqual(len(state), 20)


class TestGridSearchTuner(unittest.TestCase):
    """GridSearchTunerのテスト"""
    
    def test_grid_search_basic(self):
        """基本的なグリッドサーチテスト"""
        param_grid = {
            'param1': [1, 2, 3],
            'param2': [0.1, 0.2]
        }
        
        tuner = GridSearchTuner(param_grid)
        
        # 簡単な目的関数（param1が大きく、param2が小さいほど良い）
        def objective(hyperparams):
            score = hyperparams['param1'] * 10 - hyperparams['param2'] * 100
            metrics = {'test_metric': score * 0.1}
            return score, metrics
        
        best_params = tuner.tune(objective, max_evaluations=10)
        
        # 最適解の確認
        self.assertEqual(best_params['param1'], 3)
        self.assertEqual(best_params['param2'], 0.1)
        
        # 結果が記録されていることを確認
        self.assertGreater(len(tuner.results), 0)


class TestRandomSearchTuner(unittest.TestCase):
    """RandomSearchTunerのテスト"""
    
    def test_random_search_basic(self):
        """基本的なランダムサーチテスト"""
        param_configs = {
            'learning_rate': HyperparameterConfig(
                name='learning_rate',
                min_value=1e-4,
                max_value=1e-2,
                is_log_scale=True
            ),
            'batch_size': HyperparameterConfig(
                name='batch_size',
                discrete_values=[16, 32, 64, 128],
                is_integer=True
            )
        }
        
        tuner = RandomSearchTuner(param_configs)
        
        # 目的関数
        def objective(hyperparams):
            # learning_rate=1e-3, batch_size=64が最適と仮定
            lr_penalty = abs(hyperparams['learning_rate'] - 1e-3) * 10000
            bs_penalty = abs(hyperparams['batch_size'] - 64)
            score = 1000 - lr_penalty - bs_penalty
            metrics = {'penalty': lr_penalty + bs_penalty}
            return score, metrics
        
        best_params = tuner.tune(objective, max_evaluations=20, random_seed=42)
        
        # 結果の確認
        self.assertIn('learning_rate', best_params)
        self.assertIn('batch_size', best_params)
        self.assertIn(best_params['batch_size'], [16, 32, 64, 128])
        
        # 結果が記録されていることを確認
        self.assertEqual(len(tuner.results), 20)


class TestBayesianTuner(unittest.TestCase):
    """BayesianTunerのテスト"""
    
    def test_bayesian_tuner_basic(self):
        """基本的なベイジアン最適化テスト"""
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
        
        # 2次元の目的関数（x=1, y=2で最大）
        def objective(hyperparams):
            x, y = hyperparams['x'], hyperparams['y']
            score = -(x - 1)**2 - (y - 2)**2 + 10  # 最大値10
            metrics = {'distance_from_optimum': abs(x - 1) + abs(y - 2)}
            return score, metrics
        
        best_params = tuner.tune(objective, max_evaluations=25, random_seed=42)
        
        # 最適解に近いことを確認
        self.assertLess(abs(best_params['x'] - 1), 2.0)
        self.assertLess(abs(best_params['y'] - 2), 2.0)
        
        # 履歴が記録されていることを確認
        self.assertEqual(len(tuner.param_history), 25)
        self.assertEqual(len(tuner.score_history), 25)


class TestHyperparameterTuningManager(unittest.TestCase):
    """HyperparameterTuningManagerのテスト"""
    
    def test_param_grid_creation(self):
        """パラメーターグリッド作成のテスト"""
        manager = HyperparameterTuningManager()
        
        # DQN用グリッド
        dqn_grid = manager.create_dqn_param_grid()
        expected_keys = ['learning_rate', 'batch_size', 'gamma', 'epsilon_decay', 
                        'target_update_frequency', 'hidden_size']
        for key in expected_keys:
            self.assertIn(key, dqn_grid)
            self.assertGreater(len(dqn_grid[key]), 1)
        
        # PPO用グリッド
        ppo_grid = manager.create_ppo_param_grid()
        expected_keys = ['learning_rate', 'n_steps', 'batch_size', 'n_epochs',
                        'gamma', 'gae_lambda', 'clip_range']
        for key in expected_keys:
            self.assertIn(key, ppo_grid)
            self.assertGreater(len(ppo_grid[key]), 1)
    
    def test_param_configs_creation(self):
        """パラメーター設定作成のテスト"""
        manager = HyperparameterTuningManager()
        
        # DQN用設定
        dqn_configs = manager.create_dqn_param_configs()
        for name, config in dqn_configs.items():
            self.assertIsInstance(config, HyperparameterConfig)
            self.assertEqual(config.name, name)
        
        # PPO用設定
        ppo_configs = manager.create_ppo_param_configs()
        for name, config in ppo_configs.items():
            self.assertIsInstance(config, HyperparameterConfig)
            self.assertEqual(config.name, name)
    
    def test_results_save_load(self):
        """結果の保存・読み込みテスト"""
        manager = HyperparameterTuningManager()
        
        # 模擬結果を作成
        from ml.lib.hyperparameter_tuner import TuningResult
        results = [
            TuningResult(
                hyperparams={'lr': 0.001, 'bs': 64},
                score=100.0,
                training_time=10.5,
                additional_metrics={'accuracy': 0.95}
            ),
            TuningResult(
                hyperparams={'lr': 0.01, 'bs': 32},
                score=90.0,
                training_time=8.3,
                additional_metrics={'accuracy': 0.92}
            )
        ]
        
        # 一時ファイルに保存・読み込み
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as tmp_file:
            temp_path = tmp_file.name
        
        try:
            manager.save_results(results, temp_path)
            loaded_results = manager.load_results(temp_path)
            
            # 結果が正しく保存・読み込みされたことを確認
            self.assertEqual(len(loaded_results), 2)
            self.assertEqual(loaded_results[0].hyperparams['lr'], 0.001)
            self.assertEqual(loaded_results[1].score, 90.0)
        finally:
            # 一時ファイルを削除
            try:
                os.unlink(temp_path)
            except (OSError, PermissionError):
                pass  # Windows環境での削除エラーを無視
    
    def test_results_analysis(self):
        """結果分析のテスト"""
        manager = HyperparameterTuningManager()
        
        # 模擬結果を作成
        from ml.lib.hyperparameter_tuner import TuningResult
        results = [
            TuningResult({'lr': 0.001}, 100.0, 10.0, {}),
            TuningResult({'lr': 0.01}, 90.0, 8.0, {}),
            TuningResult({'lr': 0.1}, 80.0, 6.0, {}),
            TuningResult({'lr': 0.0001}, 95.0, 12.0, {})
        ]
        
        analysis = manager.analyze_results(results)
        
        # 分析結果の確認
        self.assertEqual(analysis['best_score'], 100.0)
        self.assertEqual(analysis['best_hyperparams']['lr'], 0.001)
        self.assertEqual(analysis['total_evaluations'], 4)
        self.assertIn('mean_score', analysis)
        self.assertIn('parameter_importance', analysis)


def create_mock_objective():
    """テスト用の模擬目的関数"""
    def objective(hyperparams):
        # 簡単な2次関数（最適解はlr=0.001付近）
        lr = hyperparams.get('learning_rate', 0.001)
        score = 1000 - (lr - 0.001) ** 2 * 1000000
        metrics = {'lr_penalty': abs(lr - 0.001)}
        return score, metrics
    return objective


if __name__ == '__main__':
    unittest.main()