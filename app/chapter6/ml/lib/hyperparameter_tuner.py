"""ハイパーパラメーターチューニングシステム"""

import json
import numpy as np
import itertools
from typing import Dict, List, Any, Tuple, Optional, Callable
from dataclasses import dataclass
import time
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))

@dataclass
class HyperparameterConfig:
    """ハイパーパラメーター設定"""
    name: str
    min_value: Optional[float] = None
    max_value: Optional[float] = None
    is_log_scale: bool = False
    is_integer: bool = False
    discrete_values: Optional[List[Any]] = None

@dataclass
class TuningResult:
    """チューニング結果"""
    hyperparams: Dict[str, Any]
    score: float
    training_time: float
    additional_metrics: Dict[str, float]

class GridSearchTuner:
    """グリッドサーチによるハイパーパラメーターチューニング"""
    
    def __init__(self, param_grid: Dict[str, List[Any]]):
        """
        Args:
            param_grid: パラメーター名と候補値のリストの辞書
        """
        self.param_grid = param_grid
        self.results: List[TuningResult] = []
    
    def tune(self, 
             objective_function: Callable[[Dict[str, Any]], Tuple[float, Dict[str, float]]],
             max_evaluations: int = 50) -> Dict[str, Any]:
        """グリッドサーチでチューニング実行
        
        Args:
            objective_function: (hyperparams) -> (score, metrics) の関数
            max_evaluations: 最大評価回数
            
        Returns:
            最適なハイパーパラメーター
        """
        # 全組み合わせを生成
        param_names = list(self.param_grid.keys())
        param_values = list(self.param_grid.values())
        
        combinations = list(itertools.product(*param_values))
        
        # 最大評価回数に制限
        if len(combinations) > max_evaluations:
            # ランダムサンプリング
            indices = np.random.choice(len(combinations), max_evaluations, replace=False)
            combinations = [combinations[i] for i in indices]
        
        print(f"グリッドサーチ開始: {len(combinations)}個の組み合わせを評価")
        
        best_score = float('-inf')
        best_params = None
        
        for i, combination in enumerate(combinations):
            # パラメーター辞書を構築
            hyperparams = dict(zip(param_names, combination))
            
            print(f"評価 {i+1}/{len(combinations)}: {hyperparams}")
            
            start_time = time.time()
            try:
                score, metrics = objective_function(hyperparams)
                training_time = time.time() - start_time
                
                # 結果を記録
                result = TuningResult(
                    hyperparams=hyperparams.copy(),
                    score=score,
                    training_time=training_time,
                    additional_metrics=metrics
                )
                self.results.append(result)
                
                # 最適解を更新
                if score > best_score:
                    best_score = score
                    best_params = hyperparams.copy()
                    print(f"新しいベストスコア: {best_score:.4f}")
                
            except Exception as e:
                print(f"評価エラー: {e}")
                continue
        
        print(f"グリッドサーチ完了. ベストスコア: {best_score:.4f}")
        return best_params if best_params else {}

class RandomSearchTuner:
    """ランダムサーチによるハイパーパラメーターチューニング"""
    
    def __init__(self, param_configs: Dict[str, HyperparameterConfig]):
        """
        Args:
            param_configs: パラメーター名とその設定の辞書
        """
        self.param_configs = param_configs
        self.results: List[TuningResult] = []
    
    def tune(self, 
             objective_function: Callable[[Dict[str, Any]], Tuple[float, Dict[str, float]]],
             max_evaluations: int = 50,
             random_seed: int = 42) -> Dict[str, Any]:
        """ランダムサーチでチューニング実行
        
        Args:
            objective_function: (hyperparams) -> (score, metrics) の関数
            max_evaluations: 最大評価回数
            random_seed: ランダムシード
            
        Returns:
            最適なハイパーパラメーター
        """
        np.random.seed(random_seed)
        
        print(f"ランダムサーチ開始: {max_evaluations}回の評価を実行")
        
        best_score = float('-inf')
        best_params = None
        
        for i in range(max_evaluations):
            # ランダムなハイパーパラメーターを生成
            hyperparams = self._sample_hyperparams()
            
            print(f"評価 {i+1}/{max_evaluations}: {hyperparams}")
            
            start_time = time.time()
            try:
                score, metrics = objective_function(hyperparams)
                training_time = time.time() - start_time
                
                # 結果を記録
                result = TuningResult(
                    hyperparams=hyperparams.copy(),
                    score=score,
                    training_time=training_time,
                    additional_metrics=metrics
                )
                self.results.append(result)
                
                # 最適解を更新
                if score > best_score:
                    best_score = score
                    best_params = hyperparams.copy()
                    print(f"新しいベストスコア: {best_score:.4f}")
                
            except Exception as e:
                print(f"評価エラー: {e}")
                continue
        
        print(f"ランダムサーチ完了. ベストスコア: {best_score:.4f}")
        return best_params if best_params else {}
    
    def _sample_hyperparams(self) -> Dict[str, Any]:
        """ランダムなハイパーパラメーターをサンプリング"""
        hyperparams = {}
        
        for name, config in self.param_configs.items():
            if config.discrete_values is not None:
                # 離散値からサンプリング
                value = np.random.choice(config.discrete_values)
            else:
                # 連続値からサンプリング
                if config.min_value is None or config.max_value is None:
                    raise ValueError(f"Parameter {name}: min_value and max_value must be set for continuous parameters")
                
                if config.is_log_scale:
                    # 対数スケールでサンプリング
                    log_min = np.log10(config.min_value)
                    log_max = np.log10(config.max_value)
                    log_value = np.random.uniform(log_min, log_max)
                    value = 10 ** log_value
                else:
                    # 線形スケールでサンプリング
                    value = np.random.uniform(config.min_value, config.max_value)
                
                # 整数型の場合は丸める
                if config.is_integer:
                    value = int(round(value))
            
            hyperparams[name] = value
        
        return hyperparams

class BayesianTuner:
    """簡易ベイジアン最適化（ガウス過程風）"""
    
    def __init__(self, param_configs: Dict[str, HyperparameterConfig]):
        """
        Args:
            param_configs: パラメーター名とその設定の辞書
        """
        self.param_configs = param_configs
        self.results: List[TuningResult] = []
        self.param_history = []
        self.score_history = []
    
    def tune(self, 
             objective_function: Callable[[Dict[str, Any]], Tuple[float, Dict[str, float]]],
             max_evaluations: int = 50,
             random_seed: int = 42,
             exploration_weight: float = 0.1) -> Dict[str, Any]:
        """簡易ベイジアン最適化でチューニング実行
        
        Args:
            objective_function: (hyperparams) -> (score, metrics) の関数
            max_evaluations: 最大評価回数
            random_seed: ランダムシード
            exploration_weight: 探索の重み
            
        Returns:
            最適なハイパーパラメーター
        """
        np.random.seed(random_seed)
        
        print(f"簡易ベイジアン最適化開始: {max_evaluations}回の評価を実行")
        
        # 最初の数回はランダムサンプリング
        n_random_init = min(10, max_evaluations // 5)
        
        best_score = float('-inf')
        best_params = None
        
        for i in range(max_evaluations):
            if i < n_random_init:
                # 初期はランダムサンプリング
                hyperparams = self._sample_hyperparams_random()
            else:
                # ベイジアン最適化風の次候補選択
                hyperparams = self._select_next_candidate(exploration_weight)
            
            print(f"評価 {i+1}/{max_evaluations}: {hyperparams}")
            
            start_time = time.time()
            try:
                score, metrics = objective_function(hyperparams)
                training_time = time.time() - start_time
                
                # 履歴に記録
                self.param_history.append(hyperparams.copy())
                self.score_history.append(score)
                
                # 結果を記録
                result = TuningResult(
                    hyperparams=hyperparams.copy(),
                    score=score,
                    training_time=training_time,
                    additional_metrics=metrics
                )
                self.results.append(result)
                
                # 最適解を更新
                if score > best_score:
                    best_score = score
                    best_params = hyperparams.copy()
                    print(f"新しいベストスコア: {best_score:.4f}")
                
            except Exception as e:
                print(f"評価エラー: {e}")
                # エラーの場合も履歴に記録（低いスコア）
                self.param_history.append(hyperparams.copy())
                self.score_history.append(float('-inf'))
                continue
        
        print(f"ベイジアン最適化完了. ベストスコア: {best_score:.4f}")
        return best_params if best_params else {}
    
    def _sample_hyperparams_random(self) -> Dict[str, Any]:
        """ランダムなハイパーパラメーターをサンプリング"""
        hyperparams = {}
        
        for name, config in self.param_configs.items():
            if config.discrete_values is not None:
                value = np.random.choice(config.discrete_values)
            else:
                if config.min_value is None or config.max_value is None:
                    raise ValueError(f"Parameter {name}: min_value and max_value must be set for continuous parameters")
                
                if config.is_log_scale:
                    log_min = np.log10(config.min_value)
                    log_max = np.log10(config.max_value)
                    log_value = np.random.uniform(log_min, log_max)
                    value = 10 ** log_value
                else:
                    value = np.random.uniform(config.min_value, config.max_value)
                
                if config.is_integer:
                    value = int(round(value))
            
            hyperparams[name] = value
        
        return hyperparams
    
    def _select_next_candidate(self, exploration_weight: float) -> Dict[str, Any]:
        """次の候補をベイジアン最適化風に選択"""
        # 簡易実装：過去のベストパラメーターの近傍をガウシアンノイズで探索
        if not self.score_history:
            return self._sample_hyperparams_random()
        
        # ベストスコアのインデックス
        best_idx = np.argmax(self.score_history)
        best_params = self.param_history[best_idx]
        
        # ベストパラメーターを中心とした正規分布でサンプリング
        new_params = {}
        
        for name, config in self.param_configs.items():
            if config.discrete_values is not None:
                # 離散値の場合：確率的に選択
                if np.random.random() < exploration_weight:
                    new_params[name] = np.random.choice(config.discrete_values)
                else:
                    new_params[name] = best_params[name]
            else:
                # 連続値の場合：ガウシアンノイズを追加
                best_value = best_params[name]
                
                if config.min_value is None or config.max_value is None:
                    # 範囲が設定されていない場合はランダムに
                    new_params[name] = best_params[name]
                    continue
                
                if config.is_log_scale:
                    # 対数スケールでのガウシアンノイズ
                    log_best = np.log10(best_value)
                    log_range = np.log10(config.max_value) - np.log10(config.min_value)
                    noise_std = log_range * exploration_weight
                    log_new = np.random.normal(log_best, noise_std)
                    log_new = np.clip(log_new, np.log10(config.min_value), np.log10(config.max_value))
                    value = 10 ** log_new
                else:
                    # 線形スケールでのガウシアンノイズ
                    value_range = config.max_value - config.min_value
                    noise_std = value_range * exploration_weight
                    value = np.random.normal(best_value, noise_std)
                    value = np.clip(value, config.min_value, config.max_value)
                
                if config.is_integer:
                    value = int(round(value))
                
                new_params[name] = value
        
        return new_params

class HyperparameterTuningManager:
    """ハイパーパラメーターチューニング管理クラス"""
    
    def __init__(self):
        self.tuning_history = []
    
    def create_dqn_param_grid(self) -> Dict[str, List[Any]]:
        """DQN用パラメーターグリッドを作成"""
        return {
            'learning_rate': [1e-4, 5e-4, 1e-3, 5e-3],
            'batch_size': [32, 64, 128],
            'gamma': [0.95, 0.99, 0.995],
            'epsilon_decay': [0.995, 0.999, 0.9995],
            'target_update_frequency': [500, 1000, 2000],
            'hidden_size': [64, 128, 256]
        }
    
    def create_ppo_param_grid(self) -> Dict[str, List[Any]]:
        """PPO用パラメーターグリッドを作成"""
        return {
            'learning_rate': [1e-4, 3e-4, 1e-3],
            'n_steps': [256, 512, 1024],
            'batch_size': [32, 64, 128],
            'n_epochs': [4, 8, 10],
            'gamma': [0.95, 0.99, 0.995],
            'gae_lambda': [0.9, 0.95, 0.98],
            'clip_range': [0.1, 0.2, 0.3]
        }
    
    def create_dqn_param_configs(self) -> Dict[str, HyperparameterConfig]:
        """DQN用パラメーター設定を作成"""
        return {
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
            ),
            'epsilon_decay': HyperparameterConfig(
                name='epsilon_decay',
                min_value=0.99,
                max_value=0.9999
            ),
            'target_update_frequency': HyperparameterConfig(
                name='target_update_frequency',
                min_value=100,
                max_value=5000,
                is_integer=True
            ),
            'hidden_size': HyperparameterConfig(
                name='hidden_size',
                discrete_values=[64, 128, 256, 512],
                is_integer=True
            )
        }
    
    def create_ppo_param_configs(self) -> Dict[str, HyperparameterConfig]:
        """PPO用パラメーター設定を作成"""
        return {
            'learning_rate': HyperparameterConfig(
                name='learning_rate',
                min_value=1e-5,
                max_value=1e-2,
                is_log_scale=True
            ),
            'n_steps': HyperparameterConfig(
                name='n_steps',
                discrete_values=[128, 256, 512, 1024, 2048],
                is_integer=True
            ),
            'batch_size': HyperparameterConfig(
                name='batch_size',
                discrete_values=[16, 32, 64, 128],
                is_integer=True
            ),
            'n_epochs': HyperparameterConfig(
                name='n_epochs',
                min_value=3,
                max_value=20,
                is_integer=True
            ),
            'gamma': HyperparameterConfig(
                name='gamma',
                min_value=0.9,
                max_value=0.999
            ),
            'gae_lambda': HyperparameterConfig(
                name='gae_lambda',
                min_value=0.8,
                max_value=0.99
            ),
            'clip_range': HyperparameterConfig(
                name='clip_range',
                min_value=0.05,
                max_value=0.5
            )
        }
    
    def save_results(self, results: List[TuningResult], filepath: str) -> None:
        """結果をJSONファイルに保存"""
        serializable_results = []
        for result in results:
            serializable_results.append({
                'hyperparams': result.hyperparams,
                'score': result.score,
                'training_time': result.training_time,
                'additional_metrics': result.additional_metrics
            })
        
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(serializable_results, f, indent=2, ensure_ascii=False)
        
        print(f"チューニング結果を保存: {filepath}")
    
    def load_results(self, filepath: str) -> List[TuningResult]:
        """結果をJSONファイルから読み込み"""
        with open(filepath, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        results = []
        for item in data:
            results.append(TuningResult(
                hyperparams=item['hyperparams'],
                score=item['score'],
                training_time=item['training_time'],
                additional_metrics=item['additional_metrics']
            ))
        
        return results
    
    def analyze_results(self, results: List[TuningResult]) -> Dict[str, Any]:
        """チューニング結果を分析"""
        if not results:
            return {}
        
        scores = [r.score for r in results]
        times = [r.training_time for r in results]
        
        # ベスト結果
        best_idx = np.argmax(scores)
        best_result = results[best_idx]
        
        # 統計情報
        analysis = {
            'best_score': float(np.max(scores)),
            'best_hyperparams': best_result.hyperparams,
            'mean_score': float(np.mean(scores)),
            'std_score': float(np.std(scores)),
            'mean_training_time': float(np.mean(times)),
            'total_evaluations': len(results),
            'score_percentiles': {
                '25th': float(np.percentile(scores, 25)),
                '50th': float(np.percentile(scores, 50)),
                '75th': float(np.percentile(scores, 75)),
                '95th': float(np.percentile(scores, 95))
            }
        }
        
        # パラメーター相関分析（簡易）
        param_importance = {}
        if len(results) > 5:
            param_names = list(results[0].hyperparams.keys())
            for param_name in param_names:
                try:
                    param_values = [r.hyperparams[param_name] for r in results]
                    if all(isinstance(v, (int, float)) for v in param_values):
                        correlation = np.corrcoef(param_values, scores)[0, 1]
                        param_importance[param_name] = float(correlation) if not np.isnan(correlation) else 0.0
                except:
                    param_importance[param_name] = 0.0
        
        analysis['parameter_importance'] = param_importance
        
        return analysis

def create_objective_function_example():
    """目的関数の例（実際のトレーニング関数をラップする）"""
    
    def objective(hyperparams: Dict[str, Any]) -> Tuple[float, Dict[str, float]]:
        """
        実際の訓練を実行し、性能スコアを返す目的関数の例
        
        Args:
            hyperparams: 評価するハイパーパラメーター
            
        Returns:
            (score, additional_metrics)のタプル
        """
        # ここで実際のML訓練を実行
        # 例：DQNまたはPPO訓練
        
        # 模擬実装（実際は訓練スクリプトを呼び出す）
        learning_rate = hyperparams.get('learning_rate', 3e-4)
        batch_size = hyperparams.get('batch_size', 64)
        
        # 訓練時間をシミュレート
        time.sleep(0.1)
        
        # 性能スコアを模擬（実際は訓練結果）
        # learning_rateが適度で、batch_sizeが大きすぎない時に高スコア
        score = 1000 - abs(learning_rate - 3e-4) * 10000 - (batch_size - 64) ** 2
        score += np.random.normal(0, 50)  # ノイズ
        
        additional_metrics = {
            'final_reward': score + np.random.normal(0, 10),
            'convergence_speed': max(0, 100 - batch_size)
        }
        
        return score, additional_metrics
    
    return objective