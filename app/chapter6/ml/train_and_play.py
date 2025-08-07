"""エージェント訓練後に視覚的プレイデモを実行"""

import os
import sys
import time
import argparse
from stable_baselines3 import DQN, PPO
from stable_baselines3.common.monitor import Monitor

# プロジェクトルートをパスに追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
from ml.lib.mega_wing_env import MegaWingEnv
from ml.play_with_agent import AgentGameDemo


def train_agent(agent_type: str, total_timesteps: int = 20000, model_save_path: str = None):
    """エージェントを訓練して保存"""
    print(f"=== {agent_type.upper()} Agent Training ===")
    
    # 環境作成
    env = MegaWingEnv(observation_type="vector", max_steps=1000)
    env = Monitor(env)
    
    # エージェント作成
    if agent_type.lower() == "dqn":
        agent = DQN(
            policy='MlpPolicy',
            env=env,
            learning_rate=1e-3,
            buffer_size=50000,
            learning_starts=1000,
            batch_size=64,
            train_freq=4,
            target_update_interval=1000,
            exploration_fraction=0.3,
            exploration_final_eps=0.02,
            verbose=1
        )
    elif agent_type.lower() == "ppo":
        agent = PPO(
            policy='MlpPolicy',
            env=env,
            learning_rate=3e-4,
            n_steps=1024,
            batch_size=64,
            n_epochs=10,
            gamma=0.99,
            gae_lambda=0.95,
            clip_range=0.2,
            ent_coef=0.01,
            verbose=1
        )
    else:
        raise ValueError(f"Unsupported agent type: {agent_type}")
    
    # 訓練実行
    print(f"Training {agent_type} for {total_timesteps} timesteps...")
    start_time = time.time()
    
    agent.learn(total_timesteps=total_timesteps, progress_bar=True)
    
    training_time = time.time() - start_time
    print(f"Training completed in {training_time:.2f} seconds")
    
    # モデル保存
    if model_save_path:
        agent.save(model_save_path)
        print(f"Model saved to: {model_save_path}")
    
    # 簡易評価
    print("Evaluating trained agent...")
    total_rewards = []
    
    for episode in range(5):
        obs, _ = env.reset()
        episode_reward = 0
        
        while True:
            action, _ = agent.predict(obs, deterministic=True)
            obs, reward, done, truncated, info = env.step(action)
            episode_reward += reward
            
            if done or truncated:
                break
        
        total_rewards.append(episode_reward)
        print(f"Episode {episode + 1}: reward={episode_reward:.2f}, score={info.get('score', 0)}")
    
    avg_reward = sum(total_rewards) / len(total_rewards)
    print(f"Average reward: {avg_reward:.2f}")
    
    return agent, model_save_path


def play_with_trained_agent(agent_type: str, model_path: str = None):
    """訓練済みエージェントでゲームプレイデモ"""
    print(f"=== Visual Game Demo with {agent_type.upper()} Agent ===")
    
    demo = AgentGameDemo()
    
    if model_path and os.path.exists(model_path):
        demo.load_agent(agent_type.lower(), model_path)
        print(f"Loaded pre-trained {agent_type} model from: {model_path}")
    else:
        demo.load_agent(agent_type.lower())
        print(f"Using fresh {agent_type} agent")
    
    print("\n視覚デモを開始します...")
    print("操作方法:")
    print("1-5: エージェント切り替え")
    print("R: ゲームリスタート") 
    print("P: ポーズ")
    print("I: 情報パネル表示切替")
    print("Q: 終了")
    
    try:
        demo.initialize_pyxel()
    except KeyboardInterrupt:
        print("\nデモを終了しました。")


def main():
    """メイン実行関数"""
    parser = argparse.ArgumentParser(description="Train and play with ML agents")
    parser.add_argument("--agent", choices=["dqn", "ppo"], default="dqn",
                       help="Agent type to train (default: dqn)")
    parser.add_argument("--timesteps", type=int, default=20000,
                       help="Training timesteps (default: 20000)")
    parser.add_argument("--model-path", type=str, default=None,
                       help="Path to save/load model")
    parser.add_argument("--train-only", action="store_true",
                       help="Only train, don't play")
    parser.add_argument("--play-only", action="store_true", 
                       help="Only play with existing model")
    parser.add_argument("--quick", action="store_true",
                       help="Quick training (5000 timesteps)")
    
    args = parser.parse_args()
    
    if args.quick:
        args.timesteps = 5000
    
    # モデル保存パスの自動生成
    if args.model_path is None:
        timestamp = time.strftime("%Y%m%d_%H%M%S")
        args.model_path = f"models/{args.agent}_agent_{timestamp}"
        
        # modelsディレクトリ作成
        os.makedirs("models", exist_ok=True)
    
    print("Mega Wing - Train and Play Demo")
    print("=" * 50)
    print(f"Agent: {args.agent.upper()}")
    print(f"Training timesteps: {args.timesteps}")
    print(f"Model path: {args.model_path}")
    print()
    
    try:
        if not args.play_only:
            # エージェント訓練
            agent, model_path = train_agent(args.agent, args.timesteps, args.model_path)
        else:
            model_path = args.model_path
            
        if not args.train_only:
            # 視覚デモ実行
            play_with_trained_agent(args.agent, model_path)
            
    except Exception as e:
        print(f"エラーが発生しました: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()