"""Mega Wing ML統合システム"""

__version__ = "1.0.0"
__author__ = "AI Assistant"
__description__ = "機械学習統合システム for Mega Wing シューティングゲーム"

# 主要クラスのエクスポート
from .lib.game_state_extractor import GameStateExtractor
from .lib.mega_wing_env import MegaWingEnv
from .lib.smart_enemy import SmartEnemy, EnemyBrain, SmartEnemyManager  
from .lib.player_assist_ai import PlayerAssistAI, AutoAimSystem, AvoidanceSystem

__all__ = [
    "GameStateExtractor",
    "MegaWingEnv", 
    "SmartEnemy",
    "EnemyBrain",
    "SmartEnemyManager",
    "PlayerAssistAI", 
    "AutoAimSystem",
    "AvoidanceSystem"
]