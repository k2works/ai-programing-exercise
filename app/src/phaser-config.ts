import Phaser from 'phaser'
import { LoadingScene } from './scene/LoadingScene'
import { TitleScene } from './scene/TitleScene'
import { TestScene } from './scene/TestScene'
import { GameScene } from './scene/GameScene'
import { ScenarioScene } from './scene/ScenarioScene'

/**
 * ノベルゲーム用のPhaser3設定クラス
 */
export class PhaserGameConfig {
  private config: Phaser.Types.Core.GameConfig

  constructor() {
    this.config = {
      type: Phaser.AUTO,
      width: 800,
      height: 600,
      backgroundColor: '#2c3e50',
      parent: 'game-container',
			scene: [LoadingScene, TitleScene, TestScene, GameScene, ScenarioScene],
      physics: {
        default: 'arcade',
        arcade: {
          gravity: { y: 0, x: 0 },
          debug: false,
        },
      },
    }
  }

  public getConfig(): Phaser.Types.Core.GameConfig {
    return this.config
  }

  public createGame(): Phaser.Game {
    return new Phaser.Game(this.config)
  }
}
