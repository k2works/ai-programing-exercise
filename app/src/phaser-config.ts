import Phaser from 'phaser';

/**
 * 基本的なPhaserゲーム設定のテストクラス
 */
export class PhaserGameConfig {
  private config: Phaser.Types.Core.GameConfig;

  constructor() {
    this.config = {
      type: Phaser.AUTO,
      width: 800,
      height: 600,
      backgroundColor: '#2c3e50',
      parent: 'game-container',
      scene: {
        preload: this.preload,
        create: this.create,
      },
    };
  }

  public getConfig(): Phaser.Types.Core.GameConfig {
    return this.config;
  }

  private preload(): void {
    // アセット読み込み用（現在は空）
  }

  private create(): void {
    // シーン作成用（現在は空）
  }

  public createGame(): Phaser.Game {
    return new Phaser.Game(this.config);
  }
}
