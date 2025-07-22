// ゲーム設定値を管理するクラス
export class Config {
  // ステージのサイズ
  static readonly STAGE_WIDTH = 6
  static readonly STAGE_HEIGHT = 12

  // ぷよのサイズ（ピクセル）
  static readonly PUYO_SIZE = 32

  // ゲーム速度（フレーム間隔）
  static readonly GAME_SPEED = 60

  // ぷよの色定義
  static readonly COLORS = [
    '#000000', // 0: 空（黒）
    '#ff0000', // 1: 赤
    '#00ff00', // 2: 緑
    '#0000ff', // 3: 青  
    '#ffff00', // 4: 黄色
  ]
}