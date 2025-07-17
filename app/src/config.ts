export class Config {
  private _puyoImageWidth: number = 40 // ぷよぷよ画像の幅
  private _puyoImageHeight: number = 40 // ぷよぷよ画像の高さ
  private readonly _fontHeight: number = 33
  private readonly _stageRows: number = 12 // ステージの縦の個数
  private readonly _stageCols: number = 6 // ステージの横の個数
  private readonly _stageBackgroundColor: string = '#bbbbff'
  private readonly _scoreBackgroundColor: string = '#24c0bb'
  private readonly _playerFallSpeed: number = 60 // プレイ中の自然落下スピード
  private readonly _playerDownSpeed: number = 10 // プレイ中の下キー押下時の落下スピード
  private readonly _playerMoveFrame: number = 10 // 左右移動に消費するフレーム数
  private readonly _playerRotateFrame: number = 10 // 回転に消費するフレーム数

  get puyoImageWidth(): number {
    // フィールドサイズ追加
    // 高さが全部入るように
    this._puyoImageWidth = (window.innerHeight - this._fontHeight) / this._stageRows
    return this._puyoImageWidth
  }

  get puyoImageHeight(): number {
    // フィールドサイズ追加
    // 高さが全部入るように
    this._puyoImageHeight = (window.innerHeight - this._fontHeight) / this._stageRows
    return this._puyoImageHeight
  }

  get fontHeight(): number {
    return this._fontHeight
  }

  get stageRows(): number {
    return this._stageRows
  }

  get stageCols(): number {
    return this._stageCols
  }

  get stageBackgroundColor(): string {
    return this._stageBackgroundColor
  }

  get scoreBackgroundColor(): string {
    return this._scoreBackgroundColor
  }

  get playerFallSpeed(): number {
    return this._playerFallSpeed
  }

  get playerDownSpeed(): number {
    return this._playerDownSpeed
  }

  get playerMoveFrame(): number {
    return this._playerMoveFrame
  }

  get playerRotateFrame(): number {
    return this._playerRotateFrame
  }
}
