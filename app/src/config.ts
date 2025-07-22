export class Config {
    private readonly _puyoImageWidth: number = 40; // ぷよぷよ画像の幅
    private readonly _puyoImageHeight: number = 40; // ぷよぷよ画像の高さ
    private readonly _fontHeight: number = 33;
    private readonly _stageCols: number = 6; // ステージの横の個数
    private readonly _stageRows: number = 12; // ステージの縦の個数
    private readonly _stageBackgroundColor: string = "#ffffff"; // ステージの背景色
    private readonly _scoreBackgroundColor: string = "#24c0bb";
    private readonly _puyoColors: number = 4; // 何色のぷよを使うか
    private readonly _freeFallingSpeed: number = 16; // 自由落下のスピード
    private readonly _playerFallingSpeed: number = 0.9; // プレイ中の自然落下スピード
    private readonly _playerDownSpeed: number = 10; // プレイ中の下キー押下時の落下スピード
    private readonly _playerGroundFrame: number = 20; // 何フレーム設置したらぷよを固定するか
    private readonly _playerMoveFrame: number = 10; // 左右移動に消費するフレーム数
    private readonly _playerRotateFrame: number = 10; // 回転に消費するフレーム数
    private readonly _erasePuyoCount: number = 4; // 何個以上揃ったら消えるか
    private readonly _eraseAnimationDuration: number = 30; // 何フレームでぷよを消すか
    private readonly _zenkeshiDuration: number = 150; // 全消し時のアニメーションミリセカンド
    private readonly _gameOverFrame: number = 3000; // ゲームオーバー演出のサイクルフレーム

    get puyoImageWidth(): number {
        // フィールドサイズ追加
        // 高さが全部入るように
        this._puyoImageWidth =
            (window.innerHeight - this._fontHeight) / this._stageRows;
        return this._puyoImageWidth;
    }

    get puyoImageHeight(): number {
        // フィールドサイズ追加
        // 高さが全部入るように
        this._puyoImageHeight =
            (window.innerHeight - this._fontHeight) / this._stageRows;
        return this._puyoImageHeight;
    }

    get fontHeight(): number {
        return this._fontHeight;
    }

    get stageCols(): number {
        return this._stageCols;
    }

    get stageRows(): number {
        return this._stageRows;
    }

    get stageBackgroundColor(): string {
        return this._stageBackgroundColor;
    }

    get scoreBackgroundColor(): string {
        return this._scoreBackgroundColor;
    }

    get puyoColors(): number {
        return this._puyoColors;
    }

    get freeFallingSpeed(): number {
        return this._freeFallingSpeed;
    }

    get playerFallingSpeed(): number {
        return this._playerFallingSpeed;
    }

    get playerDownSpeed(): number {
        return this._playerDownSpeed;
    }

    get playerGroundFrame(): number {
        return this._playerGroundFrame;
    }

    get playerMoveFrame(): number {
        return this._playerMoveFrame;
    }

    get playerRotateFrame(): number {
        return this._playerRotateFrame;
    }

    get erasePuyoCount(): number {
        return this._erasePuyoCount;
    }

    get  eraseAnimationDuration(): number {
        return this._eraseAnimationDuration;
    }

    get zenkeshiDuration(): number {
        return this._zenkeshiDuration;
    }

    get gameOverFrame(): number {
        return this._gameOverFrame;
    }
}
