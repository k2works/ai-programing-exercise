import {Config} from "./config";
import {PuyoImage} from "./puyoimage";
import {Stage} from "./stage";
import {Player} from "./player";
import {Score} from "./score";

export type GameMode =
  | "start"
  | "checkFall"
  | "fall"
  | "checkErase"
  | "erasing"
  | "newPuyo"
  | "playing"
  | "moving"
  | "rotating"
  | "fix"
  | "gameOver"
  | "batankyu";

export class Game {
  private mode: GameMode = "start"; // ゲームの現在の状況
  private frame: number = 0; // ゲームの現在フレーム(1/60秒ごとに1追加される)
  private combinationCount: number = 0; // 何連鎖かどうか
  private readonly config: Config;
  private puyoImage: PuyoImage | undefined;
  private stage: Stage | undefined;
  private player: Player | undefined;
  private score: Score | undefined;

  constructor() {
    this.config = new Config();
  }

  initialize(): void {
    // 画像を準備する
    this.puyoImage = new PuyoImage(this.config);
    // ステージを準備する
    this.stage = new Stage(this.config, this.puyoImage);
    // ユーザー操作の準備をする
    this.player = new Player(this.config);
    // スコア表示の準備をする
    this.score = new Score(this.config, this.stage);

    // ゲームモードを設定
    this.mode = 'start';
  }

  loop(): void {
    // フレームカウントを更新
    this.frame++;

    // ゲームの状態に応じた処理
    if (this.stage  === undefined || this.puyoImage === undefined || this.player === undefined || this.score === undefined) return;

    switch (this.mode) {
      case "start":
        // 最初は、もしかしたら空中にあるかもしれないぷよを自由落下させるところからスタート
        this.mode = "checkFall";
        break;
      case "checkFall":
        // 落ちるかどうか判定する
        if (this.stage.checkFall()) {
          this.mode = "fall";
        } else {
          // 落ちない場合は、消せるかどうか判定する
          this.mode = "checkErase";
        }
        break;
      case "fall":
        // 自由落下させる
        if (!this.stage.fall()) {
          // 自由落下が終わったら、消せるかどうか判定する
          this.mode = "checkErase";
        }
        break;
      case "checkErase":
        // 消せるかどうか判定する
        const eraseInfo = this.stage.checkErase(this.frame);
        if (eraseInfo) {
          this.mode = "erasing";
          this.combinationCount++;
          // 得点を計算する
          this.score.calculateScore(
              this.combinationCount,
              eraseInfo.piece,
              eraseInfo.color
          );
          this.stage.hideZenkeshi();
        } else {
          if (this.stage.puyoCount === 0 && this.combinationCount > 0) {
            // 全消しの処理をする
            this.stage.showZenkeshi();
            this.score.addScore(3600);
          }
          this.combinationCount = 0;
          // 消せなかったら、新しいぷよを登場させる
          this.mode = "newPuyo";
        }
        break;
      case "erasing":
        // 消すアニメーションをする
        if (!this.stage.erasing(this.frame)) {
          // 消すアニメーションが終わったら、自由落下を確認する
          this.mode = "checkFall";
        }
        break;
      case "newPuyo":
        // 新しいぷよを登場させる
        if (!this.player.createNewPuyo(this.stage, this.puyoImage, this.score)) {
          // 新しいぷよを登場させられなかったら、ゲームオーバー
          this.mode = "gameOver";
        }
        this.mode = "playing";
        break;
      case "playing":
        // プレイ中の処理
        if (this.player.fall(this.player.keyStatus.down)) {
          // 落下が終わっていたら、ぷよを固定する
          this.player.fix();
          this.mode = "checkFall";
        } else {
          // 落下中でなければ、プレイヤーの操作を処理する
          const nextMode = this.player.playing(this.frame);
          if (nextMode !== 'playing') {
            this.mode = nextMode;
          }
        }
        break;
      case "moving":
        // 移動中の処理
        if (!this.player.moving(this.frame)) {
          // 移動が終わったら、プレイ中に戻す
          this.mode = "playing";
        }
        break;
      case "rotating":
        // 回転中の処理
        if (!this.player.rotating(this.frame, this.player.currentRotationAngle)) {
          // 回転が終わったら、プレイ中に戻す
          this.mode = "playing";
        }
        break;
      case "gameOver":
        // ばたんきゅーの準備をする
        this.puyoImage.prepareBatankyu(this.frame, this.stage);
        this.mode = "batankyu";
        break;
      case "batankyu":
        this.puyoImage.batankyu(this.frame);
        this.player.batankyu();
        if (this.player.keyStatus.up) {
          this.initialize();
        }
        break;
    }
    this.frame++;
    requestAnimationFrame(this.loop.bind(this)); // 1/60秒後にもう一度呼び出す
  }
}
