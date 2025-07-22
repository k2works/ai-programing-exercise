import { PuyoPair } from './Puyo'
import { Stage } from './Stage'

// プレイヤーの入力とぷよの操作を管理するクラス
export class Player {
  private stage: Stage

  constructor(stage: Stage) {
    this.stage = stage
  }

  // ぷよを左に移動する
  movePuyoLeft(puyo: PuyoPair): PuyoPair {
    const newPuyo = puyo.clone()
    newPuyo.main.x -= 1
    newPuyo.sub.x -= 1

    // 移動可能かチェック
    if (this.isValidPuyoPosition(newPuyo)) {
      return newPuyo
    }
    
    // 移動できない場合は元の位置を返す
    return puyo
  }

  // ぷよを右に移動する
  movePuyoRight(puyo: PuyoPair): PuyoPair {
    const newPuyo = puyo.clone()
    newPuyo.main.x += 1
    newPuyo.sub.x += 1

    // 移動可能かチェック
    if (this.isValidPuyoPosition(newPuyo)) {
      return newPuyo
    }
    
    // 移動できない場合は元の位置を返す
    return puyo
  }

  // ぷよを下に移動する（落下）
  dropPuyoDown(puyo: PuyoPair): PuyoPair {
    const newPuyo = puyo.clone()
    newPuyo.main.y += 1
    newPuyo.sub.y += 1

    // 移動可能かチェック
    if (this.isValidPuyoPosition(newPuyo)) {
      return newPuyo
    }
    
    // 移動できない場合は元の位置を返す（着地）
    return puyo
  }

  // ぷよの位置が有効かどうかをチェック
  isValidPuyoPosition(puyo: PuyoPair): boolean {
    // メインぷよのチェック
    if (!this.stage.isValidPosition(puyo.main.x, puyo.main.y)) {
      return false
    }

    // サブぷよのチェック
    if (!this.stage.isValidPosition(puyo.sub.x, puyo.sub.y)) {
      return false
    }

    // 既存のぷよとの衝突チェック
    if (this.stage.getCell(puyo.main.x, puyo.main.y) !== 0) {
      return false
    }

    if (this.stage.getCell(puyo.sub.x, puyo.sub.y) !== 0) {
      return false
    }

    return true
  }
}