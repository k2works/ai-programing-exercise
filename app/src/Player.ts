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

  // ぷよを時計回りに回転する
  rotatePuyoClockwise(puyo: PuyoPair): PuyoPair {
    const newPuyo = puyo.clone()
    
    // メインぷよを基準にサブぷよを時計回りに回転
    // 相対位置を計算
    const relativeX = puyo.sub.x - puyo.main.x
    const relativeY = puyo.sub.y - puyo.main.y
    
    // 時計回り回転: (x, y) -> (y, -x)
    const newRelativeX = relativeY
    const newRelativeY = -relativeX
    
    newPuyo.sub.x = puyo.main.x + newRelativeX
    newPuyo.sub.y = puyo.main.y + newRelativeY

    // 回転可能かチェック（壁キック含む）
    return this.tryRotationWithWallKick(puyo, newPuyo)
  }

  // ぷよを反時計回りに回転する
  rotatePuyoCounterClockwise(puyo: PuyoPair): PuyoPair {
    const newPuyo = puyo.clone()
    
    // メインぷよを基準にサブぷよを反時計回りに回転
    // 相対位置を計算
    const relativeX = puyo.sub.x - puyo.main.x
    const relativeY = puyo.sub.y - puyo.main.y
    
    // 反時計回り回転: (x, y) -> (-y, x)
    const newRelativeX = -relativeY
    const newRelativeY = relativeX
    
    newPuyo.sub.x = puyo.main.x + newRelativeX
    newPuyo.sub.y = puyo.main.y + newRelativeY

    // 回転可能かチェック（壁キック含む）
    return this.tryRotationWithWallKick(puyo, newPuyo)
  }

  // 回転を試行し、壁キックも考慮する
  private tryRotationWithWallKick(originalPuyo: PuyoPair, rotatedPuyo: PuyoPair): PuyoPair {
    // 通常の回転が可能かチェック
    if (this.isValidPuyoPosition(rotatedPuyo)) {
      return rotatedPuyo
    }

    // 壁キックを試行
    const wallKickOffsets = [
      { x: -1, y: 0 }, // 左にずらす
      { x: 1, y: 0 },  // 右にずらす
      { x: 0, y: -1 }, // 上にずらす
      { x: 0, y: 1 },  // 下にずらす
    ]

    for (const offset of wallKickOffsets) {
      const kickedPuyo = rotatedPuyo.clone()
      kickedPuyo.main.x += offset.x
      kickedPuyo.main.y += offset.y
      kickedPuyo.sub.x += offset.x
      kickedPuyo.sub.y += offset.y

      if (this.isValidPuyoPosition(kickedPuyo)) {
        return kickedPuyo
      }
    }

    // 回転できない場合は元の位置を返す
    return originalPuyo
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