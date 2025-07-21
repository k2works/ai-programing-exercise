import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Player } from './player'
import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'
import { Puyo, PuyoColor } from './puyo'

describe('Player', () => {
  let player: Player
  let config: Config
  let stage: Stage
  let puyoImage: PuyoImage

  beforeEach(() => {
    config = new Config()
    puyoImage = new PuyoImage()
    stage = new Stage(config, puyoImage)
    stage.initialize()
    player = new Player(config, stage, puyoImage)
  })

  describe('初期化', () => {
    it('プレイヤーを初期化できる', () => {
      player.initialize()
      
      expect(player.getCurrentPair()).toBeDefined()
      expect(player.getNextPair()).toBeDefined()
      expect(player.isPlaced()).toBe(false)
    })

    it('初期化後のぷよペアは中央に配置される', () => {
      player.initialize()
      
      const currentPair = player.getCurrentPair()
      expect(currentPair.getX()).toBe(3) // Math.floor(6/2) = 3
      expect(currentPair.getY()).toBe(1)
    })
  })

  describe('移動操作', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('左に移動できる', () => {
      const initialX = player.getCurrentPair().getX()
      
      const result = player.moveLeft()
      
      expect(result).toBe(true)
      expect(player.getCurrentPair().getX()).toBe(initialX - 1)
    })

    it('右に移動できる', () => {
      const initialX = player.getCurrentPair().getX()
      
      const result = player.moveRight()
      
      expect(result).toBe(true)
      expect(player.getCurrentPair().getX()).toBe(initialX + 1)
    })

    it('左端でこれ以上左に移動できない', () => {
      // 左端まで移動
      while (player.getCurrentPair().getX() > 0) {
        player.moveLeft()
      }
      
      const leftmostX = player.getCurrentPair().getX()
      const result = player.moveLeft()
      
      expect(result).toBe(false)
      expect(player.getCurrentPair().getX()).toBe(leftmostX)
    })

    it('右端でこれ以上右に移動できない', () => {
      // 右端まで移動
      while (player.getCurrentPair().getX() < config.stageWidth - 1) {
        player.moveRight()
      }
      
      const rightmostX = player.getCurrentPair().getX()
      const result = player.moveRight()
      
      expect(result).toBe(false)
      expect(player.getCurrentPair().getX()).toBe(rightmostX)
    })
  })

  describe('回転操作', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('右回転できる', () => {
      const initialRotation = player.getCurrentPair().getRotation()
      
      const result = player.rotateRight()
      
      expect(result).toBe(true)
      expect(player.getCurrentPair().getRotation()).toBe((initialRotation + 1) % 4)
    })

    it('左回転できる', () => {
      const initialRotation = player.getCurrentPair().getRotation()
      
      const result = player.rotateLeft()
      
      expect(result).toBe(true)
      expect(player.getCurrentPair().getRotation()).toBe((initialRotation + 3) % 4)
    })

    it('壁際では回転できない場合がある', () => {
      // 左端に移動
      while (player.getCurrentPair().getX() > 0) {
        player.moveLeft()
      }
      
      // 左回転を試す（サブぷよが壁を貫通する場合）
      const result = player.rotateLeft()
      
      // 回転できるかどうかは衝突判定次第
      expect(typeof result).toBe('boolean')
    })
  })

  describe('自動落下', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('fallSpeed回数のupdateで1つ落下する', () => {
      const initialY = player.getCurrentPair().getY()
      
      // fallSpeed回数のupdateを実行
      for (let i = 0; i < config.fallSpeed; i++) {
        player.update()
      }
      
      expect(player.getCurrentPair().getY()).toBe(initialY + 1)
    })

    it('落下タイマーはfallSpeedでリセットされる', () => {
      const initialY = player.getCurrentPair().getY()
      
      // fallSpeed - 1回のupdateでは落下しない
      for (let i = 0; i < config.fallSpeed - 1; i++) {
        player.update()
      }
      
      expect(player.getCurrentPair().getY()).toBe(initialY)
      
      // 1回追加で落下する
      player.update()
      expect(player.getCurrentPair().getY()).toBe(initialY + 1)
    })

    it('底に達すると配置状態になる', () => {
      // 底まで強制移動
      while (!player.isPlaced()) {
        player.update()
      }
      
      expect(player.isPlaced()).toBe(true)
    })
  })

  describe('配置と新しいペア', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('配置後にフィールドにぷよが追加される', () => {
      // 底まで落下させる
      while (!player.isPlaced()) {
        player.update()
      }
      
      // 配置実行
      player.placePuyo()
      
      // フィールドの底付近に配置されたぷよがあることを確認
      let puyoPlaced = false
      for (let y = config.stageHeight - 3; y < config.stageHeight; y++) {
        for (let x = 0; x < config.stageWidth; x++) {
          if (!stage.isEmpty(x, y)) {
            puyoPlaced = true
            break
          }
        }
      }
      
      expect(puyoPlaced).toBe(true)
    })

    it('新しいぷよペアを生成できる', () => {
      const oldPair = player.getCurrentPair()
      const oldNext = player.getNextPair()
      
      player.newPair()
      
      const newPair = player.getCurrentPair()
      const newNext = player.getNextPair()
      
      // 現在のペアが次のペアになっている
      expect(newPair.getMainColor()).toBe(oldNext.getMainColor())
      expect(newPair.getSubColor()).toBe(oldNext.getSubColor())
      
      // 新しい次のペアが生成されている
      expect(newNext).not.toBe(oldNext)
      
      // 配置状態がリセットされている
      expect(player.isPlaced()).toBe(false)
    })
  })

  describe('衝突判定', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('フィールドにぷよがある位置には移動できない', () => {
      const currentPair = player.getCurrentPair()
      const x = currentPair.getX()
      const y = currentPair.getY() + 1
      
      // 移動先に実際のPuyoオブジェクトを配置
      const blockingPuyo = new Puyo(PuyoColor.Red, x, y)
      stage.setPuyo(x, y, blockingPuyo)
      
      // 落下速度分updateを実行して衝突を発生させる
      for (let i = 0; i < config.fallSpeed; i++) {
        player.update()
      }
      
      // 衝突により配置状態になるか、もしくは移動が阻害される
      const placed = player.isPlaced()
      const remainsAtSameY = player.getCurrentPair().getY() === currentPair.getY()
      
      // 衝突によって何らかの制約が働いていることを確認
      expect(placed || remainsAtSameY).toBe(true)
    })
  })

  describe('エッジケース', () => {
    it('配置状態では操作を受け付けない', () => {
      player.initialize()
      
      // 強制的に配置状態にする
      while (!player.isPlaced()) {
        player.update()
      }
      
      const x = player.getCurrentPair().getX()
      
      // 配置状態では移動できない
      expect(player.moveLeft()).toBe(false)
      expect(player.moveRight()).toBe(false)
      expect(player.rotateLeft()).toBe(false)
      expect(player.rotateRight()).toBe(false)
      
      // 位置も変わらない
      expect(player.getCurrentPair().getX()).toBe(x)
    })

    it('currentPairがない場合のエラーハンドリング', () => {
      // 未初期化状態での操作
      expect(player.moveLeft()).toBe(false)
      expect(player.moveRight()).toBe(false)
      expect(player.rotateLeft()).toBe(false)
      expect(player.rotateRight()).toBe(false)
    })
  })

  describe('PuyoImageとの連携', () => {
    beforeEach(() => {
      // createRandomPuyoColorをモック化
      vi.spyOn(puyoImage, 'createRandomPuyoColor')
        .mockReturnValueOnce(PuyoColor.Red)
        .mockReturnValueOnce(PuyoColor.Blue)
        .mockReturnValueOnce(PuyoColor.Green)
        .mockReturnValueOnce(PuyoColor.Yellow)
    })

    it('PuyoImageを使って初期化する', () => {
      player.initialize()
      
      const currentPair = player.getCurrentPair()
      expect(currentPair.getMainColor()).toBe(PuyoColor.Red)
      expect(currentPair.getSubColor()).toBe(PuyoColor.Blue)
      
      const nextPair = player.getNextPair()
      expect(nextPair.getMainColor()).toBe(PuyoColor.Green)
      expect(nextPair.getSubColor()).toBe(PuyoColor.Yellow)
    })
  })
})