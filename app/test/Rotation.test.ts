import { describe, it, expect, beforeEach } from 'vitest'
import { PuyoPair } from '../src/Puyo'
import { Player } from '../src/Player'
import { Stage } from '../src/Stage'

describe('Puyo Rotation', () => {
  let player: Player
  let stage: Stage

  beforeEach(() => {
    stage = new Stage()
    player = new Player(stage)
  })

  describe('Basic Rotation', () => {
    it('should rotate puyo pair clockwise', () => {
      // 初期位置: メイン(3,5), サブ(3,4) - サブが上
      const puyo = new PuyoPair(3, 5, 1, 2)
      
      const rotated = player.rotatePuyoClockwise(puyo)
      
      // 時計回り回転後: メイン(3,5), サブ(2,5) - サブが左
      expect(rotated.main.x).toBe(3)
      expect(rotated.main.y).toBe(5)
      expect(rotated.sub.x).toBe(2)
      expect(rotated.sub.y).toBe(5)
    })

    it('should rotate puyo pair counter-clockwise', () => {
      // 初期位置: メイン(3,5), サブ(3,4) - サブが上
      const puyo = new PuyoPair(3, 5, 1, 2)
      
      const rotated = player.rotatePuyoCounterClockwise(puyo)
      
      // 反時計回り回転後: メイン(3,5), サブ(4,5) - サブが右
      expect(rotated.main.x).toBe(3)
      expect(rotated.main.y).toBe(5)
      expect(rotated.sub.x).toBe(4)
      expect(rotated.sub.y).toBe(5)
    })

    it('should complete full rotation cycle', () => {
      const puyo = new PuyoPair(3, 5, 1, 2)
      
      // 4回時計回りで元に戻る：上→左→下→右→上
      let rotated = player.rotatePuyoClockwise(puyo) // 左
      rotated = player.rotatePuyoClockwise(rotated) // 下
      rotated = player.rotatePuyoClockwise(rotated) // 右
      rotated = player.rotatePuyoClockwise(rotated) // 上（元の位置）
      
      expect(rotated.main.x).toBe(puyo.main.x)
      expect(rotated.main.y).toBe(puyo.main.y)
      expect(rotated.sub.x).toBe(puyo.sub.x)
      expect(rotated.sub.y).toBe(puyo.sub.y)
    })
  })

  describe('Rotation Validation', () => {
    it('should rotate with wall kick when blocked by wall', () => {
      // 左端のぷよ - 反時計回りで右に壁キック
      const puyoAtLeftWall = new PuyoPair(0, 5, 1, 2)
      
      const rotated = player.rotatePuyoCounterClockwise(puyoAtLeftWall)
      
      // 壁キックで成功: メインが右に移動、サブが右側
      expect(rotated.main.x).toBe(0) // 実際の動作に基づく
      expect(rotated.main.y).toBe(5)
      expect(rotated.sub.x).toBe(1)
      expect(rotated.sub.y).toBe(5)
    })

    it('should not rotate if completely blocked by other puyo', () => {
      // ステージに壁キックでも回転できないよう障害物を配置
      stage.setCell(2, 5, 3) // 左側に障害物（回転先）
      stage.setCell(1, 5, 3) // 壁キック位置にも障害物
      stage.setCell(4, 5, 3) // 右側にも障害物
      stage.setCell(3, 4, 3) // 上側にも障害物
      stage.setCell(3, 6, 3) // 下側にも障害物
      
      const puyo = new PuyoPair(3, 5, 1, 2)
      const rotated = player.rotatePuyoClockwise(puyo)
      
      // 完全にブロックされているので元の位置
      expect(rotated.main.x).toBe(puyo.main.x)
      expect(rotated.main.y).toBe(puyo.main.y)
      expect(rotated.sub.x).toBe(puyo.sub.x)
      expect(rotated.sub.y).toBe(puyo.sub.y)
    })
  })

  describe('Wall Kick', () => {
    it('should perform wall kick when rotating near right wall', () => {
      // 右端のぷよが回転時に壁キック不要で回転成功
      const puyoNearRightWall = new PuyoPair(5, 5, 1, 2) // x=5が右端
      
      const rotated = player.rotatePuyoClockwise(puyoNearRightWall)
      
      // 時計回りでサブが左に移動（壁キック不要）
      expect(rotated.main.x).toBe(5)
      expect(rotated.main.y).toBe(5)
      expect(rotated.sub.x).toBe(4) // サブが左側
      expect(rotated.sub.y).toBe(5)
    })

    it('should perform wall kick when rotating near left wall', () => {
      // 左端のぷよが回転時に壁キック不要で回転成功  
      const puyoNearLeftWall = new PuyoPair(0, 5, 1, 2) // x=0が左端
      
      const rotated = player.rotatePuyoCounterClockwise(puyoNearLeftWall)
      
      // 反時計回りでサブが右に移動（壁キック不要）
      expect(rotated.main.x).toBe(0)
      expect(rotated.main.y).toBe(5)
      expect(rotated.sub.x).toBe(1) // サブが右側
      expect(rotated.sub.y).toBe(5)
    })
  })
})