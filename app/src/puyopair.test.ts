import { describe, it, expect, beforeEach } from 'vitest'
import { PuyoPair, PairRotation } from './puyopair'
import { PuyoColor } from './puyo'

describe('ぷよペア', () => {
  let puyoPair: PuyoPair

  describe('ぷよペアの基本構造', () => {
    beforeEach(() => {
      puyoPair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 2, 1)
    })

    it('ぷよペアを作成できる', () => {
      expect(puyoPair.getMainColor()).toBe(PuyoColor.Red)
      expect(puyoPair.getSubColor()).toBe(PuyoColor.Blue)
      expect(puyoPair.getX()).toBe(2)
      expect(puyoPair.getY()).toBe(1)
    })

    it('初期状態では上向き回転状態である', () => {
      expect(puyoPair.getRotation()).toBe(PairRotation.Up)
    })

    it('位置を設定できる', () => {
      puyoPair.setPosition(3, 4)
      
      expect(puyoPair.getX()).toBe(3)
      expect(puyoPair.getY()).toBe(4)
    })
  })

  describe('ぷよペアの回転', () => {
    beforeEach(() => {
      puyoPair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 2, 1)
    })

    it('右回転できる', () => {
      puyoPair.rotateRight()
      expect(puyoPair.getRotation()).toBe(PairRotation.Right)
      
      puyoPair.rotateRight()
      expect(puyoPair.getRotation()).toBe(PairRotation.Down)
      
      puyoPair.rotateRight()
      expect(puyoPair.getRotation()).toBe(PairRotation.Left)
      
      puyoPair.rotateRight()
      expect(puyoPair.getRotation()).toBe(PairRotation.Up)
    })

    it('左回転できる', () => {
      puyoPair.rotateLeft()
      expect(puyoPair.getRotation()).toBe(PairRotation.Left)
      
      puyoPair.rotateLeft()
      expect(puyoPair.getRotation()).toBe(PairRotation.Down)
      
      puyoPair.rotateLeft()
      expect(puyoPair.getRotation()).toBe(PairRotation.Right)
      
      puyoPair.rotateLeft()
      expect(puyoPair.getRotation()).toBe(PairRotation.Up)
    })
  })

  describe('ぷよペアの座標計算', () => {
    beforeEach(() => {
      puyoPair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 2, 3)
    })

    it('上向き時のサブぷよ座標を取得できる', () => {
      const [subX, subY] = puyoPair.getSubPosition()
      expect(subX).toBe(2)
      expect(subY).toBe(2)
    })

    it('右向き時のサブぷよ座標を取得できる', () => {
      puyoPair.rotateRight()
      const [subX, subY] = puyoPair.getSubPosition()
      expect(subX).toBe(3)
      expect(subY).toBe(3)
    })

    it('下向き時のサブぷよ座標を取得できる', () => {
      puyoPair.rotateRight()
      puyoPair.rotateRight()
      const [subX, subY] = puyoPair.getSubPosition()
      expect(subX).toBe(2)
      expect(subY).toBe(4)
    })

    it('左向き時のサブぷよ座標を取得できる', () => {
      puyoPair.rotateLeft()
      const [subX, subY] = puyoPair.getSubPosition()
      expect(subX).toBe(1)
      expect(subY).toBe(3)
    })
  })
})