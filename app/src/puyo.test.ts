import { describe, it, expect } from 'vitest'
import { Puyo, PuyoColor } from './puyo'

describe('Puyo', () => {
  describe('基本機能', () => {
    it('ぷよを作成できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 2, 3)
      
      expect(puyo.getColor()).toBe(PuyoColor.Red)
      expect(puyo.getX()).toBe(2)
      expect(puyo.getY()).toBe(3)
    })

    it('空のぷよを作成できる', () => {
      const puyo = new Puyo(PuyoColor.Empty, 0, 0)
      
      expect(puyo.getColor()).toBe(PuyoColor.Empty)
      expect(puyo.isEmpty()).toBe(true)
    })

    it('色付きぷよは空ではない', () => {
      const redPuyo = new Puyo(PuyoColor.Red, 0, 0)
      const bluePuyo = new Puyo(PuyoColor.Blue, 0, 0)
      const greenPuyo = new Puyo(PuyoColor.Green, 0, 0)
      const yellowPuyo = new Puyo(PuyoColor.Yellow, 0, 0)
      
      expect(redPuyo.isEmpty()).toBe(false)
      expect(bluePuyo.isEmpty()).toBe(false)
      expect(greenPuyo.isEmpty()).toBe(false)
      expect(yellowPuyo.isEmpty()).toBe(false)
    })
  })

  describe('状態変更', () => {
    it('色を変更できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 0, 0)
      
      puyo.setColor(PuyoColor.Blue)
      
      expect(puyo.getColor()).toBe(PuyoColor.Blue)
    })

    it('位置を変更できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 0, 0)
      
      puyo.setPosition(5, 7)
      
      expect(puyo.getX()).toBe(5)
      expect(puyo.getY()).toBe(7)
    })

    it('空ぷよに変更できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 0, 0)
      
      puyo.setColor(PuyoColor.Empty)
      
      expect(puyo.isEmpty()).toBe(true)
    })
  })

  describe('境界値テスト', () => {
    it('負の座標でもぷよを作成できる', () => {
      const puyo = new Puyo(PuyoColor.Red, -1, -1)
      
      expect(puyo.getX()).toBe(-1)
      expect(puyo.getY()).toBe(-1)
    })

    it('大きな座標でもぷよを作成できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 1000, 1000)
      
      expect(puyo.getX()).toBe(1000)
      expect(puyo.getY()).toBe(1000)
    })
  })
})

describe('PuyoColor', () => {
  it('正しい色の値が定義されている', () => {
    expect(PuyoColor.Empty).toBe(0)
    expect(PuyoColor.Red).toBe(1)
    expect(PuyoColor.Blue).toBe(2)
    expect(PuyoColor.Green).toBe(3)
    expect(PuyoColor.Yellow).toBe(4)
  })
})