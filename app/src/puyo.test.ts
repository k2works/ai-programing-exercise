import { describe, it, expect } from 'vitest'
import { Puyo, PuyoColor } from './puyo'

describe('ぷよ', () => {
  describe('ぷよの基本構造', () => {
    it('ぷよを作成できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 2, 1)

      expect(puyo.getColor()).toBe(PuyoColor.Red)
      expect(puyo.getX()).toBe(2)
      expect(puyo.getY()).toBe(1)
    })

    it('ぷよの位置を更新できる', () => {
      const puyo = new Puyo(PuyoColor.Blue, 0, 0)

      puyo.setPosition(3, 5)

      expect(puyo.getX()).toBe(3)
      expect(puyo.getY()).toBe(5)
    })

    it('空のぷよを作成できる', () => {
      const puyo = new Puyo(PuyoColor.Empty, 0, 0)

      expect(puyo.isEmpty()).toBe(true)
      expect(puyo.getColor()).toBe(PuyoColor.Empty)
    })

    it('色付きのぷよは空ではない', () => {
      const puyo = new Puyo(PuyoColor.Green, 0, 0)

      expect(puyo.isEmpty()).toBe(false)
    })
  })

  describe('ぷよの色', () => {
    it('すべての色が定義されている', () => {
      expect(PuyoColor.Empty).toBeDefined()
      expect(PuyoColor.Red).toBeDefined()
      expect(PuyoColor.Blue).toBeDefined()
      expect(PuyoColor.Green).toBeDefined()
      expect(PuyoColor.Yellow).toBeDefined()
    })
  })
})