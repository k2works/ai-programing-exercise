import { describe, it, expect } from 'vitest'
import { PuyoPair, PairRotation } from './puyopair'
import { PuyoColor } from './puyo'

describe('PuyoPair', () => {
  describe('基本機能', () => {
    it('ぷよペアを作成できる', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      expect(pair.getMainColor()).toBe(PuyoColor.Red)
      expect(pair.getSubColor()).toBe(PuyoColor.Blue)
      expect(pair.getX()).toBe(3)
      expect(pair.getY()).toBe(5)
      expect(pair.getRotation()).toBe(PairRotation.Up)
    })

    it('初期状態でサブぷよは上にある', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      const [subX, subY] = pair.getSubPosition()
      
      expect(subX).toBe(3)
      expect(subY).toBe(4) // メインぷよ(3,5)の上
    })
  })

  describe('回転システム', () => {
    it('右回転できる', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.rotateRight()
      
      expect(pair.getRotation()).toBe(PairRotation.Right)
    })

    it('左回転できる', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.rotateLeft()
      
      expect(pair.getRotation()).toBe(PairRotation.Left)
    })

    it('右回転4回で元の状態に戻る', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      const initialRotation = pair.getRotation()
      
      pair.rotateRight()
      pair.rotateRight()
      pair.rotateRight()
      pair.rotateRight()
      
      expect(pair.getRotation()).toBe(initialRotation)
    })

    it('左回転4回で元の状態に戻る', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      const initialRotation = pair.getRotation()
      
      pair.rotateLeft()
      pair.rotateLeft()
      pair.rotateLeft()
      pair.rotateLeft()
      
      expect(pair.getRotation()).toBe(initialRotation)
    })

    it('右回転と左回転は互いに逆回転', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      const initialRotation = pair.getRotation()
      
      pair.rotateRight()
      pair.rotateLeft()
      
      expect(pair.getRotation()).toBe(initialRotation)
    })
  })

  describe('サブぷよ位置計算', () => {
    it('Up回転時：サブぷよは上にある', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      // 初期状態はUp
      
      const [subX, subY] = pair.getSubPosition()
      
      expect(subX).toBe(3)
      expect(subY).toBe(4)
    })

    it('Right回転時：サブぷよは右にある', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.rotateRight() // Up -> Right
      const [subX, subY] = pair.getSubPosition()
      
      expect(subX).toBe(4)
      expect(subY).toBe(5)
    })

    it('Down回転時：サブぷよは下にある', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.rotateRight()
      pair.rotateRight() // Up -> Right -> Down
      const [subX, subY] = pair.getSubPosition()
      
      expect(subX).toBe(3)
      expect(subY).toBe(6)
    })

    it('Left回転時：サブぷよは左にある', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.rotateLeft() // Up -> Left
      const [subX, subY] = pair.getSubPosition()
      
      expect(subX).toBe(2)
      expect(subY).toBe(5)
    })
  })

  describe('位置変更', () => {
    it('位置を変更できる', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.setPosition(7, 10)
      
      expect(pair.getX()).toBe(7)
      expect(pair.getY()).toBe(10)
    })

    it('位置変更後も相対位置が正しく計算される', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Blue, 3, 5)
      
      pair.setPosition(7, 10)
      pair.rotateRight() // Right
      
      const [subX, subY] = pair.getSubPosition()
      expect(subX).toBe(8) // 7 + 1
      expect(subY).toBe(10)
    })
  })

  describe('同色ぷよペア', () => {
    it('同じ色のぷよペアを作成できる', () => {
      const pair = new PuyoPair(PuyoColor.Red, PuyoColor.Red, 3, 5)
      
      expect(pair.getMainColor()).toBe(PuyoColor.Red)
      expect(pair.getSubColor()).toBe(PuyoColor.Red)
    })
  })
})

describe('PairRotation', () => {
  it('正しい回転値が定義されている', () => {
    expect(PairRotation.Up).toBe(0)
    expect(PairRotation.Right).toBe(1)
    expect(PairRotation.Down).toBe(2)
    expect(PairRotation.Left).toBe(3)
  })
})