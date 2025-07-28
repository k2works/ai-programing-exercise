import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { Puyo } from '../puyo'

describe('Puyo', () => {
  let puyo: Puyo

  beforeEach(() => {
    // Math.randomをモック化して予測可能な値を返すようにする
    vi.spyOn(Math, 'random').mockReturnValue(0.5)
    puyo = new Puyo()
  })

  afterEach(() => {
    vi.restoreAllMocks()
  })

  describe('generateNewPuyoPair', () => {
    it('1-4の範囲でランダムな色のペアを生成する', () => {
      // Math.randomが0.5を返すので、色は3になる（0.5 * 4 + 1 = 3）
      const pair = puyo.generateNewPuyoPair()

      expect(pair.color1).toBe(3)
      expect(pair.color2).toBe(3)
      expect(pair.color1).toBeGreaterThanOrEqual(1)
      expect(pair.color1).toBeLessThanOrEqual(4)
      expect(pair.color2).toBeGreaterThanOrEqual(1)
      expect(pair.color2).toBeLessThanOrEqual(4)
    })
  })

  describe('spawnActivePuyo', () => {
    it('ゲームオーバーでない場合、新しいアクティブぷよを生成する', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)

      const result = puyo.spawnActivePuyo(gameOverCallback)

      expect(result).toBe(true)
      expect(gameOverCallback).toHaveBeenCalled()

      const activePuyo = puyo.getActivePuyo()
      expect(activePuyo).not.toBeNull()
      expect(activePuyo!.x).toBe(2)
      expect(activePuyo!.y).toBe(0)
      expect(activePuyo!.direction).toBe(0)
    })

    it('ゲームオーバーの場合、アクティブぷよを生成しない', () => {
      const gameOverCallback = vi.fn().mockReturnValue(true)

      const result = puyo.spawnActivePuyo(gameOverCallback)

      expect(result).toBe(false)
      expect(puyo.getActivePuyo()).toBeNull()
    })
  })

  describe('getActivePuyo', () => {
    it('アクティブぷよが存在しない場合はnullを返す', () => {
      expect(puyo.getActivePuyo()).toBeNull()
    })

    it('アクティブぷよが存在する場合は状態を返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      const activePuyo = puyo.getActivePuyo()
      expect(activePuyo).not.toBeNull()
      expect(activePuyo!.x).toBe(2)
      expect(activePuyo!.y).toBe(0)
    })
  })

  describe('getNextPuyo', () => {
    it('次のぷよの情報を返す', () => {
      const nextPuyo = puyo.getNextPuyo()
      expect(nextPuyo).toHaveProperty('color1')
      expect(nextPuyo).toHaveProperty('color2')
      expect(nextPuyo.color1).toBeGreaterThanOrEqual(1)
      expect(nextPuyo.color1).toBeLessThanOrEqual(4)
    })
  })

  describe('getActivePuyoDirection', () => {
    it('アクティブぷよが存在しない場合は0を返す', () => {
      expect(puyo.getActivePuyoDirection()).toBe(0)
    })

    it('アクティブぷよが存在する場合は方向を返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      expect(puyo.getActivePuyoDirection()).toBe(0)
    })
  })

  describe('getActivePuyoPositions', () => {
    it('アクティブぷよが存在しない場合は空配列を返す', () => {
      expect(puyo.getActivePuyoPositions()).toEqual([])
    })

    it('アクティブぷよが存在する場合は位置配列を返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      const positions = puyo.getActivePuyoPositions()
      expect(positions).toHaveLength(2)
      expect(positions[0]).toEqual({ x: 2, y: 0 }) // 中心ぷよ
      expect(positions[1]).toEqual({ x: 2, y: 1 }) // 下向き（direction: 0）
    })
  })

  describe('rotateActivePuyo', () => {
    it('アクティブぷよが存在しない場合は何もしない', () => {
      puyo.rotateActivePuyo()
      expect(puyo.getActivePuyo()).toBeNull()
    })

    it('アクティブぷよが存在する場合は方向を1つ進める', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      expect(puyo.getActivePuyoDirection()).toBe(0)

      puyo.rotateActivePuyo()
      expect(puyo.getActivePuyoDirection()).toBe(1)

      puyo.rotateActivePuyo()
      expect(puyo.getActivePuyoDirection()).toBe(2)

      puyo.rotateActivePuyo()
      expect(puyo.getActivePuyoDirection()).toBe(3)

      puyo.rotateActivePuyo()
      expect(puyo.getActivePuyoDirection()).toBe(0) // 4つで一周
    })
  })

  describe('clearActivePuyo', () => {
    it('アクティブぷよをクリアする', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      expect(puyo.getActivePuyo()).not.toBeNull()

      puyo.clearActivePuyo()
      expect(puyo.getActivePuyo()).toBeNull()
    })
  })

  describe('updateActivePuyoPosition', () => {
    it('アクティブぷよが存在しない場合は何もしない', () => {
      puyo.updateActivePuyoPosition(1, 1)
      expect(puyo.getActivePuyo()).toBeNull()
    })

    it('アクティブぷよが存在する場合は位置を更新する', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      puyo.updateActivePuyoPosition(1, 1)

      const activePuyo = puyo.getActivePuyo()
      expect(activePuyo!.x).toBe(3) // 2 + 1
      expect(activePuyo!.y).toBe(1) // 0 + 1
    })
  })

  describe('hasActivePuyo', () => {
    it('アクティブぷよが存在しない場合はfalseを返す', () => {
      expect(puyo.hasActivePuyo()).toBe(false)
    })

    it('アクティブぷよが存在する場合はtrueを返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      expect(puyo.hasActivePuyo()).toBe(true)
    })
  })

  describe('calculatePuyoPositions', () => {
    it('指定した位置と方向でのぷよ位置を計算する', () => {
      // 縦配置、下向き（direction: 0）
      const positions0 = puyo.calculatePuyoPositions(2, 3, 0)
      expect(positions0).toEqual([
        { x: 2, y: 3 }, // 中心
        { x: 2, y: 4 }, // 下
      ])

      // 横配置、右向き（direction: 1）
      const positions1 = puyo.calculatePuyoPositions(2, 3, 1)
      expect(positions1).toEqual([
        { x: 2, y: 3 }, // 中心
        { x: 3, y: 3 }, // 右
      ])

      // 縦配置、上向き（direction: 2）
      const positions2 = puyo.calculatePuyoPositions(2, 3, 2)
      expect(positions2).toEqual([
        { x: 2, y: 3 }, // 中心
        { x: 2, y: 2 }, // 上
      ])

      // 横配置、左向き（direction: 3）
      const positions3 = puyo.calculatePuyoPositions(2, 3, 3)
      expect(positions3).toEqual([
        { x: 2, y: 3 }, // 中心
        { x: 1, y: 3 }, // 左
      ])
    })
  })

  describe('canRotate', () => {
    it('アクティブぷよが存在しない場合はfalseを返す', () => {
      const isPositionValidCallback = vi.fn().mockReturnValue(true)

      expect(puyo.canRotate(isPositionValidCallback)).toBe(false)
      expect(isPositionValidCallback).not.toHaveBeenCalled()
    })

    it('回転後の位置が有効な場合はtrueを返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      const isPositionValidCallback = vi.fn().mockReturnValue(true)

      expect(puyo.canRotate(isPositionValidCallback)).toBe(true)
      expect(isPositionValidCallback).toHaveBeenCalledWith([
        { x: 2, y: 0 }, // 中心
        { x: 3, y: 0 }, // 右（direction: 0 → 1）
      ])
    })

    it('回転後の位置が無効な場合はfalseを返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      const isPositionValidCallback = vi.fn().mockReturnValue(false)

      expect(puyo.canRotate(isPositionValidCallback)).toBe(false)
    })
  })

  describe('canMoveTo', () => {
    it('アクティブぷよが存在しない場合はfalseを返す', () => {
      const isPositionValidCallback = vi.fn().mockReturnValue(true)

      expect(puyo.canMoveTo(1, 0, isPositionValidCallback)).toBe(false)
      expect(isPositionValidCallback).not.toHaveBeenCalled()
    })

    it('移動先の位置が有効な場合はtrueを返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      const isPositionValidCallback = vi.fn().mockReturnValue(true)

      expect(puyo.canMoveTo(1, 0, isPositionValidCallback)).toBe(true)
      expect(isPositionValidCallback).toHaveBeenCalledWith([
        { x: 3, y: 0 }, // 中心（2+1, 0+0）
        { x: 3, y: 1 }, // 下（direction: 0）
      ])
    })

    it('移動先の位置が無効な場合はfalseを返す', () => {
      const gameOverCallback = vi.fn().mockReturnValue(false)
      puyo.spawnActivePuyo(gameOverCallback)

      const isPositionValidCallback = vi.fn().mockReturnValue(false)

      expect(puyo.canMoveTo(1, 0, isPositionValidCallback)).toBe(false)
    })
  })
})
