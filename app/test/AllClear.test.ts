import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../src/Game'

describe('All Clear System (Zenkeshi)', () => {
  let canvas: HTMLCanvasElement
  let game: Game

  beforeEach(() => {
    // Canvas contextをモック
    canvas = document.createElement('canvas')
    canvas.width = 200
    canvas.height = 400
    
    const mockContext = {
      clearRect: vi.fn(),
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      fillStyle: '',
      strokeStyle: ''
    } as any
    
    vi.spyOn(canvas, 'getContext').mockReturnValue(mockContext)
    
    game = new Game(canvas)
  })

  describe('All Clear Detection', () => {
    it('should detect all clear when stage is completely empty', () => {
      game.start()
      const stage = game.getStage()

      // 空のステージをチェック
      const isAllClear = game.checkAllClear()
      expect(isAllClear).toBe(true)
    })

    it('should not detect all clear when puyo remain on stage', () => {
      game.start()
      const stage = game.getStage()

      // 1つだけぷよを配置
      stage.setCell(0, 11, 1)

      const isAllClear = game.checkAllClear()
      expect(isAllClear).toBe(false)
    })

    it('should detect all clear after elimination clears entire stage', () => {
      game.start()
      const stage = game.getStage()

      // 消去によって全部なくなるパターンを設定
      stage.setCell(0, 11, 1) // 赤
      stage.setCell(1, 11, 1) // 赤
      stage.setCell(2, 11, 1) // 赤
      stage.setCell(3, 11, 1) // 赤

      // 消去前は全消しではない
      expect(game.checkAllClear()).toBe(false)

      // 消去実行
      game.processEliminationWithChain()

      // 消去後は全消し状態
      expect(game.checkAllClear()).toBe(true)
    })
  })

  describe('All Clear Bonus Calculation', () => {
    it('should calculate all clear bonus correctly', () => {
      const baseScore = 100
      const allClearBonus = game.calculateAllClearBonus(baseScore)
      
      // 全消しボーナスは基本スコアの30倍が一般的
      expect(allClearBonus).toBe(baseScore * 30)
    })

    it('should have different all clear bonus for different base scores', () => {
      const bonus1 = game.calculateAllClearBonus(50)
      const bonus2 = game.calculateAllClearBonus(100)
      
      expect(bonus2).toBe(bonus1 * 2) // 基本スコアに比例
    })
  })

  describe('All Clear Integration with Chain System', () => {
    it('should apply all clear bonus when chain results in complete elimination', () => {
      game.start()
      const stage = game.getStage()

      // 連鎖で全消しになるパターン
      // 底: 赤4つ（最初に消去）
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      // 上: 青4つ（赤消去後に落下して消去、全消し達成）
      stage.setCell(0, 10, 2)
      stage.setCell(1, 9, 2)
      stage.setCell(2, 8, 2)
      stage.setCell(3, 7, 2)

      const initialScore = game.getScore()
      
      // 連鎖処理（全消しボーナス込み）を実行
      const result = game.processEliminationWithAllClearCheck()
      
      expect(result.isAllClear).toBe(true)
      expect(result.chainCount).toBe(2)
      expect(result.allClearBonus).toBeGreaterThan(0)
      
      const finalScore = game.getScore()
      const totalGained = finalScore - initialScore
      
      // 通常の連鎖スコア + 全消しボーナスが含まれている
      expect(totalGained).toBeGreaterThan(result.allClearBonus)
    })

    it('should not apply all clear bonus when puyo remain after chain', () => {
      game.start()
      const stage = game.getStage()

      // 連鎖するが全消しにならないパターン
      stage.setCell(0, 11, 1) // 赤（消去される）
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      stage.setCell(0, 10, 2) // 青（消去される）
      stage.setCell(1, 9, 2)
      stage.setCell(2, 8, 2)
      stage.setCell(3, 7, 2)
      
      stage.setCell(5, 11, 3) // 緑（残る）

      const result = game.processEliminationWithAllClearCheck()
      
      expect(result.isAllClear).toBe(false)
      expect(result.allClearBonus).toBe(0)
    })

    it('should provide detailed all clear information for UI', () => {
      game.start()
      const stage = game.getStage()

      // シンプルな全消しパターン
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      const result = game.processEliminationWithAllClearCheck()
      
      expect(result).toHaveProperty('isAllClear')
      expect(result).toHaveProperty('chainCount')
      expect(result).toHaveProperty('allClearBonus')
      expect(result).toHaveProperty('totalScore')
      expect(result).toHaveProperty('chainDetails')
    })
  })

  describe('All Clear Animation and Effects', () => {
    it('should provide all clear effect information', () => {
      const effectInfo = game.getAllClearEffect()
      
      expect(effectInfo).toHaveProperty('message')
      expect(effectInfo).toHaveProperty('duration')
      expect(effectInfo).toHaveProperty('color')
      
      expect(effectInfo.message).toContain('全消し')
      expect(effectInfo.duration).toBeGreaterThan(0)
    })

    it('should track all clear achievements', () => {
      game.start()
      const stage = game.getStage()

      // 全消しを実行
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      const initialAllClears = game.getAllClearCount()
      game.processEliminationWithAllClearCheck()
      const finalAllClears = game.getAllClearCount()
      
      expect(finalAllClears).toBe(initialAllClears + 1)
    })
  })

  describe('Edge Cases', () => {
    it('should handle multiple all clears in single game', () => {
      game.start()
      
      // 1回目の全消し
      let stage = game.getStage()
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      game.processEliminationWithAllClearCheck()
      expect(game.getAllClearCount()).toBe(1)

      // 2回目の全消しのセットアップ（新しいぷよが落下後）
      stage.setCell(2, 11, 2)
      stage.setCell(3, 11, 2)
      stage.setCell(4, 11, 2)
      stage.setCell(5, 11, 2)
      
      game.processEliminationWithAllClearCheck()
      expect(game.getAllClearCount()).toBe(2)
    })

    it('should reset all clear count on new game', () => {
      game.start()
      
      // 全消しを実行してカウントを増やす
      const stage = game.getStage()
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      game.processEliminationWithAllClearCheck()
      expect(game.getAllClearCount()).toBe(1)

      // 新しいゲームを開始
      game.start()
      expect(game.getAllClearCount()).toBe(0)
    })

    it('should handle all clear with single puyo group', () => {
      game.start()
      const stage = game.getStage()

      // 最小の全消しパターン（4つのぷよ）
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      const result = game.processEliminationWithAllClearCheck()
      
      expect(result.isAllClear).toBe(true)
      expect(result.chainCount).toBe(1)
      expect(result.allClearBonus).toBeGreaterThan(0)
    })
  })
})