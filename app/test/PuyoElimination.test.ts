import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../src/Game'
import { Stage } from '../src/Stage'

describe('Puyo Elimination System', () => {
  let canvas: HTMLCanvasElement
  let game: Game
  let stage: Stage

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
    stage = game.getStage()
  })

  describe('Connected Group Detection', () => {
    it('should detect horizontal connected group of same color', () => {
      // 水平に4つの同色ぷよを配置 (赤色 = 1)
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      const connectedGroups = stage.findConnectedGroups()
      
      expect(connectedGroups.length).toBe(1)
      expect(connectedGroups[0].length).toBe(4)
      expect(connectedGroups[0]).toEqual(
        expect.arrayContaining([
          { x: 0, y: 11, color: 1 },
          { x: 1, y: 11, color: 1 },
          { x: 2, y: 11, color: 1 },
          { x: 3, y: 11, color: 1 }
        ])
      )
    })

    it('should detect vertical connected group of same color', () => {
      // 垂直に4つの同色ぷよを配置 (青色 = 3)
      stage.setCell(2, 8, 3)
      stage.setCell(2, 9, 3)
      stage.setCell(2, 10, 3)
      stage.setCell(2, 11, 3)

      const connectedGroups = stage.findConnectedGroups()
      
      expect(connectedGroups.length).toBe(1)
      expect(connectedGroups[0].length).toBe(4)
      expect(connectedGroups[0]).toEqual(
        expect.arrayContaining([
          { x: 2, y: 8, color: 3 },
          { x: 2, y: 9, color: 3 },
          { x: 2, y: 10, color: 3 },
          { x: 2, y: 11, color: 3 }
        ])
      )
    })

    it('should detect L-shaped connected group', () => {
      // L字型に5つの同色ぷよを配置
      stage.setCell(1, 10, 2)
      stage.setCell(1, 11, 2)
      stage.setCell(2, 11, 2)
      stage.setCell(3, 11, 2)
      stage.setCell(4, 11, 2)

      const connectedGroups = stage.findConnectedGroups()
      
      expect(connectedGroups.length).toBe(1)
      expect(connectedGroups[0].length).toBe(5)
    })

    it('should not connect different colored puyo', () => {
      // 異なる色のぷよは連結しない
      stage.setCell(0, 11, 1) // 赤
      stage.setCell(1, 11, 2) // 緑
      stage.setCell(2, 11, 1) // 赤
      stage.setCell(3, 11, 1) // 赤

      const connectedGroups = stage.findConnectedGroups()
      
      // 孤立した赤1つ、緑1つ、連結した赤2つで3つのグループ
      expect(connectedGroups.length).toBe(3)
      
      // 2つの赤いぷよの連結グループ
      const redPairGroup = connectedGroups.find(group => group.length === 2)
      expect(redPairGroup).toBeDefined()
      expect(redPairGroup!.every(p => p.color === 1)).toBe(true)
      
      // 1つの緑ぷよグループ
      const greenGroup = connectedGroups.find(group => group.length === 1 && group[0].color === 2)
      expect(greenGroup).toBeDefined()
      
      // 孤立した赤ぷよグループ
      const redSingleGroup = connectedGroups.find(group => group.length === 1 && group[0].color === 1)
      expect(redSingleGroup).toBeDefined()
    })

    it('should ignore groups with less than 4 puyo', () => {
      // 3つの同色ぷよを配置（消去対象外）
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)

      const eliminatableGroups = stage.findEliminatableGroups()
      
      expect(eliminatableGroups.length).toBe(0)
    })
  })

  describe('Puyo Elimination', () => {
    it('should eliminate groups with 4 or more connected puyo', () => {
      // 4つの同色ぷよを配置
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)

      const eliminatedCount = stage.eliminatePuyo()
      
      expect(eliminatedCount).toBe(4)
      
      // 消去されているかチェック
      expect(stage.getCell(0, 11)).toBe(0)
      expect(stage.getCell(1, 11)).toBe(0)
      expect(stage.getCell(2, 11)).toBe(0)
      expect(stage.getCell(3, 11)).toBe(0)
    })

    it('should eliminate multiple groups simultaneously', () => {
      // 2つの独立した消去対象グループを配置
      // グループ1: 水平4つ
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)
      stage.setCell(3, 11, 1)
      
      // グループ2: 垂直4つ
      stage.setCell(5, 8, 2)
      stage.setCell(5, 9, 2)
      stage.setCell(5, 10, 2)
      stage.setCell(5, 11, 2)

      const eliminatedCount = stage.eliminatePuyo()
      
      expect(eliminatedCount).toBe(8) // 4 + 4
    })

    it('should not eliminate groups with less than 4 puyo', () => {
      // 3つの同色ぷよを配置
      stage.setCell(0, 11, 1)
      stage.setCell(1, 11, 1)
      stage.setCell(2, 11, 1)

      const eliminatedCount = stage.eliminatePuyo()
      
      expect(eliminatedCount).toBe(0)
      
      // 消去されていないかチェック
      expect(stage.getCell(0, 11)).toBe(1)
      expect(stage.getCell(1, 11)).toBe(1)
      expect(stage.getCell(2, 11)).toBe(1)
    })
  })

  describe('Gravity After Elimination', () => {
    it('should apply gravity after elimination', () => {
      // 上にあるぷよが消去後に落下する設定
      stage.setCell(1, 9, 2)  // 落下するぷよ
      stage.setCell(1, 10, 1) // 消去されるぷよの一部
      stage.setCell(1, 11, 1) // 消去されるぷよの一部
      stage.setCell(2, 10, 1) // 消去されるぷよの一部
      stage.setCell(2, 11, 1) // 消去されるぷよの一部

      stage.eliminatePuyo()
      stage.applyGravity()
      
      // 上にあったぷよが落下している
      expect(stage.getCell(1, 11)).toBe(2)
      expect(stage.getCell(1, 9)).toBe(0)
    })

    it('should handle multiple columns falling independently', () => {
      // 複数の列で独立した落下
      stage.setCell(0, 8, 3)  // 落下するぷよ
      stage.setCell(0, 10, 1) // 消去対象
      stage.setCell(0, 11, 1) // 消去対象
      stage.setCell(1, 10, 1) // 消去対象
      stage.setCell(1, 11, 1) // 消去対象
      
      stage.setCell(2, 9, 4)  // 別の列の落下ぷよ
      stage.setCell(2, 11, 2) // 残るぷよ

      stage.eliminatePuyo()
      stage.applyGravity()
      
      // 各列で独立して落下
      expect(stage.getCell(0, 11)).toBe(3) // 列0のぷよが落下
      expect(stage.getCell(2, 10)).toBe(4) // 列2のぷよが落下
      expect(stage.getCell(2, 11)).toBe(2) // 列2の下のぷよは残る
    })
  })

  describe('Score System', () => {
    it('should calculate basic score for eliminated puyo', () => {
      game.start()
      const gameStage = game.getStage()
      
      // 4つの同色ぷよを配置
      gameStage.setCell(0, 11, 1)
      gameStage.setCell(1, 11, 1)
      gameStage.setCell(2, 11, 1)
      gameStage.setCell(3, 11, 1)

      const initialScore = game.getScore()
      
      // 消去可能なグループがあることを確認
      const eliminatableGroups = gameStage.findEliminatableGroups()
      expect(eliminatableGroups.length).toBeGreaterThan(0)
      
      // 消去処理を実行
      game.processElimination()
      
      const newScore = game.getScore()
      
      expect(newScore).toBeGreaterThan(initialScore)
    })

    it('should give higher score for larger groups', () => {
      game.start()
      let gameStage = game.getStage()
      
      // テスト1: 4つのグループ
      gameStage.setCell(0, 11, 1)
      gameStage.setCell(1, 11, 1)
      gameStage.setCell(2, 11, 1)
      gameStage.setCell(3, 11, 1)
      
      // 消去可能なグループがあることを確認
      const eliminatableGroups4 = gameStage.findEliminatableGroups()
      expect(eliminatableGroups4.length).toBeGreaterThan(0)
      
      game.processElimination()
      const scoreFor4 = game.getScore()
      
      // リセット
      game.start()
      gameStage = game.getStage()
      
      // テスト2: 5つのグループ
      gameStage.setCell(0, 10, 2)
      gameStage.setCell(0, 11, 2)
      gameStage.setCell(1, 11, 2)
      gameStage.setCell(2, 11, 2)
      gameStage.setCell(3, 11, 2)
      
      // 消去可能なグループがあることを確認
      const eliminatableGroups5 = gameStage.findEliminatableGroups()
      expect(eliminatableGroups5.length).toBeGreaterThan(0)
      
      game.processElimination()
      const scoreFor5 = game.getScore()
      
      expect(scoreFor5).toBeGreaterThan(scoreFor4)
    })
  })

  describe('Game Integration', () => {
    it('should process elimination when puyo lands', () => {
      game.start()
      const gameStage = game.getStage()
      
      // ステージ下部に消去可能な状態を作る（3つ配置済み）
      gameStage.setCell(1, 11, 1)
      gameStage.setCell(2, 11, 1)
      gameStage.setCell(3, 11, 1)
      
      // 落下中のぷよを同じ色にして4つ目を作る
      const currentPuyo = game.getCurrentPuyo()
      if (currentPuyo) {
        currentPuyo.main.color = 1 // 同じ色に設定
        currentPuyo.main.x = 0
        currentPuyo.main.y = 11
        currentPuyo.sub.color = 1 // サブも同じ色
        currentPuyo.sub.x = 4
        currentPuyo.sub.y = 11
      }
      
      // ぷよを固定して消去処理をトリガー
      game.fixCurrentPuyo()
      
      // 消去されているはず
      expect(gameStage.getCell(1, 11)).toBe(0)
      expect(gameStage.getCell(2, 11)).toBe(0)
      expect(gameStage.getCell(3, 11)).toBe(0)
    })
  })
})