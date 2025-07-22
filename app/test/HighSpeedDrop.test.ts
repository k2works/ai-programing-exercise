import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../src/Game'

describe('High Speed Drop', () => {
  let canvas: HTMLCanvasElement
  let game: Game

  beforeEach(() => {
    // テスト用のcanvas要素とcontextをモック
    canvas = document.createElement('canvas')
    canvas.width = 200
    canvas.height = 400
    
    // Canvas contextをモック
    const mockContext = {
      clearRect: vi.fn(),
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      fillStyle: '',
      strokeStyle: ''
    } as any
    
    vi.spyOn(canvas, 'getContext').mockReturnValue(mockContext)
    
    game = new Game(canvas)
    game.start()
  })

  describe('Normal Drop Speed', () => {
    it('should drop puyo at normal speed by default', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      const initialY = initialPuyo.main.y
      
      // 通常速度で数フレーム進める（60フレーム = 1回落下）
      for (let i = 0; i < 59; i++) {
        game.update()
      }
      
      // まだ落下していないはず
      const puyoAfter59Frames = game.getCurrentPuyo()
      if (!puyoAfter59Frames) throw new Error('No current puyo')
      expect(puyoAfter59Frames.main.y).toBe(initialY)
      
      // 60フレーム目で落下
      game.update()
      const puyoAfter60Frames = game.getCurrentPuyo()
      if (!puyoAfter60Frames) throw new Error('No current puyo')
      expect(puyoAfter60Frames.main.y).toBe(initialY + 1)
    })
  })

  describe('High Speed Drop', () => {
    it('should drop puyo faster when high speed drop is enabled', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      const initialY = initialPuyo.main.y
      
      // 高速落下を有効にする
      game.enableHighSpeedDrop()
      
      // 1フレームで落下するはず
      game.update()
      
      const puyoAfterUpdate = game.getCurrentPuyo()
      if (!puyoAfterUpdate) throw new Error('No current puyo')
      expect(puyoAfterUpdate.main.y).toBe(initialY + 1)
    })

    it('should return to normal speed when high speed drop is disabled', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      // 高速落下を有効にしてから無効にする
      game.enableHighSpeedDrop()
      game.disableHighSpeedDrop()
      
      const initialY = initialPuyo.main.y
      
      // 通常速度に戻っているかテスト（59フレームでは落下しない）
      for (let i = 0; i < 59; i++) {
        game.update()
      }
      
      const puyoAfter59Frames = game.getCurrentPuyo()
      if (!puyoAfter59Frames) throw new Error('No current puyo')
      expect(puyoAfter59Frames.main.y).toBe(initialY)
    })

    it('should allow other operations during high speed drop', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      const initialX = initialPuyo.main.x
      
      // 高速落下を有効にする
      game.enableHighSpeedDrop()
      
      // 左右移動を試行
      game.handleInput('ArrowLeft')
      
      const puyoAfterMove = game.getCurrentPuyo()
      if (!puyoAfterMove) throw new Error('No current puyo')
      
      // 左右移動が可能なはず
      expect(puyoAfterMove.main.x).toBe(initialX - 1)
    })

    it('should allow rotation during high speed drop', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      const initialSubPosition = {
        x: initialPuyo.sub.x,
        y: initialPuyo.sub.y
      }
      
      // 高速落下を有効にする
      game.enableHighSpeedDrop()
      
      // 回転を試行
      game.handleInput('KeyX')
      
      const puyoAfterRotation = game.getCurrentPuyo()
      if (!puyoAfterRotation) throw new Error('No current puyo')
      
      // 回転が可能なはず（サブぷよの位置が変わる）
      expect(
        puyoAfterRotation.sub.x !== initialSubPosition.x ||
        puyoAfterRotation.sub.y !== initialSubPosition.y
      ).toBe(true)
    })
  })

  describe('Keyboard Input Integration', () => {
    it('should enable high speed drop when down arrow is pressed', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      const initialY = initialPuyo.main.y
      
      // 下矢印キーを押す
      game.handleInput('ArrowDown')
      
      // 1フレームで落下するはず
      game.update()
      
      const puyoAfterUpdate = game.getCurrentPuyo()
      if (!puyoAfterUpdate) throw new Error('No current puyo')
      expect(puyoAfterUpdate.main.y).toBe(initialY + 1)
    })

    it('should enable high speed drop when S key is pressed', () => {
      const initialPuyo = game.getCurrentPuyo()
      if (!initialPuyo) throw new Error('No current puyo')
      
      const initialY = initialPuyo.main.y
      
      // Sキーを押す
      game.handleInput('KeyS')
      
      // 1フレームで落下するはず
      game.update()
      
      const puyoAfterUpdate = game.getCurrentPuyo()
      if (!puyoAfterUpdate) throw new Error('No current puyo')
      expect(puyoAfterUpdate.main.y).toBe(initialY + 1)
    })
  })
})