import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../src/Game'

describe('Game - Movement and Gravity', () => {
  let game: Game
  let mockContext: any
  let mockCanvas: any

  beforeEach(() => {
    // モックCanvasContext2Dを作成
    mockContext = {
      fillStyle: '',
      fillRect: vi.fn(),
      clearRect: vi.fn(),
      strokeRect: vi.fn(),
      strokeStyle: '',
      font: '',
      fillText: vi.fn(),
    }
    
    // モックCanvasElementを作成
    mockCanvas = {
      getContext: () => mockContext,
      width: 400,
      height: 600,
    }
    
    game = new Game(mockCanvas)
  })

  it('should update puyo position on game update', () => {
    game.start()
    const initialPuyo = game.getCurrentPuyo()
    const initialY = initialPuyo?.main.y || 0

    game.update()
    
    const updatedPuyo = game.getCurrentPuyo()
    const newY = updatedPuyo?.main.y || 0
    
    expect(newY).toBeGreaterThan(initialY) // ぷよが落下している
  })

  it('should handle keyboard input for left movement', () => {
    game.start()
    const initialPuyo = game.getCurrentPuyo()
    const initialX = initialPuyo?.main.x || 0

    game.handleInput('ArrowLeft')
    
    const updatedPuyo = game.getCurrentPuyo()
    const newX = updatedPuyo?.main.x || 0
    
    expect(newX).toBe(initialX - 1) // 左に移動
  })

  it('should handle keyboard input for right movement', () => {
    game.start()
    const initialPuyo = game.getCurrentPuyo()
    const initialX = initialPuyo?.main.x || 0

    game.handleInput('ArrowRight')
    
    const updatedPuyo = game.getCurrentPuyo()
    const newX = updatedPuyo?.main.x || 0
    
    expect(newX).toBe(initialX + 1) // 右に移動
  })

  it('should have game loop methods', () => {
    expect(typeof game.update).toBe('function')
    expect(typeof game.handleInput).toBe('function')
  })
})