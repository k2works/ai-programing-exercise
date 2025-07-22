import { describe, it, expect, beforeEach } from 'vitest'
import { Game } from '../src/Game'

describe('Game', () => {
  let game: Game
  
  beforeEach(() => {
    // モックCanvasContext2Dを作成
    const mockContext = {
      fillStyle: '',
      fillRect: () => {},
      clearRect: () => {},
      strokeRect: () => {},
      strokeStyle: '',
      font: '',
      fillText: () => {},
    }
    
    // モックCanvasElementを作成
    const mockCanvas = {
      getContext: () => mockContext,
      width: 400,
      height: 600,
    }
    
    game = new Game(mockCanvas as any)
  })

  it('should create a game instance', () => {
    expect(game).toBeDefined()
    expect(game).toBeInstanceOf(Game)
  })

  it('should have a start method', () => {
    expect(typeof game.start).toBe('function')
  })

  it('should start the game without errors', () => {
    expect(() => game.start()).not.toThrow()
  })

  it('should initialize game state on start', () => {
    game.start()
    expect(game.isRunning()).toBe(true)
  })

  it('should have stage after initialization', () => {
    game.start()
    expect(game.getStage()).toBeDefined()
  })

  it('should have current puyo after initialization', () => {
    game.start()
    expect(game.getCurrentPuyo()).toBeDefined()
  })
})