import { describe, it, expect } from 'vitest'
import { Game } from '../src/main'

describe('Game', () => {
  it('should create a game instance', () => {
    const game = new Game()
    expect(game).toBeDefined()
    expect(game).toBeInstanceOf(Game)
  })

  it('should have a start method', () => {
    const game = new Game()
    expect(typeof game.start).toBe('function')
  })

  it('should start the game without errors', () => {
    const game = new Game()
    expect(() => game.start()).not.toThrow()
  })
})