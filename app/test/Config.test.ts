import { describe, it, expect } from 'vitest'
import { Config } from '../src/Config'

describe('Config', () => {
  it('should have stage dimensions', () => {
    expect(Config.STAGE_WIDTH).toBeDefined()
    expect(Config.STAGE_HEIGHT).toBeDefined()
    expect(typeof Config.STAGE_WIDTH).toBe('number')
    expect(typeof Config.STAGE_HEIGHT).toBe('number')
    expect(Config.STAGE_WIDTH).toBeGreaterThan(0)
    expect(Config.STAGE_HEIGHT).toBeGreaterThan(0)
  })

  it('should have puyo size configuration', () => {
    expect(Config.PUYO_SIZE).toBeDefined()
    expect(typeof Config.PUYO_SIZE).toBe('number')
    expect(Config.PUYO_SIZE).toBeGreaterThan(0)
  })

  it('should have game speed configuration', () => {
    expect(Config.GAME_SPEED).toBeDefined()
    expect(typeof Config.GAME_SPEED).toBe('number')
    expect(Config.GAME_SPEED).toBeGreaterThan(0)
  })

  it('should have color definitions', () => {
    expect(Config.COLORS).toBeDefined()
    expect(Array.isArray(Config.COLORS)).toBe(true)
    expect(Config.COLORS.length).toBeGreaterThan(0)
  })
})