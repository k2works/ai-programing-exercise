import { describe, it, expect } from 'vitest'
import { Stage } from '../src/Stage'

describe('Stage', () => {
  it('should create a stage instance', () => {
    const stage = new Stage()
    expect(stage).toBeDefined()
    expect(stage).toBeInstanceOf(Stage)
  })

  it('should initialize with empty grid', () => {
    const stage = new Stage()
    expect(stage.isEmpty()).toBe(true)
  })

  it('should have dimensions', () => {
    const stage = new Stage()
    expect(stage.getWidth()).toBeGreaterThan(0)
    expect(stage.getHeight()).toBeGreaterThan(0)
  })

  it('should be able to check if position is valid', () => {
    const stage = new Stage()
    expect(stage.isValidPosition(0, 0)).toBe(true)
    expect(stage.isValidPosition(-1, 0)).toBe(false)
    expect(stage.isValidPosition(0, -1)).toBe(false)
    expect(stage.isValidPosition(stage.getWidth(), 0)).toBe(false)
    expect(stage.isValidPosition(0, stage.getHeight())).toBe(false)
  })

  it('should be able to get cell value', () => {
    const stage = new Stage()
    expect(stage.getCell(0, 0)).toBe(0) // 0 = empty
  })
})