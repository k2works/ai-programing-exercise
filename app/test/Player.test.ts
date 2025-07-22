import { describe, it, expect, beforeEach } from 'vitest'
import { Player } from '../src/Player'
import { PuyoPair } from '../src/Puyo'
import { Stage } from '../src/Stage'

describe('Player', () => {
  let player: Player
  let stage: Stage
  let puyo: PuyoPair

  beforeEach(() => {
    stage = new Stage()
    puyo = new PuyoPair(2, 1, 1, 2) // 中央に配置
    player = new Player(stage)
  })

  it('should create a player instance', () => {
    expect(player).toBeDefined()
    expect(player).toBeInstanceOf(Player)
  })

  it('should be able to move puyo left', () => {
    const moved = player.movePuyoLeft(puyo)
    expect(moved.main.x).toBe(puyo.main.x - 1)
    expect(moved.sub.x).toBe(puyo.sub.x - 1)
    expect(moved.main.y).toBe(puyo.main.y)
    expect(moved.sub.y).toBe(puyo.sub.y)
  })

  it('should be able to move puyo right', () => {
    const moved = player.movePuyoRight(puyo)
    expect(moved.main.x).toBe(puyo.main.x + 1)
    expect(moved.sub.x).toBe(puyo.sub.x + 1)
    expect(moved.main.y).toBe(puyo.main.y)
    expect(moved.sub.y).toBe(puyo.sub.y)
  })

  it('should not move puyo left if blocked by wall', () => {
    const puyoAtLeftWall = new PuyoPair(0, 1, 1, 2)
    const moved = player.movePuyoLeft(puyoAtLeftWall)
    expect(moved.main.x).toBe(puyoAtLeftWall.main.x) // 移動しない
    expect(moved.sub.x).toBe(puyoAtLeftWall.sub.x)
  })

  it('should not move puyo right if blocked by wall', () => {
    const puyoAtRightWall = new PuyoPair(5, 1, 1, 2) // 右端
    const moved = player.movePuyoRight(puyoAtRightWall)
    expect(moved.main.x).toBe(puyoAtRightWall.main.x) // 移動しない
    expect(moved.sub.x).toBe(puyoAtRightWall.sub.x)
  })

  it('should be able to drop puyo down', () => {
    const dropped = player.dropPuyoDown(puyo)
    expect(dropped.main.y).toBe(puyo.main.y + 1)
    expect(dropped.sub.y).toBe(puyo.sub.y + 1)
    expect(dropped.main.x).toBe(puyo.main.x)
    expect(dropped.sub.x).toBe(puyo.sub.x)
  })

  it('should validate puyo position', () => {
    expect(player.isValidPuyoPosition(puyo)).toBe(true)
    
    const invalidPuyo = new PuyoPair(-1, 1, 1, 2) // 壁外
    expect(player.isValidPuyoPosition(invalidPuyo)).toBe(false)
  })
})