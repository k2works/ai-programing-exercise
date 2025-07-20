import { describe, it, expect, beforeEach } from 'vitest'
import { Player } from './player'
import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'

describe('プレイヤー', () => {
  let player: Player
  let config: Config
  let stage: Stage
  let puyoImage: PuyoImage

  beforeEach(() => {
    config = new Config()
    puyoImage = new PuyoImage(config)
    stage = new Stage(config, puyoImage)
    stage.initialize()
    player = new Player(config, stage, puyoImage)
  })

  describe('プレイヤーの初期化', () => {
    it('プレイヤーを初期化できる', () => {
      player.initialize()

      expect(player.getCurrentPair()).toBeDefined()
      expect(player.getNextPair()).toBeDefined()
    })

    it('初期化時に現在のぷよペアがステージ上部中央に配置される', () => {
      player.initialize()

      const currentPair = player.getCurrentPair()
      expect(currentPair.getX()).toBe(Math.floor(config.stageWidth / 2))
      expect(currentPair.getY()).toBe(1)
    })
  })

  describe('ぷよペアの横移動', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('左に移動できる', () => {
      const initialX = player.getCurrentPair().getX()
      
      const moved = player.moveLeft()
      
      expect(moved).toBe(true)
      expect(player.getCurrentPair().getX()).toBe(initialX - 1)
    })

    it('右に移動できる', () => {
      const initialX = player.getCurrentPair().getX()
      
      const moved = player.moveRight()
      
      expect(moved).toBe(true)
      expect(player.getCurrentPair().getX()).toBe(initialX + 1)
    })

    it('左端では左に移動できない', () => {
      const currentPair = player.getCurrentPair()
      currentPair.setPosition(0, 1)
      
      const moved = player.moveLeft()
      
      expect(moved).toBe(false)
      expect(currentPair.getX()).toBe(0)
    })

    it('右端では右に移動できない', () => {
      const currentPair = player.getCurrentPair()
      currentPair.setPosition(config.stageWidth - 1, 1)
      
      const moved = player.moveRight()
      
      expect(moved).toBe(false)
      expect(currentPair.getX()).toBe(config.stageWidth - 1)
    })
  })

  describe('ぷよペアの回転', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('右回転できる', () => {
      const initialRotation = player.getCurrentPair().getRotation()
      
      const rotated = player.rotateRight()
      
      expect(rotated).toBe(true)
      expect(player.getCurrentPair().getRotation()).toBe((initialRotation + 1) % 4)
    })

    it('左回転できる', () => {
      const initialRotation = player.getCurrentPair().getRotation()
      
      const rotated = player.rotateLeft()
      
      expect(rotated).toBe(true)
      expect(player.getCurrentPair().getRotation()).toBe((initialRotation + 3) % 4)
    })
  })

  describe('ぷよペアの自動落下', () => {
    beforeEach(() => {
      player.initialize()
    })

    it('一定時間経過すると下に移動する', () => {
      const initialY = player.getCurrentPair().getY()
      
      // fallSpeedの時間経過をシミュレート
      for (let i = 0; i < config.fallSpeed; i++) {
        player.update()
      }
      
      expect(player.getCurrentPair().getY()).toBe(initialY + 1)
    })

    it('下に移動できない場合は配置される', () => {
      const currentPair = player.getCurrentPair()
      currentPair.setPosition(2, config.stageHeight - 1)
      
      // fallSpeedの時間経過をシミュレート
      for (let i = 0; i < config.fallSpeed; i++) {
        player.update()
      }
      
      expect(player.isPlaced()).toBe(true)
    })
  })
})