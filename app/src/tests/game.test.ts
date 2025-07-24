import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from '../game'

describe('Game', () => {
  let canvas: HTMLCanvasElement
  let scoreDisplay: HTMLElement
  let game: Game
  let mockContext: CanvasRenderingContext2D

  beforeEach(() => {
    // Canvas 2Dコンテキストのモック
    mockContext = {
      fillStyle: '',
      fillRect: vi.fn(),
      clearRect: vi.fn(),
      drawImage: vi.fn(),
    } as unknown as CanvasRenderingContext2D

    // Canvas要素をモック
    canvas = document.createElement('canvas')
    canvas.width = 320
    canvas.height = 480

    // getContextメソッドをモック
    vi.spyOn(canvas, 'getContext').mockReturnValue(mockContext)

    // スコア表示要素をモック
    scoreDisplay = document.createElement('div')

    game = new Game(canvas, scoreDisplay)
  })

  describe('初期化', () => {
    it('ゲームインスタンスが作成できる', () => {
      expect(game).toBeInstanceOf(Game)
    })

    it('初期スコアは0である', () => {
      expect(game.getScore()).toBe(0)
    })

    it('初期状態では実行中ではない', () => {
      expect(game.isGameRunning()).toBe(false)
    })
  })

  describe('ゲーム開始', () => {
    it('start()を呼ぶとゲームが開始される', () => {
      game.start()
      expect(game.isGameRunning()).toBe(true)
      game.stop() // テスト後のクリーンアップ
    })

    it('start()を複数回呼んでも問題ない', () => {
      game.start()
      game.start()
      expect(game.isGameRunning()).toBe(true)
      game.stop() // テスト後のクリーンアップ
    })
  })

  describe('ゲーム停止', () => {
    it('stop()を呼ぶとゲームが停止される', () => {
      game.start()
      game.stop()
      expect(game.isGameRunning()).toBe(false)
    })
  })
})
