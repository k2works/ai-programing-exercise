import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Game } from './game'
import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'
import { Player } from './player'
import { Score } from './score'

// HTMLCanvasElementのモック
class MockCanvas {
  width = 40
  height = 40
  style = { position: 'absolute', left: '0px', top: '0px' }
  
  getContext() {
    return {
      fillStyle: '',
      beginPath: vi.fn(),
      arc: vi.fn(),
      fill: vi.fn(),
      fillRect: vi.fn(),
      fillText: vi.fn(),
      font: '',
      textAlign: '',
      textBaseline: ''
    }
  }
  
  cloneNode() {
    return new MockCanvas()
  }
}

// globalのモック設定
Object.defineProperty(global, 'HTMLCanvasElement', {
  value: MockCanvas,
  writable: true
})

describe('ゲーム', () => {
  let game: Game

  beforeEach(() => {
    // DOMの準備
    document.body.innerHTML = `
            <div id="stage"></div>
            <div id="score"></div>
            <div id="next"></div>
            <div id="next2"></div>
            <canvas id="puyo1" width="40" height="40"></canvas>
            <canvas id="puyo2" width="40" height="40"></canvas>
            <canvas id="puyo3" width="40" height="40"></canvas>
            <canvas id="puyo4" width="40" height="40"></canvas>
            <canvas id="puyo5" width="40" height="40"></canvas>
            <canvas id="batankyu" width="240" height="480"></canvas>
            <canvas id="font0" width="24" height="33"></canvas>
            <canvas id="font1" width="24" height="33"></canvas>
            <canvas id="font2" width="24" height="33"></canvas>
            <canvas id="font3" width="24" height="33"></canvas>
            <canvas id="font4" width="24" height="33"></canvas>
            <canvas id="font5" width="24" height="33"></canvas>
            <canvas id="font6" width="24" height="33"></canvas>
            <canvas id="font7" width="24" height="33"></canvas>
            <canvas id="font8" width="24" height="33"></canvas>
            <canvas id="font9" width="24" height="33"></canvas>
        `
    
    // window.innerHeightとinnerWidthを設定
    Object.defineProperty(window, 'innerHeight', {
      writable: true,
      configurable: true,
      value: 800
    })
    Object.defineProperty(window, 'innerWidth', {
      writable: true,
      configurable: true,
      value: 600
    })

    game = new Game()
  })

  describe('ゲームの初期化', () => {
    it('ゲームを初期化すると、必要なコンポーネントが作成される', () => {
      game.initialize()

      expect(game['config']).toBeInstanceOf(Config)
      expect(game['puyoImage']).toBeInstanceOf(PuyoImage)
      expect(game['stage']).toBeInstanceOf(Stage)
      expect(game['player']).toBeInstanceOf(Player)
      expect(game['score']).toBeInstanceOf(Score)
    })

    it('ゲームを初期化すると、ゲームモードがplayingになる', () => {
      game.initialize()

      expect(game['mode']).toEqual('playing')
    })
  })

  describe('ゲームループ', () => {
    it('ゲームループを開始すると、requestAnimationFrameが呼ばれる', () => {
      // requestAnimationFrameのモック
      const originalRequestAnimationFrame = window.requestAnimationFrame
      const mockRequestAnimationFrame = vi.fn()
      window.requestAnimationFrame = mockRequestAnimationFrame

      try {
        game.loop()

        expect(mockRequestAnimationFrame).toHaveBeenCalledTimes(1)
        expect(mockRequestAnimationFrame).toHaveBeenCalledWith(expect.any(Function))
      } finally {
        // モックを元に戻す
        window.requestAnimationFrame = originalRequestAnimationFrame
      }
    })
  })

	describe('イテレーション6: ゲームオーバー', () => {
		it('ステージ上部にぷよがあるとゲームオーバーモードになる', () => {
			game.initialize()

			// ステージ上部にぷよを配置
			if (game['stage']) {
				game['stage'].board[0][2] = 1
			}

			// 新しいぷよを作成しようとするとゲームオーバーになる
			game['mode'] = 'newPuyo'

			// requestAnimationFrameのモック
			const originalRequestAnimationFrame = window.requestAnimationFrame
			const mockRequestAnimationFrame = vi.fn()
			window.requestAnimationFrame = mockRequestAnimationFrame

			try {
				game.loop()

				expect(game['mode']).toBe('gameOver')
			} finally {
				window.requestAnimationFrame = originalRequestAnimationFrame
			}
		})

		it('ゲームオーバーモードでばたんきゅー画像が表示される', () => {
			game.initialize()
			game['mode'] = 'gameOver'

			// requestAnimationFrameのモック
			const originalRequestAnimationFrame = window.requestAnimationFrame
			const mockRequestAnimationFrame = vi.fn()
			window.requestAnimationFrame = mockRequestAnimationFrame

			try {
				// ゲームオーバー処理を実行
				game.loop()

				// ばたんきゅー画像がステージに追加されることを確認
				if (game['puyoImage']) {
					const batankyuElement = game['puyoImage'].batankyuImage
					expect(batankyuElement).toBeDefined()

					// ゲームオーバーフレームが増加することを確認
					expect(game['puyoImage']['gameOverFrame']).toBeGreaterThan(0)
				}
			} finally {
				window.requestAnimationFrame = originalRequestAnimationFrame
			}
		})

		it('ゲームオーバー後120フレーム経過でゲーム終了になる', () => {
			game.initialize()
			game['mode'] = 'gameOver'

			// requestAnimationFrameのモック
			const originalRequestAnimationFrame = window.requestAnimationFrame
			const mockRequestAnimationFrame = vi.fn()
			window.requestAnimationFrame = mockRequestAnimationFrame

			// console.logのモック
			const originalConsoleLog = console.log
			const mockConsoleLog = vi.fn()
			console.log = mockConsoleLog

			try {
				// ゲームオーバーフレームを120を超えるまで実行
				for (let i = 0; i < 121; i++) {
					game.loop()
				}

				// "Game Over!"がコンソールに出力されることを確認
				expect(mockConsoleLog).toHaveBeenCalledWith('Game Over!')
			} finally {
				window.requestAnimationFrame = originalRequestAnimationFrame
				console.log = originalConsoleLog
			}
		})
	})
})
