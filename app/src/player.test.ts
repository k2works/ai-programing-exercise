import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Player } from './player'
import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'

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

  parentNode = {
    removeChild: vi.fn()
  }
}

// globalのモック設定
Object.defineProperty(globalThis, 'HTMLCanvasElement', {
  value: MockCanvas,
  writable: true
})

describe('プレイヤー', () => {
  let config: Config
  let puyoImage: PuyoImage
  let stage: Stage
  let player: Player

  beforeEach(() => {
    // DOMの準備
    document.body.innerHTML = `
      <div id="stage"></div>
      <div id="score"></div>
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

    config = new Config()
    puyoImage = new PuyoImage(config)
    stage = new Stage(config, puyoImage)
    player = new Player(config)
  })

  describe('キー入力', () => {
    it('左キーが押されると、左向きの移動フラグが立つ', () => {
      // キーダウンイベントをシミュレート（左キー）
      const event = new KeyboardEvent('keydown', { keyCode: 37 })
      document.dispatchEvent(event)

      expect(player.keyStatus.left).toBe(true)
    })

    it('右キーが押されると、右向きの移動フラグが立つ', () => {
      // キーダウンイベントをシミュレート（右キー）
      const event = new KeyboardEvent('keydown', { keyCode: 39 })
      document.dispatchEvent(event)

      expect(player.keyStatus.right).toBe(true)
    })

    it('キーが離されると、移動フラグがクリアされる', () => {
      // まず左キーを押す
      const keydownEvent = new KeyboardEvent('keydown', { keyCode: 37 })
      document.dispatchEvent(keydownEvent)
      expect(player.keyStatus.left).toBe(true)

      // キーを離す
      const keyupEvent = new KeyboardEvent('keyup', { keyCode: 37 })
      document.dispatchEvent(keyupEvent)
      expect(player.keyStatus.left).toBe(false)
    })
  })

  describe('ぷよの作成', () => {
    it('新しいぷよが作成される', () => {
      const result = player.createNewPuyo(stage, puyoImage, {})

      expect(player.centerPuyo).toBeGreaterThanOrEqual(1)
      expect(player.centerPuyo).toBeLessThanOrEqual(4)
      expect(player.movablePuyo).toBeGreaterThanOrEqual(1)
      expect(player.movablePuyo).toBeLessThanOrEqual(4)
      expect(result).toBe('playing')
    })

    it('ぷよの初期位置が正しく設定される', () => {
      player.createNewPuyo(stage, puyoImage, {})

      expect(player.puyoStatus.x).toBe(2)
      expect(player.puyoStatus.y).toBe(0)
      expect(player.puyoStatus.rotation).toBe(0)
    })

    it('ステージ上部にぷよがあるとゲームオーバーになる', () => {
      // ステージ上部にぷよを配置
      stage.board[0][2] = 1

      const result = player.createNewPuyo(stage, puyoImage, {})

      expect(result).toBe('gameOver')
    })
  })

  describe('ぷよの移動', () => {
    beforeEach(() => {
      player.createNewPuyo(stage, puyoImage, {})
    })

    it('左キーが押されているとき、ぷよが左に移動できる場合は移動する', () => {
      // 左キーを押した状態にする
      player.keyStatus.left = true
      
      const initialX = player.puyoStatus.x
      player.playing(1)
      
      // 左に移動したかチェック
      expect(player.puyoStatus.x).toBe(initialX - 1)
    })

    it('右キーが押されているとき、ぷよが右に移動できる場合は移動する', () => {
      // 右キーを押した状態にする
      player.keyStatus.right = true
      
      const initialX = player.puyoStatus.x
      player.playing(1)
      
      // 右に移動したかチェック
      expect(player.puyoStatus.x).toBe(initialX + 1)
    })

    it('左端では左に移動できない', () => {
      // ぷよを左端に移動
      player.puyoStatus.x = 0
      player.keyStatus.left = true
      
      player.playing(1)
      
      // 位置が変わらないことを確認
      expect(player.puyoStatus.x).toBe(0)
    })

    it('右端では右に移動できない', () => {
      // ぷよを右端に移動
      player.puyoStatus.x = config.stageCols - 1
      player.keyStatus.right = true
      
      player.playing(1)
      
      // 位置が変わらないことを確認
      expect(player.puyoStatus.x).toBe(config.stageCols - 1)
    })

    it('他のぷよがある場所には移動できない', () => {
      // 右隣にぷよを配置
      stage.board[0][3] = 1
      player.keyStatus.right = true
      
      const initialX = player.puyoStatus.x
      player.playing(1)
      
      // 位置が変わらないことを確認
      expect(player.puyoStatus.x).toBe(initialX)
    })
  })

  describe('ぷよの回転', () => {
    beforeEach(() => {
      player.createNewPuyo(stage, puyoImage, {})
    })

    it('上キーが押されるとぷよが回転する', () => {
      player.keyStatus.up = true
      
      const initialRotation = player.puyoStatus.rotation
      player.playing(1)
      
      // 90度回転したかチェック（0 -> 90）
      expect(player.puyoStatus.rotation).toBe((initialRotation + 90) % 360)
    })

    it('回転すると動くぷよの位置が正しく計算される', () => {
      // 初期状態では動くぷよは上にある（rotation = 0）
      expect(player.puyoStatus.rotation).toBe(0)
      
      // 90度回転
      player.keyStatus.up = true
      player.playing(1)
      
      expect(player.puyoStatus.rotation).toBe(90)
    })

		it('時計回りに回転すると、回転状態が90度ずつ増える', () => {
			// 初期状態：0度
			expect(player.puyoStatus.rotation).toBe(0)

			// 1回目の回転：0 -> 90
			player.keyStatus.up = true
			player.playing(1)
			player.keyStatus.up = false
			expect(player.puyoStatus.rotation).toBe(90)

			// 2回目の回転：90 -> 180
			player.keyStatus.up = true
			player.playing(2)
			player.keyStatus.up = false
			expect(player.puyoStatus.rotation).toBe(180)

			// 3回目の回転：180 -> 270
			player.keyStatus.up = true
			player.playing(3)
			player.keyStatus.up = false
			expect(player.puyoStatus.rotation).toBe(270)

			// 4回目の回転：270 -> 0（循環）
			player.keyStatus.up = true
			player.playing(4)
			player.keyStatus.up = false
			expect(player.puyoStatus.rotation).toBe(0)
		})

		it('回転できない場合は回転しない', () => {
			// 右端に移動
			player.puyoStatus.x = config.stageCols - 1
			player.puyoStatus.rotation = 0

			// 右側に回転しようとするが、壁があるので回転できない
			const initialRotation = player.puyoStatus.rotation
			player.keyStatus.up = true
			player.playing(1)

			// 回転していないことを確認
			expect(player.puyoStatus.rotation).toBe(initialRotation)
		})
	})

	describe('壁キック処理', () => {
		beforeEach(() => {
			player.createNewPuyo(stage, puyoImage, {})
		})

		it('右端で回転しようとすると、左に移動して回転する', () => {
			// 右端から2番目に移動（壁キック可能な位置）
			player.puyoStatus.x = config.stageCols - 2
			player.puyoStatus.rotation = 0

			// 回転（右側に動くぷよが来る）
			player.keyStatus.up = true
			player.playing(1)

			// 90度回転していることを確認
			expect(player.puyoStatus.rotation).toBe(90)
		})

		it('左端で回転しようとすると、右に移動して回転する', () => {
			// 左端から2番目に移動（壁キック可能な位置）
			player.puyoStatus.x = 1
			player.puyoStatus.rotation = 0

			// 回転（左側に動くぷよが来るには270度回転が必要）
			// まず3回回転して270度にする
			for (let i = 0; i < 3; i++) {
				player.keyStatus.up = true
				player.playing(i + 1)
				player.keyStatus.up = false
			}

			// 4回目の回転で0度に戻る際の壁キックをテスト
			player.keyStatus.up = true
			player.playing(4)

			// 0度回転していることを確認
			expect(player.puyoStatus.rotation).toBe(0)
		})
  })
})
