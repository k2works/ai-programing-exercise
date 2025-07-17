import { describe, it, expect, beforeEach, vi } from 'vitest'
import { Stage } from './stage'
import { Config } from './config'
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

describe('ステージ', () => {
  let config: Config
  let puyoImage: PuyoImage
  let stage: Stage

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
  })

  describe('ぷよの消去システム', () => {
    it('同じ色のぷよが4つ縦につながっていると消去対象になる', () => {
      // ステージに赤ぷよを縦に4つ配置
      stage.board[8][2] = 1  // 赤ぷよ
      stage.board[9][2] = 1  // 赤ぷよ
      stage.board[10][2] = 1 // 赤ぷよ
      stage.board[11][2] = 1 // 赤ぷよ

      const result = stage.checkErase()

      expect(result.eraseCount).toBe(4)
      expect(result.colorCount).toBe(1)
    })

    it('同じ色のぷよが4つ横につながっていると消去対象になる', () => {
      // ステージに青ぷよを横に4つ配置
      stage.board[11][1] = 2  // 青ぷよ
      stage.board[11][2] = 2  // 青ぷよ
      stage.board[11][3] = 2  // 青ぷよ
      stage.board[11][4] = 2  // 青ぷよ

      const result = stage.checkErase()

      expect(result.eraseCount).toBe(4)
      expect(result.colorCount).toBe(1)
    })

    it('同じ色のぷよがL字型につながっていると消去対象になる', () => {
      // ステージに黄ぷよをL字型に配置
      stage.board[9][2] = 3   // 黄ぷよ
      stage.board[10][2] = 3  // 黄ぷよ
      stage.board[11][2] = 3  // 黄ぷよ
      stage.board[11][3] = 3  // 黄ぷよ

      const result = stage.checkErase()

      expect(result.eraseCount).toBe(4)
      expect(result.colorCount).toBe(1)
    })

    it('同じ色のぷよが3つまでは消去対象にならない', () => {
      // ステージに赤ぷよを縦に3つ配置
      stage.board[9][2] = 1   // 赤ぷよ
      stage.board[10][2] = 1  // 赤ぷよ
      stage.board[11][2] = 1  // 赤ぷよ

      const result = stage.checkErase()

      expect(result.eraseCount).toBe(0)
      expect(result.colorCount).toBe(0)
    })

    it('異なる色のぷよは消去対象にならない', () => {
      // ステージに異なる色のぷよを配置
      stage.board[8][2] = 1   // 赤ぷよ
      stage.board[9][2] = 2   // 青ぷよ
      stage.board[10][2] = 3  // 黄ぷよ
      stage.board[11][2] = 4  // 緑ぷよ

      const result = stage.checkErase()

      expect(result.eraseCount).toBe(0)
      expect(result.colorCount).toBe(0)
    })

    it('複数の色で同時に消去が発生する場合、正しく計算される', () => {
      // 赤ぷよを縦に4つ
      stage.board[8][1] = 1   // 赤ぷよ
      stage.board[9][1] = 1   // 赤ぷよ
      stage.board[10][1] = 1  // 赤ぷよ
      stage.board[11][1] = 1  // 赤ぷよ

      // 青ぷよを横に4つ
      stage.board[10][2] = 2  // 青ぷよ
      stage.board[10][3] = 2  // 青ぷよ
      stage.board[10][4] = 2  // 青ぷよ
      stage.board[10][5] = 2  // 青ぷよ

      const result = stage.checkErase()

      expect(result.eraseCount).toBe(8)
      expect(result.colorCount).toBe(2)
    })
  })

  describe('消去実行', () => {
    it('消去対象のぷよが実際に消去される', () => {
      // ステージに赤ぷよを縦に4つ配置
      stage.board[8][2] = 1   // 赤ぷよ
      stage.board[9][2] = 1   // 赤ぷよ
      stage.board[10][2] = 1  // 赤ぷよ
      stage.board[11][2] = 1  // 赤ぷよ

      stage.checkErase()
      stage.eraseBoards()

      // 消去対象のぷよが消えていることを確認
      expect(stage.board[8][2]).toBe(0)
      expect(stage.board[9][2]).toBe(0)
      expect(stage.board[10][2]).toBe(0)
      expect(stage.board[11][2]).toBe(0)
    })
  })
})
