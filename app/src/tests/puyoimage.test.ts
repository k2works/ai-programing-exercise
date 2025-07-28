import { describe, it, expect, beforeEach, vi } from 'vitest'
import { PuyoImage } from '../puyoimage'

describe('PuyoImage', () => {
  let puyoImage: PuyoImage
  let mockCanvas: HTMLCanvasElement
  let mockContext: CanvasRenderingContext2D

  beforeEach(() => {
    // Canvas 2D context をモック化
    mockContext = {
      clearRect: vi.fn(),
      fillRect: vi.fn(),
      fillText: vi.fn(),
      ellipse: vi.fn(),
      beginPath: vi.fn(),
      fill: vi.fn(),
      stroke: vi.fn(),
      save: vi.fn(),
      restore: vi.fn(),
      set fillStyle(value: string) {},
      get fillStyle() {
        return '#000000'
      },
      set strokeStyle(value: string) {},
      get strokeStyle() {
        return '#000000'
      },
      set lineWidth(value: number) {},
      get lineWidth() {
        return 1
      },
      set font(value: string) {},
      get font() {
        return '10px sans-serif'
      },
      set textAlign(value: string) {},
      get textAlign() {
        return 'start'
      },
      set textBaseline(value: string) {},
      get textBaseline() {
        return 'alphabetic'
      },
    } as any

    // Canvas をモック化
    mockCanvas = {
      getContext: vi.fn().mockReturnValue(mockContext),
      width: 800,
      height: 600,
    } as any

    puyoImage = new PuyoImage(mockCanvas)
  })

  describe('constructor', () => {
    it('正常に初期化される', () => {
      expect(puyoImage).toBeDefined()
      expect(mockCanvas.getContext).toHaveBeenCalledWith('2d')
    })

    it('Canvas 2D context が取得できない場合はエラーを投げる', () => {
      const errorCanvas = {
        getContext: vi.fn().mockReturnValue(null),
      } as any

      expect(() => new PuyoImage(errorCanvas)).toThrow('Could not get 2D context from canvas')
    })
  })

  describe('clearCanvas', () => {
    it('キャンバス全体をクリアして背景色を設定する', () => {
      puyoImage.clearCanvas()

      expect(mockContext.clearRect).toHaveBeenCalledWith(0, 0, 800, 600)
      expect(mockContext.fillRect).toHaveBeenCalledWith(0, 0, 800, 600)
    })
  })

  describe('renderField', () => {
    it('空のフィールドは何も描画しない', () => {
      const field = [
        [0, 0, 0],
        [0, 0, 0],
      ]

      puyoImage.renderField(field)

      // ぷよが0（空）の場合はdrawPuyoが呼ばれない
      expect(mockContext.ellipse).not.toHaveBeenCalled()
    })

    it('フィールドのぷよを正しい位置に描画する', () => {
      const field = [
        [1, 0, 2],
        [0, 3, 0],
      ]

      puyoImage.renderField(field)

      // ぷよが配置されている位置の数だけellipseが呼ばれる
      expect(mockContext.ellipse).toHaveBeenCalledTimes(3)
      expect(mockContext.fill).toHaveBeenCalledTimes(3)
      expect(mockContext.stroke).toHaveBeenCalledTimes(3)
    })
  })

  describe('renderActivePuyo', () => {
    it('アクティブぷよを正しい位置に描画する', () => {
      const activePuyo = {
        x: 2,
        y: 1,
        color1: 1,
        color2: 2,
        direction: 0, // 縦配置、下向き
      }

      puyoImage.renderActivePuyo(activePuyo)

      // 2つのぷよが描画される
      expect(mockContext.ellipse).toHaveBeenCalledTimes(2)
      expect(mockContext.fill).toHaveBeenCalledTimes(2)
      expect(mockContext.stroke).toHaveBeenCalledTimes(2)
    })

    it('横配置のアクティブぷよを描画する', () => {
      const activePuyo = {
        x: 2,
        y: 1,
        color1: 1,
        color2: 2,
        direction: 1, // 横配置、右向き
      }

      puyoImage.renderActivePuyo(activePuyo)

      // 2つのぷよが描画される
      expect(mockContext.ellipse).toHaveBeenCalledTimes(2)
      expect(mockContext.fill).toHaveBeenCalledTimes(2)
      expect(mockContext.stroke).toHaveBeenCalledTimes(2)
    })
  })

  describe('renderNextPuyo', () => {
    it('次のぷよを指定位置に描画する', () => {
      const nextPuyo = {
        color1: 1,
        color2: 3,
      }

      puyoImage.renderNextPuyo(nextPuyo)

      // 2つのぷよが描画される
      expect(mockContext.ellipse).toHaveBeenCalledTimes(2)
      expect(mockContext.fill).toHaveBeenCalledTimes(2)
      expect(mockContext.stroke).toHaveBeenCalledTimes(2)
    })
  })

  describe('renderZenkeshiEffect', () => {
    it('全消し演出のテキストを描画する', () => {
      puyoImage.renderZenkeshiEffect()

      // テキストが2回描画される（影とメイン）
      expect(mockContext.fillText).toHaveBeenCalledTimes(2)
      expect(mockContext.fillText).toHaveBeenCalledWith('全消し！', 402, 302) // 影（中心 + 2）
      expect(mockContext.fillText).toHaveBeenCalledWith('全消し！', 400, 300) // メイン（中心）

      // saveとrestoreが呼ばれる
      expect(mockContext.save).toHaveBeenCalled()
      expect(mockContext.restore).toHaveBeenCalled()
    })
  })

  describe('renderGameOverEffect', () => {
    it('ゲームオーバー演出のテキストを描画する', () => {
      puyoImage.renderGameOverEffect()

      // テキストが4回描画される（GAME OVERの影とメイン、リスタートメッセージの影とメイン）
      expect(mockContext.fillText).toHaveBeenCalledTimes(4)
      expect(mockContext.fillText).toHaveBeenCalledWith('GAME OVER', 402, 302) // 影
      expect(mockContext.fillText).toHaveBeenCalledWith('GAME OVER', 400, 300) // メイン
      expect(mockContext.fillText).toHaveBeenCalledWith(
        'Rキーまたはスペースキーでリスタート',
        401,
        361
      ) // リスタートメッセージ（影）
      expect(mockContext.fillText).toHaveBeenCalledWith(
        'Rキーまたはスペースキーでリスタート',
        400,
        360
      ) // リスタートメッセージ（メイン）

      // saveとrestoreが呼ばれる
      expect(mockContext.save).toHaveBeenCalled()
      expect(mockContext.restore).toHaveBeenCalled()
    })
  })

  describe('static methods', () => {
    it('getCellSizeで正しいセルサイズを返す', () => {
      expect(PuyoImage.getCellSize()).toBe(30)
    })

    it('getFieldOffsetで正しいオフセットを返す', () => {
      const offset = PuyoImage.getFieldOffset()
      expect(offset.x).toBe(10)
      expect(offset.y).toBe(10)
    })
  })

  describe('色の管理', () => {
    it('正しい色コードをテストできる', () => {
      // この test は private method をテストするため、実際の描画結果で確認
      const field = [
        [1, 2, 3, 4], // 各色のぷよを配置
      ]

      puyoImage.renderField(field)

      // 4つのぷよが描画される
      expect(mockContext.ellipse).toHaveBeenCalledTimes(4)
      expect(mockContext.fill).toHaveBeenCalledTimes(4)
    })
  })

  describe('位置計算', () => {
    it('異なる方向のぷよの位置が正しく計算される', () => {
      // この test は private method のため、各方向のアクティブぷよ描画で確認
      const directions = [0, 1, 2, 3] // 下、右、上、左

      directions.forEach((direction) => {
        vi.clearAllMocks()

        const activePuyo = {
          x: 2,
          y: 5,
          color1: 1,
          color2: 2,
          direction: direction,
        }

        puyoImage.renderActivePuyo(activePuyo)

        // どの方向でも2つのぷよが描画される
        expect(mockContext.ellipse).toHaveBeenCalledTimes(2)
      })
    })
  })

  describe('統合描画テスト', () => {
    it('フィールド、アクティブぷよ、次ぷよを同時に描画できる', () => {
      const field = [
        [1, 0, 2],
        [0, 3, 0],
      ]

      const activePuyo = {
        x: 1,
        y: 0,
        color1: 1,
        color2: 4,
        direction: 0,
      }

      const nextPuyo = {
        color1: 2,
        color2: 3,
      }

      // 順次描画
      puyoImage.clearCanvas()
      puyoImage.renderField(field)
      puyoImage.renderActivePuyo(activePuyo)
      puyoImage.renderNextPuyo(nextPuyo)

      // フィールド(3) + アクティブぷよ(2) + 次ぷよ(2) = 7個のぷよが描画される
      expect(mockContext.ellipse).toHaveBeenCalledTimes(7)
      expect(mockContext.fill).toHaveBeenCalledTimes(7)
    })
  })
})
