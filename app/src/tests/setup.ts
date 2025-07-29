import { vi, beforeEach } from 'vitest'

// requestAnimationFrame のモック
;(globalThis as any).requestAnimationFrame = vi.fn((callback) => {
  return setTimeout(callback, 16) // 60fpsを想定した16msの遅延
})

// cancelAnimationFrame のモック
;(globalThis as any).cancelAnimationFrame = vi.fn((id) => {
  clearTimeout(id)
})

// 共通のCanvas 2D contextモックを作成
const createMockContext = () => ({
  clearRect: vi.fn(),
  fillRect: vi.fn(),
  strokeRect: vi.fn(),
  beginPath: vi.fn(),
  ellipse: vi.fn(),
  fill: vi.fn(),
  stroke: vi.fn(),
  save: vi.fn(),
  restore: vi.fn(),
  fillText: vi.fn(),
  fillStyle: '',
  strokeStyle: '',
  lineWidth: 1,
  font: '',
  textAlign: '',
  textBaseline: '',
})

// HTMLCanvasElement のモック（グローバル設定）
Object.defineProperty(HTMLCanvasElement.prototype, 'getContext', {
  value: vi.fn(() => {
    const context = createMockContext()
    // デバッグ用ログ
    console.log('Global mock getContext called, returning context with save:', typeof context.save)
    return context
  }),
  writable: true,
  configurable: true,
})

Object.defineProperty(HTMLCanvasElement.prototype, 'width', {
  value: 400,
  writable: true,
  configurable: true,
})

Object.defineProperty(HTMLCanvasElement.prototype, 'height', {
  value: 600,
  writable: true,
  configurable: true,
})

// HTMLCanvasElement のモック（各テスト前のリセット）
beforeEach(() => {
  // 各テストの前にモックをリフレッシュ
  const mockGetContext = vi.fn(() => {
    const context = createMockContext()
    // デバッグ用ログ
    console.log('BeforeEach mock getContext called, returning context with save:', typeof context.save)
    return context
  })
  
  Object.defineProperty(HTMLCanvasElement.prototype, 'getContext', {
    value: mockGetContext,
    writable: true,
    configurable: true,
  })
})

// グローバルにcanvas contextモックを利用可能にする
;(global as any).createMockContext = createMockContext
