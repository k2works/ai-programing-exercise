import { vi, beforeEach } from 'vitest'

// requestAnimationFrame のモック
;(globalThis as any).requestAnimationFrame = vi.fn((callback) => {
  return setTimeout(callback, 16) // 60fpsを想定した16msの遅延
})

// cancelAnimationFrame のモック
;(globalThis as any).cancelAnimationFrame = vi.fn((id) => {
  clearTimeout(id)
})

// HTMLCanvasElement のモック
beforeEach(() => {
  Object.defineProperty(HTMLCanvasElement.prototype, 'getContext', {
    value: vi.fn(() => ({
      clearRect: vi.fn(),
      fillRect: vi.fn(),
      strokeRect: vi.fn(),
      beginPath: vi.fn(),
      ellipse: vi.fn(),
      fill: vi.fn(),
      stroke: vi.fn(),
      fillStyle: '',
      strokeStyle: '',
      lineWidth: 1,
    })),
    writable: true,
    configurable: true,
  })
})
