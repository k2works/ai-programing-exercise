import { vi } from 'vitest'

// requestAnimationFrame のモック
;(globalThis as any).requestAnimationFrame = vi.fn((callback) => {
  return setTimeout(callback, 16) // 60fpsを想定した16msの遅延
})

// cancelAnimationFrame のモック
;(globalThis as any).cancelAnimationFrame = vi.fn((id) => {
  clearTimeout(id)
})
