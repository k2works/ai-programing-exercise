// テスト環境のセットアップファイル
import { vi } from 'vitest'

// phaser3spectorjsモジュールのモック
vi.mock('phaser3spectorjs', () => ({
  default: {},
}))

// グローバル型の宣言
declare global {
  interface HTMLCanvasElement {
    getContext(contextId: string): CanvasRenderingContext2D | WebGLRenderingContext | null
  }
}

// HTMLCanvasElementのgetContextメソッドをモック
if (typeof HTMLCanvasElement !== 'undefined') {
  Object.defineProperty(HTMLCanvasElement.prototype, 'getContext', {
    value: vi.fn().mockImplementation((contextType: string) => {
      if (contextType === '2d') {
        return {
          fillStyle: '',
          fillRect: vi.fn(),
          clearRect: vi.fn(),
          getImageData: vi.fn(() => ({
            data: new Uint8ClampedArray(4),
          })),
          putImageData: vi.fn(),
          createImageData: vi.fn(() => ({
            data: new Uint8ClampedArray(4),
          })),
          setTransform: vi.fn(),
          drawImage: vi.fn(),
          save: vi.fn(),
          restore: vi.fn(),
          beginPath: vi.fn(),
          moveTo: vi.fn(),
          lineTo: vi.fn(),
          closePath: vi.fn(),
          stroke: vi.fn(),
          fill: vi.fn(),
          measureText: vi.fn(() => ({ width: 0 })),
          isPointInPath: vi.fn(),
          createLinearGradient: vi.fn(),
          createRadialGradient: vi.fn(),
          createPattern: vi.fn(),
        }
      }
      if (contextType === 'webgl' || contextType === 'webgl2') {
        return {
          createShader: vi.fn(),
          shaderSource: vi.fn(),
          compileShader: vi.fn(),
          createProgram: vi.fn(),
          attachShader: vi.fn(),
          linkProgram: vi.fn(),
          useProgram: vi.fn(),
          createBuffer: vi.fn(),
          bindBuffer: vi.fn(),
          bufferData: vi.fn(),
          vertexAttribPointer: vi.fn(),
          enableVertexAttribArray: vi.fn(),
          drawArrays: vi.fn(),
          clearColor: vi.fn(),
          clear: vi.fn(),
          viewport: vi.fn(),
          getShaderParameter: vi.fn(),
          getShaderInfoLog: vi.fn(),
          getProgramParameter: vi.fn(),
          getProgramInfoLog: vi.fn(),
          getAttribLocation: vi.fn(),
          getUniformLocation: vi.fn(),
          uniform1f: vi.fn(),
          uniform2f: vi.fn(),
          uniform3f: vi.fn(),
          uniform4f: vi.fn(),
          uniformMatrix4fv: vi.fn(),
          createTexture: vi.fn(),
          bindTexture: vi.fn(),
          texImage2D: vi.fn(),
          texParameteri: vi.fn(),
          activeTexture: vi.fn(),
          generateMipmap: vi.fn(),
          VERTEX_SHADER: 35633,
          FRAGMENT_SHADER: 35632,
          COMPILE_STATUS: 35713,
          LINK_STATUS: 35714,
          ARRAY_BUFFER: 34962,
          STATIC_DRAW: 35044,
          FLOAT: 5126,
          TRIANGLES: 4,
          COLOR_BUFFER_BIT: 16384,
          TEXTURE_2D: 3553,
          RGBA: 6408,
          UNSIGNED_BYTE: 5121,
          TEXTURE_MIN_FILTER: 10241,
          TEXTURE_MAG_FILTER: 10240,
          LINEAR: 9729,
          TEXTURE0: 33984,
        }
      }
      return null
    }),
  })
}
