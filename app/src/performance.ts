// パフォーマンス監視・最適化ユーティリティ

export class PerformanceMonitor {
  private frameTimes: number[] = []
  private lastFrameTime = performance.now()
  public frameCount = 0
  private readonly MAX_FRAME_HISTORY = 60

  recordFrame(): void {
    const now = performance.now()
    const deltaTime = now - this.lastFrameTime
    
    this.frameTimes.push(deltaTime)
    this.frameCount++
    
    if (this.frameTimes.length > this.MAX_FRAME_HISTORY) {
      this.frameTimes.shift()
    }
    
    this.lastFrameTime = now
  }

  getAverageFPS(): number {
    if (this.frameTimes.length === 0) return 0
    
    const averageDelta = this.frameTimes.reduce((sum, time) => sum + time, 0) / this.frameTimes.length
    return 1000 / averageDelta
  }

  getCurrentFPS(): number {
    if (this.frameTimes.length === 0) return 0
    
    const lastDelta = this.frameTimes[this.frameTimes.length - 1]
    return 1000 / lastDelta
  }

  getFrameTimeStats(): { min: number; max: number; avg: number } {
    if (this.frameTimes.length === 0) {
      return { min: 0, max: 0, avg: 0 }
    }

    const min = Math.min(...this.frameTimes)
    const max = Math.max(...this.frameTimes)
    const avg = this.frameTimes.reduce((sum, time) => sum + time, 0) / this.frameTimes.length

    return { min, max, avg }
  }

  isPerformanceGood(): boolean {
    return this.getAverageFPS() > 50 // 50FPS以上を良好とみなす
  }

  logPerformance(): void {
    const fps = this.getAverageFPS()
    const stats = this.getFrameTimeStats()
    
    console.log(`📊 Performance Stats:`)
    console.log(`  Average FPS: ${fps.toFixed(2)}`)
    console.log(`  Frame time: ${stats.avg.toFixed(2)}ms (min: ${stats.min.toFixed(2)}, max: ${stats.max.toFixed(2)})`)
    console.log(`  Total frames: ${this.frameCount}`)
    console.log(`  Status: ${this.isPerformanceGood() ? '✅ Good' : '⚠️ Needs optimization'}`)
  }

  reset(): void {
    this.frameTimes = []
    this.frameCount = 0
    this.lastFrameTime = performance.now()
  }
}

// オブジェクトプール - ガベージコレクション圧迫を軽減
export class ObjectPool<T> {
  private pool: T[] = []
  private createFn: () => T
  private resetFn?: (obj: T) => void

  constructor(createFn: () => T, resetFn?: (obj: T) => void) {
    this.createFn = createFn
    this.resetFn = resetFn
  }

  acquire(): T {
    if (this.pool.length > 0) {
      return this.pool.pop()!
    }
    return this.createFn()
  }

  release(obj: T): void {
    if (this.resetFn) {
      this.resetFn(obj)
    }
    this.pool.push(obj)
  }

  getPoolSize(): number {
    return this.pool.length
  }

  clear(): void {
    this.pool = []
  }
}

// メモ化デコレータ - 計算結果のキャッシュ
export class MemoizationCache<K, V> {
  private cache = new Map<string, V>()
  private maxSize: number

  constructor(maxSize = 1000) {
    this.maxSize = maxSize
  }

  get(key: K): V | undefined {
    const keyStr = JSON.stringify(key)
    return this.cache.get(keyStr)
  }

  set(key: K, value: V): void {
    const keyStr = JSON.stringify(key)
    
    if (this.cache.size >= this.maxSize) {
      // LRU: 最も古いエントリを削除
      const firstKey = this.cache.keys().next().value
      if (firstKey !== undefined) {
        this.cache.delete(firstKey)
      }
    }
    
    this.cache.set(keyStr, value)
  }

  has(key: K): boolean {
    const keyStr = JSON.stringify(key)
    return this.cache.has(keyStr)
  }

  clear(): void {
    this.cache.clear()
  }

  getSize(): number {
    return this.cache.size
  }
}

// 描画最適化ユーティリティ
export class DrawOptimizer {
  private lastDrawState: string | null = null
  private dirtyRegions: Set<string> = new Set()

  markDirty(x: number, y: number): void {
    this.dirtyRegions.add(`${x},${y}`)
  }

  markClean(x: number, y: number): void {
    this.dirtyRegions.delete(`${x},${y}`)
  }

  isDirty(x: number, y: number): boolean {
    return this.dirtyRegions.has(`${x},${y}`)
  }

  shouldRedraw(newState: string): boolean {
    if (this.lastDrawState !== newState) {
      this.lastDrawState = newState
      return true
    }
    return false
  }

  hasDirtyRegions(): boolean {
    return this.dirtyRegions.size > 0
  }

  clearDirtyRegions(): void {
    this.dirtyRegions.clear()
  }

  getDirtyRegions(): string[] {
    return Array.from(this.dirtyRegions)
  }
}

// レンダリング最適化のためのバッチ処理
export class RenderBatch {
  private operations: Array<() => void> = []
  private isProcessing = false

  addOperation(operation: () => void): void {
    this.operations.push(operation)
  }

  flush(): void {
    if (this.isProcessing) return
    
    this.isProcessing = true
    
    try {
      for (const operation of this.operations) {
        operation()
      }
    } finally {
      this.operations = []
      this.isProcessing = false
    }
  }

  clear(): void {
    if (!this.isProcessing) {
      this.operations = []
    }
  }

  getOperationCount(): number {
    return this.operations.length
  }
}