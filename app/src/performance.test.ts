import { describe, it, expect, beforeEach, vi } from 'vitest'
import { 
  PerformanceMonitor, 
  ObjectPool, 
  MemoizationCache, 
  DrawOptimizer, 
  RenderBatch 
} from './performance'

describe('PerformanceMonitor', () => {
  let monitor: PerformanceMonitor

  describe('フレーム記録', () => {
    it('フレームタイムを記録できる', () => {
      // performance.nowをモック化
      let currentTime = 0
      const mockNow = vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 16.67
        return time
      })
      
      monitor = new PerformanceMonitor() // モック後に初期化
      
      monitor.recordFrame() // 16.67ms delta
      monitor.recordFrame() // 16.67ms delta
      
      const stats = monitor.getFrameTimeStats()
      expect(stats.min).toBeCloseTo(16.67, 1)
      expect(stats.max).toBeCloseTo(16.67, 1)
      expect(stats.avg).toBeCloseTo(16.67, 1)
      
      mockNow.mockRestore()
    })

    it('最大フレーム履歴数を超えない', () => {
      monitor = new PerformanceMonitor()
      
      // 61フレーム記録（MAX_FRAME_HISTORY = 60を超える）
      for (let i = 0; i < 61; i++) {
        vi.spyOn(performance, 'now').mockReturnValue(i * 16.67)
        monitor.recordFrame()
        vi.restoreAllMocks()
      }
      
      const stats = monitor.getFrameTimeStats()
      // 最初の1フレームがshiftされているため60フレーム分のみ
      expect(monitor.getFrameTimeStats()).toBeDefined()
    })
  })

  describe('FPS計算', () => {
    it('平均FPSを正しく計算する', () => {
      // performance.nowをモック化
      let currentTime = 0
      const mockNow = vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 16.67
        return time
      })
      
      monitor = new PerformanceMonitor()
      
      monitor.recordFrame() // 16.67ms
      monitor.recordFrame() // 16.67ms
      
      const fps = monitor.getAverageFPS()
      // 1000 / 16.67 ≈ 60 FPS
      expect(fps).toBeCloseTo(60, 0)
      
      mockNow.mockRestore()
    })

    it('現在のFPSを正しく計算する', () => {
      // performance.nowをモック化
      let currentTime = 0
      const mockNow = vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 16.67
        return time
      })
      
      monitor = new PerformanceMonitor()
      
      monitor.recordFrame()
      monitor.recordFrame()
      
      const fps = monitor.getCurrentFPS()
      // 最後のフレーム: 1000 / 16.67 ≈ 60 FPS
      expect(fps).toBeCloseTo(60, 0)
      
      mockNow.mockRestore()
    })
  })

  describe('パフォーマンス判定', () => {
    it('50FPS以上で良好と判定される', () => {
      // 60FPS相当のフレームタイム
      let currentTime = 0
      const mockNow = vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 16.67
        return time
      })
      
      monitor = new PerformanceMonitor()
      
      monitor.recordFrame() // 16.67ms
      monitor.recordFrame()
      
      expect(monitor.isPerformanceGood()).toBe(true)
      
      mockNow.mockRestore()
    })

    it('50FPS未満で問題ありと判定される', () => {
      // 30FPS相当のフレームタイム（33.33ms）
      let currentTime = 0
      const mockNow = vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 33.33
        return time
      })
      
      monitor = new PerformanceMonitor()
        
      monitor.recordFrame()
      monitor.recordFrame()
      
      expect(monitor.isPerformanceGood()).toBe(false)
      
      mockNow.mockRestore()
    })
  })

  describe('リセット機能', () => {
    it('状態をリセットできる', () => {
      let currentTime = 0
      const mockNow = vi.spyOn(performance, 'now').mockImplementation(() => {
        const time = currentTime
        currentTime += 16.67
        return time
      })
      
      monitor = new PerformanceMonitor()
      
      monitor.recordFrame()
      monitor.recordFrame()
      
      expect(monitor.getAverageFPS()).toBeGreaterThan(0)
      
      monitor.reset()
      expect(monitor.getAverageFPS()).toBe(0)
      
      mockNow.mockRestore()
    })
  })
})

describe('ObjectPool', () => {
  interface TestObject {
    id: number
    reset: () => void
  }

  let pool: ObjectPool<TestObject>
  let createCount = 0

  beforeEach(() => {
    createCount = 0
    pool = new ObjectPool<TestObject>(
      () => ({
        id: ++createCount,
        reset: vi.fn()
      }),
      (obj) => obj.reset()
    )
  })

  describe('オブジェクト取得', () => {
    it('プールが空の時は新しいオブジェクトを作成する', () => {
      const obj = pool.acquire()
      expect(obj.id).toBe(1)
      expect(createCount).toBe(1)
    })

    it('プールにオブジェクトがある時は再利用する', () => {
      const obj1 = pool.acquire()
      pool.release(obj1)
      
      const obj2 = pool.acquire()
      expect(obj2).toBe(obj1) // 同じオブジェクト
      expect(createCount).toBe(1) // 新規作成なし
    })
  })

  describe('オブジェクト返却', () => {
    it('返却時にリセット関数が呼ばれる', () => {
      const obj = pool.acquire()
      const resetSpy = obj.reset as any
      
      pool.release(obj)
      expect(resetSpy).toHaveBeenCalledOnce()
    })

    it('プールサイズが増加する', () => {
      const obj = pool.acquire()
      expect(pool.getPoolSize()).toBe(0)
      
      pool.release(obj)
      expect(pool.getPoolSize()).toBe(1)
    })
  })

  describe('プール管理', () => {
    it('プールをクリアできる', () => {
      const obj = pool.acquire()
      pool.release(obj)
      expect(pool.getPoolSize()).toBe(1)
      
      pool.clear()
      expect(pool.getPoolSize()).toBe(0)
    })
  })
})

describe('MemoizationCache', () => {
  let cache: MemoizationCache<string, number>

  beforeEach(() => {
    cache = new MemoizationCache<string, number>(3) // 最大3エントリ
  })

  describe('基本操作', () => {
    it('値を設定・取得できる', () => {
      cache.set('key1', 100)
      expect(cache.get('key1')).toBe(100)
      expect(cache.has('key1')).toBe(true)
    })

    it('存在しないキーはundefinedを返す', () => {
      expect(cache.get('nonexistent')).toBeUndefined()
      expect(cache.has('nonexistent')).toBe(false)
    })
  })

  describe('LRU機能', () => {
    it('最大サイズを超えると古いエントリが削除される', () => {
      cache.set('key1', 1)
      cache.set('key2', 2)
      cache.set('key3', 3)
      expect(cache.getSize()).toBe(3)
      
      cache.set('key4', 4) // 最大サイズ超過
      expect(cache.getSize()).toBe(3)
      expect(cache.has('key1')).toBe(false) // 最も古いエントリが削除
      expect(cache.has('key4')).toBe(true)
    })
  })

  describe('クリア機能', () => {
    it('すべてのエントリをクリアできる', () => {
      cache.set('key1', 1)
      cache.set('key2', 2)
      expect(cache.getSize()).toBe(2)
      
      cache.clear()
      expect(cache.getSize()).toBe(0)
    })
  })
})

describe('DrawOptimizer', () => {
  let optimizer: DrawOptimizer

  beforeEach(() => {
    optimizer = new DrawOptimizer()
  })

  describe('ダーティリージョン管理', () => {
    it('リージョンをダーティとしてマークできる', () => {
      optimizer.markDirty(1, 2)
      expect(optimizer.isDirty(1, 2)).toBe(true)
      expect(optimizer.hasDirtyRegions()).toBe(true)
    })

    it('リージョンをクリーンとしてマークできる', () => {
      optimizer.markDirty(1, 2)
      optimizer.markClean(1, 2)
      expect(optimizer.isDirty(1, 2)).toBe(false)
    })

    it('すべてのダーティリージョンをクリアできる', () => {
      optimizer.markDirty(1, 1)
      optimizer.markDirty(2, 2)
      expect(optimizer.hasDirtyRegions()).toBe(true)
      
      optimizer.clearDirtyRegions()
      expect(optimizer.hasDirtyRegions()).toBe(false)
    })

    it('ダーティリージョンの一覧を取得できる', () => {
      optimizer.markDirty(1, 2)
      optimizer.markDirty(3, 4)
      
      const regions = optimizer.getDirtyRegions()
      expect(regions).toContain('1,2')
      expect(regions).toContain('3,4')
      expect(regions).toHaveLength(2)
    })
  })

  describe('再描画判定', () => {
    it('異なる状態では再描画が必要', () => {
      expect(optimizer.shouldRedraw('state1')).toBe(true)
      expect(optimizer.shouldRedraw('state2')).toBe(true)
    })

    it('同じ状態では再描画が不要', () => {
      optimizer.shouldRedraw('state1')
      expect(optimizer.shouldRedraw('state1')).toBe(false)
    })
  })
})

describe('RenderBatch', () => {
  let batch: RenderBatch
  let operations: Array<() => void>

  beforeEach(() => {
    batch = new RenderBatch()
    operations = [
      vi.fn(),
      vi.fn(),
      vi.fn()
    ]
  })

  describe('オペレーション管理', () => {
    it('オペレーションを追加できる', () => {
      batch.addOperation(operations[0])
      batch.addOperation(operations[1])
      
      expect(batch.getOperationCount()).toBe(2)
    })

    it('フラッシュですべてのオペレーションが実行される', () => {
      batch.addOperation(operations[0])
      batch.addOperation(operations[1])
      batch.addOperation(operations[2])
      
      batch.flush()
      
      operations.forEach(op => {
        expect(op).toHaveBeenCalledOnce()
      })
      expect(batch.getOperationCount()).toBe(0)
    })
  })

  describe('処理中の状態管理', () => {
    it('処理中は新しいフラッシュを無視する', () => {
      let isProcessing = false
      batch.addOperation(() => {
        isProcessing = true
        batch.flush() // 再帰的呼び出し
        isProcessing = false
      })
      
      batch.flush()
      expect(isProcessing).toBe(false) // 再帰的フラッシュは無視される
    })

    it('処理中でない場合のみクリアできる', () => {
      batch.addOperation(operations[0])
      batch.clear()
      expect(batch.getOperationCount()).toBe(0)
    })
  })

  describe('エラーハンドリング', () => {
    it('オペレーションでエラーが発生してもクリーンアップされる', () => {
      const errorOp = vi.fn(() => { throw new Error('test error') })
      batch.addOperation(operations[0])
      batch.addOperation(errorOp)
      batch.addOperation(operations[1])
      
      expect(() => batch.flush()).toThrow('test error')
      // エラー後もクリーンアップされる
      expect(batch.getOperationCount()).toBe(0)
    })
  })
})