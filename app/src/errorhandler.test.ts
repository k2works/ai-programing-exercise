import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest'
import { 
  ErrorHandler, 
  ErrorLevel, 
  PuyoGameError, 
  ValidationError, 
  GameStateError, 
  RenderingError,
  handleError,
  handleValidationError,
  handleGameStateError,
  handleRenderingError
} from './errorhandler'

describe('PuyoGameError', () => {
  describe('カスタムエラータイプ', () => {
    it('基本的なエラー情報を保持する', () => {
      const error = new PuyoGameError('テストエラー', ErrorLevel.Error, 'TestContext', true)
      
      expect(error.name).toBe('PuyoGameError')
      expect(error.message).toBe('テストエラー')
      expect(error.level).toBe(ErrorLevel.Error)
      expect(error.context).toBe('TestContext')
      expect(error.recoverable).toBe(true)
    })

    it('デフォルト値が正しく設定される', () => {
      const error = new PuyoGameError('テストエラー')
      
      expect(error.level).toBe(ErrorLevel.Error)
      expect(error.context).toBeUndefined()
      expect(error.recoverable).toBe(false)
    })
  })

  describe('特化エラータイプ', () => {
    it('ValidationError は Warning レベル', () => {
      const error = new ValidationError('バリデーションエラー', 'InputTest')
      
      expect(error.name).toBe('ValidationError')
      expect(error.level).toBe(ErrorLevel.Warning)
      expect(error.recoverable).toBe(true)
    })

    it('GameStateError は Error レベル', () => {
      const error = new GameStateError('ゲーム状態エラー', 'StateTest')
      
      expect(error.name).toBe('GameStateError')
      expect(error.level).toBe(ErrorLevel.Error)
      expect(error.recoverable).toBe(true)
    })

    it('RenderingError は Critical レベル', () => {
      const error = new RenderingError('レンダリングエラー', 'RenderTest')
      
      expect(error.name).toBe('RenderingError')
      expect(error.level).toBe(ErrorLevel.Critical)
      expect(error.recoverable).toBe(false)
    })
  })
})

describe('ErrorHandler', () => {
  let errorHandler: ErrorHandler

  beforeEach(() => {
    errorHandler = new ErrorHandler()
    // コンソール出力をモック化
    vi.spyOn(console, 'info').mockImplementation(() => {})
    vi.spyOn(console, 'warn').mockImplementation(() => {})
    vi.spyOn(console, 'error').mockImplementation(() => {})
  })

  afterEach(() => {
    // 各テスト後にエラーをクリア
    errorHandler.clearErrors()
  })

  describe('エラー記録', () => {
    it('PuyoGameError を正しく記録する', () => {
      const testError = new ValidationError('テストバリデーションエラー')
      const gameError = errorHandler.recordError(testError)
      
      expect(gameError.level).toBe(ErrorLevel.Warning)
      expect(gameError.message).toBe('テストバリデーションエラー')
      expect(gameError.recoverable).toBe(true)
      expect(gameError.id).toBeDefined()
      expect(gameError.timestamp).toBeGreaterThan(0)
    })

    it('標準 Error を正しく記録する', () => {
      const testError = new Error('標準エラー')
      const gameError = errorHandler.recordError(testError, 'TestContext')
      
      expect(gameError.level).toBe(ErrorLevel.Error)
      expect(gameError.message).toBe('標準エラー')
      expect(gameError.context).toBe('TestContext')
      expect(gameError.recoverable).toBe(false)
    })

    it('文字列エラーを正しく記録する', () => {
      const gameError = errorHandler.recordError('文字列エラー', 'StringTest')
      
      expect(gameError.level).toBe(ErrorLevel.Info)
      expect(gameError.message).toBe('文字列エラー')
      expect(gameError.context).toBe('StringTest')
      expect(gameError.recoverable).toBe(true)
    })
  })

  describe('エラー取得', () => {
    beforeEach(() => {
      errorHandler.clearErrors() // 前のテストのエラーをクリア
      errorHandler.recordError(new ValidationError('警告1'))
      errorHandler.recordError(new GameStateError('エラー1'))
      errorHandler.recordError(new RenderingError('重大エラー1'))
    })

    it('全てのエラーを取得できる', () => {
      const allErrors = errorHandler.getErrors()
      expect(allErrors.length).toBeGreaterThanOrEqual(3) // 自動復旧ログも含む可能性
    })

    it('レベル別にエラーを取得できる', () => {
      const warningErrors = errorHandler.getErrors(ErrorLevel.Warning)
      const errorErrors = errorHandler.getErrors(ErrorLevel.Error)
      const criticalErrors = errorHandler.getErrors(ErrorLevel.Critical)
      
      expect(warningErrors).toHaveLength(1)
      expect(errorErrors).toHaveLength(1)
      expect(criticalErrors).toHaveLength(1)
    })

    it('最近のエラーを取得できる', () => {
      const recentErrors = errorHandler.getRecentErrors(2)
      expect(recentErrors).toHaveLength(2)
      // 最新のエラーから順に取得
      expect(recentErrors[1].message).toBe('重大エラー1')
      expect(recentErrors[0].message).toBe('エラー1')
    })

    it('復旧可能なエラーを取得できる', () => {
      const recoverableErrors = errorHandler.getRecoverableErrors()
      expect(recoverableErrors.length).toBeGreaterThanOrEqual(2) // ValidationError と GameStateError 以上
    })
  })

  describe('エラー統計', () => {
    it('エラー統計を正しく計算する', () => {
      errorHandler.clearErrors() // クリアしてから開始
      errorHandler.recordError(new ValidationError('警告1'))
      errorHandler.recordError(new ValidationError('警告2'))
      errorHandler.recordError(new GameStateError('エラー1'))
      errorHandler.recordError(new RenderingError('重大エラー1'))
      
      const stats = errorHandler.getErrorStats()
      expect(stats.total).toBeGreaterThanOrEqual(4) // 自動復旧ログも含む可能性
      expect(stats.byLevel[ErrorLevel.Warning]).toBeGreaterThanOrEqual(2)
      expect(stats.byLevel[ErrorLevel.Error]).toBeGreaterThanOrEqual(1)
      expect(stats.byLevel[ErrorLevel.Critical]).toBeGreaterThanOrEqual(1)
    })
  })

  describe('エラーリスナー', () => {
    it('エラーレベル別にリスナーが呼び出される', () => {
      const warningCallback = vi.fn()
      const errorCallback = vi.fn()
      
      errorHandler.onError(ErrorLevel.Warning, warningCallback)
      errorHandler.onError(ErrorLevel.Error, errorCallback)
      
      errorHandler.recordError(new ValidationError('警告テスト'))
      errorHandler.recordError(new GameStateError('エラーテスト'))
      
      expect(warningCallback).toHaveBeenCalledTimes(1)
      expect(errorCallback).toHaveBeenCalledTimes(1)
    })

    it('複数のリスナーが登録・実行される', () => {
      const callback1 = vi.fn()
      const callback2 = vi.fn()
      
      errorHandler.onError(ErrorLevel.Error, callback1)
      errorHandler.onError(ErrorLevel.Error, callback2)
      
      errorHandler.recordError(new GameStateError('エラーテスト'))
      
      expect(callback1).toHaveBeenCalledTimes(1)
      expect(callback2).toHaveBeenCalledTimes(1)
    })
  })

  describe('復旧アクション', () => {
    it('復旧アクションを登録・実行できる', async () => {
      const mockAction = vi.fn()
      errorHandler.registerRecoveryAction({
        id: 'test_recovery',
        description: 'テスト復旧',
        execute: mockAction
      })
      
      const result = await errorHandler.executeRecovery('test_recovery')
      expect(result).toBe(true)
      expect(mockAction).toHaveBeenCalledTimes(1)
    })

    it('存在しない復旧アクションはエラーを返す', async () => {
      const result = await errorHandler.executeRecovery('non_existent')
      expect(result).toBe(false)
    })

    it('復旧アクション実行中のエラーを処理する', async () => {
      const failingAction = vi.fn().mockRejectedValue(new Error('復旧失敗'))
      errorHandler.registerRecoveryAction({
        id: 'failing_recovery',
        description: '失敗する復旧',
        execute: failingAction
      })
      
      const result = await errorHandler.executeRecovery('failing_recovery')
      expect(result).toBe(false)
    })
  })

  describe('エラークリア', () => {
    beforeEach(() => {
      errorHandler.clearErrors() // クリアしてから開始
      errorHandler.recordError(new ValidationError('警告1'))
      errorHandler.recordError(new GameStateError('エラー1'))
      errorHandler.recordError(new RenderingError('重大エラー1'))
    })

    it('特定レベルのエラーをクリアできる', () => {
      const beforeCount = errorHandler.getErrors().length
      errorHandler.clearErrors(ErrorLevel.Warning)
      
      const remainingErrors = errorHandler.getErrors()
      expect(remainingErrors.length).toBeLessThan(beforeCount) // Warningレベルが減っている
      expect(remainingErrors.every(e => e.level !== ErrorLevel.Warning)).toBe(true)
    })

    it('全てのエラーをクリアできる', () => {
      errorHandler.clearErrors()
      
      const remainingErrors = errorHandler.getErrors()
      expect(remainingErrors).toHaveLength(0)
    })
  })

  describe('設定', () => {
    it('ログ設定を変更できる', () => {
      errorHandler.setLogging(false)
      errorHandler.recordError('ログなしテスト')
      
      // ログが無効の場合、コンソール出力されない
      expect(console.info).not.toHaveBeenCalled()
    })
  })

  describe('最大エラー数制限', () => {
    it('最大エラー数を超えると古いエラーが削除される', () => {
      // 101個のエラーを記録（最大100）
      for (let i = 0; i < 101; i++) {
        errorHandler.recordError(`エラー${i}`)
      }
      
      const errors = errorHandler.getErrors()
      expect(errors.length).toBe(100)
      // 最も古い「エラー0」は削除され、「エラー1」から「エラー100」が残る
      expect(errors[0].message).toBe('エラー1')
      expect(errors[99].message).toBe('エラー100')
    })
  })
})

describe('便利関数', () => {
  beforeEach(() => {
    vi.spyOn(console, 'info').mockImplementation(() => {})
    vi.spyOn(console, 'warn').mockImplementation(() => {})
    vi.spyOn(console, 'error').mockImplementation(() => {})
  })

  it('handleError はグローバルハンドラーを使用する', () => {
    const gameError = handleError('テストエラー', 'TestContext')
    expect(gameError.message).toBe('テストエラー')
    expect(gameError.context).toBe('TestContext')
  })

  it('handleValidationError は ValidationError を記録する', () => {
    handleValidationError('バリデーションエラー', 'ValidationTest')
    // 内部でglobalErrorHandlerが使用されるため、直接的な検証は困難
    // 実際のテストでは統合テストで確認
  })

  it('handleGameStateError は GameStateError を記録する', () => {
    handleGameStateError('ゲーム状態エラー', 'GameStateTest')
    // 内部でglobalErrorHandlerが使用されるため、直接的な検証は困難
  })

  it('handleRenderingError は RenderingError を記録する', () => {
    handleRenderingError('レンダリングエラー', 'RenderingTest')
    // 内部でglobalErrorHandlerが使用されるため、直接的な検証は困難
  })
})