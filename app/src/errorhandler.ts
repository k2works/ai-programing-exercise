// エラーハンドリングシステム

export enum ErrorLevel {
  Info = 0,
  Warning = 1,
  Error = 2,
  Critical = 3
}

export interface GameError {
  id: string
  level: ErrorLevel
  message: string
  context?: string
  timestamp: number
  stack?: string
  recoverable: boolean
}

export interface ErrorRecoveryAction {
  id: string
  description: string
  execute: () => void | Promise<void>
}

// カスタムエラータイプ
export class PuyoGameError extends Error {
  public readonly level: ErrorLevel
  public readonly context?: string
  public readonly recoverable: boolean

  constructor(
    message: string, 
    level: ErrorLevel = ErrorLevel.Error, 
    context?: string, 
    recoverable: boolean = false
  ) {
    super(message)
    this.name = 'PuyoGameError'
    this.level = level
    this.context = context
    this.recoverable = recoverable
  }
}

// 入力バリデーションエラー
export class ValidationError extends PuyoGameError {
  constructor(message: string, context?: string) {
    super(message, ErrorLevel.Warning, context, true)
    this.name = 'ValidationError'
  }
}

// ゲーム状態エラー
export class GameStateError extends PuyoGameError {
  constructor(message: string, context?: string) {
    super(message, ErrorLevel.Error, context, true)
    this.name = 'GameStateError'
  }
}

// レンダリングエラー
export class RenderingError extends PuyoGameError {
  constructor(message: string, context?: string) {
    super(message, ErrorLevel.Critical, context, false)
    this.name = 'RenderingError'
  }
}

export class ErrorHandler {
  private errors: GameError[] = []
  private errorListeners: Map<ErrorLevel, ((error: GameError) => void)[]> = new Map()
  private recoveryActions: Map<string, ErrorRecoveryAction> = new Map()
  private readonly maxErrors = 100
  private isLogging = true

  constructor() {
    // 各エラーレベルのリスナー初期化
    Object.values(ErrorLevel).forEach(level => {
      if (typeof level === 'number') {
        this.errorListeners.set(level, [])
      }
    })

    // 未捕捉エラーの処理
    this.setupGlobalErrorHandlers()
  }

  // エラー記録
  recordError(error: Error | PuyoGameError | string, context?: string): GameError {
    let gameError: GameError

    if (error instanceof PuyoGameError) {
      gameError = {
        id: this.generateErrorId(),
        level: error.level,
        message: error.message,
        context: error.context || context,
        timestamp: Date.now(),
        stack: error.stack,
        recoverable: error.recoverable
      }
    } else if (error instanceof Error) {
      gameError = {
        id: this.generateErrorId(),
        level: ErrorLevel.Error,
        message: error.message,
        context,
        timestamp: Date.now(),
        stack: error.stack,
        recoverable: false
      }
    } else {
      gameError = {
        id: this.generateErrorId(),
        level: ErrorLevel.Info,
        message: error,
        context,
        timestamp: Date.now(),
        recoverable: true
      }
    }

    this.addError(gameError)
    return gameError
  }

  // エラー追加
  private addError(error: GameError): void {
    this.errors.push(error)

    // 最大エラー数を超えた場合、古いものを削除
    if (this.errors.length > this.maxErrors) {
      this.errors.shift()
    }

    // ログ出力
    if (this.isLogging) {
      this.logError(error)
    }

    // リスナー通知
    this.notifyListeners(error)

    // 自動復旧試行
    this.attemptRecovery(error)
  }

  // エラーレベル別リスナー登録
  onError(level: ErrorLevel, callback: (error: GameError) => void): void {
    const listeners = this.errorListeners.get(level) || []
    listeners.push(callback)
    this.errorListeners.set(level, listeners)
  }

  // 復旧アクション登録
  registerRecoveryAction(action: ErrorRecoveryAction): void {
    this.recoveryActions.set(action.id, action)
  }

  // エラー取得
  getErrors(level?: ErrorLevel): GameError[] {
    if (level !== undefined) {
      return this.errors.filter(error => error.level === level)
    }
    return [...this.errors]
  }

  // 最近のエラー取得
  getRecentErrors(count: number = 10): GameError[] {
    return this.errors.slice(-count)
  }

  // エラー統計
  getErrorStats(): { total: number; byLevel: Record<ErrorLevel, number> } {
    const byLevel = {} as Record<ErrorLevel, number>
    
    Object.values(ErrorLevel).forEach(level => {
      if (typeof level === 'number') {
        byLevel[level] = 0
      }
    })

    this.errors.forEach(error => {
      byLevel[error.level]++
    })

    return {
      total: this.errors.length,
      byLevel
    }
  }

  // エラークリア
  clearErrors(level?: ErrorLevel): void {
    if (level !== undefined) {
      this.errors = this.errors.filter(error => error.level !== level)
    } else {
      this.errors = []
    }
  }

  // 復旧可能エラー取得
  getRecoverableErrors(): GameError[] {
    return this.errors.filter(error => error.recoverable)
  }

  // ログ設定
  setLogging(enabled: boolean): void {
    this.isLogging = enabled
  }

  // エラー復旧実行
  async executeRecovery(actionId: string): Promise<boolean> {
    const action = this.recoveryActions.get(actionId)
    if (!action) {
      this.recordError(`復旧アクション '${actionId}' が見つかりません`)
      return false
    }

    try {
      await action.execute()
      this.recordError(`復旧アクション '${action.description}' が正常に実行されました`, 'Recovery')
      return true
    } catch (error) {
      this.recordError(new PuyoGameError(
        `復旧アクション '${action.description}' の実行に失敗: ${error}`,
        ErrorLevel.Error,
        'Recovery',
        false
      ))
      return false
    }
  }

  private generateErrorId(): string {
    return `error_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`
  }

  private logError(error: GameError): void {
    const levelName = ErrorLevel[error.level]
    const timestamp = new Date(error.timestamp).toISOString()
    
    const logMessage = [
      `[${timestamp}] ${levelName}: ${error.message}`,
      error.context ? `Context: ${error.context}` : '',
      error.stack ? `Stack: ${error.stack}` : ''
    ].filter(Boolean).join('\n')

    switch (error.level) {
      case ErrorLevel.Info:
        console.info(logMessage)
        break
      case ErrorLevel.Warning:
        console.warn(logMessage)
        break
      case ErrorLevel.Error:
        console.error(logMessage)
        break
      case ErrorLevel.Critical:
        console.error('🚨 CRITICAL:', logMessage)
        break
    }
  }

  private notifyListeners(error: GameError): void {
    const listeners = this.errorListeners.get(error.level) || []
    listeners.forEach(callback => {
      try {
        callback(error)
      } catch (listenerError) {
        // リスナー自体のエラーは無限ループを避けるため最小限に記録
        console.error('Error in error listener:', listenerError)
      }
    })
  }

  private attemptRecovery(error: GameError): void {
    if (!error.recoverable) return

    // 自動復旧ロジック
    switch (error.level) {
      case ErrorLevel.Warning:
        // 警告レベルは通常継続可能
        this.recordError('警告レベルエラーを検出しましたが、ゲームを継続します', 'AutoRecovery')
        break
      case ErrorLevel.Error:
        // エラーレベルは状況に応じて復旧
        if (error.context === 'Rendering') {
          this.executeRecovery('fallback_renderer')
        }
        break
    }
  }

  private setupGlobalErrorHandlers(): void {
    // 未捕捉エラー
    window.addEventListener('error', (event) => {
      this.recordError(new PuyoGameError(
        event.message || 'Uncaught Error',
        ErrorLevel.Critical,
        `${event.filename}:${event.lineno}:${event.colno}`,
        false
      ))
    })

    // 未捕捉Promise拒否
    window.addEventListener('unhandledrejection', (event) => {
      this.recordError(new PuyoGameError(
        event.reason?.message || 'Unhandled Promise Rejection',
        ErrorLevel.Critical,
        'Promise',
        false
      ))
    })
  }
}

// グローバルエラーハンドラーシングルトン
export const globalErrorHandler = new ErrorHandler()

// 便利な関数
export function handleError(error: Error | string, context?: string): GameError {
  return globalErrorHandler.recordError(error, context)
}

export function handleValidationError(message: string, context?: string): void {
  globalErrorHandler.recordError(new ValidationError(message, context))
}

export function handleGameStateError(message: string, context?: string): void {
  globalErrorHandler.recordError(new GameStateError(message, context))
}

export function handleRenderingError(message: string, context?: string): void {
  globalErrorHandler.recordError(new RenderingError(message, context))
}