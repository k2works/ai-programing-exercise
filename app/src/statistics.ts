// 統計機能管理システム
export interface GameStatistics {
  // ゲーム統計
  totalGamesPlayed: number
  totalGamesWon: number
  totalScore: number
  highScore: number
  averageScore: number
  
  // プレイ時間統計
  totalPlayTime: number        // 秒単位
  longestSession: number       // 最長セッション（秒）
  averageSessionTime: number   // 平均セッション時間（秒）
  
  // ぷよ統計
  totalPuyoDropped: number     // 落としたぷよ数
  totalLinesCleared: number    // 消したライン数
  totalChainsMade: number      // 作ったチェーン数
  longestChain: number         // 最長チェーン
  averageChainLength: number   // 平均チェーン長
  
  // 効率統計
  puyoPerMinute: number        // 分間ぷよ数（APM）
  scorePerPuyo: number         // ぷよあたりスコア効率
  
  // 日付統計
  firstPlayDate: string        // 初回プレイ日
  lastPlayDate: string         // 最後のプレイ日
  playDays: number            // 異なる日にプレイした日数
  
  // レベル別統計
  levelStatistics: Record<number, {
    gamesPlayed: number
    bestScore: number
    averageScore: number
    totalPlayTime: number
  }>
  
  // 月別統計
  monthlyStats: Record<string, {  // YYYY-MM形式
    gamesPlayed: number
    totalScore: number
    totalPlayTime: number
    bestScore: number
  }>
}

export interface SessionData {
  sessionId: string
  startTime: number
  endTime?: number
  score: number
  level: number
  chainsCount: number
  longestChain: number
  puyoDropped: number
  linesCleared: number
  isCompleted: boolean
}

export class StatisticsManager {
  private static readonly STORAGE_KEY = 'puyo-puyo-statistics'
  private static readonly SESSION_KEY = 'puyo-puyo-current-session'
  
  private statistics: GameStatistics
  private currentSession: SessionData | null = null
  private sessionStartTime: number = 0
  
  constructor() {
    this.statistics = this.loadStatistics()
    this.loadCurrentSession()
  }

  private getDefaultStatistics(): GameStatistics {
    return {
      totalGamesPlayed: 0,
      totalGamesWon: 0,
      totalScore: 0,
      highScore: 0,
      averageScore: 0,
      
      totalPlayTime: 0,
      longestSession: 0,
      averageSessionTime: 0,
      
      totalPuyoDropped: 0,
      totalLinesCleared: 0,
      totalChainsMade: 0,
      longestChain: 0,
      averageChainLength: 0,
      
      puyoPerMinute: 0,
      scorePerPuyo: 0,
      
      firstPlayDate: '',
      lastPlayDate: '',
      playDays: 0,
      
      levelStatistics: {},
      monthlyStats: {}
    }
  }

  private loadStatistics(): GameStatistics {
    try {
      const stored = localStorage.getItem(StatisticsManager.STORAGE_KEY)
      if (stored) {
        const parsed = JSON.parse(stored)
        // デフォルト値とマージして不足プロパティを補完
        return { ...this.getDefaultStatistics(), ...parsed }
      }
    } catch (error) {
      console.warn('統計データの読み込みに失敗しました:', error)
    }
    
    return this.getDefaultStatistics()
  }

  private saveStatistics(): void {
    try {
      localStorage.setItem(StatisticsManager.STORAGE_KEY, JSON.stringify(this.statistics))
    } catch (error) {
      console.warn('統計データの保存に失敗しました:', error)
    }
  }

  private loadCurrentSession(): void {
    try {
      const stored = localStorage.getItem(StatisticsManager.SESSION_KEY)
      if (stored) {
        this.currentSession = JSON.parse(stored)
      }
    } catch (error) {
      console.warn('セッションデータの読み込みに失敗しました:', error)
      this.currentSession = null
    }
  }

  private saveCurrentSession(): void {
    try {
      if (this.currentSession) {
        localStorage.setItem(StatisticsManager.SESSION_KEY, JSON.stringify(this.currentSession))
      }
    } catch (error) {
      console.warn('セッションデータの保存に失敗しました:', error)
    }
  }

  private clearCurrentSession(): void {
    this.currentSession = null
    try {
      localStorage.removeItem(StatisticsManager.SESSION_KEY)
    } catch (error) {
      console.warn('セッションデータの削除に失敗しました:', error)
    }
  }

  // セッション開始
  startSession(level: number = 1): void {
    this.sessionStartTime = Date.now()
    
    this.currentSession = {
      sessionId: `session_${this.sessionStartTime}`,
      startTime: this.sessionStartTime,
      score: 0,
      level,
      chainsCount: 0,
      longestChain: 0,
      puyoDropped: 0,
      linesCleared: 0,
      isCompleted: false
    }
    
    this.saveCurrentSession()
  }

  // セッション終了
  endSession(finalScore: number, completed: boolean = false): void {
    if (!this.currentSession) return

    const endTime = Date.now()
    const sessionDuration = Math.floor((endTime - this.currentSession.startTime) / 1000)
    
    this.currentSession.endTime = endTime
    this.currentSession.score = finalScore
    this.currentSession.isCompleted = completed
    
    // 統計更新
    this.updateStatisticsFromSession(this.currentSession, sessionDuration)
    
    // セッションをクリア
    this.clearCurrentSession()
  }

  private updateStatisticsFromSession(session: SessionData, duration: number): void {
    const today = new Date().toISOString().split('T')[0] // YYYY-MM-DD
    const month = today.substring(0, 7) // YYYY-MM
    
    // 基本統計更新
    this.statistics.totalGamesPlayed++
    if (session.isCompleted) {
      this.statistics.totalGamesWon++
    }
    
    this.statistics.totalScore += session.score
    this.statistics.highScore = Math.max(this.statistics.highScore, session.score)
    this.statistics.averageScore = this.statistics.totalScore / this.statistics.totalGamesPlayed
    
    // プレイ時間統計
    this.statistics.totalPlayTime += duration
    this.statistics.longestSession = Math.max(this.statistics.longestSession, duration)
    this.statistics.averageSessionTime = this.statistics.totalPlayTime / this.statistics.totalGamesPlayed
    
    // ぷよ統計
    this.statistics.totalPuyoDropped += session.puyoDropped
    this.statistics.totalLinesCleared += session.linesCleared
    this.statistics.totalChainsMade += session.chainsCount
    this.statistics.longestChain = Math.max(this.statistics.longestChain, session.longestChain)
    
    if (this.statistics.totalChainsMade > 0) {
      // 平均チェーン長の計算は簡略化（実際にはより複雑な計算が必要）
      this.statistics.averageChainLength = this.statistics.totalChainsMade / this.statistics.totalGamesPlayed
    }
    
    // 効率統計
    if (duration > 0) {
      this.statistics.puyoPerMinute = (this.statistics.totalPuyoDropped / this.statistics.totalPlayTime) * 60
    }
    if (this.statistics.totalPuyoDropped > 0) {
      this.statistics.scorePerPuyo = this.statistics.totalScore / this.statistics.totalPuyoDropped
    }
    
    // 日付統計
    if (!this.statistics.firstPlayDate) {
      this.statistics.firstPlayDate = today
    }
    this.statistics.lastPlayDate = today
    
    // プレイした日数の計算（簡略化）
    this.statistics.playDays = Math.max(this.statistics.playDays, 1)
    
    // レベル別統計
    if (!this.statistics.levelStatistics[session.level]) {
      this.statistics.levelStatistics[session.level] = {
        gamesPlayed: 0,
        bestScore: 0,
        averageScore: 0,
        totalPlayTime: 0
      }
    }
    
    const levelStats = this.statistics.levelStatistics[session.level]
    levelStats.gamesPlayed++
    levelStats.bestScore = Math.max(levelStats.bestScore, session.score)
    levelStats.totalPlayTime += duration
    levelStats.averageScore = (levelStats.averageScore * (levelStats.gamesPlayed - 1) + session.score) / levelStats.gamesPlayed
    
    // 月別統計
    if (!this.statistics.monthlyStats[month]) {
      this.statistics.monthlyStats[month] = {
        gamesPlayed: 0,
        totalScore: 0,
        totalPlayTime: 0,
        bestScore: 0
      }
    }
    
    const monthStats = this.statistics.monthlyStats[month]
    monthStats.gamesPlayed++
    monthStats.totalScore += session.score
    monthStats.totalPlayTime += duration
    monthStats.bestScore = Math.max(monthStats.bestScore, session.score)
    
    this.saveStatistics()
  }

  // セッション中の統計更新
  updateSessionStats(data: {
    score?: number
    chainsCount?: number
    longestChain?: number
    puyoDropped?: number
    linesCleared?: number
  }): void {
    if (!this.currentSession) return
    
    if (data.score !== undefined) {
      this.currentSession.score = data.score
    }
    if (data.chainsCount !== undefined) {
      this.currentSession.chainsCount = data.chainsCount
    }
    if (data.longestChain !== undefined) {
      this.currentSession.longestChain = Math.max(this.currentSession.longestChain, data.longestChain)
    }
    if (data.puyoDropped !== undefined) {
      this.currentSession.puyoDropped += data.puyoDropped
    }
    if (data.linesCleared !== undefined) {
      this.currentSession.linesCleared += data.linesCleared
    }
    
    this.saveCurrentSession()
  }

  // 統計データ取得
  getStatistics(): GameStatistics {
    return { ...this.statistics }
  }

  // 現在のセッション取得
  getCurrentSession(): SessionData | null {
    return this.currentSession ? { ...this.currentSession } : null
  }

  // 統計データリセット
  resetStatistics(): void {
    this.statistics = this.getDefaultStatistics()
    this.saveStatistics()
  }

  // ランキング取得（スコア上位）
  getHighScoreRanking(): Array<{ score: number; date: string; level: number }> {
    // 実際の実装では、個々のゲーム記録を保存する必要がある
    // ここでは簡略化して、現在のハイスコアのみを返す
    if (this.statistics.highScore > 0) {
      return [{
        score: this.statistics.highScore,
        date: this.statistics.lastPlayDate,
        level: 1 // 簡略化
      }]
    }
    return []
  }

  // 統計サマリー取得
  getStatisticsSummary(): {
    gamesPlayed: number
    winRate: number
    averageScore: number
    totalPlayTime: string
    favoriteLevel: number
    bestMonth: string
  } {
    const winRate = this.statistics.totalGamesPlayed > 0 
      ? (this.statistics.totalGamesWon / this.statistics.totalGamesPlayed) * 100 
      : 0

    const totalPlayTimeFormatted = this.formatDuration(this.statistics.totalPlayTime)
    
    // 最も多くプレイしたレベルを取得
    let favoriteLevel = 1
    let maxGames = 0
    for (const [level, stats] of Object.entries(this.statistics.levelStatistics)) {
      if (stats.gamesPlayed > maxGames) {
        maxGames = stats.gamesPlayed
        favoriteLevel = parseInt(level)
      }
    }
    
    // 最高スコアを記録した月を取得
    let bestMonth = ''
    let bestScore = 0
    for (const [month, stats] of Object.entries(this.statistics.monthlyStats)) {
      if (stats.bestScore > bestScore) {
        bestScore = stats.bestScore
        bestMonth = month
      }
    }
    
    return {
      gamesPlayed: this.statistics.totalGamesPlayed,
      winRate: Math.round(winRate),
      averageScore: Math.round(this.statistics.averageScore),
      totalPlayTime: totalPlayTimeFormatted,
      favoriteLevel,
      bestMonth
    }
  }

  // 進捗データ取得（達成度など）
  getProgressData(): {
    achievements: Array<{
      name: string
      description: string
      completed: boolean
      progress: number
      target: number
    }>
    milestones: Array<{
      name: string
      value: number
      unit: string
      rank: 'bronze' | 'silver' | 'gold' | 'platinum'
    }>
  } {
    const achievements = [
      {
        name: '初心者',
        description: '10ゲームプレイする',
        completed: this.statistics.totalGamesPlayed >= 10,
        progress: this.statistics.totalGamesPlayed,
        target: 10
      },
      {
        name: 'チェーンマスター',
        description: '5チェーン以上を作る',
        completed: this.statistics.longestChain >= 5,
        progress: this.statistics.longestChain,
        target: 5
      },
      {
        name: '高得点',
        description: '10,000点以上を獲得する',
        completed: this.statistics.highScore >= 10000,
        progress: this.statistics.highScore,
        target: 10000
      },
      {
        name: '継続は力なり',
        description: '総プレイ時間1時間達成',
        completed: this.statistics.totalPlayTime >= 3600,
        progress: this.statistics.totalPlayTime,
        target: 3600
      }
    ]

    const milestones = []
    
    // スコアマイルストーン
    if (this.statistics.highScore >= 50000) {
      milestones.push({ name: 'ハイスコア', value: this.statistics.highScore, unit: '点', rank: 'platinum' as const })
    } else if (this.statistics.highScore >= 25000) {
      milestones.push({ name: 'ハイスコア', value: this.statistics.highScore, unit: '点', rank: 'gold' as const })
    } else if (this.statistics.highScore >= 10000) {
      milestones.push({ name: 'ハイスコア', value: this.statistics.highScore, unit: '点', rank: 'silver' as const })
    } else if (this.statistics.highScore >= 5000) {
      milestones.push({ name: 'ハイスコア', value: this.statistics.highScore, unit: '点', rank: 'bronze' as const })
    }
    
    // チェーンマイルストーン
    if (this.statistics.longestChain >= 10) {
      milestones.push({ name: '最長チェーン', value: this.statistics.longestChain, unit: 'チェーン', rank: 'platinum' as const })
    } else if (this.statistics.longestChain >= 7) {
      milestones.push({ name: '最長チェーン', value: this.statistics.longestChain, unit: 'チェーン', rank: 'gold' as const })
    } else if (this.statistics.longestChain >= 5) {
      milestones.push({ name: '最長チェーン', value: this.statistics.longestChain, unit: 'チェーン', rank: 'silver' as const })
    } else if (this.statistics.longestChain >= 3) {
      milestones.push({ name: '最長チェーン', value: this.statistics.longestChain, unit: 'チェーン', rank: 'bronze' as const })
    }

    return { achievements, milestones }
  }

  private formatDuration(seconds: number): string {
    if (seconds < 60) {
      return `${seconds}秒`
    } else if (seconds < 3600) {
      const minutes = Math.floor(seconds / 60)
      const remainingSeconds = seconds % 60
      return remainingSeconds > 0 ? `${minutes}分${remainingSeconds}秒` : `${minutes}分`
    } else {
      const hours = Math.floor(seconds / 3600)
      const remainingMinutes = Math.floor((seconds % 3600) / 60)
      return remainingMinutes > 0 ? `${hours}時間${remainingMinutes}分` : `${hours}時間`
    }
  }

  // 統計データエクスポート
  exportStatistics(): string {
    return JSON.stringify({
      statistics: this.statistics,
      exportDate: new Date().toISOString(),
      version: '1.0'
    }, null, 2)
  }

  // 統計データインポート
  importStatistics(jsonString: string): boolean {
    try {
      const data = JSON.parse(jsonString)
      
      if (data.statistics && typeof data.statistics === 'object') {
        // データ整合性チェック（基本的なもののみ）
        const imported = { ...this.getDefaultStatistics(), ...data.statistics }
        this.statistics = imported
        this.saveStatistics()
        return true
      }
      
      return false
    } catch (error) {
      console.warn('統計データのインポートに失敗しました:', error)
      return false
    }
  }
}

// グローバル統計マネージャー
export const statisticsManager = new StatisticsManager()