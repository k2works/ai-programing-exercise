import { describe, it, expect, beforeEach, vi, afterEach, afterAll } from 'vitest'
import { StatisticsManager, GameStatistics, SessionData } from './statistics'

// localStorage のモック化
const localStorageMock = {
  getItem: vi.fn(),
  setItem: vi.fn(),
  removeItem: vi.fn(),
  clear: vi.fn()
}

Object.defineProperty(window, 'localStorage', {
  value: localStorageMock
})

// Dateのモック化
const mockDate = new Date('2023-12-01T10:00:00Z')
const originalDate = Date
;(globalThis as any).Date = vi.fn(() => mockDate) as any
Date.now = vi.fn(() => mockDate.getTime())
Date.prototype.toISOString = vi.fn(() => mockDate.toISOString())

describe('StatisticsManager', () => {
  let statisticsManager: StatisticsManager
  
  beforeEach(() => {
    vi.clearAllMocks()
    localStorageMock.getItem.mockReturnValue(null)
    statisticsManager = new StatisticsManager()
  })

  afterEach(() => {
    vi.clearAllMocks()
  })

  describe('初期化', () => {
    it('StatisticsManagerを正常に作成できる', () => {
      expect(statisticsManager).toBeInstanceOf(StatisticsManager)
    })

    it('デフォルト統計が正しく初期化される', () => {
      const stats = statisticsManager.getStatistics()
      expect(stats.totalGamesPlayed).toBe(0)
      expect(stats.totalScore).toBe(0)
      expect(stats.highScore).toBe(0)
      expect(stats.totalPlayTime).toBe(0)
    })

    it('保存済み統計が正しく読み込まれる', () => {
      const savedStats = {
        totalGamesPlayed: 5,
        highScore: 12000,
        totalPlayTime: 300
      }
      localStorageMock.getItem.mockReturnValue(JSON.stringify(savedStats))
      
      const manager = new StatisticsManager()
      const stats = manager.getStatistics()
      
      expect(stats.totalGamesPlayed).toBe(5)
      expect(stats.highScore).toBe(12000)
      expect(stats.totalPlayTime).toBe(300)
    })
  })

  describe('セッション管理', () => {
    it('セッションを開始できる', () => {
      statisticsManager.startSession(1)
      const session = statisticsManager.getCurrentSession()
      
      expect(session).not.toBeNull()
      expect(session!.level).toBe(1)
      expect(session!.score).toBe(0)
      expect(session!.startTime).toBe(mockDate.getTime())
    })

    it('セッション中の統計を更新できる', () => {
      statisticsManager.startSession(1)
      
      statisticsManager.updateSessionStats({
        score: 5000,
        chainsCount: 3,
        longestChain: 4,
        puyoDropped: 20,
        linesCleared: 5
      })
      
      const session = statisticsManager.getCurrentSession()
      expect(session!.score).toBe(5000)
      expect(session!.chainsCount).toBe(3)
      expect(session!.longestChain).toBe(4)
      expect(session!.puyoDropped).toBe(20)
      expect(session!.linesCleared).toBe(5)
    })

    it('セッションを終了して統計を更新できる', () => {
      statisticsManager.startSession(1)
      statisticsManager.updateSessionStats({
        score: 8000,
        chainsCount: 2,
        longestChain: 3,
        puyoDropped: 30,
        linesCleared: 8
      })
      
      // 1分後にセッション終了
      const endTime = mockDate.getTime() + 60000
      Date.now = vi.fn(() => endTime)
      
      statisticsManager.endSession(8000, true)
      
      const stats = statisticsManager.getStatistics()
      expect(stats.totalGamesPlayed).toBe(1)
      expect(stats.totalGamesWon).toBe(1)
      expect(stats.totalScore).toBe(8000)
      expect(stats.highScore).toBe(8000)
      expect(stats.totalPlayTime).toBe(60)
      expect(stats.totalPuyoDropped).toBe(30)
      expect(stats.totalLinesCleared).toBe(8)
      
      // セッションがクリアされることを確認
      expect(statisticsManager.getCurrentSession()).toBeNull()
    })
  })

  describe('統計計算', () => {
    beforeEach(() => {
      // 複数セッションのテストデータセットアップ
      statisticsManager.startSession(1)
      statisticsManager.updateSessionStats({
        score: 5000,
        chainsCount: 2,
        longestChain: 3,
        puyoDropped: 20,
        linesCleared: 4
      })
      statisticsManager.endSession(5000, true)
      
      statisticsManager.startSession(2)
      statisticsManager.updateSessionStats({
        score: 12000,
        chainsCount: 4,
        longestChain: 6,
        puyoDropped: 40,
        linesCleared: 10
      })
      statisticsManager.endSession(12000, false)
    })

    it('平均スコアが正しく計算される', () => {
      const stats = statisticsManager.getStatistics()
      expect(stats.averageScore).toBe(8500) // (5000 + 12000) / 2
    })

    it('ハイスコアが正しく更新される', () => {
      const stats = statisticsManager.getStatistics()
      expect(stats.highScore).toBe(12000)
    })

    it('最長チェーンが正しく記録される', () => {
      const stats = statisticsManager.getStatistics()
      expect(stats.longestChain).toBe(6)
    })

    it('勝率が正しく計算される', () => {
      const summary = statisticsManager.getStatisticsSummary()
      expect(summary.winRate).toBe(50) // 1勝 / 2ゲーム = 50%
    })

    it('レベル別統計が正しく記録される', () => {
      const stats = statisticsManager.getStatistics()
      
      expect(stats.levelStatistics[1].gamesPlayed).toBe(1)
      expect(stats.levelStatistics[1].bestScore).toBe(5000)
      expect(stats.levelStatistics[2].gamesPlayed).toBe(1)
      expect(stats.levelStatistics[2].bestScore).toBe(12000)
    })
  })

  describe('統計サマリー', () => {
    it('統計サマリーを正しく生成できる', () => {
      statisticsManager.startSession(1)
      statisticsManager.endSession(10000, true)
      
      const summary = statisticsManager.getStatisticsSummary()
      
      expect(summary.gamesPlayed).toBe(1)
      expect(summary.winRate).toBe(100)
      expect(summary.averageScore).toBe(10000)
      expect(summary.favoriteLevel).toBe(1)
    })

    it('プレイ時間が正しくフォーマットされる', () => {
      // 長時間セッションをシミュレート
      statisticsManager.startSession(1)
      const endTime = mockDate.getTime() + 7260000 // 2時間1分後
      Date.now = vi.fn(() => endTime)
      statisticsManager.endSession(15000, true)
      
      const summary = statisticsManager.getStatisticsSummary()
      expect(summary.totalPlayTime).toContain('時間')
    })
  })

  describe('達成度とマイルストーン', () => {
    it('達成度を正しく計算できる', () => {
      // 複数ゲームをプレイして達成度をテスト
      for (let i = 0; i < 12; i++) {
        statisticsManager.startSession(1)
        statisticsManager.updateSessionStats({
          score: 8000,
          longestChain: 6
        })
        statisticsManager.endSession(8000, true)
      }
      
      const progress = statisticsManager.getProgressData()
      
      // 「初心者」達成度をチェック（10ゲーム以上）
      const beginnerAchievement = progress.achievements.find(a => a.name === '初心者')
      expect(beginnerAchievement?.completed).toBe(true)
      
      // 「チェーンマスター」達成度をチェック（5チェーン以上）
      const chainAchievement = progress.achievements.find(a => a.name === 'チェーンマスター')
      expect(chainAchievement?.completed).toBe(true)
    })

    it('マイルストーンを正しく判定できる', () => {
      statisticsManager.startSession(1)
      statisticsManager.updateSessionStats({
        score: 15000,
        longestChain: 8
      })
      statisticsManager.endSession(15000, true)
      
      const progress = statisticsManager.getProgressData()
      
      expect(progress.milestones.length).toBeGreaterThan(0)
      const scoresMilestone = progress.milestones.find(m => m.name === 'ハイスコア')
      expect(scoresMilestone?.value).toBe(15000)
    })
  })

  describe('データの永続化', () => {
    it('統計データをリセットできる', () => {
      statisticsManager.startSession(1)
      statisticsManager.endSession(5000, true)
      
      statisticsManager.resetStatistics()
      
      const stats = statisticsManager.getStatistics()
      expect(stats.totalGamesPlayed).toBe(0)
      expect(stats.totalScore).toBe(0)
      expect(stats.highScore).toBe(0)
    })

    it('統計データをエクスポートできる', () => {
      statisticsManager.startSession(1)
      statisticsManager.endSession(7000, true)
      
      const exported = statisticsManager.exportStatistics()
      const data = JSON.parse(exported)
      
      expect(data.statistics.totalGamesPlayed).toBe(1)
      expect(data.statistics.totalScore).toBe(7000)
      expect(data.exportDate).toBeDefined()
      expect(data.version).toBe('1.0')
    })

    it('統計データをインポートできる', () => {
      const importData = {
        statistics: {
          totalGamesPlayed: 15,
          highScore: 25000,
          totalScore: 180000
        },
        exportDate: '2023-12-01T12:00:00.000Z',
        version: '1.0'
      }
      
      const success = statisticsManager.importStatistics(JSON.stringify(importData))
      expect(success).toBe(true)
      
      const stats = statisticsManager.getStatistics()
      expect(stats.totalGamesPlayed).toBe(15)
      expect(stats.highScore).toBe(25000)
      expect(stats.totalScore).toBe(180000)
    })

    it('不正なデータのインポートは失敗する', () => {
      const success = statisticsManager.importStatistics('invalid json')
      expect(success).toBe(false)
    })
  })

  describe('ハイスコアランキング', () => {
    it('ハイスコアランキングを取得できる', () => {
      statisticsManager.startSession(1)
      statisticsManager.endSession(20000, true)
      
      const ranking = statisticsManager.getHighScoreRanking()
      
      expect(ranking.length).toBe(1)
      expect(ranking[0].score).toBe(20000)
      expect(ranking[0].date).toBe('2023-12-01')
    })

    it('スコアがない場合は空のランキングを返す', () => {
      const ranking = statisticsManager.getHighScoreRanking()
      expect(ranking).toEqual([])
    })
  })
})

// Dateを元に戻す
afterAll(() => {
  ;(globalThis as any).Date = originalDate
})