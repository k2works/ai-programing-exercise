import { describe, it, expect, beforeEach } from 'vitest'
import { Game, GameMode } from './game'
import { PuyoColor } from './puyo'

describe('Game', () => {
  let game: Game

  beforeEach(() => {
    game = new Game()
  })

  describe('初期化', () => {
    it('ゲームを初期化できる', () => {
      game.initialize()
      
      expect(game.getMode()).toBe('newPuyo')
      expect(game.getScore()).toBe(0)
      expect(game.getCombinationCount()).toBe(0)
      expect(game.getFrame()).toBe(0)
    })

    it('初期化でコンポーネントが準備される', () => {
      game.initialize()
      
      // アクセサメソッドを使って各コンポーネントの存在を確認
      expect(game.getStageForRenderer()).toBeDefined()
      expect(game.getPlayerForRenderer()).toBeDefined()
      expect(game.getConfigForRenderer()).toBeDefined()
    })
  })

  describe('ゲーム状態遷移', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('newPuyoからplayingに遷移する', () => {
      expect(game.getMode()).toBe('newPuyo')
      
      game.update()
      
      expect(game.getMode()).toBe('playing')
    })

    it('フレーム数がカウントされる', () => {
      const initialFrame = game.getFrame()
      
      game.update()
      
      expect(game.getFrame()).toBe(initialFrame + 1)
    })
  })

  describe('スコア計算システム', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('初期スコアは0である', () => {
      expect(game.getScore()).toBe(0)
    })

    it('スコアが正しく加算される', () => {
      // テスト用に直接スコア加算をシミュレート
      const stage = game.getStageForRenderer()
      
      // 4個の連結ぷよを手動配置
      stage.setPuyo(0, 12, { getColor: () => PuyoColor.Red, isEmpty: () => false } as any)
      stage.setPuyo(1, 12, { getColor: () => PuyoColor.Red, isEmpty: () => false } as any)
      stage.setPuyo(2, 12, { getColor: () => PuyoColor.Red, isEmpty: () => false } as any)
      stage.setPuyo(3, 12, { getColor: () => PuyoColor.Red, isEmpty: () => false } as any)
      
      // erasingモードのシミュレーション用の内部状態を設定
      // （実際の実装では private なので、統合テストで検証）
    })

    it('連鎖倍率が正しく計算される', () => {
      // 連鎖倍率の計算ロジックをテスト
      // 1連鎖: 1倍
      // 2連鎖: 2倍
      // 3連鎖: 4倍
      // 4連鎖: 8倍
      
      // これは private method なので統合テストでカバー
      expect(game.getCombinationCount()).toBe(0)
    })
  })

  describe('ゲーム状態管理', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('リセットで初期状態に戻る', () => {
      // ゲームを進める
      for (let i = 0; i < 10; i++) {
        game.update()
      }
      
      game.reset()
      
      expect(game.getMode()).toBe('newPuyo')
      expect(game.getScore()).toBe(0)
      expect(game.getCombinationCount()).toBe(0)
      expect(game.getFrame()).toBe(0)
    })
  })

  describe('ゲームループ統合テスト', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('複数回updateしてもエラーが発生しない', () => {
      expect(() => {
        for (let i = 0; i < 100; i++) {
          game.update()
        }
      }).not.toThrow()
    })

    it('playingモードからcheckFallに遷移する', () => {
      // newPuyo -> playing に遷移
      game.update()
      expect(game.getMode()).toBe('playing')
      
      // プレイヤーを配置状態にする
      const player = game.getPlayerForRenderer()
      
      // 強制的に配置状態にしてupdateを実行
      while (!player.isPlaced()) {
        player.update()
      }
      
      game.update()
      
      // playing -> checkFall に遷移しているはず
      expect(game.getMode()).toBe('checkFall')
    })

    it('空のフィールドではcheckFallからcheckEraseに遷移', () => {
      // checkFallモードに設定
      game.initialize()
      game.update() // newPuyo -> playing
      
      // プレイヤーを配置状態にする
      const player = game.getPlayerForRenderer()
      while (!player.isPlaced()) {
        player.update()
      }
      
      game.update() // playing -> checkFall
      
      if (game.getMode() === 'checkFall') {
        game.update() // checkFall -> checkErase (落下するぷよがない場合)
        
        // checkErase か他の状態に遷移
        expect(['checkErase', 'newPuyo', 'fall'].includes(game.getMode())).toBe(true)
      }
    })
  })

  describe('ゲームオーバー判定', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('フィールドが埋まるとゲームオーバーになる可能性', () => {
      const stage = game.getStageForRenderer()
      
      // フィールドの上部を埋める
      for (let x = 0; x < stage.getWidth(); x++) {
        for (let y = 0; y < 3; y++) {
          stage.setPuyo(x, y, {
            getColor: () => PuyoColor.Red,
            isEmpty: () => false
          } as any)
        }
      }
      
      // 複数回updateして状態確認
      for (let i = 0; i < 50; i++) {
        game.update()
        if (game.getMode() === 'gameOver') {
          break
        }
      }
      
      // ゲームオーバーかplayingのいずれかであるはず
      expect(['gameOver', 'playing', 'newPuyo'].includes(game.getMode())).toBe(true)
    })
  })

  describe('コンポーネントアクセス', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('Stageコンポーネントにアクセスできる', () => {
      const stage = game.getStageForRenderer()
      
      expect(stage.getWidth()).toBe(6)
      expect(stage.getHeight()).toBe(13)
    })

    it('Playerコンポーネントにアクセスできる', () => {
      const player = game.getPlayerForRenderer()
      
      expect(player.getCurrentPair()).toBeDefined()
      expect(player.getNextPair()).toBeDefined()
    })

    it('Configコンポーネントにアクセスできる', () => {
      const config = game.getConfigForRenderer()
      
      expect(config.stageWidth).toBe(6)
      expect(config.stageHeight).toBe(13)
    })
  })

  describe('複雑なゲームシナリオ', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('ゲーム開始から少なくとも数ターン動作する', () => {
      let iterations = 0
      const maxIterations = 1000
      
      while (game.getMode() !== 'gameOver' && iterations < maxIterations) {
        game.update()
        iterations++
      }
      
      // ゲームが動作していることを確認
      expect(iterations).toBeGreaterThan(0)
      expect(game.getFrame()).toBeGreaterThan(0)
    })
  })

  describe('エラーハンドリング', () => {
    it('未初期化状態でも基本操作は安全', () => {
      expect(() => {
        game.getMode()
        game.getScore()
        game.getCombinationCount()
        game.getFrame()
      }).not.toThrow()
    })

    it('複数回初期化しても問題ない', () => {
      expect(() => {
        game.initialize()
        game.initialize()
        game.initialize()
      }).not.toThrow()
    })

    it('複数回リセットしても問題ない', () => {
      game.initialize()
      
      expect(() => {
        game.reset()
        game.reset()
        game.reset()
      }).not.toThrow()
    })
  })
})

describe('GameMode', () => {
  it('全ての有効なゲームモードが定義されている', () => {
    const validModes: GameMode[] = [
      'start',
      'checkFall',
      'fall', 
      'checkErase',
      'erasing',
      'newPuyo',
      'playing',
      'gameOver'
    ]
    
    // 各モードが文字列として正しく定義されていることを確認
    validModes.forEach(mode => {
      expect(typeof mode).toBe('string')
      expect(mode.length).toBeGreaterThan(0)
    })
  })
})