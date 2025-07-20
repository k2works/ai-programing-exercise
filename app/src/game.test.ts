import { describe, it, expect, beforeEach } from 'vitest'
import { Game } from './game'
import { Config } from './config'
import { Stage } from './stage'
import { PuyoImage } from './puyoimage'
import { Player } from './player'
import { Score } from './score'
import { Puyo, PuyoColor } from './puyo'

describe('ゲーム', () => {
  let game: Game

  beforeEach(() => {
    // DOMの準備
    document.body.innerHTML = `
            <div id="stage"></div>
            <div id="score"></div>
            <div id="next"></div>
            <div id="next2"></div>
        `
    game = new Game()
  })

  describe('ゲームの初期化', () => {
    it('ゲームを初期化すると、必要なコンポーネントが作成される', () => {
      game.initialize()

      expect(game['config']).toBeInstanceOf(Config)
      expect(game['puyoImage']).toBeInstanceOf(PuyoImage)
      expect(game['stage']).toBeInstanceOf(Stage)
      expect(game['player']).toBeInstanceOf(Player)
      expect(game['score']).toBeInstanceOf(Score)
    })

    it('ゲームを初期化すると、ゲームモードがstartになる', () => {
      game.initialize()

      expect(game['mode']).toEqual('start')
    })
  })

  describe('ゲームループ', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('updateメソッドが存在する', () => {
      expect(typeof game.update).toBe('function')
    })

    it('startモードからnewPuyoモードに遷移する', () => {
      expect(game['mode']).toEqual('start')
      
      game.update()
      
      expect(game['mode']).toEqual('newPuyo')
    })

    it('newPuyoモードからplayingモードに遷移する', () => {
      game['mode'] = 'newPuyo'
      
      game.update()
      
      expect(game['mode']).toEqual('playing')
    })

    it('updateが呼ばれるとフレームカウンターが増加する', () => {
      const initialFrame = game['frame']
      
      game.update()
      
      expect(game['frame']).toEqual(initialFrame + 1)
    })
  })

  describe('落下処理', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('playingモードでプレイヤーのぷよが配置されるとcheckFallモードに遷移する', () => {
      game['mode'] = 'playing'
      
      // プレイヤーのぷよを強制的に配置状態にする
      game['player']['placed'] = true
      
      game.update()
      
      expect(game['mode']).toEqual('checkFall')
    })

    it('checkFallモードで落下が必要な場合、fallモードに遷移する', () => {
      // ステージに浮いているぷよを配置
      const stage = game['stage']
      const puyo = new Puyo(PuyoColor.Red, 2, 2)
      stage.setPuyo(2, 2, puyo)
      
      game['mode'] = 'checkFall'
      game.update()
      
      expect(game['mode']).toEqual('fall')
    })

    it('checkFallモードで落下が不要な場合、checkEraseモードに遷移する', () => {
      game['mode'] = 'checkFall'
      game.update()
      
      expect(game['mode']).toEqual('checkErase')
    })
  })

  describe('プレイヤー操作統合', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('playingモードでプレイヤーが更新される', () => {
      game['mode'] = 'playing'
      const initialFallTimer = game['player']['fallTimer']
      
      game.update()
      
      expect(game['player']['fallTimer']).toBe(initialFallTimer + 1)
    })

    it('newPuyoモードで新しいぷよペアが生成される', () => {
      const oldCurrentPair = game['player'].getCurrentPair()
      
      game['mode'] = 'newPuyo'
      game.update()
      
      const newCurrentPair = game['player'].getCurrentPair()
      expect(newCurrentPair).not.toBe(oldCurrentPair)
    })

    it('プレイヤーのぷよが配置されたらcheckFallモードに遷移する', () => {
      game['mode'] = 'playing'
      game['player']['placed'] = true
      
      game.update()
      
      expect(game['mode']).toEqual('checkFall')
    })
  })

  describe('消去処理', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('checkEraseモードで消去対象のぷよがある場合、erasingモードに遷移する', () => {
      // 4個の同色ぷよを配置
      const stage = game['stage']
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      game['mode'] = 'checkErase'
      game.update()

      expect(game['mode']).toEqual('erasing')
    })

    it('checkEraseモードで消去対象のぷよがない場合、newPuyoモードに遷移する', () => {
      game['mode'] = 'checkErase'
      game.update()

      expect(game['mode']).toEqual('newPuyo')
    })

    it('erasingモードでぷよが実際に消去される', () => {
      // 4個の同色ぷよを配置
      const stage = game['stage']
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      // 消去グループを事前に設定
      const erasableGroups = stage.findErasableGroups()
      game['erasableGroups'] = erasableGroups

      game['mode'] = 'erasing'
      game.update()

      // ぷよが消去されていることを確認
      for (let y = 9; y < 13; y++) {
        expect(stage.isEmpty(2, y)).toBe(true)
      }
      expect(game['mode']).toEqual('checkFall')
    })

    it('消去時にスコアが加算される', () => {
      // 4個の同色ぷよを配置
      const stage = game['stage']
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      const initialScore = game['score'].getScore()
      
      // 消去グループを事前に設定
      const erasableGroups = stage.findErasableGroups()
      game['erasableGroups'] = erasableGroups

      game['mode'] = 'erasing'
      game.update()

      expect(game['score'].getScore()).toBeGreaterThan(initialScore)
    })
  })

  describe('連鎖システム', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('連鎖カウンターが正しく初期化される', () => {
      expect(game['combinationCount']).toBe(0)
    })

    it('ぷよが消去されると連鎖カウンターが増加する', () => {
      // 4個の同色ぷよを配置
      const stage = game['stage']
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      game['mode'] = 'checkErase'
      game.update() // checkErase -> erasing
      game.update() // erasing -> checkFall

      expect(game['combinationCount']).toBe(1)
    })

    it('連鎖が終了するとカウンターがリセットされる', () => {
      // 連鎖カウンターを設定
      game['combinationCount'] = 3

      // 消去対象なしでcheckErase
      game['mode'] = 'checkErase'
      game.update() // checkErase -> newPuyo

      expect(game['combinationCount']).toBe(0)
    })

    it('連鎖倍率によってスコアが増加する', () => {
      // 4個の同色ぷよを配置
      const stage = game['stage']
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      // 連鎖カウンターを設定（2連鎖目）
      game['combinationCount'] = 1

      const initialScore = game['score'].getScore()
      
      game['mode'] = 'checkErase'
      game.update() // checkErase -> erasing
      game.update() // erasing -> checkFall

      const scoreIncrease = game['score'].getScore() - initialScore
      // 2連鎖なので基本スコア(40)より高くなるはず
      expect(scoreIncrease).toBeGreaterThan(40)
    })
  })

  describe('ゲームオーバー判定', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('新しいぷよが配置できない場合、ゲームオーバーになる', () => {
      // ステージ上部を埋める
      const stage = game['stage']
      for (let y = 0; y < 3; y++) {
        for (let x = 0; x < stage.getWidth(); x++) {
          const puyo = new Puyo(PuyoColor.Red, x, y)
          stage.setPuyo(x, y, puyo)
        }
      }

      game['mode'] = 'newPuyo'
      game.update()

      expect(game['mode']).toBe('gameOver')
    })

    it('ゲームオーバー状態では更新されない', () => {
      game['mode'] = 'gameOver'
      const initialFrame = game['frame']
      
      game.update()
      
      expect(game['frame']).toBe(initialFrame + 1) // フレームは進むが
      expect(game['mode']).toBe('gameOver') // モードは変わらない
    })

    it('ゲームをリセットできる', () => {
      // ゲームオーバー状態にする
      game['mode'] = 'gameOver'
      game['combinationCount'] = 5
      game['score'].addScore(1000)

      game.reset()

      expect(game['mode']).toBe('start')
      expect(game['combinationCount']).toBe(0)
      expect(game['score'].getScore()).toBe(0)
    })
  })

  describe('統合テスト', () => {
    beforeEach(() => {
      game.initialize()
    })

    it('基本的なゲームフロー：配置→消去→新しいぷよ', () => {
      const stage = game['stage']
      
      // 4個の同色ぷよを配置
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      const initialScore = game['score'].getScore()

      // 消去フロー
      game['mode'] = 'checkErase'
      game.update() // checkErase -> erasing 
      
      game.update() // erasing -> checkFall (消去実行)
      expect(game['combinationCount']).toBe(1)
      
      game.update() // checkFall -> checkErase (落下なし)
      game.update() // checkErase -> newPuyo (消去対象なし、連鎖終了)
      
      expect(game['combinationCount']).toBe(0) // 連鎖終了でリセット
      expect(game['mode']).toBe('newPuyo')
      expect(game['score'].getScore()).toBeGreaterThan(initialScore)
    })
  })
})
