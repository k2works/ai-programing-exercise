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
})
