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

    it('playingモードで落下が必要な場合、checkFallモードに遷移する', () => {
      // ステージに浮いているぷよを配置
      const stage = game['stage']
      const puyo = new Puyo(PuyoColor.Red, 2, 2)
      stage.setPuyo(2, 2, puyo)
      
      game['mode'] = 'playing'
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

    it('checkFallモードで落下が不要な場合、newPuyoモードに遷移する', () => {
      game['mode'] = 'checkFall'
      game.update()
      
      expect(game['mode']).toEqual('newPuyo')
    })
  })
})
