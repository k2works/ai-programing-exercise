import { describe, it, expect, beforeEach } from 'vitest'
import { Stage } from './stage'
import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Puyo, PuyoColor } from './puyo'

describe('ステージ', () => {
  let stage: Stage
  let config: Config
  let puyoImage: PuyoImage

  beforeEach(() => {
    config = new Config()
    puyoImage = new PuyoImage(config)
    stage = new Stage(config, puyoImage)
  })

  describe('ステージの初期化', () => {
    it('ステージを初期化できる', () => {
      stage.initialize()

      expect(stage.getWidth()).toBe(config.stageWidth)
      expect(stage.getHeight()).toBe(config.stageHeight)
    })

    it('初期化時にすべてのセルが空になる', () => {
      stage.initialize()

      for (let x = 0; x < config.stageWidth; x++) {
        for (let y = 0; y < config.stageHeight; y++) {
          const puyo = stage.getPuyo(x, y)
          expect(puyo.isEmpty()).toBe(true)
        }
      }
    })
  })

  describe('ぷよの配置', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('ぷよを配置できる', () => {
      const puyo = new Puyo(PuyoColor.Red, 2, 3)
      
      stage.setPuyo(2, 3, puyo)
      
      const retrievedPuyo = stage.getPuyo(2, 3)
      expect(retrievedPuyo.getColor()).toBe(PuyoColor.Red)
    })

    it('範囲外の座標には配置できない', () => {
      const puyo = new Puyo(PuyoColor.Blue, -1, 0)
      
      expect(() => stage.setPuyo(-1, 0, puyo)).toThrow()
      expect(() => stage.setPuyo(config.stageWidth, 0, puyo)).toThrow()
      expect(() => stage.setPuyo(0, -1, puyo)).toThrow()
      expect(() => stage.setPuyo(0, config.stageHeight, puyo)).toThrow()
    })

    it('指定した位置が空かどうかを確認できる', () => {
      expect(stage.isEmpty(2, 3)).toBe(true)
      
      const puyo = new Puyo(PuyoColor.Green, 2, 3)
      stage.setPuyo(2, 3, puyo)
      
      expect(stage.isEmpty(2, 3)).toBe(false)
    })
  })

  describe('ぷよの落下処理', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('何もぷよが配置されていない場合、落下する必要がない', () => {
      const needsFall = stage.checkFall()
      
      expect(needsFall).toBe(false)
    })

    it('空中に浮いているぷよがある場合、落下する必要がある', () => {
      const puyo = new Puyo(PuyoColor.Red, 2, 2)
      stage.setPuyo(2, 2, puyo)
      
      const needsFall = stage.checkFall()
      
      expect(needsFall).toBe(true)
    })

    it('底についているぷよは落下する必要がない', () => {
      const puyo = new Puyo(PuyoColor.Blue, 2, config.stageHeight - 1)
      stage.setPuyo(2, config.stageHeight - 1, puyo)
      
      const needsFall = stage.checkFall()
      
      expect(needsFall).toBe(false)
    })

    it('下にぷよがあるぷよは落下する必要がない', () => {
      const bottomPuyo = new Puyo(PuyoColor.Green, 2, config.stageHeight - 1)
      const topPuyo = new Puyo(PuyoColor.Red, 2, config.stageHeight - 2)
      stage.setPuyo(2, config.stageHeight - 1, bottomPuyo)
      stage.setPuyo(2, config.stageHeight - 2, topPuyo)
      
      const needsFall = stage.checkFall()
      
      expect(needsFall).toBe(false)
    })

    it('ぷよを1段落下させることができる', () => {
      const puyo = new Puyo(PuyoColor.Yellow, 2, 2)
      stage.setPuyo(2, 2, puyo)
      
      stage.applyFall()
      
      expect(stage.isEmpty(2, 2)).toBe(true)
      expect(stage.getPuyo(2, 3).getColor()).toBe(PuyoColor.Yellow)
    })
  })

  describe('ぷよの消去処理', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('4個以上の同色ぷよが縦に連結している場合、消去対象として検出される', () => {
      // 縦に4個の赤ぷよを配置
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      const erasableGroups = stage.findErasableGroups()

      expect(erasableGroups.length).toBe(1)
      expect(erasableGroups[0].length).toBe(4)
    })

    it('4個以上の同色ぷよが横に連結している場合、消去対象として検出される', () => {
      // 横に4個の青ぷよを配置
      for (let x = 1; x < 5; x++) {
        const puyo = new Puyo(PuyoColor.Blue, x, 12)
        stage.setPuyo(x, 12, puyo)
      }

      const erasableGroups = stage.findErasableGroups()

      expect(erasableGroups.length).toBe(1)
      expect(erasableGroups[0].length).toBe(4)
    })

    it('L字型に連結した4個以上の同色ぷよが消去対象として検出される', () => {
      // L字型に5個の緑ぷよを配置
      const positions = [
        [2, 10], [2, 11], [2, 12], // 縦の部分
        [3, 12], [4, 12]           // 横の部分
      ]
      
      positions.forEach(([x, y]) => {
        const puyo = new Puyo(PuyoColor.Green, x, y)
        stage.setPuyo(x, y, puyo)
      })

      const erasableGroups = stage.findErasableGroups()

      expect(erasableGroups.length).toBe(1)
      expect(erasableGroups[0].length).toBe(5)
    })

    it('3個以下の同色ぷよは消去対象として検出されない', () => {
      // 3個の黄ぷよを配置
      for (let y = 10; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Yellow, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      const erasableGroups = stage.findErasableGroups()

      expect(erasableGroups.length).toBe(0)
    })

    it('複数の異なる色のぷよグループがそれぞれ消去対象として検出される', () => {
      // 赤ぷよ4個（縦）
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 1, y)
        stage.setPuyo(1, y, puyo)
      }

      // 青ぷよ4個（横）
      for (let x = 2; x < 6; x++) {
        const puyo = new Puyo(PuyoColor.Blue, x, 12)
        stage.setPuyo(x, 12, puyo)
      }

      const erasableGroups = stage.findErasableGroups()

      expect(erasableGroups.length).toBe(2)
    })

    it('消去対象のぷよを実際に消去できる', () => {
      // 4個の赤ぷよを配置
      for (let y = 9; y < 13; y++) {
        const puyo = new Puyo(PuyoColor.Red, 2, y)
        stage.setPuyo(2, y, puyo)
      }

      const erasableGroups = stage.findErasableGroups()
      const eraseCount = stage.erasePuyos(erasableGroups)

      expect(eraseCount).toBe(4)
      for (let y = 9; y < 13; y++) {
        expect(stage.isEmpty(2, y)).toBe(true)
      }
    })
  })
})