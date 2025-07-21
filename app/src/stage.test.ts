import { describe, it, expect, beforeEach } from 'vitest'
import { Stage } from './stage'
import { Config } from './config'
import { PuyoImage } from './puyoimage'
import { Puyo, PuyoColor } from './puyo'

describe('Stage', () => {
  let stage: Stage
  let config: Config
  let puyoImage: PuyoImage

  beforeEach(() => {
    config = new Config()
    puyoImage = new PuyoImage()
    stage = new Stage(config, puyoImage)
  })

  describe('初期化', () => {
    it('ステージを正しく初期化できる', () => {
      stage.initialize()
      
      expect(stage.getWidth()).toBe(6)
      expect(stage.getHeight()).toBe(13)
    })

    it('初期化後は全て空のぷよである', () => {
      stage.initialize()
      
      for (let x = 0; x < stage.getWidth(); x++) {
        for (let y = 0; y < stage.getHeight(); y++) {
          expect(stage.isEmpty(x, y)).toBe(true)
        }
      }
    })
  })

  describe('ぷよの配置と取得', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('ぷよを配置して取得できる', () => {
      const redPuyo = new Puyo(PuyoColor.Red, 2, 3)
      
      stage.setPuyo(2, 3, redPuyo)
      const retrievedPuyo = stage.getPuyo(2, 3)
      
      expect(retrievedPuyo.getColor()).toBe(PuyoColor.Red)
      expect(retrievedPuyo.getX()).toBe(2)
      expect(retrievedPuyo.getY()).toBe(3)
    })

    it('ぷよを配置した位置は空ではない', () => {
      const redPuyo = new Puyo(PuyoColor.Red, 2, 3)
      
      stage.setPuyo(2, 3, redPuyo)
      
      expect(stage.isEmpty(2, 3)).toBe(false)
    })

    it('異なる色のぷよを複数配置できる', () => {
      const redPuyo = new Puyo(PuyoColor.Red, 0, 0)
      const bluePuyo = new Puyo(PuyoColor.Blue, 1, 0)
      
      stage.setPuyo(0, 0, redPuyo)
      stage.setPuyo(1, 0, bluePuyo)
      
      expect(stage.getPuyo(0, 0).getColor()).toBe(PuyoColor.Red)
      expect(stage.getPuyo(1, 0).getColor()).toBe(PuyoColor.Blue)
    })
  })

  describe('範囲外アクセスエラー', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('負の座標でアクセスするとエラーが発生する', () => {
      expect(() => stage.getPuyo(-1, 0)).toThrow('座標が範囲外です: (-1, 0)')
      expect(() => stage.getPuyo(0, -1)).toThrow('座標が範囲外です: (0, -1)')
    })

    it('幅を超える座標でアクセスするとエラーが発生する', () => {
      expect(() => stage.getPuyo(6, 0)).toThrow('座標が範囲外です: (6, 0)')
    })

    it('高さを超える座標でアクセスするとエラーが発生する', () => {
      expect(() => stage.getPuyo(0, 13)).toThrow('座標が範囲外です: (0, 13)')
    })
  })

  describe('座標検証', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('有効な座標を正しく判定する', () => {
      expect(stage.isValidPosition(0, 0)).toBe(true)
      expect(stage.isValidPosition(5, 12)).toBe(true)
      expect(stage.isValidPosition(3, 6)).toBe(true)
    })

    it('無効な座標を正しく判定する', () => {
      expect(stage.isValidPosition(-1, 0)).toBe(false)
      expect(stage.isValidPosition(6, 0)).toBe(false)
      expect(stage.isValidPosition(0, -1)).toBe(false)
      expect(stage.isValidPosition(0, 13)).toBe(false)
    })

    it('範囲外座標ではisEmptyがfalseを返す', () => {
      expect(stage.isEmpty(-1, 0)).toBe(false)
      expect(stage.isEmpty(6, 0)).toBe(false)
      expect(stage.isEmpty(0, -1)).toBe(false)
      expect(stage.isEmpty(0, 13)).toBe(false)
    })
  })

  describe('落下判定', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('空のフィールドでは落下しない', () => {
      expect(stage.checkFall()).toBe(false)
    })

    it('浮いているぷよがあると落下判定がtrueになる', () => {
      // 底から1つ上に浮いているぷよを配置
      const redPuyo = new Puyo(PuyoColor.Red, 2, 11)
      stage.setPuyo(2, 11, redPuyo)
      
      expect(stage.checkFall()).toBe(true)
    })

    it('底についているぷよは落下判定にならない', () => {
      // 底に配置
      const redPuyo = new Puyo(PuyoColor.Red, 2, 12)
      stage.setPuyo(2, 12, redPuyo)
      
      expect(stage.checkFall()).toBe(false)
    })

    it('積み上がったぷよは落下判定にならない', () => {
      // 底から積み上げ
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(2, 11, new Puyo(PuyoColor.Blue, 2, 11))
      
      expect(stage.checkFall()).toBe(false)
    })
  })

  describe('落下処理', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('浮いているぷよが1段落下する', () => {
      // 浮いているぷよを配置
      stage.setPuyo(2, 10, new Puyo(PuyoColor.Red, 2, 10))
      
      stage.applyFall()
      
      expect(stage.isEmpty(2, 10)).toBe(true)
      expect(stage.getPuyo(2, 11).getColor()).toBe(PuyoColor.Red)
    })

    it('複数のぷよが同時に落下する', () => {
      // 複数の浮いているぷよを配置
      stage.setPuyo(1, 10, new Puyo(PuyoColor.Red, 1, 10))
      stage.setPuyo(3, 9, new Puyo(PuyoColor.Blue, 3, 9))
      
      stage.applyFall()
      
      expect(stage.isEmpty(1, 10)).toBe(true)
      expect(stage.getPuyo(1, 11).getColor()).toBe(PuyoColor.Red)
      expect(stage.isEmpty(3, 9)).toBe(true)
      expect(stage.getPuyo(3, 10).getColor()).toBe(PuyoColor.Blue)
    })
  })

  describe('連結ぷよ検出', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('4個未満の連結は消去対象にならない', () => {
      // 3個の赤ぷよを横に並べる
      stage.setPuyo(1, 12, new Puyo(PuyoColor.Red, 1, 12))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(3, 12, new Puyo(PuyoColor.Red, 3, 12))
      
      const groups = stage.findErasableGroups()
      
      expect(groups).toHaveLength(0)
    })

    it('4個の横連結ぷよが検出される', () => {
      // 4個の赤ぷよを横に並べる
      stage.setPuyo(1, 12, new Puyo(PuyoColor.Red, 1, 12))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(3, 12, new Puyo(PuyoColor.Red, 3, 12))
      stage.setPuyo(4, 12, new Puyo(PuyoColor.Red, 4, 12))
      
      const groups = stage.findErasableGroups()
      
      expect(groups).toHaveLength(1)
      expect(groups[0]).toHaveLength(4)
    })

    it('4個の縦連結ぷよが検出される', () => {
      // 4個の赤ぷよを縦に並べる
      stage.setPuyo(2, 9, new Puyo(PuyoColor.Red, 2, 9))
      stage.setPuyo(2, 10, new Puyo(PuyoColor.Red, 2, 10))
      stage.setPuyo(2, 11, new Puyo(PuyoColor.Red, 2, 11))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      
      const groups = stage.findErasableGroups()
      
      expect(groups).toHaveLength(1)
      expect(groups[0]).toHaveLength(4)
    })

    it('L字型の連結ぷよが検出される', () => {
      // L字型の配置
      // R R
      // R
      // R
      stage.setPuyo(0, 10, new Puyo(PuyoColor.Red, 0, 10))
      stage.setPuyo(1, 10, new Puyo(PuyoColor.Red, 1, 10))
      stage.setPuyo(0, 11, new Puyo(PuyoColor.Red, 0, 11))
      stage.setPuyo(0, 12, new Puyo(PuyoColor.Red, 0, 12))
      
      const groups = stage.findErasableGroups()
      
      expect(groups).toHaveLength(1)
      expect(groups[0]).toHaveLength(4)
      expect(groups[0]).toEqual(
        expect.arrayContaining([
          [0, 10], [1, 10], [0, 11], [0, 12]
        ])
      )
    })

    it('異なる色は連結しない', () => {
      // 赤と青が隣接
      stage.setPuyo(1, 12, new Puyo(PuyoColor.Red, 1, 12))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(3, 12, new Puyo(PuyoColor.Blue, 3, 12))
      stage.setPuyo(4, 12, new Puyo(PuyoColor.Blue, 4, 12))
      
      const groups = stage.findErasableGroups()
      
      expect(groups).toHaveLength(0) // どちらも4個未満
    })

    it('複数の独立した連結グループが検出される', () => {
      // 赤の4個連結
      stage.setPuyo(0, 12, new Puyo(PuyoColor.Red, 0, 12))
      stage.setPuyo(1, 12, new Puyo(PuyoColor.Red, 1, 12))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(3, 12, new Puyo(PuyoColor.Red, 3, 12))
      
      // 青の4個連結（別の場所）
      stage.setPuyo(0, 8, new Puyo(PuyoColor.Blue, 0, 8))
      stage.setPuyo(0, 9, new Puyo(PuyoColor.Blue, 0, 9))
      stage.setPuyo(0, 10, new Puyo(PuyoColor.Blue, 0, 10))
      stage.setPuyo(0, 11, new Puyo(PuyoColor.Blue, 0, 11))
      
      const groups = stage.findErasableGroups()
      
      expect(groups).toHaveLength(2)
    })
  })

  describe('ぷよ消去', () => {
    beforeEach(() => {
      stage.initialize()
    })

    it('指定したグループのぷよが消去される', () => {
      // 4個のぷよを配置
      stage.setPuyo(1, 12, new Puyo(PuyoColor.Red, 1, 12))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(3, 12, new Puyo(PuyoColor.Red, 3, 12))
      stage.setPuyo(4, 12, new Puyo(PuyoColor.Red, 4, 12))
      
      const groups = stage.findErasableGroups()
      const erasedCount = stage.erasePuyos(groups)
      
      expect(erasedCount).toBe(4)
      expect(stage.isEmpty(1, 12)).toBe(true)
      expect(stage.isEmpty(2, 12)).toBe(true)
      expect(stage.isEmpty(3, 12)).toBe(true)
      expect(stage.isEmpty(4, 12)).toBe(true)
    })

    it('消去しないぷよは残る', () => {
      // 4個の赤ぷよ（消去対象）
      stage.setPuyo(1, 12, new Puyo(PuyoColor.Red, 1, 12))
      stage.setPuyo(2, 12, new Puyo(PuyoColor.Red, 2, 12))
      stage.setPuyo(3, 12, new Puyo(PuyoColor.Red, 3, 12))
      stage.setPuyo(4, 12, new Puyo(PuyoColor.Red, 4, 12))
      
      // 1個の青ぷよ（消去されない）
      stage.setPuyo(0, 12, new Puyo(PuyoColor.Blue, 0, 12))
      
      const groups = stage.findErasableGroups()
      stage.erasePuyos(groups)
      
      expect(stage.getPuyo(0, 12).getColor()).toBe(PuyoColor.Blue)
    })
  })
})